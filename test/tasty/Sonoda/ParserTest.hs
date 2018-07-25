{-# LANGUAGE QuasiQuotes #-}

-- | Expect to success the parsers with the lexers
module Sonoda.ParserTest where

import Control.Arrow ((>>>))
import Control.Lens ((^?), _Left)
import Control.Monad ((<=<))
import Data.Semigroup ((<>))
import Data.String.Here (here)
import RIO
import Sonoda.Test.Code (trimMargin)
import Sonoda.Types
import System.Random.NameCase (CamelName(..))
import Test.Hspec (describe, it)
import Test.Hspec.Expectations (shouldBe)
import Test.SmallCheck.Series (NonNegative(..))
import Test.Tasty.Hspec (Spec)
import qualified RIO.Text as T
import qualified Sonoda.Lexer as SL
import qualified Sonoda.Parser as SP
import qualified Sonoda.Types as ST

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- |
-- Execute the lexer and the parser with a code.
-- If something is failed, return what fails and where it fails.
readExpr :: String -> Either Failure Expr
readExpr = SP.parseExpr <=< SL.lex

spec_parse_errors :: Spec
spec_parse_errors =
  describe "shows where it is failed" $ do
    -- These expects to be passed the lexing, and to be not passed the parsing
    it "with a casual code" $
      readExpr "\\x:Nat.Nat" ^? _Left . _where_ `shouldBe` Just (TokenPos 1 8)
    it "with lines" $ do
      let code = [here|\x:Nat.
                      |  Nat
                      |] & trimMargin '|'
      readExpr code ^? _Left . _where_ `shouldBe` Just (TokenPos 2 3)

scprop_natVal_can_be_parsed :: NonNegative Int -> Bool
scprop_natVal_can_be_parsed (NonNegative n) =
  readExpr (show n) == Right (ExprAtomic . TermNat $ Nat n)

spec_boolVal_unitVal_can_be_parsed :: Spec
spec_boolVal_unitVal_can_be_parsed = do
  it "True,False" $ do
    readExpr "True"  `shouldBe` Right (ExprAtomic $ TermBool True)
    readExpr "False" `shouldBe` Right (ExprAtomic $ TermBool False)
  it "Unit" $
    readExpr "Unit" `shouldBe` Right (ExprAtomic TermUnit)

prop_identifiers_can_be_parsed :: CamelName -> Bool
prop_identifiers_can_be_parsed (unCamelName >>> T.unpack -> ident)
  = readExpr ident == Right (ExprIdent ident)

spec_keyword_prefix_identifiers_can_be_parsed :: Spec
spec_keyword_prefix_identifiers_can_be_parsed =
  it "keyword like identifiers are valid identifiers" $ do
    readExpr "if_"          `shouldBe` Right (ExprIdent "if_")
    readExpr "thenTrue"     `shouldBe` Right (ExprIdent "thenTrue")
    readExpr "else06210800" `shouldBe` Right (ExprIdent "else06210800")

-- | Make a lambda abstraction that returns the argument immediately
identicalAbst :: Identifier -> Type -> Expr
identicalAbst x t = ExprLambda x t $ ExprIdent x

spec_lambda_abstractions_can_be_parsed :: Spec
spec_lambda_abstractions_can_be_parsed = do
  it "abstractions" $ do
    readExpr "\\n:Nat.10" `shouldBe` Right (ExprLambda "n" natT $ ST.nat 10)
    readExpr "\\n:Nat.n"  `shouldBe` Right (identicalAbst "n" natT)
    readExpr "\\x:Bool.x" `shouldBe` Right (identicalAbst "x" boolT)
    readExpr "\\x:Unit.x" `shouldBe` Right (identicalAbst "x" unitT)
    readExpr "\\x:Unit.(x)" `shouldBe` Right (ExprLambda "x" unitT . ExprParens $ ExprIdent "x")
    readExpr "\\s:Unit -> Unit.\\z:Unit.z" `shouldBe` Right (churchNum "s" "z" 0)
    readExpr "\\s:Unit -> Unit.\\z:Unit.s z" `shouldBe` Right (churchNum "s" "z" 1)
    readExpr "\\s:Unit -> Unit.\\z:Unit.s (s z)" `shouldBe` Right (churchNum "s" "z" 2)
    readExpr "\\x:Unit.x" `shouldBe` Right (identicalAbst "x" unitT)
  describe "abstractions and types" $ do
    it "atomic types" $ do
      readExpr "\\x:Nat.x"  `shouldBe` Right (identicalAbst "x" natT)
      readExpr "\\x:Bool.x" `shouldBe` Right (identicalAbst "x" boolT)
      readExpr "\\x:Unit.x" `shouldBe` Right (identicalAbst "x" unitT)
    it "arrow types" $ do
        readExpr "\\x:Nat -> Nat.x" `shouldBe` Right (identicalAbst "x" $ natT ~> natT)
        readExpr "\\x:Nat -> Bool -> Unit.x" `shouldBe` Right (identicalAbst "x" $ natT ~> boolT ~> unitT)
        readExpr "\\x:(Nat -> Bool) -> Unit.x" `shouldBe` Right (identicalAbst "x" $ TypeParens (natT ~> boolT) ~> unitT)
  where
    churchNum :: String -> String -> Int -> Expr
    churchNum s z n =
      let num = replicate n $ ExprIdent s
      in ExprLambda s (unitT ~> unitT) . ExprLambda z unitT $
          foldr ExprApply (ExprIdent z) num

spec_function_applications_can_be_parsed :: Spec
spec_function_applications_can_be_parsed = do
  it "applications" $ do
    let t = ExprParens $ identicalAbst "x" natT
    readExpr "(\\x:Nat.x) 10" `shouldBe` Right (t `ExprApply` ST.nat 10)
  it "real world codes" $
    readExpr code `shouldBe` Right exprOfCode
  where
    code =
      "\\plus:Nat -> Nat -> Nat. \\n : Nat. \\m : Nat.\n" <>
      "   if equal m 0\n" <>
      "     then n\n" <>
      "     else plus (succ n) (pred m)"

    exprOfCode =
      ExprLambda "plus" (natT ~> natT ~> natT) . ExprLambda "n" natT . ExprLambda "m" natT $
        if_ condClause
          thenClause
          elseClause

    -- "equals m 0" maybe parsed as like "(equals m) 0", may not be "equals (m 0)"
    condClause :: Expr
    condClause = (ExprIdent "equal" `ExprApply` ExprIdent "m") `ExprApply` ST.nat 0
    thenClause :: Expr
    thenClause = ExprIdent "n"
    elseClause :: Expr
    elseClause = (ExprIdent "plus" `ExprApply` (ExprIdent "succ" `ExprApply` ExprIdent "n" ))
                                   `ExprApply` (ExprIdent "pred" `ExprApply` ExprIdent "m")

spec_syntax_can_be_parsed :: Spec
spec_syntax_can_be_parsed =
  describe "if" $
    it "with basic terms" $ do
      readExpr "if True then 10 else 20" `shouldBe`
        Right (if_ (ST.bool True) (ST.nat 10)
                               (ST.nat 20))
      readExpr "if isZero 0 then 10 else 20" `shouldBe`
        Right (if_ (ExprIdent "isZero" `ExprApply` ST.nat 0)
                (ST.nat 10)
                (ST.nat 20))
      readExpr "if False then succ 0 else pred 1" `shouldBe`
        Right (if_ (ST.bool False)
                (ExprIdent "succ" `ExprApply` ST.nat 0)
                (ExprIdent "pred" `ExprApply` ST.nat 1))
