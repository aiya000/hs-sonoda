{-# LANGUAGE QuasiQuotes #-}

-- | Expect to success the parsers
module Sonoda.ParserTest where

import Control.Arrow ((>>>))
import Control.Lens ((^?), _Left)
import Control.Monad ((<=<))
import Data.Default (def)
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
  where
    -- Execute the lexer and the parser with a code.
    -- If something is failed, return what fails and where it fails.
    readExpr :: String -> Either Failure Expr
    readExpr = SP.parseExpr <=< SL.lex

-- | Simular to 'SP.parseExpr', but the source and the result are without 'TokenPos'
parseExpr :: [Token] -> Either Failure Expr
parseExpr = SP.parseExpr . map (, def)

scprop_natVal_can_be_parsed :: NonNegative Int -> Bool
scprop_natVal_can_be_parsed (NonNegative n) =
  parseExpr [TokenANat n] == Right (natExpr n)
    where
      natExpr = ExprAtomic . TermNat . Nat

spec_boolVal_unitVal_can_be_parsed :: Spec
spec_boolVal_unitVal_can_be_parsed = do
  it "True,False" $ do
    parseExpr [TokenAnIdent "True"] `shouldBe` Right (ExprAtomic $ TermBool True)
    parseExpr [TokenAnIdent "False"] `shouldBe` Right (ExprAtomic $ TermBool False)
  it "Unit" $
    parseExpr [TokenAnIdent "Unit"] `shouldBe` Right (ExprAtomic TermUnit)

prop_identifiers_can_be_parsed :: CamelName -> Bool
prop_identifiers_can_be_parsed (unCamelName >>> T.unpack -> ident)
  = parseExpr [TokenAnIdent ident] == Right (ExprIdent ident)

spec_lambda_abstractions_can_be_parsed :: Spec
spec_lambda_abstractions_can_be_parsed = do
  it "basic abstractions" $ do
    -- \n:Nat.10
    parseExpr (tokensLambda "n" [tokenNat] [TokenANat 10])
      `shouldBe` Right (ExprLambda "n" natT $ natE 10)
    -- \x:Unit.x
    parseExpr (tokensLambda "x" [tokenUnit] [TokenAnIdent "x"])
      `shouldBe` Right (ExprLambda "x" unitT $ ExprIdent "x")
    -- \x:Bool -> Bool.x
    parseExpr (tokensLambda "x" [tokenBool, o, tokenBool] [TokenAnIdent "x"])
      `shouldBe` Right (ExprLambda "x" (boolT ~> boolT) $ ExprIdent "x")
    -- \x:Nat -> Bool -> Unit.x
    parseExpr (tokensLambda "x" [tokenNat, o, tokenBool, o, tokenUnit] [TokenAnIdent "x"])
      `shouldBe` Right (ExprLambda "x" (natT ~> boolT ~> unitT) $ ExprIdent "x")

    -- \x:(Nat -> Bool) -> Unit.x
    let f = tokensLambda "x"
              [TokenParensBegin, tokenNat, o, tokenBool, TokenParensEnd, o, tokenUnit]
              [TokenAnIdent "x"]
        f' = ExprLambda "x"
              (TypeParens (natT ~> boolT) ~> unitT)
              $ ExprIdent "x"
    parseExpr f `shouldBe` Right f'

    -- \s:Unit -> Unit.\z:Unit.z
    let zz   = tokensLambda "z" [tokenUnit] [TokenAnIdent "z"]
        zero = tokensLambda "s" [tokenUnit, o, tokenUnit] zz
    let zz'   = ExprLambda "z" unitT $ ExprIdent "x"
        zero' = ExprLambda "s" (unitT ~> unitT) zz'
    parseExpr zero `shouldBe` Right zero'

  it "abstractions with removable notations" $ do
    -- \x:Unit.((x))  ==>  \x:Unit.x
    let f = tokensLambda "x" [tokenUnit]
              [TokenParensBegin, TokenParensBegin, TokenAnIdent "x", TokenParensEnd, TokenParensEnd]
        f' = ExprLambda "x" unitT
              $ ExprIdent "x"
    parseExpr f `shouldBe` Right f'
    -- \x:Nat -> (Bool -> Unit).x  ==>  \x:Nat -> Bool -> Unit.x
    let f1 = tokensLambda "x"
                [tokenNat, o, TokenParensBegin, tokenBool, o, tokenUnit, TokenParensEnd]
                [TokenAnIdent "x"]
        f1' = ExprLambda "x"
                (natT ~> boolT ~> unitT)
                $ ExprIdent "x"
    parseExpr f1 `shouldBe` Right f1'
  where
    -- Maybe, `tokenNat o tokenNat` looks like `tokenNat -> tokenNat`
    o = TokenArrow

    natE :: Int -> Expr
    natE = ExprAtomic . TermNat . Nat

    natT :: Type
    natT = TypeAtomic TypeNat

    boolT :: Type
    boolT = TypeAtomic TypeBool

    unitT :: Type
    unitT = TypeAtomic TypeUnit

    (~>) :: Type -> Type -> Type
    (~>) = TypeArrow
    infixr 9 ~>

    tokenNat  = TokenAnIdent "Nat"
    tokenUnit = TokenAnIdent "Unit"
    tokenBool = TokenAnIdent "Bool"

    tokensLambda :: String -> [Token] -> [Token] -> [Token]
    tokensLambda ident type_ body = [TokenBackslash, TokenAnIdent ident, TokenColon] <> type_ <> [TokenDot] <> body

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
