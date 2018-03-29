{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Expect to success the parsers
module Sonoda.ParserTest where

import Control.Arrow ((>>>), (&&&))
import Data.Semigroup ((<>))
import Sonoda.Parser
import Sonoda.Types
import System.Random.NameCase (CamelName(..))
import Test.Hspec (describe, it, expectationFailure)
import Test.Hspec.Expectations (Expectation, HasCallStack, shouldBe)
import Test.SmallCheck.Series (NonNegative(..))
import Test.Tasty.Hspec (Spec)
import qualified Data.Text as T

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- |
-- Expect the `Either SomeException a` is `Right`,
-- also the `a` equals a second `a`
(^==) :: Eq a => Either e a -> a -> Bool
(Left  _) ^== _ = False
(Right x) ^== y = x == y

-- | Similar to '^==' but it is in hspec
shouldBeParsedTo :: (HasCallStack, Show e, Show a, Eq a) => Either e a -> a -> Expectation
shouldBeParsedTo (Left  e) _ = expectationFailure $ "a parse is failed: " <> show e
shouldBeParsedTo (Right x) y = x `shouldBe` y


scprop_natVal_can_be_parsed_correctly :: NonNegative Int -> Bool
scprop_natVal_can_be_parsed_correctly (NonNegative n) =
  parseExpr (T.pack $ show n) ^== ExprAtomic (TermNat $ Nat n)


spec_boolVal_unitVal_can_be_parsed_correctly :: Spec
spec_boolVal_unitVal_can_be_parsed_correctly = do
  it "True,False" $ do
    parseExpr "True"  `shouldBeParsedTo` ExprAtomic (TermBool True)
    parseExpr "False" `shouldBeParsedTo` ExprAtomic (TermBool False)
  it "Unit" $
    parseExpr "Unit" `shouldBeParsedTo` ExprAtomic TermUnit


prop_identifiers_can_be_parsed_correctly :: CamelName -> Bool
prop_identifiers_can_be_parsed_correctly (unCamelName &&& (unCamelName >>> T.unpack) -> (ident, ident'))
  = parseExpr ident ^== ExprIdent ident'

spec_keyword_prefix_identifiers_can_be_parsed_correctly :: Spec
spec_keyword_prefix_identifiers_can_be_parsed_correctly =
  it "identifiers like `if_`, `thenTrue`, `else06210800` are valid identifiers" $ do
    parseExpr "if_"          `shouldBeParsedTo` ExprIdent "if_"
    parseExpr "thenTrue"     `shouldBeParsedTo` ExprIdent "thenTrue"
    parseExpr "else06210800" `shouldBeParsedTo` ExprIdent "else06210800"


spec_types_can_be_parsed_correctly :: Spec
spec_types_can_be_parsed_correctly = do
  describe "atomic types" $ do
    it "Nat"  $ parseType "Nat"  `shouldBeParsedTo` natT
    it "Bool" $ parseType "Bool" `shouldBeParsedTo` boolT
    it "Unit" $ parseType "Unit" `shouldBeParsedTo` unitT
  describe "arrow types" $ do
    it "Nat -> Nat" $
      parseType "Nat -> Nat" `shouldBeParsedTo` (natT ~> natT)
    it "Nat -> Bool -> Unit" $
      parseType "Nat -> Bool -> Unit" `shouldBeParsedTo` (natT ~> boolT ~> unitT)
    it "(Nat -> Bool) -> Unit" $
      parseType "(Nat -> Bool) -> Unit" `shouldBeParsedTo` (TypeParens (natT ~> boolT) ~> unitT)
  describe "equalities" $ do
    it "(a) ~ a"   $ parseType "(Nat)"   `shouldBeParsedTo` natT
    it "((a)) ~ a" $ parseType "((Nat))" `shouldBeParsedTo` natT
    it "a -> (b) ~ a -> b" $ do
      parseType "Nat -> (Bool -> Unit)" `shouldBeParsedTo` (natT ~> boolT ~> unitT)
      parseType "(Nat -> Unit) -> (Unit -> Bool)" `shouldBeParsedTo` (TypeParens (natT ~> unitT) ~> unitT ~> boolT)


spec_lambda_forms_can_be_parsed_correctly :: Spec
spec_lambda_forms_can_be_parsed_correctly = do
  it "abstractions" $ do
    parseExpr "\\n:Nat.10" `shouldBeParsedTo` ExprLambda "n" natT (nat 10)
    parseExpr "\\n:Nat.n"  `shouldBeParsedTo` trivial "n" natT
    parseExpr "\\x:Bool.x" `shouldBeParsedTo` trivial "x" boolT
    parseExpr "\\x:Unit.x" `shouldBeParsedTo` trivial "x" unitT
    parseExpr "\\s:Unit -> Unit.\\z:Unit.z" `shouldBeParsedTo` churchNum "s" "z" 0
    parseExpr "\\s:Unit -> Unit.\\z:Unit.s z" `shouldBeParsedTo` churchNum "s" "z" 1
    parseExpr "\\s:Unit -> Unit.\\z:Unit.s (s z)" `shouldBeParsedTo` churchNum "s" "z" 2
    parseExpr "\\x:Unit.x" `shouldBeParsedTo` trivial "x" unitT
  it "applications" $
    parseExpr "(\\x:Nat.x) 10" `shouldBeParsedTo` (\$) (ExprParens $ trivial "x" natT) (nat 10)
  it "real world codes" $
    parseExpr code `shouldBeParsedTo` exprOfCode
  where
    trivial :: Identifier -> Type -> Expr
    trivial x t = ExprLambda x t $ ExprIdent x

    churchNum :: String -> String -> Int -> Expr
    churchNum s z n =
      let num = replicate n $ ExprIdent s
      in ExprLambda s (unitT ~> unitT) . ExprLambda z unitT $
          foldr (\$) (ExprIdent z) num

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
    condClause = (ExprIdent "equal" \$ ExprIdent "m") \$ nat 0
    thenClause :: Expr
    thenClause = ExprIdent "n"
    elseClause :: Expr
    elseClause = (ExprIdent "plus" \$ (ExprIdent "succ" \$ ExprIdent "n" ))
                                   \$ (ExprIdent "pred" \$ ExprIdent "m")


spec_syntax_can_be_parsed_correctly :: Spec
spec_syntax_can_be_parsed_correctly =
  describe "if" $
    it "with basic terms" $ do
      parseExpr "if True then 10 else 20" `shouldBeParsedTo`
        if_ (bool True) (nat 10)
                        (nat 20)
      parseExpr "if isZero 0 then 10 else 20" `shouldBeParsedTo`
        if_ (ExprIdent "isZero" \$ nat 0) (nat 10)
                                          (nat 20)
      parseExpr "if False then succ 0 else pred 1" `shouldBeParsedTo`
        if_ (bool False) (ExprIdent "succ" \$ nat 0)
                         (ExprIdent "pred" \$ nat 1)
