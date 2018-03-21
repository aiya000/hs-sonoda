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
  = parseExpr ident ^== ExprLambda (LambdaIdent ident')


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
    parseExpr "\\n:Nat.n"  `shouldBeParsedTo` trivial "n" natT
    parseExpr "\\x:Bool.x" `shouldBeParsedTo` trivial "x" boolT
    parseExpr "\\x:Unit.x" `shouldBeParsedTo` trivial "x" unitT
  it "real world codes" $
    parseExpr code `shouldBeParsedTo` exprOfCode
  where
    trivial :: Identifier -> Type -> Expr
    trivial x t = lambda x t $ ident x

    churchNum :: String -> String -> Int -> Expr
    churchNum s z n =
      let num = replicate n $ LambdaIdent s
      in lambda s (unitT ~> unitT) . lambda z unitT $
          foldr ((ExprParens.) . apply) (ident z) num

    code =
      "\\plus:Nat -> Nat -> Nat. \\n : Nat. \\m : Nat.\n" <>
      "   if equal m 0\n" <>
      "     then n\n" <>
      "     else plus (succ n) (pred m)"

    exprOfCode =
      lambda "plus" (natT ~> natT ~> natT) . lambda "n" natT . lambda "m" natT $
        if_ condClause
          thenClause
          elseClause

    condClause :: Expr
    condClause = ExprLambda $ LambdaIdent "equal" \$ ident "m" \$ nat 0
    thenClause = ident "n"
    elseClause =
      ExprLambda $ LambdaIdent "plus"
                \$ (ExprLambda $ LambdaIdent "succ" \$ ident "n" )
                \$ (ExprLambda $ LambdaIdent "pred" \$ ident "m")


--spec_syntax_can_be_parsed_correctly :: Spec
--spec_syntax_can_be_parsed_correctly =
--  describe "if" $ do
--    it "can be parsed with any arguments" $ do
--      parseExpr "if (
