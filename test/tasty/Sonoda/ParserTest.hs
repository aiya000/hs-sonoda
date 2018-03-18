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
isParsedTo :: (HasCallStack, Show e, Show a, Eq a) => Either e a -> a -> Expectation
isParsedTo (Left  e) _ = expectationFailure $ "a parse is failed: " <> show e
isParsedTo (Right x) y = x `shouldBe` y


scprop_natVal_can_be_parsed_correctly :: NonNegative Int -> Bool
scprop_natVal_can_be_parsed_correctly (NonNegative n) =
  parseExpr (T.pack $ show n) ^== ExprAtomic (TermNat $ Nat n)


spec_boolVal_unitVal_can_be_parsed_correctly :: Spec
spec_boolVal_unitVal_can_be_parsed_correctly = do
  it "True,False" $ do
    parseExpr "True"  `isParsedTo` ExprAtomic (TermBool True)
    parseExpr "False" `isParsedTo` ExprAtomic (TermBool False)
  it "Unit" $
    parseExpr "Unit" `isParsedTo` ExprAtomic TermUnit


prop_identifiers_can_be_parsed_correctly :: CamelName -> Bool
prop_identifiers_can_be_parsed_correctly (unCamelName &&& (unCamelName >>> T.unpack) -> (ident, ident'))
  = parseExpr ident ^== ExprLambda (LambdaIdent ident')


spec_types_can_be_parsed_correctly :: Spec
spec_types_can_be_parsed_correctly = do
  describe "atomic types" $ do
    it "Nat"  $ parseType "Nat"  `isParsedTo` natT
    it "Bool" $ parseType "Bool" `isParsedTo` boolT
    it "Unit" $ parseType "Unit" `isParsedTo` unitT
  describe "arrow types" $ do
    it "Nat -> Nat" $
      parseType "Nat -> Nat" `isParsedTo` (natT ~> natT)
    it "Nat -> Bool -> Unit" $
      parseType "Nat -> Bool -> Unit" `isParsedTo` (natT ~> boolT ~> unitT)
    it "(Nat -> Bool) -> Unit" $
      parseType "(Nat -> Bool) -> Unit" `isParsedTo` (TypeParens (natT ~> boolT) ~> unitT)
    it "Nat -> (Bool -> Unit)" $
      parseType "Nat -> (Bool -> Unit)" `isParsedTo` (natT ~> boolT ~> unitT)
    it "(Nat -> Unit) -> (Unit -> Bool)" $
      parseType "(Nat -> Unit) -> (Unit -> Bool)" `isParsedTo` ((natT ~> unitT) ~> (unitT ~> natT))


spec_lambda_abstractions_can_be_parsed_correctly :: Spec
spec_lambda_abstractions_can_be_parsed_correctly = do
  it "basic forms" $ do
    parseExpr "\\n:Nat.n"  `isParsedTo` trivial "n" natT
    parseExpr "\\x:Bool.x" `isParsedTo` trivial "x" boolT
    parseExpr "\\x:Unit.x" `isParsedTo` trivial "x" unitT
  it "programmatic codes" $
    parseExpr programmaticCode `isParsedTo` programmaticExpr
  where
    trivial :: Identifier -> Type -> Expr
    trivial x t = ExprLambda $ LambdaAbst x t (ExprLambda $ LambdaIdent x)

    programmaticCode =
      "\\plus:Nat -> Nat -> Nat\\n : Nat. \\m : Nat.\n" <>
      "   if equal m 0\n" <>
      "     then n\n" <>
      "     else plus (succ n) (pred m)"

    programmaticExpr =
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
