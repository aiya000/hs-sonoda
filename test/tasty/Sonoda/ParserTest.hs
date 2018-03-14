{-# LANGUAGE OverloadedStrings #-}

-- | Expect to success the parsers
module Sonoda.ParserTest where

import Sonoda.Parser
import Sonoda.Types
import Test.Hspec (describe, it, pending)
import Test.Hspec.Expectations (Expectation, HasCallStack, shouldBe)
import Test.SmallCheck.Series (NonNegative(..))
import Test.Tasty.Hspec (Spec)
import qualified Data.Text as T

-- |
-- Expect the `Either SomeException a` is `Right`,
-- also the `a` equals a second `a`
(?==) :: Eq a => Either e a -> a -> Bool
(Left  _) ?== _ = False
(Right x) ?== y = x == y

-- | Similar to '?==' but it is in hspec
rightShouldBe :: (HasCallStack, Show a, Eq a) => Either e a -> a -> Expectation
rightShouldBe (Left  _) _ = pending
rightShouldBe (Right x) y = x `shouldBe` y


scprop_the_expression_rules_for_natVal :: NonNegative Int -> Bool
scprop_the_expression_rules_for_natVal (NonNegative n) =
  parseExpr (T.pack $ show n) ?== ExprAtomic (TermNat $ Nat n)

spec_the_expression_rules_boolVal_unitVal :: Spec
spec_the_expression_rules_boolVal_unitVal = do
  describe "boolVal" $
    it "True,False can be parsed correctly" $ do
      parseExpr "True"  `rightShouldBe` ExprAtomic (TermBool True)
      parseExpr "False" `rightShouldBe` ExprAtomic (TermBool False)
  describe "unitVal" $
    it "Unit can be parsed correctly" $
      parseExpr "Unit" `rightShouldBe` ExprAtomic TermUnit
