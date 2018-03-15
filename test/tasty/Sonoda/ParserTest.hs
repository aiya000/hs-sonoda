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
(^==) :: Eq a => Either e a -> a -> Bool
(Left  _) ^== _ = False
(Right x) ^== y = x == y

-- | Similar to '^==' but it is in hspec
isParsedTo :: (HasCallStack, Show a, Eq a) => Either e a -> a -> Expectation
isParsedTo (Left  _) _ = pending
isParsedTo (Right x) y = x `shouldBe` y


scprop_natVal_can_be_parsed_correctly :: NonNegative Int -> Bool
scprop_natVal_can_be_parsed_correctly (NonNegative n) =
  parseExpr (T.pack $ show n) ^== ExprAtomic (TermNat $ Nat n)

spec_boolVal_unitVal :: Spec
spec_boolVal_unitVal =
  describe "can be parsed correctly" $ do
    it "True,False" $ do
      parseExpr "True"  `isParsedTo` ExprAtomic (TermBool True)
      parseExpr "False" `isParsedTo` ExprAtomic (TermBool False)
    it "Unit" $
      parseExpr "Unit" `isParsedTo` ExprAtomic TermUnit
