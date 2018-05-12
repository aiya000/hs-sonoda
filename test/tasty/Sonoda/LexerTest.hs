{-# LANGUAGE ViewPatterns #-}

-- | Expose for the lexer
module Sonoda.LexerTest where

import Control.Arrow ((>>>))
import Prelude hiding (lex)
import Sonoda.Lexer (lex)
import Sonoda.Types
import System.Random.NameCase (CamelName(..))
import Test.Hspec (it)
import Test.Hspec.Expectations (shouldBe)
import Test.SmallCheck.Series (NonNegative(..))
import Test.Tasty.Hspec (Spec)
import qualified RIO.Text as T

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

scprop_natVal_can_be_lexed :: NonNegative Int -> Bool
scprop_natVal_can_be_lexed (NonNegative n) =
  let n' = show n
  in lex n' == Right [TokenANat n]

spec_identifiers_can_be_lexed :: Spec
spec_identifiers_can_be_lexed = do
  it "finite values" $ do
    lex "True"  `shouldBe` Right [TokenAnIdent "True"]
    lex "False" `shouldBe` Right [TokenAnIdent "False"]
    lex "Unit" `shouldBe` Right [TokenAnIdent "Unit"]
  it "basic types" $ do
    lex "Nat" `shouldBe` Right [TokenAnIdent "Nat"]
    lex "Bool" `shouldBe` Right [TokenAnIdent "Bool"]
  it "alphabet keywords" $ do
    lex "if" `shouldBe` Right [TokenAnIdent "if"]
    lex "then" `shouldBe` Right [TokenAnIdent "then"]
    lex "else" `shouldBe` Right [TokenAnIdent "else"]
  it "with underscores" $ do
    lex "_" `shouldBe` Right [TokenAnIdent "_"]
    lex "_blue" `shouldBe` Right [TokenAnIdent "_blue"]
    lex "yellow_" `shouldBe` Right [TokenAnIdent "yellow_"]
    lex "pastel_purple" `shouldBe` Right [TokenAnIdent "pastel_purple"]

-- | Subspecieses of identifiers
prop_variables_can_be_lexed :: CamelName -> Bool
prop_variables_can_be_lexed (unCamelName >>> T.unpack -> ident)
  = lex ident == Right [TokenAnIdent ident]

spec_types_can_be_lexed :: Spec
spec_types_can_be_lexed =
  it "arrow types" $
    lex "Nat -> Bool" `shouldBe` Right [TokenAnIdent "Nat", TokenArrow, TokenAnIdent "Bool"]

spec_lambda_abstractions_can_be_lexed :: Spec
spec_lambda_abstractions_can_be_lexed =
  it "lambda abstractions" $
    lex "\\x:Unit.10" `shouldBe` Right [ TokenBackslash, TokenAnIdent "x"
                                       , TokenColon, TokenAnIdent "Unit"
                                       , TokenANat 10
                                       ]
