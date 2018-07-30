{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Test the lexer
module Sonoda.LexerTest where

import Control.Arrow ((>>>))
import Control.Lens ((^?), _Left, _Right)
import Data.Function ((&))
import Data.String.Here (here)
import RIO
import Sonoda.Lexer (lex)
import Sonoda.Test.Code (trimMargin)
import Sonoda.Types
import System.Random.NameCase (CamelName(..))
import Test.Hspec (describe, it)
import Test.Hspec.Expectations (shouldBe)
import Test.SmallCheck.Series (NonNegative(..))
import Test.Tasty.Hspec (Spec)
import qualified RIO.Text as T

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

lexToSuccess :: String -> Maybe [Token]
lexToSuccess x = lex x ^? _Right . to (fmap fst)

lexToFailure :: String -> Maybe TokenPos
lexToFailure x = lex x ^? _Left . _where_

spec_lexical_errors :: Spec
spec_lexical_errors =
  describe "shows where it is failed" $ do
    -- NOTE: "<" is an invalid token
    it "with a trivial code" $ do
      let code = "<"
      lexToFailure code `shouldBe` Just (TokenPos 1 1)
    it "with a casual code" $ do
      let code = "\\x:Nat.<at"
      lexToFailure code `shouldBe` Just (TokenPos 1 8)
    it "with lines" $ do
      let code = [here|\x:T.
                      |  < x
                      |] & trimMargin '|'
      lexToFailure code `shouldBe` Just (TokenPos 2 3)

scprop_natVal_can_be_lexed :: NonNegative Int -> Bool
scprop_natVal_can_be_lexed (NonNegative n) =
  lexToSuccess (show n) == Just [TokenANat n]

spec_identifiers_can_be_lexed :: Spec
spec_identifiers_can_be_lexed = do
  it "finite values" $ do
    lexToSuccess "True"  `shouldBe` Just [TokenAnIdent "True"]
    lexToSuccess "False" `shouldBe` Just [TokenAnIdent "False"]
    lexToSuccess "Unit"  `shouldBe` Just [TokenAnIdent "Unit"]
  it "basic types" $ do
    lexToSuccess "Nat"  `shouldBe` Just [TokenAnIdent "Nat"]
    lexToSuccess "Bool" `shouldBe` Just [TokenAnIdent "Bool"]
  it "alphabet keywords" $ do
    lexToSuccess "if"   `shouldBe` Just [TokenAnIdent "if"]
    lexToSuccess "then" `shouldBe` Just [TokenAnIdent "then"]
    lexToSuccess "else" `shouldBe` Just [TokenAnIdent "else"]
  it "with underscores" $ do
    lexToSuccess "_"       `shouldBe` Just [TokenAnIdent "_"]
    lexToSuccess "_if"   `shouldBe` Just [TokenAnIdent "_if"]
    lexToSuccess "then_" `shouldBe` Just [TokenAnIdent "then_"]
    lexToSuccess "else_let" `shouldBe` Just [TokenAnIdent "else_let"]

-- | Subspecieses of identifiers
prop_variables_can_be_lexed :: CamelName -> Bool
prop_variables_can_be_lexed (unCamelName >>> T.unpack -> ident)
  = lexToSuccess ident == Just [TokenAnIdent ident]

spec_types_can_be_lexed :: Spec
spec_types_can_be_lexed =
  it "arrow types" $
    lexToSuccess "Nat -> Bool" `shouldBe` Just [TokenAnIdent "Nat", TokenArrow, TokenAnIdent "Bool"]

spec_lambda_abstractions_can_be_lexed :: Spec
spec_lambda_abstractions_can_be_lexed =
  it "lambda abstractions" $
    lexToSuccess "\\x:Unit.10" `shouldBe` Just [ TokenBackslash, TokenAnIdent "x"
                                               , TokenColon, TokenAnIdent "Unit"
                                               , TokenDot, TokenANat 10
                                               ]
