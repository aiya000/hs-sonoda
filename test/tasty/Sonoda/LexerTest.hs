{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Expose for the lexer
module Sonoda.LexerTest where

import Control.Arrow ((>>>))
import Control.Lens ((^?), _Left, _Right)
import Data.Function ((&))
import Data.String.Here (i, here)
import RIO
import Sonoda.Lexer (lex)
import Sonoda.Types
import System.Random.NameCase (CamelName(..))
import Test.Hspec (describe, it)
import Test.Hspec.Expectations (shouldBe)
import Test.SmallCheck.Series (NonNegative(..))
import Test.Tasty.Hspec (Spec)
import qualified RIO.Text as T

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

lexAndSuccess :: String -> Maybe [Token]
lexAndSuccess x = lex x ^? _Right . to (fmap fst)

lexAndFailure :: String -> Maybe TokenPos
lexAndFailure x = lex x ^? _Left . _where_

spec_lexical_errors :: Spec
spec_lexical_errors =
  describe "shows where it is failed" $ do
    -- NOTE: "<" is an invalid token
    it "at a first token" $ do
      let invalidIdent = "<"
      lexAndFailure invalidIdent `shouldBe` Just (TokenPos 1 1)
    it "on a code" $ do
      let code = [here|\x:T.
                      |  < x
                      |] & trimMargin '|'
      lexAndFailure code `shouldBe` Just (TokenPos 2 3)
  where
    dropHead :: [a] -> [a]
    dropHead [] = []
    dropHead (_:xs) = xs

    trimMargin :: Char -> String -> String
    trimMargin _ (lines -> []) = ""
    trimMargin delim (lines -> (firstLine:tailLines)) =
      let removeMargin = dropHead . dropWhile (/= delim) -- remove before '|' and '|'
      in unlines $ firstLine : fmap removeMargin tailLines
    trimMargin _ _ = error [i|${(__FILE__ :: String)}:L${(__LINE__ :: Int)}: fatal error! Sorry, please report an issue :(|]

scprop_natVal_can_be_lexed :: NonNegative Int -> Bool
scprop_natVal_can_be_lexed (NonNegative n) =
  lexAndSuccess (show n) == Just [TokenANat n]

spec_identifiers_can_be_lexed :: Spec
spec_identifiers_can_be_lexed = do
  it "finite values" $ do
    lexAndSuccess "True"  `shouldBe` Just [TokenAnIdent "True"]
    lexAndSuccess "False" `shouldBe` Just [TokenAnIdent "False"]
    lexAndSuccess "Unit"  `shouldBe` Just [TokenAnIdent "Unit"]
  it "basic types" $ do
    lexAndSuccess "Nat"  `shouldBe` Just [TokenAnIdent "Nat"]
    lexAndSuccess "Bool" `shouldBe` Just [TokenAnIdent "Bool"]
  it "alphabet keywords" $ do
    lexAndSuccess "if"   `shouldBe` Just [TokenAnIdent "if"]
    lexAndSuccess "then" `shouldBe` Just [TokenAnIdent "then"]
    lexAndSuccess "else" `shouldBe` Just [TokenAnIdent "else"]
  it "with underscores" $ do
    lexAndSuccess "_"       `shouldBe` Just [TokenAnIdent "_"]
    lexAndSuccess "_blue"   `shouldBe` Just [TokenAnIdent "_blue"]
    lexAndSuccess "yellow_" `shouldBe` Just [TokenAnIdent "yellow_"]
    lexAndSuccess "pastel_purple" `shouldBe` Just [TokenAnIdent "pastel_purple"]

-- | Subspecieses of identifiers
prop_variables_can_be_lexed :: CamelName -> Bool
prop_variables_can_be_lexed (unCamelName >>> T.unpack -> ident)
  = lexAndSuccess ident == Just [TokenAnIdent ident]

spec_types_can_be_lexed :: Spec
spec_types_can_be_lexed =
  it "arrow types" $
    lexAndSuccess "Nat -> Bool" `shouldBe` Just [TokenAnIdent "Nat", TokenArrow, TokenAnIdent "Bool"]

spec_lambda_abstractions_can_be_lexed :: Spec
spec_lambda_abstractions_can_be_lexed =
  it "lambda abstractions" $
    lexAndSuccess "\\x:Unit.10" `shouldBe` Just [ TokenBackslash, TokenAnIdent "x"
                                                , TokenColon, TokenAnIdent "Unit"
                                                , TokenDot, TokenANat 10
                                                ]
