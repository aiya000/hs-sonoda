{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Sonoda.Lexer
  ( lex
  ) where

import Data.String.Here (i)
import RIO
import Safe (readMay)
import Sonoda.Types.Lexer (Token(..))
import qualified Control.Applicative as P
import qualified Data.Attoparsec.Text as P
import qualified Prelude as Prelude
import qualified Prelude as Unsafe
import qualified RIO.Text as T

lex :: String -> Either String [Token]
lex "" = Right []
lex (' ':xs) = lex xs
lex ('-':'>':xs) = (TokenArrow:) <$> lex xs
lex ('\\':xs) = (TokenBackslash:) <$> lex xs
lex ('.':xs)  = (TokenDot:) <$> lex xs
lex (':':xs)  = (TokenColon:) <$> lex xs
lex ('(':xs)  = (TokenParensBegin:) <$> lex xs
lex (')':xs)  = (TokenParensEnd:) <$> lex xs
lex input =
  let (got, rest) = Unsafe.head $ Prelude.lex input in
  case readMay got of
    Just nat -> (TokenANat nat:) <$> lex rest
    Nothing ->
      if isAnIdent got
        then (TokenAnIdent got:) <$> lex rest
        else Left [i|${(__FILE__ :: String)}:L${show (__LINE__ :: Int)}: An unknown lexeme is got (${got})|]
  where
    isAnIdent :: String -> Bool
    isAnIdent (T.pack -> x) =
      isRight . flip P.parseOnly x $
        (P.letter <|> P.char '_') >> P.many (P.letter <|> P.char '_' <|> P.digit)
