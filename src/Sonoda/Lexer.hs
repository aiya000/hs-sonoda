{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Sonoda.Lexer
  ( lex
  ) where

import Control.Lens ((<+=))
import Control.Monad.State.Strict (get)
import Control.Monad.Except (throwError)
import Data.String.Here (i)
import RIO
import Safe (readMay)
import Sonoda.Types.Lexer
import qualified Control.Applicative as P
import qualified Data.Attoparsec.Text as P
import qualified Prelude
import qualified RIO.List.Partial as Unsafe
import qualified RIO.Text as T

-- |
-- Tokenize an input.
-- If the lexer is failed, return the message with that where is failed.
lex :: String -> Either Failure [(Token, TokenPos)]
lex = runSonodaProcessor . rex

-- | An inside of 'lex' (mean a right term of `lex` xD)
rex :: String -> SonodaProcessor [(Token, TokenPos)]
rex "" = pure []
rex input = do
  pos <- get
  case input of
    ('\r':'\n':xs) -> continueToBelow xs
    ('\r':xs)      -> continueToBelow xs
    ('\n':xs)      -> continueToBelow xs
    (' ':xs)       -> continueToForward 1 xs
    ('-':'>':xs) -> continueToForwardWith (TokenArrow, pos) 2 xs
    ('\\':xs)    -> continueToForwardWith (TokenBackslash, pos) 1 xs
    ('.':xs)     -> continueToForwardWith (TokenDot, pos) 1 xs
    (':':xs)     -> continueToForwardWith (TokenColon, pos) 1 xs
    ('(':xs)     -> continueToForwardWith (TokenParensBegin, pos) 1 xs
    (')':xs)     -> continueToForwardWith (TokenParensEnd, pos) 1 xs
    input -> do
      let (got, rest) = Unsafe.head $ Prelude.lex input
      case readMay got of
        Nothing  -> identOrFail (got, rest) pos
        Just nat -> continueToForwardWith (TokenANat nat, pos) (length got) rest
  where
    -- Take a rest input.
    -- Also go to the next line
    continueToBelow :: String -> SonodaProcessor [(Token, TokenPos)]
    continueToBelow rest = do
      _lineNum <+= 1
      rex rest

    -- Take a length of a consumed token, and a rest input.
    -- Also go to the taken next column
    continueToForward :: Int -> String -> SonodaProcessor [(Token, TokenPos)]
    continueToForward n rest = do
      _colNum <+= n
      rex rest

    -- Take a got token, a position of the token,
    -- a length of a consumed token, and a rest input.
    -- Also go to the taken next column
    continueToForwardWith :: (Token, TokenPos) -> Int -> String -> SonodaProcessor [(Token, TokenPos)]
    continueToForwardWith x n rest = do
      _colNum <+= n
      (x:) <$> rex rest

    identOrFail :: (String, String) -> TokenPos -> SonodaProcessor [(Token, TokenPos)]
    identOrFail (got, rest) pos
      | isAnIdent got
          = continueToForwardWith (TokenAnIdent got, pos) (length got) rest
      | otherwise
          = throwError [i|${(__FILE__ :: String)}:L${show (__LINE__ :: Int)}: An unknown lexeme is got (${got})|]

    isAnIdent :: String -> Bool
    isAnIdent (T.pack -> x) =
      isRight . flip P.parseOnly x $
        (P.letter <|> P.char '_') >> P.many (P.letter <|> P.char '_' <|> P.digit)
