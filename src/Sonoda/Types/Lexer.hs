{-# LANGUAGE TemplateHaskell #-}

-- | Expose types for 'Sonoda.Lexer'
module Sonoda.Types.Lexer where

import Control.Arrow ((>>>))
import Control.Lens (makeLensesFor)
import Control.Monad.Except (MonadError, ExceptT, runExceptT)
import Control.Monad.State.Strict (MonadState, State, runState)
import Data.Default (Default(..))
import Data.Text.Prettyprint.Doc (Pretty(..))
import RIO

-- |
-- Represent a line/column number of a file position,
-- collected by 'Sonoda.Lexer.lex',
-- read by 'Sonoda.Parser.parseExpr' on the parser error.
data TokenPos = TokenPos
  { lineNum :: Int
  , colNum  :: Int
  } deriving (Show, Eq)

makeLensesFor
  [ ("lineNum", "_lineNum")
  , ("colNum", "_colNum")
  ] ''TokenPos

instance Default TokenPos where
  -- | All the lexing starts with row 1 and column 1
  def = TokenPos 1 1

instance Pretty TokenPos where
  pretty (TokenPos l c) = "(" <> pretty l <> "," <> pretty c <> ")"

-- | A negative context for 'Sonoda.Lexer' and 'Sonoda.Parser'
data Failure = Failure
  { what_  :: String   -- ^ What is failed / Why it is failed
  , where_ :: TokenPos -- ^ Where it is failed
  } deriving (Show, Eq)

makeLensesFor
  [ ("what_", "_what_")
  , ("where_", "_where_")
  ] ''Failure

--NOTE: Consider to use freer-effects instead for performances
-- |
-- A monad for the lexer, the parser, and another processors.
--
-- This is possible to throw a `String` as a processor error,
-- and saves where is working at now.
-- Please see 'runSonodaProcessor'.
newtype SonodaProcessor a = SonodaProcessor
  { unSonodaProcessor :: ExceptT String (State TokenPos) a
  } deriving ( Functor, Applicative, Monad
             , MonadState TokenPos
             , MonadError String
             )

-- |
-- Run the lexer.
-- if the lexer is failed,
-- notify a error cause (`String`) and where is failed ('TokenPos'),
runSonodaProcessor :: SonodaProcessor a -> Either Failure a
runSonodaProcessor = unSonodaProcessor
                 >>> (runExceptT :: ExceptT String (State TokenPos) a -> State TokenPos (Either String a))
                 >>> (flip runState def :: State TokenPos (Either String a) -> (Either String a, TokenPos))
                 >>> intoLeft
  where
    intoLeft :: (Either String b, TokenPos) -> Either Failure b
    intoLeft (Left x, y)  = Left $ Failure x y
    intoLeft (Right x, _) = Right x

-- | sonoda's lexemes
data Token = TokenANat Int
           | TokenArrow
           | TokenBackslash
           | TokenDot
           | TokenColon
           | TokenParensBegin
           | TokenParensEnd
           | TokenAnIdent String
           | TokenEof -- ^ For only alex's `alexEOF`
  deriving (Show, Eq)

instance Pretty Token where
  pretty (TokenANat n)    = pretty n
  pretty TokenArrow       = "->"
  pretty TokenBackslash   = "\\"
  pretty TokenDot         = "."
  pretty TokenColon       = ":"
  pretty TokenParensBegin = "("
  pretty TokenParensEnd   = ")"
  pretty (TokenAnIdent x) = pretty x
  pretty TokenEof         = "<EOF>"
