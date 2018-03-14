{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Expose parser combinators for sonoda
module Sonoda.Parser where

import Control.Applicative ((<|>))
import Control.Exception.Safe (MonadThrow, throwM, Exception(..), SomeException, StringException(..))
import Control.Exception.Safe.Checked (Throws)
import Data.ByteString.UTF8 (ByteString)
import Data.HashSet (empty)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Sonoda.Types
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Token.Highlight (Highlight(..))
import Text.Trifecta.Delta (HasDelta(..))
import Text.Trifecta.Parser (parseString)
import Text.Trifecta.Result (Result(..))
import qualified Data.Text as T

-- | A constraint for a programmatic parsing
type CodeParsing m = (TokenParsing m, Monad m, Throws ParseException)

-- | this is occured when a parse is failed
newtype ParseException = ParseException
  { unParseException :: StringException
  } deriving (Show, Exception)

-- | The smart constructor
parseException :: String -> ParseException
parseException msg = ParseException (StringException msg ?callStack)

-- | Similar to 'Control.Exception.Safe.throwString' but throw it as a 'ParseException'
throwParseExc :: (HasCallStack, MonadThrow m) => String -> m a
throwParseExc = throwM . parseException


-- | Parse an expression
parseExpr :: Text -> Either SomeException Expr
parseExpr (T.unpack -> code) =
  case parseString exprParser defaultDelta code of
    Success x -> return x
    Failure e -> throwParseExc $ show e
  where
    --NOTE: What is 'Delta'?
    defaultDelta = delta ("" :: ByteString)


-- | Parse a code to 
exprParser :: (TokenParsing m, Monad m) => m Expr
exprParser =  ExprAtomic <$> atomicValParser
          <|> ExprLambda <$> lambdaParser
          <|> ExprSyntax <$> syntaxParser
          <|> parens exprParser


-- | Parser a value
atomicValParser :: (TokenParsing m, Monad m) => m AtomicVal
atomicValParser = natValParser <|> boolValParser <|> unitValParser
  where
    natValParser :: TokenParsing m => m AtomicVal
    natValParser = TermNat <$> natural'

    boolValParser :: (TokenParsing m, Monad m) => m AtomicVal
    boolValParser =  (textSymbol "True"  >> return (TermBool True))
                 <|> (textSymbol "False" >> return (TermBool False))

    unitValParser :: (TokenParsing m, Monad m) => m AtomicVal
    unitValParser = textSymbol "Unit" >> return TermUnit


-- | Parse an identifier (camelCase)
identifierParser :: (TokenParsing m, Monad m) => m Identifier
identifierParser = ident camelCase
  where
    camelCase :: TokenParsing m => IdentifierStyle m
    camelCase = IdentifierStyle { _styleName              = "camelCase"
                                , _styleStart             = lower
                                , _styleLetter            = letter
                                , _styleReserved          = empty
                                , _styleHighlight         = Identifier
                                , _styleReservedHighlight = Identifier
                                }


-- | Parse a lambda abstraction
lambdaParser :: (TokenParsing m, Monad m) => m Lambda
lambdaParser =  LambdaExpr <$> exprParser
            <|> LambdaIdent <$> identifierParser
            <|> abstractionParser
            <|> funcApplyParser
  where
    abstractionParser :: (TokenParsing m, Monad m) => m Lambda
    abstractionParser = do
      backslash
      i <- identifierParser
      colon
      t <- typeParser
      dot
      x <- exprParser
      return $ LambdaAbst i t x

    funcApplyParser :: (TokenParsing m, Monad m) => m Lambda
    funcApplyParser = LambdaApply <$> lambdaParser <*> exprParser


-- | Parse a syntax
syntaxParser :: (TokenParsing m, Monad m) => m Syntax
syntaxParser = ifParser
  where
    ifParser :: (TokenParsing m, Monad m) => m Syntax
    ifParser = do
      textSymbol "if"
      x <- exprParser
      textSymbol "then"
      y <- exprParser
      textSymbol "else"
      z <- exprParser
      return $ If x y z


typeParser :: TokenParsing m => m Type
typeParser = undefined


-- | Similar to 'natural', but take 'Nat'
natural' :: TokenParsing m => m Nat
natural' = Nat . fromInteger <$> natural

-- | Similar to `char '\'` but for `TokenParsing`
backslash :: (TokenParsing m, Monad m) => m Char
backslash = do
  token $ char '\\'
  return '\\'
