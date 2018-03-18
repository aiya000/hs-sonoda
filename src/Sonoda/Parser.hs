{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Expose parser combinators for sonoda
module Sonoda.Parser
  ( ParseException (..)
  , parseExpr
  , parseType
  , exprParser
  , typeParser
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (MonadThrow, throw, Exception(..), SomeException, StringException(..))
import Data.ByteString.UTF8 (ByteString)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Sonoda.Types hiding ((~>))
import Text.Parser.Char
import Text.Parser.Combinators (many, sepByNonEmpty)
import Text.Parser.Token hiding (ident)
import Text.Trifecta.Delta (HasDelta(..))
import Text.Trifecta.Parser (Parser, parseString)
import Text.Trifecta.Result (Result(..))
import qualified Data.Text as T

-- | A constraint for a programmatic parsing
type CodeParsing m = (TokenParsing m, Monad m)

-- | this is occured when a parse is failed
newtype ParseException = ParseException
  { unParseException :: StringException
  } deriving (Show, Exception)

-- | The smart constructor
parseException :: String -> ParseException
parseException msg = ParseException (StringException msg ?callStack)

-- | Similar to 'Control.Exception.Safe.throwString' but throw it as a 'ParseException'
throwParseExc :: (HasCallStack, MonadThrow m) => String -> m a
throwParseExc = throw . parseException


-- |
-- Parse a code with the parser,
-- Take the result or 'SomeException'
parseIt :: Parser a -> Text -> Either SomeException a
parseIt parser (T.unpack -> code) =
  case parseString parser defaultDelta code of
    Success x -> return x
    Failure e -> throwParseExc $ show e
  where
    --NOTE: What is 'Delta'?
    defaultDelta = delta ("" :: ByteString)

-- | Parse an expression
parseExpr :: Text -> Either SomeException Expr
parseExpr = parseIt exprParser

-- | Parse a type
parseType :: Text -> Either SomeException Type
parseType = parseIt typeParser


-- | A parser of expressions
exprParser :: CodeParsing m => m Expr
exprParser =  ExprAtomic <$> atomicValParser
          <|> ExprLambda <$> lambdaParser
          <|> ExprSyntax <$> syntaxParser
          <|> parens exprParser


atomicValParser :: TokenParsing m => m AtomicVal
atomicValParser = natValParser <|> boolValParser <|> unitValParser
  where
    natValParser :: TokenParsing m => m AtomicVal
    natValParser = TermNat <$> natural'

    boolValParser :: TokenParsing m => m AtomicVal
    boolValParser =  (textSymbol "True"  *> pure (TermBool True))
                 <|> (textSymbol "False" *> pure (TermBool False))

    unitValParser :: TokenParsing m => m AtomicVal
    unitValParser = textSymbol "Unit" *> pure TermUnit


identifierParser :: TokenParsing m => m Identifier
identifierParser = do
  _  <- whiteSpace
  x  <- lower
  xs <- many $ upper <|> lower <|> digit
  _  <- whiteSpace
  pure (x:xs)


lambdaParser :: CodeParsing m => m Lambda
lambdaParser =  LambdaIdent <$> identifierParser
            <|> abstractionParser
            <|> applicationParser
            <|> LambdaExpr <$> exprParser
  where
    abstractionParser :: CodeParsing m => m Lambda
    abstractionParser = do
      symbolic '\\'
      i <- identifierParser
      colon
      t <- typeParser
      dot
      x <- exprParser
      pure $ LambdaAbst i t x

    applicationParser :: CodeParsing m => m Lambda
    applicationParser = LambdaApply <$> lambdaParser <*> exprParser


syntaxParser :: CodeParsing m => m Syntax
syntaxParser = ifParser
  where
    ifParser :: CodeParsing m => m Syntax
    ifParser = do
      textSymbol "if"
      x <- exprParser
      textSymbol "then"
      y <- exprParser
      textSymbol "else"
      z <- exprParser
      pure $ If x y z


atomicTypeParser :: CodeParsing m => m Type
atomicTypeParser = natTypeParser <|> boolTypeParser <|> unitTypeParser
  where
    natTypeParser  = textSymbol "Nat"  *> pure natT
    boolTypeParser = textSymbol "Bool" *> pure boolT
    unitTypeParser = textSymbol "Unit" *> pure unitT

-- | A parser of types
typeParser :: CodeParsing m => m Type
typeParser = normalize <$> typeParser'
  where
    -- Remove superflous semantics
    normalize :: Type -> Type
    normalize (TypeParens x) = normalize x
    normalize (TypeArrow x (TypeParens (TypeArrow y z))) = normalize $ TypeArrow x (TypeArrow y z)
    normalize x = x

    typeParser' :: CodeParsing m => m Type
    typeParser' = do
      (x:|xs) <- (atomicTypeParser <|> innerTypeParser) `sepByNonEmpty` textSymbol "->"
      pure $ foldr1 TypeArrow (x:xs)

    innerTypeParser :: CodeParsing m => m Type
    innerTypeParser = TypeParens <$> parens typeParser


-- | Similar to 'natural', but take 'Nat'
natural' :: TokenParsing m => m Nat
natural' = Nat . fromInteger <$> natural
