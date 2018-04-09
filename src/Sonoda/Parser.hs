{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
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
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Sonoda.Types
import Text.Parser.Combinators ((<?>))
import Text.Parser.Token (TokenParsing)
import Text.Trifecta.Delta (HasDelta(..))
import Text.Trifecta.Parser (Parser, parseString)
import Text.Trifecta.Result (Result(..))
import qualified Data.Text as T
import qualified Text.Parser.Char as P
import qualified Text.Parser.Combinators as P
import qualified Text.Parser.Token as P

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
exprParser = ExprAtomic <$> atomicValParser
         <|> lambdaParser
         <|> ExprSyntax <$> syntaxParser
         <|> P.parens exprParser
         <|> applicationParser

-- | A parser for lambda abstractions
lambdaParser :: CodeParsing m => m Expr
lambdaParser = P.try . (<?> "lambda abstraction") $ do
  P.symbolic '\\'
  n <- identifierParser
  P.colon
  t <- typeParser
  P.dot
  x <- exprParser
  pure $ ExprLambda n t x

-- | A parser for function applications
applicationParser :: CodeParsing m => m Expr
applicationParser = P.try . (<?> "application") $ do
  first        <- terminatorParser
  maybeApplyee <- const Nothing <$> P.eof <|> Just <$> exprParser
  case maybeApplyee of
    Nothing      -> pure first
    Just applyee -> pure $ ExprApply first applyee
  where
    -- A parser for terms that is not an application
    terminatorParser :: CodeParsing m => m Expr
    terminatorParser = ExprAtomic <$> atomicValParser
                   <|> lambdaParser
                   <|> ExprSyntax <$> syntaxParser
                   <|> ExprParens <$> P.parens exprParser
                   <|> ExprIdent  <$> identifierParser

-- | A parser for primitive values
atomicValParser :: TokenParsing m => m AtomicVal
atomicValParser = natValParser <|> boolValParser <|> unitValParser
  where
    natValParser :: TokenParsing m => m AtomicVal
    natValParser = TermNat <$> natural'

    boolValParser :: TokenParsing m => m AtomicVal
    boolValParser =  (P.textSymbol "True"  $> TermBool True)
                 <|> (P.textSymbol "False" $> TermBool False)

    unitValParser :: TokenParsing m => m AtomicVal
    unitValParser = P.textSymbol "Unit" $> TermUnit

-- | A parser for variable names
identifierParser :: TokenParsing m => m Identifier
identifierParser = P.try . (<?> "identifier") $ do
  _  <- P.whiteSpace
  x  <- P.lower
  xs <- P.many $ P.upper <|> P.lower <|> P.digit <|> P.char '_'
  _  <- P.whiteSpace
  pure (x:xs)

-- | A parser for syntaxes
syntaxParser :: CodeParsing m => m Syntax
syntaxParser = ifParser
  where
    ifParser :: CodeParsing m => m Syntax
    ifParser = P.try $ do
      P.textSymbol "if"
      x <- exprParser
      P.textSymbol "then"
      y <- exprParser
      P.textSymbol "else"
      z <- exprParser
      pure $ If x y z


-- | A parser for types
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
      (x:|xs) <- (atomicTypeParser <|> innerTypeParser) `P.sepByNonEmpty` P.textSymbol "->"
      pure $ foldr1 TypeArrow (x:xs)

    innerTypeParser :: CodeParsing m => m Type
    innerTypeParser = TypeParens <$> P.parens typeParser

-- | A parser for embedded primitive types
atomicTypeParser :: CodeParsing m => m Type
atomicTypeParser = natTypeParser <|> boolTypeParser <|> unitTypeParser
  where
    natTypeParser  = P.textSymbol "Nat"  $> natT
    boolTypeParser = P.textSymbol "Bool" $> boolT
    unitTypeParser = P.textSymbol "Unit" $> unitT


-- | Similar to 'natural', but take 'Nat'
natural' :: TokenParsing m => m Nat
natural' = Nat . fromInteger <$> P.natural
