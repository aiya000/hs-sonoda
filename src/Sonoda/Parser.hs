{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Expose parser combinators for sonoda
module Sonoda.Parser
  ( parseExpr
  , parseType
  ) where

import Control.Exception.Safe (SomeException)
import Data.Text (Text)
import Sonoda.Types

-- | Parse an expression
parseExpr :: Text -> Either SomeException Expr
parseExpr = undefined

-- | Parse a type
parseType :: Text -> Either SomeException Type
parseType = undefined
