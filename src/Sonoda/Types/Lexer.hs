-- | Expose types for 'Sonoda.Lexer'
module Sonoda.Types.Lexer where

import RIO

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
