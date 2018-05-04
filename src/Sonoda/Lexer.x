{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Sonoda.Lexer
  ( lex
  ) where

import Data.Semigroup ((<>))
import Prelude hiding (lex)
import Safe (readMay)
import Sonoda.Types.Lexer
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+ ;
  $digit+                         { giveANat              }
  "->"                            { give TokenArrow       }
  \\                              { give TokenBackslash   }
  \.                              { give TokenDot         }
  \:                              { give TokenColon       }
  \(                              { give TokenParensBegin }
  \)                              { give TokenParensEnd   }
  [$alpha \_] [$alpha \_ $digit]* { giveAnIdent           }

{
giveANat :: AlexAction [Token]
giveANat = \(_, _, _, got) _ ->
  case readMay got of
    Nothing -> alexError $ "a gotten number seems invalid: `" <> got <> "`"
    Just x  -> alexMonadScan >>= pure . (TokenANat x:)

giveAnIdent :: AlexAction [Token]
giveAnIdent = \(_, _, _, got) _ ->
  alexMonadScan >>= pure . (TokenAnIdent got:)

give :: Token -> AlexAction [Token]
give x = \_ _ -> do
  xs <- alexMonadScan
  pure (x:xs)

alexEOF :: Alex [Token]
alexEOF = pure [TokenEof]

-- | Lex it, but give a `Left` if the lex is failed
lex :: String -> Either String [Token]
lex x = fmap removeEOF . runAlex x $ alexMonadScan
  where
    removeEOF :: [Token] -> [Token]
    removeEOF xs | last xs == TokenEof = init xs
                 | otherwise           = xs
}
