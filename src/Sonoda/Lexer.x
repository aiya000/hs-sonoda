{
module Sonoda.Lexer
  ( lex
  ) where

import Data.Semigroup ((<>))
import Prelude hiding (lex)
import Safe (readMay)
import Sonoda.Types.Lexer
}

%wrapper "monad"

tokens :-
  $white+ ;
  [1-9][0-9]* { giveOrErr "a nat" TokenANat            }
  "->"        { give TokenArrow                        }
  \\          { give TokenBackslash                    }
  \.          { give TokenDot                          }
  :           { give TokenColon                        }
  \(          { give TokenParensBegin                  }
  \)          { give TokenParensEnd                    }
  [a-zA-Z_]+  { giveOrErr "an identifier" TokenAnIdent }

{
giveOrErr :: Read a => String -> (a -> Token) -> AlexAction [Token]
giveOrErr name constr = \(_, _, _, got) _ -> do
  case readMay got of
    Nothing -> alexError $ "a condition of " <> name <> " seems an invalid with: `" <> got <> "`"
    Just x  -> alexMonadScan >>= pure . (constr x:)

give :: Token -> AlexAction [Token]
give x = \_ _ -> do
  xs <- alexMonadScan
  pure (x:xs)

alexEOF :: Alex [Token]
alexEOF = pure [TokenEof]

-- | Lex it, but give a `Left` if the lex is failed
lex :: String -> Either String [Token]
lex x = runAlex x $ alexMonadScan
}
