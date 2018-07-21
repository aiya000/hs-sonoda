{
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Expose parser combinators and lexers for sonoda
module Sonoda.Parser
  ( parseExpr
  ) where

import Control.Exception.Safe (SomeException)
import Control.Monad.Except (throwError)
import Data.String.Here (i)
import Data.Text.Prettyprint.Doc (pretty)
import Prelude
import Sonoda.Types
}

%error { parseError }
%errorhandlertype explist
%monad { SonodaProcessor }
%name exprParser
%tokentype { (Token, TokenPos) }

%token
  nat      { (TokenANat $$, _)         }
  true     { (TokenAnIdent "True", _)  }
  false    { (TokenAnIdent "False", _) }
  unit     { (TokenAnIdent "Unit", _)  } -- This is either a value "Unit" or a type "Unit"
  '('      { (TokenParensBegin, _)     }
  ')'      { (TokenParensEnd, _)       }
  "if"     { (TokenAnIdent "if", _)    }
  "then"   { (TokenAnIdent "then", _)  }
  "else"   { (TokenAnIdent "else", _)  }
  '\\'     { (TokenBackslash, _)       }
  ':'      { (TokenColon, _)           }
  '.'      { (TokenDot, _)             }
  ident    { (TokenAnIdent $$, _)      }
  "->"     { (TokenArrow, _)           }
  natType  { (TokenAnIdent "Nat", _)   }
  boolType { (TokenAnIdent "Bool", _)  }

%%

Expr :: { Expr }
  : Expr Expr                    { ExprApply $1 $2     }
  | '(' Expr ')'                 { ExprParens $2       }
  | Syntax                       { ExprSyntax $1       }
  | '\\' Ident ':' Type '.' Expr { ExprLambda $2 $4 $6 }
  | AtomicVal                    { ExprAtomic $1       }
  | Ident                        { ExprIdent $1        }

Syntax :: { Syntax }
  : "if" Expr "then" Expr "else" Expr { If $2 $4 $6 }

Ident :: { Identifier }
  : ident { $1 }

AtomicVal :: { AtomicVal }
  : nat   { TermNat $ Nat $1 }
  | true  { TermBool True    }
  | false { TermBool False   }
  | unit  { TermUnit         }

Type :: { Type }
  : AtomicType     { TypeAtomic $1   }
  | Type "->" Type { TypeArrow $1 $3 }

AtomicType :: { AtomicType }
  : natType  { TypeNat  }
  | boolType { TypeBool }
  | unit     { TypeUnit }

{
parseError :: ([(Token, TokenPos)], [String]) -> SonodaProcessor a
parseError (((got, pos):_), expected) = throwError [i|${show $ pretty got} at ${show $ pretty pos}, but ${show expected} are expected.|]
parseError (_, _)                     = throwError [i|${(__FILE__ :: String)}:L${show (__LINE__ :: Int)}: fatal error! Sorry, please report an issue :(|]

parseExpr :: [(Token, TokenPos)] -> Either Failure Expr
parseExpr = runSonodaProcessor . exprParser
}
