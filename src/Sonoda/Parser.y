{
-- | Expose parser combinators and lexers for sonoda
module Sonoda.Parser
  ( parseExpr
  ) where

import Control.Exception.Safe (SomeException)
import Sonoda.Types
}

%name parseExpr
%tokentype { Token }
%monad { Either String }
%error { error "Parse error" }

%token
  nat      { TokenANat $$         }
  true     { TokenAnIdent "True"  }
  false    { TokenAnIdent "False" }
  unit     { TokenAnIdent "Unit"  } -- This is either a value "Unit" or a type "Unit"
  '('      { TokenParensBegin     }
  ')'      { TokenParensEnd       }
  "if"     { TokenAnIdent "if"    }
  "then"   { TokenAnIdent "then"  }
  "else"   { TokenAnIdent "else"  }
  '\\'     { TokenBackslash       }
  ':'      { TokenColon           }
  '.'      { TokenDot             }
  ident    { TokenAnIdent $$      }
  "->"     { TokenArrow           }
  natType  { TokenAnIdent "Nat"   }
  boolType { TokenAnIdent "Bool"  }

%%

Expr :: { Expr }
     : Expr Expr { ExprApply $1 $2 }
     | '(' Expr ')' { ExprParens $2}
     | Syntax { ExprSyntax $1 }
     | '\\' Ident ':' Type '.' Expr { ExprLambda $2 $4 $6 }
     | AtomicVal { ExprAtomic $1 }

Syntax :: { Syntax }
       : "if" Expr "then" Expr "else" Expr { If $2 $4 $6 }

Ident :: { Identifier }
      : ident { $1 }

AtomicVal :: { AtomicVal }
          : nat  { TermNat $ Nat $1  }
          | true { TermBool True }
          | false { TermBool False }
          | unit { TermUnit    }

Type :: { Type }
     : AtomicType     { TypeAtomic $1   }
     | Type "->" Type { TypeArrow $1 $3 }

AtomicType :: { AtomicType }
           : natType  { TypeNat  }
           | boolType { TypeBool }
           | unit     { TypeUnit }
