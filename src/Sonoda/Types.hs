{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Expose the AST of sonoda
module Sonoda.Types where

{-@ Nat :: {x:Int | x >= 0} -> Nat @-}
{-@ unNat :: Nat -> {x:Int | x >= 0} @-}
-- | Mean nutural numbers
newtype Nat = Nat
  { unNat :: Int
  } deriving (Show, Eq, Ord, Bounded, Enum, Num, Real, Integral)

-- | Please see 'Expr'
data AtomicVal = TermNat Nat
               | TermBool Bool
               | TermUnit -- Unit literals in sonoda
  deriving (Show, Eq)


{-@ type Identifier = {(x:xs) : String | isLower x} @-}
type Identifier = String

-- | Please see 'Expr'
data Lambda = LambdaExpr Expr
            | LambdaIdent Identifier
            | LambdaAbst Identifier Type Expr -- ^ An Identifier, a type of the identifier, and the body
            | LambdaApply Lambda Expr         -- ^ Apply an argument to a function
  deriving (Show, Eq)

-- | Please see 'Expr'
data Syntax = If Expr Expr Expr
  deriving (Show, Eq)

-- | Please see a chapter 'The exression rules' of design/design.md
data Expr = ExprAtomic AtomicVal
          | ExprLambda Lambda
          | ExprSyntax Syntax
          | ExprBracket Expr -- ^ "(" expr ")"
  deriving (Show, Eq)


-- | Please see 'Type'
data AtomicType = TypeNat | TypeBool | TypeUnit
  deriving (Show, Eq)

-- | Please see a chapter 'The typing rules' of design/design.md
data Type = TypeAtomic AtomicType
          | TypeArrow Type Type
  deriving (Show, Eq)

-- | An alias to 'TypeArrow'
(~>) :: Type -> Type -> Type
(~>) = TypeArrow
