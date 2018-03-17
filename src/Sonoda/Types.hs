{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Expose the AST of sonoda
module Sonoda.Types where

import Data.Semigroup ((<>))

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

-- | An alias to 'LambdaApply'
(\$) :: Lambda -> Expr -> Lambda
(\$) = LambdaApply
infixl 9 \$


-- | Please see 'Expr'
data Syntax = If Expr Expr Expr
  deriving (Show, Eq)

-- | Please see a chapter 'The exression rules' of design/design.md
data Expr = ExprAtomic AtomicVal
          | ExprLambda Lambda
          | ExprSyntax Syntax
          | ExprParens Expr -- ^ "(" expr ")"
  deriving (Show, Eq)

-- | Make a lambda abstraction as an 'Expr'
lambda :: Identifier -> Type -> Expr -> Expr
lambda i t x = ExprLambda $ LambdaAbst i t x

-- | Make an 'Identifier' as an 'Expr'
ident :: Identifier -> Expr
ident = ExprLambda . LambdaIdent

-- | Make a 'Nat' from `Int` as an 'Expr'
nat :: Int -> Expr
nat = ExprAtomic . TermNat . Nat

-- | Make a 'If' syntax as an 'Expr'
if_ :: Expr -> Expr -> Expr -> Expr
if_ x y z = ExprSyntax $ If x y z


-- | Please see 'Type'
data AtomicType = TypeNat | TypeBool | TypeUnit
  deriving (Eq)

instance Show AtomicType where
  show TypeNat  = "Nat"
  show TypeBool = "Bool"
  show TypeUnit = "Unit"

-- | Please see a chapter 'The typing rules' of design/design.md
data Type = TypeAtomic AtomicType
          | TypeArrow Type Type
          | TypeParens Type -- ^ "(" type ")"
  deriving (Eq)

instance Show Type where
  show (TypeAtomic x)  = show x
  show (TypeArrow (TypeArrow x y) z) = "(" <> show x <> " -> " <> show y <> ")" <> " -> " <> show z
  show (TypeArrow x y) = show x <> " -> " <> show y
  show (TypeParens x)  = "(" <> show x <> ")"

-- | An alias to 'TypeAtomic TypeNat'
natT :: Type
natT = TypeAtomic TypeNat

-- | An alias to 'TypeAtomic TypeBool'
boolT :: Type
boolT = TypeAtomic TypeBool

-- | An alias to 'TypeAtomic TypeUnit'
unitT :: Type
unitT = TypeAtomic TypeUnit

infixr 9 ~>

-- | An alias to 'TypeArrow'
(~>) :: Type -> Type -> Type
(~>) = TypeArrow
