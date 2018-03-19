{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Expose the AST of sonoda
module Sonoda.Types where

import Data.Semigroup ((<>))
import Data.String.Here (i)


{-@ Nat :: {x:Int | x >= 0} -> Nat @-}
{-@ unNat :: Nat -> {x:Int | x >= 0} @-}
-- | Mean nutural numbers
newtype Nat = Nat
  { unNat :: Int
  } deriving (Eq, Ord, Bounded, Enum, Num, Real, Integral)

instance Show Nat where
  show (Nat n) = show n

-- | Please see 'Expr'
data AtomicVal = TermNat Nat
               | TermBool Bool
               | TermUnit -- Unit literals in sonoda
  deriving (Eq)

instance Show AtomicVal where
  show (TermNat n)  = show n
  show (TermBool x) = show x
  show TermUnit     = "Unit"


{-@ type Identifier = {(x:xs) : String | isLower x} @-}
type Identifier = String

-- | Please see 'Expr'
data Lambda = LambdaExpr Expr
            | LambdaIdent Identifier
            | LambdaAbst Identifier Type Expr -- ^ An Identifier, a type of the identifier, and the body
            | LambdaApply Lambda Expr         -- ^ Apply an argument to a function
  deriving (Eq)

-- |
-- Show AST directly,
-- and the superfluous representations are included
--
-- (e.g. "(\x:Nat.x) (10)", "((10)) 20")
-- (                 ^  ^    ^^ ^^     )
instance Show Lambda where
  show (LambdaExpr  x) = show x
  show (LambdaIdent x) = x
  show (LambdaAbst n t x) = [i|\\${n}:${show t}.${show x}|]
  show (LambdaApply x y) =
    case y of
      ExprLambda (LambdaApply _ _)  -> [i|(${show x}) (${show y})|]
      ExprLambda (LambdaAbst _ _ _) -> [i|(${show x}) (${show y})|]
      _ -> [i|(${show x}) ${show y}|]

infixl 9 \$

-- | An alias to 'LambdaApply'
(\$) :: Lambda -> Expr -> Lambda
(\$) = LambdaApply


-- | Please see 'Expr'
data Syntax = If Expr Expr Expr
  deriving (Eq)

instance Show Syntax where
  show (If x y z) = [i|if ${show x} then ${show y} else ${show z}|]

-- | Please see a chapter 'The exression rules' of design/design.md
data Expr = ExprAtomic AtomicVal
          | ExprLambda Lambda
          | ExprSyntax Syntax
          | ExprParens Expr -- ^ "(" expr ")"
  deriving (Eq)

instance Show Expr where
  show (ExprAtomic x) = show x
  show (ExprLambda x) = show x
  show (ExprSyntax x) = show x
  show (ExprParens x) = "(" <> show x <> ")"

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
