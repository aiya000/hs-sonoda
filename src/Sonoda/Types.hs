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
data Syntax = If Expr Expr Expr
  deriving (Eq)

instance Show Syntax where
  show (If x y z) = [i|if ${show x} then ${show y} else ${show z}|]

-- | Please see a chapter 'The exression rules' of design/design.md
data Expr = ExprAtomic AtomicVal
          | ExprLambda Identifier Type Expr -- ^ A lambda abstraction that is constructed by an Identifier, a type of the identifier, and the body
          | ExprApply Expr Expr -- ^ Apply an argument to a function
          | ExprSyntax Syntax
          | ExprIdent Identifier
          | ExprParens Expr -- ^ "(" expr ")"
  deriving (Eq)

-- |
-- Show AST directly,
-- and the superfluous representations are not removed
--
-- (e.g. "(\x:Nat.x) (10)", "((10)) 20")
-- (                 ^  ^    ^^ ^^     )
instance Show Expr where
  show (ExprAtomic x) = show x
  show (ExprSyntax x) = show x
  show (ExprLambda n t x) = [i|\\${n}:${show t}.${show x}|]
  show (ExprParens x) = "(" <> show x <> ")"
  show (ExprIdent x) = x
  show (ExprApply x y) = show' x <> " " <> show' y
    where
      -- With parentheses or without
      show' :: Expr -> String
      show' x@ExprLambda {} = "(" <> show x <> ")"
      show' x@ExprApply {}  = "(" <> show x <> ")"
      show' x = show x

infixl 9 \$

-- | An alias to 'ExprApply'
(\$) :: Expr -> Expr -> Expr
x \$ y = ExprApply x y

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
