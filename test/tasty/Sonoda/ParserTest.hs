{-# LANGUAGE QuasiQuotes #-}

-- | Expect to success the parsers
module Sonoda.ParserTest where

import Control.Arrow ((>>>))
import Control.Lens ((^?), _Left)
import Control.Monad ((<=<))
import Data.Default (def)
import Data.Semigroup ((<>))
import Data.String.Here (here)
import RIO hiding (to)
import Sonoda.Test.Code (trimMargin)
import Sonoda.Types
import System.Random.NameCase (CamelName(..))
import Test.Hspec (describe, it)
import Test.Hspec.Expectations (shouldBe)
import Test.SmallCheck.Series (NonNegative(..))
import Test.Tasty.Hspec (Spec)
import qualified RIO.Text as T
import qualified Sonoda.Lexer as SL
import qualified Sonoda.Parser as SP

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

natE :: Int -> Expr
natE = ExprAtomic . TermNat . Nat

boolE :: Bool -> Expr
boolE = ExprAtomic . TermBool

identE :: Identifier -> Expr
identE = ExprIdent

natT :: Type
natT = TypeAtomic TypeNat

boolT :: Type
boolT = TypeAtomic TypeBool

unitT :: Type
unitT = TypeAtomic TypeUnit

(~>) :: Type -> Type -> Type
(~>) = TypeArrow
infixr 9 ~>

-- | `[tokenNat, to, tokenBool]` means `Nat -> Bool`
to :: Token
to = TokenArrow

tokenNat :: Token
tokenNat  = TokenAnIdent "Nat"

tokenUnit :: Token
tokenUnit = TokenAnIdent "Unit"

tokenBool :: Token
tokenBool = TokenAnIdent "Bool"

tokensLambda :: String -> [Token] -> [Token] -> [Token]
tokensLambda ident type_ body = [TokenBackslash, TokenAnIdent ident, TokenColon] <> type_ <> [TokenDot] <> body

-- NOTE: I like notations easy to see. But consider to use the array type or lightweight types, if this costs are heavy
tokenParens :: [Token] -> [Token]
tokenParens tokens = [TokenParensBegin] <> tokens <> [TokenParensEnd]

-- | Simular to 'SP.parseExpr', but the source and the result are without 'TokenPos'
parseExpr :: [Token] -> Either Failure Expr
parseExpr = SP.parseExpr . map (, def)

spec_parse_errors :: Spec
spec_parse_errors =
  describe "shows where it is failed" $ do
    -- These expects to be passed the lexing, and to be not passed the parsing
    it "with a casual code" $
      readExpr "\\x:Nat.Nat" ^? _Left . _where_ `shouldBe` Just (TokenPos 1 8)
    it "with lines" $ do
      let code = [here|\x:Nat.
                      |  Nat
                      |] & trimMargin '|'
      readExpr code ^? _Left . _where_ `shouldBe` Just (TokenPos 2 3)
  where
    -- Execute the lexer and the parser with a code.
    -- If something is failed, return what fails and where it fails.
    readExpr :: String -> Either Failure Expr
    readExpr = SP.parseExpr <=< SL.lex

scprop_natVal_can_be_parsed :: NonNegative Int -> Bool
scprop_natVal_can_be_parsed (NonNegative n) =
  parseExpr [TokenANat n] == Right (natExpr n)
    where
      natExpr = ExprAtomic . TermNat . Nat

spec_boolVal_unitVal_can_be_parsed :: Spec
spec_boolVal_unitVal_can_be_parsed = do
  it "True,False" $ do
    parseExpr [TokenAnIdent "True"] `shouldBe` Right (ExprAtomic $ TermBool True)
    parseExpr [TokenAnIdent "False"] `shouldBe` Right (ExprAtomic $ TermBool False)
  it "Unit" $
    parseExpr [TokenAnIdent "Unit"] `shouldBe` Right (ExprAtomic TermUnit)

prop_identifiers_can_be_parsed :: CamelName -> Bool
prop_identifiers_can_be_parsed (unCamelName >>> T.unpack -> ident)
  = parseExpr [TokenAnIdent ident] == Right (identE ident)

spec_lambda_abstractions_can_be_parsed :: Spec
spec_lambda_abstractions_can_be_parsed = do
  it "basic abstractions" $ do
    -- \n:Nat.10
    parseExpr (tokensLambda "n" [tokenNat] [TokenANat 10])
      `shouldBe` Right (ExprLambda "n" natT $ natE 10)
    -- \x:Unit.x
    parseExpr (tokensLambda "x" [tokenUnit] [TokenAnIdent "x"])
      `shouldBe` Right (ExprLambda "x" unitT $ identE "x")
    -- \x:Bool -> Bool.x
    parseExpr (tokensLambda "x" [tokenBool, to, tokenBool] [TokenAnIdent "x"])
      `shouldBe` Right (ExprLambda "x" (boolT ~> boolT) $ identE "x")
    -- \x:Nat -> Bool -> Unit.x
    parseExpr (tokensLambda "x" [tokenNat, to, tokenBool, to, tokenUnit] [TokenAnIdent "x"])
      `shouldBe` Right (ExprLambda "x" (natT ~> boolT ~> unitT) $ identE "x")

    -- \x:(Nat -> Bool) -> Unit.x
    let f = tokensLambda "x"
              [TokenParensBegin, tokenNat, to, tokenBool, TokenParensEnd, to, tokenUnit]
              [TokenAnIdent "x"]
        f' = ExprLambda "x"
              (TypeParens (natT ~> boolT) ~> unitT) $
                identE "x"
    parseExpr f `shouldBe` Right f'

    -- \s:Unit -> Unit.\z:Unit.z
    let zz   = tokensLambda "z" [tokenUnit] [TokenAnIdent "z"]
        zero = tokensLambda "s" [tokenUnit, to, tokenUnit] zz
    let zz'   = ExprLambda "z" unitT $ identE "x"
        zero' = ExprLambda "s" (unitT ~> unitT) zz'
    parseExpr zero `shouldBe` Right zero'

  it "abstractions with removable notations" $ do
    -- \x:Unit.((x))  ==>  \x:Unit.x
    let f = tokensLambda "x" [tokenUnit]
              [TokenParensBegin, TokenParensBegin, TokenAnIdent "x", TokenParensEnd, TokenParensEnd]
        f' = ExprLambda "x" unitT $ identE "x"
    parseExpr f `shouldBe` Right f'
    -- \x:Nat -> (Bool -> Unit).x  ==>  \x:Nat -> Bool -> Unit.x
    let f1 = tokensLambda "x"
                [tokenNat, to, TokenParensBegin, tokenBool, to, tokenUnit, TokenParensEnd]
                [TokenAnIdent "x"]
        f1' = ExprLambda "x" (natT ~> boolT ~> unitT) $ identE "x"
    parseExpr f1 `shouldBe` Right f1'

spec_function_applications_can_be_parsed :: Spec
spec_function_applications_can_be_parsed = do
  it "with an argument" $ do
    -- (\x:Nat.x) 10
    let f = tokenParens $ tokensLambda "x" [tokenNat] [TokenAnIdent "x"]
        t = [TokenANat 10]
    let f' = ExprParens (ExprLambda "x" natT $ identE "x")
        t' = natE 10
    parseExpr (f <> t) `shouldBe` Right (ExprApply f' t')
  it "with arguments" $ do
    -- (like)
    -- one = \s1:Unit -> Unit.\z1:Unit.s1 z1
    -- two = \s2:Unit -> Unit.\z2:Unit.s2 (s2 z2)
    -- plus = \n:(Unit -> Unit) -> Unit -> Unit.
    --        \m:(Unit -> Unit) -> Unit -> Unit.
    --          \s:Unit -> Unit.
    --            \z:Unit.n s (m s z)
    -- plus one two
    let one = tokensLambda "s1" [tokenUnit, to, tokenUnit] $
                tokensLambda "z1" [tokenUnit]
                  [TokenAnIdent "s1", TokenAnIdent "z1"]
    let two = tokensLambda "s2" [tokenUnit, to, tokenUnit] $
                tokensLambda "z2" [tokenUnit] $
                  [TokenAnIdent "s2"] <> tokenParens [TokenAnIdent "s2", TokenAnIdent "z2"]
    let plus = tokensLambda "n" (tokenParens [tokenUnit, to, tokenUnit] <> [to, tokenUnit, to, tokenUnit]) $
               tokensLambda "m" (tokenParens [tokenUnit, to, tokenUnit] <> [to, tokenUnit, to, tokenUnit]) $
                tokensLambda "s" [tokenUnit, to, tokenUnit] $
                  tokensLambda "z" [tokenUnit] $
                    [TokenAnIdent "n", TokenAnIdent "s"] <>
                      tokenParens [TokenAnIdent "m", TokenAnIdent "s", TokenAnIdent "z"]
    let app = plus <> tokenParens one <> tokenParens two
    let one' = ExprLambda "s1" (unitT ~> unitT) . ExprLambda "z1" unitT $
                ExprApply (identE "s1") (identE "z1")
    let two' = ExprLambda "s2" (unitT ~> unitT) . ExprLambda "z2" unitT $
                ExprApply (identE "s2") (identE "z2")
    let plus' = ExprLambda "n" (TypeParens (unitT ~> unitT) ~> unitT ~> unitT) $
                ExprLambda "m" (TypeParens (unitT ~> unitT) ~> unitT ~> unitT) $
                  ExprLambda "s" (unitT ~> unitT) $
                    ExprLambda "z" unitT $
                      ExprApply (ExprApply (identE "n") (identE "s"))
                                (ExprParens (ExprApply
                                  (ExprApply (identE "m") (identE "s"))
                                                          (identE "z")))
    let app' = ExprApply -- left associative
                (ExprApply (ExprParens plus') (ExprParens one'))
                (ExprParens two')
    parseExpr app `shouldBe` Right app'

spec_syntax_can_be_parsed :: Spec
spec_syntax_can_be_parsed =
  describe "if" $ do
    it "without applications" $ do
      -- if True then 10 else 20
      let t = tokensIf [TokenAnIdent "True"] [TokenANat 10] [TokenANat 20]
          t' = ifE (boolE True) (natE 10) (natE 20)
      parseExpr t `shouldBe` Right t'
    it "with a conditional application" $ do
      -- if isZero 0 then 10 else 20
      let t = tokensIf [TokenAnIdent "isZero", TokenANat 0] [TokenANat 10] [TokenANat 20]
          t' = ifE (identE "isZero" `ExprApply` natE 0) (natE 10) (natE 20)
      parseExpr t `shouldBe` Right t'
    it "with value applications" $ do
      -- if False then succ 0 else pred 1
      let t = tokensIf [TokenAnIdent "False"]
                [TokenAnIdent "succ", TokenANat 0]
                [TokenAnIdent "pred", TokenANat 1]
          t' = ifE (boolE False)
                (identE "succ" `ExprApply` natE 0)
                (identE "pred" `ExprApply` natE 1)
      parseExpr t `shouldBe` Right t'
  where
    tokensIf :: [Token] -> [Token] -> [Token] -> [Token]
    tokensIf x y z =
      [TokenAnIdent "if"] <> x <>
        [TokenAnIdent "then"] <> y <>
        [TokenAnIdent "else"] <> z

    ifE :: Expr -> Expr -> Expr -> Expr
    ifE x y z = ExprSyntax $ If x y z
