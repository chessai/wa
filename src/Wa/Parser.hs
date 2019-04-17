{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}

module Wa.Parser where

import Data.Kind
import GHC.Natural
import Wa.Api
import Wa.Dual
import Wa.Types

import Data.Functor.Const
import Control.Applicative (empty, (<|>))
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

{- |

since this is intended to run in a REPL (or cmdline), we only worry about a single-line at a time. non-anonymous identifiers must be known (i.e. appear in order). there are currently no boolean types or functions, no loops, and no listlike types.

we only infer Double if it is demanded by a type signature. In other words,
we default to `Dual`.

expressions:
  a ::= (-) dual | nat | (-) f64 | \x -> f x
  binopa ::= + | * | - | /
  unopa ::= negate | abs | signum | exp | log | sin | cos | tan
           | asin | acos | atan | sinh | cosh | tanh | asinh
           | acosh | atanh

statements:
  S ::= x := a | S1; S2 | ( S )
       | diff (a : Nat) (a : F64) (a : Dual -> Dual)
       | (a : u -> v) (a : u)
-}

--data Statement f u
--  = Assign String (Expr f u)
--  | Execute (Expr f u)

type Parser = Parsec Void String

-- | Single-line comments begin with "#".
--   There are no multi-line comments.
sc :: Parser ()
sc = L.space space1 lineComment empty
  where
    lineComment = L.skipLineComment "#"

-- | Consume whitespace /after/ every lexeme automatically, but not before it.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse some string and the whitespace after it.
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parse something between parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

nat :: Parser Natural
nat = lexeme L.decimal

f64 :: Parser Double
f64 = lexeme L.float

dual :: Parser D
dual = (\x -> D x 0) <$> f64

{-
-- | Parse a natural number.
nat :: Parser (Expr f 'Nat)
nat = (Lit . ValueNat) <$> lexeme L.decimal

-- | Parse a double.
f64 :: Parser (Expr f 'F64)
f64 = (Lit . ValueF64) <$> lexeme L.float

-- | Parse a dual number.
dual :: Parser (Expr f 'Dual)
dual = do
  x <- f64
  pure undefined 
-}
 
-- | Parse a semicolon.
semi :: Parser String
semi = symbol ";"

-- To parse various operators we can just use `symbol`, but reserved words
-- and identifiers are a bit trickier. There are two things to note:
--
--   * Parsers for reserved words should check that the parsed reserved word
--     is not a prefix/suffix of an identifier.
-- 
--   * Parsers of identifiers should check that the parsed identifier is not
--     a reserved word.

rword :: String -> Parser ()
rword w = lexeme . try $ string w *> notFollowedBy alphaNumChar

rws :: [String]
rws = []

identifier :: Parser String
identifier = lexeme . try $ p >>= check
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
      then fail $ "keyword " ++ show x ++ " cannot be an identifier"
      else pure x

-- A program in this language is simply a statement -
-- so the main parser need only parse a statement. we must remember
-- to take care of initial whitespace - our parsers only get rid of
-- whitespace /after/ the tokens.

--whileParser :: Parser Statement
--whileParser = between sc _ stmt

--stmt :: Parser Statement
--stmt = f <$> 

class ParseValue (u :: Universe) where
  type PV u = ty | ty -> u
  pv :: PV u

instance ParseValue 'Nat where
  type PV 'Nat = Parser (Value 'Nat)
  pv = fmap ValueNat nat

instance ParseValue 'F64 where
  type PV 'F64 = Parser (Value 'F64)
  pv = fmap ValueF64 f64

instance ParseValue 'Dual where
  type PV 'Dual = Parser (Value 'Dual)
  pv = fmap ValueDual dual

{-
assignStatement :: Parser (Statement 'Assignment f u)
assignStatement = do
  var <- identifier
  void (symbol "=")
  expr <- pure undefined
  pure (Assign var expr)

executeStatement :: Parser (Statement 'Execution f u)
executeStatement = do
  expr <- pure undefined
  pure (Execute expr)

dualExpr :: Parser (Expr f 'Dual)
dualExpr = makeExprParser dualTerm dualOperators

dualTerm :: Parser (Expr f 'Dual)
dualTerm = parens dualExpr
  <|> (do var <- identifier
-}
          
data Action = Assignment | Execution

data Statement :: Action -> Universe -> Type where
  Assign :: ()
    => String
    -> Expr f u
    -> Statement 'Assignment u
  Execute :: ()
    => Expr f u
    -> Statement 'Execution u
     
dualOperators :: [[Operator Parser (Expr f 'Dual)]]
dualOperators =
  [ [ Prefix (Negate <$ symbol "-")
    , Prefix (Abs <$ symbol "abs")
    , Prefix (Signum <$ (symbol "signum" <|> symbol "sign"))
    , Prefix (Exp <$ symbol "exp")
    , Prefix (Log <$ symbol "log")
    , Prefix (Sin <$ symbol "sin")
    , Prefix (Cos <$ symbol "cos")
    , Prefix (Tan <$ symbol "tan")
    , Prefix (Asin <$ symbol "asin")
    , Prefix (Acos <$ symbol "acos")
    , Prefix (Atan <$ symbol "atan")
    , Prefix (Sinh <$ symbol "sinh")
    , Prefix (Cosh <$ symbol "cosh")
    , Prefix (Tanh <$ symbol "tanh")
    , Prefix (Asinh <$ symbol "asinh")
    , Prefix (Acosh <$ symbol "acosh")
    , Prefix (Atanh <$ symbol "atanh")
    ]
  , [ InfixL (Times <$ symbol "*")
    , InfixL (FracDiv <$ symbol "/")
    ] 
  , [ InfixL (Plus <$ symbol "+")
    , InfixL (Plus <$ symbol "-")
    ]
  ]
