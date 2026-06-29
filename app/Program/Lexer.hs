{-| 
Module      : Program.Lexer
Description : Usefull parser helpers for common tokens.

Implements parsers for symbols, lexemes, integers identifiers and gates.
Also includes helpers for brackets and the definition of the `Parser` type that includes the
`State` monad.
-}

module Program.Lexer where

import Program.Syntax
import Expression.Syntax

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as M
import qualified Control.Monad.State as S

-- | Map to store variable types
type Env = M.Map String Type 
-- | Parser monad transformer with String input stream and State inner monad
type Parser = ParsecT Void String (S.State Env)

-- | Space Consumer
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- | Lexeme wrapper
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Integer parser
integer :: Parser Integer
integer = lexeme L.decimal

-- | Variable name parser
identifier :: Parser Vname
identifier = (lexeme . try) (par >>= check)
  where
    par = (:) <$> letterChar <*> many alphaNumChar <* notFollowedBy (symbol "(")
    check x = if x `elem` rwords
                then fail $ "Cannot use " ++ show x ++ " as a variable name."
                else return x

-- | Gate Name parser
monGate :: Parser QF1name
monGate = choice
    [ H <$ symbol "H"
    , X <$ symbol "X"
    ]

binGate :: Parser QF2name
binGate = choice
    [ CR <$ symbol "CR"
    , CNOT <$ symbol "CNOT"
    , SWAP <$ symbol "SWAP"
    ]
    
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")