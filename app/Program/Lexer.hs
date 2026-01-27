module Program.Lexer where

import Program.Syntax

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
-- Space Consumer
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- Lexeme wrapper
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbol parser
symbol :: String -> Parser String
symbol = L.symbol sc

-- Integer parser
integer :: Parser Integer
integer = lexeme L.decimal

-- Variable parser
identifier :: Parser Vname
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar <* notFollowedBy (symbol "(")
    check x = if x `elem` rwords
                then fail $ "Cannot use " ++ show x ++ " as a variable name."
                else return x

-- Gate Name parser
monGate :: Parser Gate1
monGate = choice
    [ H <$ symbol "H"
    , X <$ symbol "X"
    ]

binGate :: Parser Gate2
binGate = choice
    [ CX <$ symbol "CX"
    , RX <$ symbol "RX"
    ]
    
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")