module Parser (parseProgram) where

import Syntax

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

type Parser = Parsec Void String

----------- 
-- Lexer --
-----------

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
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rwords
                then fail $ "Cannot use " ++ show x ++ " as a variable name."
                else return x

-- Gate Name parser
gate :: Parser Gate
gate = choice
    [ H <$ symbol "H"
    , CX <$ symbol "CX"
    , X <$ symbol "X"
    , RX <$ symbol "RX"
    ]

-- Quantum Circuit parser
qcirc :: Parser Expr
qcirc = symbol "QuantumCircuit" *> parens (QC <$> pExpr <*> (symbol "," *> pExpr))

------------
-- Parser --
------------

-- Expression Parser

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pTerm :: Parser Expr
pTerm = choice
    [ NumLit <$> integer
    , qcirc
    , Ide <$> identifier
    , BoolLit <$> (True <$ symbol "True" <|> False <$ symbol "False")
    , parens pExpr
    ]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ Prefix (MonOp Not <$ symbol "!")
      , Prefix (MonOp Neg <$ symbol "-")
      , Prefix (MonOp Pos <$ symbol "+") ]
    , [ InfixL (BinOp Mult <$ symbol "*")
      , InfixL (BinOp Div <$ symbol "/") ]
    , [ InfixL (BinOp Add <$ symbol "+")
      , InfixL (BinOp Sub <$ symbol "-") ]
    , [ InfixN (BinOp Lt <$ symbol "<=")
      , InfixN (BinOp Gt <$ symbol ">=")
      , InfixN (BinOp Ge <$ symbol ">")
      , InfixN (BinOp Le <$ symbol "<") 
      , InfixN (BinOp Eq <$ symbol "==") ]
    , [ InfixL (BinOp And  <$ symbol "&&") ]
    , [ InfixL (BinOp Or   <$ symbol "||") ]
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Command Parser

pCommand :: Parser Command
pCommand = choice
    [ pBlock 
    , pStmt
    , pIf
    , pWhile
    ]

pBlock :: Parser Command
pBlock = between (symbol "{") (symbol "}") $ Seq <$> (many pCommand)

pStmt :: Parser Command
pStmt = do
    l <- identifier
    choice
        [ symbol "<-" *> (Assign l <$> pExpr <* symbol ";")
        , symbol "." *> pMethod l <* symbol ";"
        ]

pMethod :: Vname -> Parser Command
pMethod l = choice
    [ UnitOp l <$> gate <*> parens (pExpr `sepBy` symbol ",")
    , symbol "measure" *> parens (Measure l <$> pExpr <*> (symbol "," *> pExpr))
    , symbol "compose" *> parens (Comp l <$> pExpr)
    , symbol "addQbits" *> parens (AddQ l <$> pExpr)
    , symbol "addBits" *> parens (AddB l <$> pExpr)
    ]

pIf :: Parser Command
pIf = do
    _ <- symbol "if"
    cond <- pExpr
    _ <- symbol "then"
    thenCom <- pCommand
    elseCom <- optional (symbol "else" *> pCommand)
    return (If cond thenCom elseCom)

pWhile :: Parser Command
pWhile = do
    _ <- symbol "while"
    cond <- pExpr
    _ <- symbol ":"
    whileCom <- pCommand
    return (While cond whileCom)

-- Main Parser

parseProgram :: String -> String -> Either (ParseErrorBundle String Void) Command
parseProgram filepath input = parse (sc *> pCommand <* eof) filepath input