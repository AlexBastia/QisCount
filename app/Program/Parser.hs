module Program.Parser (parseProgram) where

import Program.Syntax
import Program.Lexer
import qualified Logic.Parser as L

import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Text.Megaparsec

-- Expression Parser

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pTerm :: Parser Expr
pTerm = choice
    [ NumLit <$> integer
    , pQcirc
    , Ide <$> identifier
    , BoolLit <$> (True <$ symbol "True" <|> False <$ symbol "False")
    , parens pExpr
    ]

pQcirc :: Parser Expr
pQcirc = symbol "QuantumCircuit" *> parens (QC [] <$> pExpr <*> (symbol "," *> pExpr))

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ Prefix (MonOp Not <$ symbol "!")
      , Prefix (MonOp Neg <$ symbol "-")
      , Prefix (MonOp Pos <$ symbol "+") ]
    , [ InfixL (BinOp Mult <$ symbol "*")
      , InfixL (BinOp Div <$ symbol "/") ]
    , [ InfixL (BinOp Add <$ symbol "+")
      , InfixL (BinOp Sub <$ symbol "-") ]
    , [ InfixN (BinOp Le <$ symbol "<=")
      , InfixN (BinOp Ge <$ symbol ">=")
      , InfixN (BinOp Gt <$ symbol ">")
      , InfixN (BinOp Lt <$ symbol "<")
      , InfixN (BinOp Ne <$ symbol "!=")
      , InfixN (BinOp Eq <$ symbol "==") ]
    , [ InfixL (BinOp And  <$ symbol "&&") ]
    , [ InfixL (BinOp Or   <$ symbol "||") ]
    ]

-- Command Parser

pCommand :: Parser Command
pCommand = choice
    [ pBlock
    , pStmt
    , pIf
    , pWhile
    ]

pBlock :: Parser Command
pBlock = between (symbol "{") (symbol "}") $ Seq <$> many pCommand

pStmt :: Parser Command
pStmt = do
    l <- identifier
    choice
        [ symbol "<-" *> (Assign l <$> pExpr <* symbol ";")
        , symbol "." *> pMethod l <* symbol ";"
        ]

pMethod :: Vname -> Parser Command
pMethod l = choice
    [ UnitOp l <$> pGate
    , symbol "measure" *> parens (Measure l <$> pExpr <*> (symbol "," *> pExpr))
    , symbol "compose" *> parens (Compose l <$> pExpr)
    , symbol "addQbits" *> parens (AddQ l <$> pExpr)
    , symbol "addBits" *> parens (AddB l <$> pExpr)
    ]

pGate :: Parser Gate
pGate = choice
    [ MonGate <$> monGate <*> parens pExpr
    , BinGate <$> binGate <*> (symbol "(" *> pExpr) <*> (symbol "," *> pExpr <* symbol ")")
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
    _ <- symbol "["
    annot <- L.pBExpr
    _ <- symbol "]"
    While cond annot <$> pCommand


-- Main Parser

pProgram :: Parser HoareTriple
pProgram = do
    _ <- symbol "["
    pre <- L.pBExpr
    _ <- symbol "]"
    com <- pCommand
    _ <- symbol "["
    post <- L.pBExpr
    _ <- symbol "]"
    return $ HoareTriple pre com post

parseProgram :: String -> String -> Either (ParseErrorBundle String Void) HoareTriple
parseProgram filepath input = parse (sc *> pProgram <* eof) filepath input