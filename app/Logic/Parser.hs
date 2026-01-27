module Logic.Parser (pBExpr) where

import Logic.Syntax
import Program.Lexer

import Control.Monad.Combinators.Expr
import Text.Megaparsec

-- Boolean Parser (parses the whole formula)
pBExpr :: Parser BExpr
pBExpr = makeExprParser pBTerm bOpTable

pBTerm :: Parser BExpr
pBTerm = choice
    [ try pComparison 
    , BoolLit <$> (True <$ symbol "True" <|> False <$ symbol "False")
    , BIde <$> identifier
    , parens pBExpr
    ]

pComparison :: Parser BExpr
pComparison = do 
    left <- pAExpr
    op <- pCOpName
    right <- pAExpr
    return (Comparison op left right)

pCOpName :: Parser COp
pCOpName = choice
    [ Eq <$ symbol "=="
    , Ne <$ symbol "!="
    , Ge <$ symbol ">="
    , Le <$ symbol "<="
    , Gt <$ symbol ">"
    , Lt <$ symbol "<"
    ]

bOpTable :: [[Operator Parser BExpr]]
bOpTable =
    [ [ Prefix (Not <$ symbol "!") ]
    , [ InfixL (And  <$ symbol "&&") ]
    , [ InfixL (Or   <$ symbol "||") ]
    ]

-- Arithmetic Parser
pAExpr :: Parser AExpr
pAExpr = makeExprParser pATerm aOpTable

pATerm :: Parser AExpr
pATerm = choice
    [ AIde <$> identifier
    , NumLit <$> integer
    , parens pAExpr
    , QOp <$> pQOpName <*> (symbol "(" *> pQExpr) <*> many (symbol "," *> pAExpr) <* symbol ")"
    ]

pQOpName :: Parser QOpName
pQOpName = choice
    [ Wid <$ symbol "width"
    , GC <$ symbol "gatecount"
    , Dep <$ symbol "depth"]

aOpTable :: [[Operator Parser AExpr]]
aOpTable = 
    [ [ Prefix (MonOp Neg <$ symbol "-")
      , Prefix (MonOp Pos <$ symbol "+") 
      ]
    , [ InfixL (BinOp Mult <$ symbol "*")
      , InfixL (BinOp Div <$ symbol "/") 
      ]
    , [ InfixL (BinOp Add <$ symbol "+")
      , InfixL (BinOp Sub <$ symbol "-") 
      ] ]

-- Quantum Circuit Parser
pQExpr :: Parser QExpr
pQExpr = choice
    [ QIde <$> identifier
    , symbol "QuantumCircuit" *> parens (QC <$> pAExpr <*> (symbol "," *> pAExpr))
    , symbol "compose" *> parens (Compose <$> pQExpr <*> (symbol "," *> pQExpr))
    , QF <$> pQFName <*> (symbol "(" *> pQExpr) <*> (symbol "," *> pAExpr `sepBy` symbol "," <* symbol ")")
    ]

pQFName :: Parser QFname
pQFName = choice
    [ symbol "append" *> (APP <$> pGname)
    , MES <$ symbol "measure" 
    , ADQ <$ symbol "addQbits"
    , ADB <$ symbol "addBits"
    ]

pGname :: Parser Gname
pGname = choice
    [ H <$ symbol "H"
    , X <$ symbol "X"
    , RX <$ symbol "RX"
    , CX <$ symbol "CX"]