{-# LANGUAGE TupleSections #-}
module Program.Parser (parseProgram) where

import Program.Syntax
import Program.Lexer
import qualified Logic.Parser as L

import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Text.Megaparsec
import qualified Data.Map as M
import qualified Control.Monad.State as S

-- Boolean Expression Parser
pBExpr :: Parser Expr
pBExpr = makeExprParser pBTerm bOpTable

pBTerm :: Parser Expr
pBTerm = choice
    [ try pComparison
    , BoolLit <$> (True <$ symbol "True" <|> False <$ symbol "False")
    , pIde TyBool
    , parens pBExpr
    ]

pComparison :: Parser Expr
pComparison = do
    left <- pAExpr
    op <- pCOpName
    BinOp op left <$> pAExpr

pCOpName :: Parser Op2
pCOpName = choice
    [ Eq <$ symbol "=="
    , Ne <$ symbol "!="
    , Ge <$ symbol ">="
    , Le <$ symbol "<="
    , Gt <$ symbol ">"
    , Lt <$ symbol "<"
    ]

bOpTable :: [[Operator Parser Expr]]
bOpTable =
    [ [ Prefix (MonOp Not <$ symbol "!") ]
    , [ InfixL (BinOp And <$ symbol "&&") ]
    , [ InfixL (BinOp Or <$ symbol "||") ]
    ]

-- Arithmetic Expression Parser
pAExpr :: Parser Expr
pAExpr = makeExprParser pATerm aOpTable

pATerm :: Parser Expr
pATerm = choice
    [ pIde TyInt
    , NumLit <$> integer
    , parens pAExpr
    ]

aOpTable :: [[Operator Parser Expr]]
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

pIde :: Type -> Parser Expr
pIde exprType = do
    ide <- identifier
    mt <- S.gets (M.lookup ide)
    case mt of
        Just t -> 
            if t == exprType then return $ Ide ide t
            else fail ("Variable '" ++ show ide ++ "' of type '" ++ show t ++ "' doesn't match type '" ++ show exprType ++ "'")
        Nothing -> error ("Undefined variable: " ++ show ide)

-- Quantum Circuit Parser
pQExpr :: Parser Expr
pQExpr = choice
    [ pIde TyQC
    , symbol "QuantumCircuit" *> parens (QC <$> pAExpr <*> (symbol "," *> pAExpr))
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
        [ symbol "<-" *> pAssign l <* symbol ";"
        , symbol "." *> pMethod l <* symbol ";"
        ]

pAssign :: Vname -> Parser Command
pAssign l = do
    (e,t) <- choice
        [ (, TyQC) <$> try pQExpr -- TODO: Non e' bellissimo, ci sono altri modi senza il try?
        , (, TyBool) <$> try pBExpr
        , (, TyInt) <$> pAExpr
        ]
    S.modify (M.insert l t)
    return $ Assign l e t

pMethod :: Vname -> Parser Command
pMethod l = choice
    [ UnitOp l <$> pGate
    , symbol "measure" *> parens (Measure l <$> pAExpr <*> (symbol "," *> pAExpr))
    , symbol "compose" *> parens (Compose l <$> pQExpr)
    , symbol "addQbits" *> parens (AddQ l <$> pAExpr)
    , symbol "addBits" *> parens (AddB l <$> pAExpr)
    ]

pGate :: Parser Gate
pGate = choice
    [ MonGate <$> monGate <*> parens pAExpr
    , BinGate <$> binGate <*> (symbol "(" *> pAExpr) <*> (symbol "," *> pAExpr <* symbol ")")
    ]
pIf :: Parser Command
pIf = do
    _ <- symbol "if"
    cond <- pBExpr
    _ <- symbol "then"
    thenCom <- pCommand
    elseCom <- optional (symbol "else" *> pCommand)
    return (If cond thenCom elseCom)

pWhile :: Parser Command
pWhile = do
    _ <- symbol "while"
    cond <- pBExpr
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
    com <- pBlock
    _ <- symbol "["
    post <- L.pBExpr
    _ <- symbol "]"
    return $ HoareTriple pre com post

parseProgram :: String -> String -> Either (ParseErrorBundle String Void) HoareTriple
parseProgram input filepath =
    let parser = runParserT (sc *> pProgram <* eof) input filepath
        res = S.runState parser M.empty
    in case res of
        (Left err, _) -> Left err
        (Right x, _) -> Right x
