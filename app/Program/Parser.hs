{-| 
Module      : Program.Parser
Description : Parses the program.

Includes parsers for Proto-Qiskit commands built using the expression parsers and defines
the parser for the entire triple. 
-}

module Program.Parser where

import Program.Syntax
import Program.Lexer
import Expression.Parser (pBExpr, pAExpr, pQExpr)

import Data.Void (Void)
import Text.Megaparsec
import qualified Data.Map as M
import qualified Control.Monad.State as S
import Program.QuantumGates (QuantumGate (QG1, QG2))
import Expression.Syntax (AExpr, Expr (QE, BE, AE))
import AssertLogic.Parser (pAssertion)

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
        [ (\e -> (QE e, TyQC)) <$> try pQExpr -- TODO: Can we avoid using try?
        , (\e -> (BE e, TyBool)) <$> try pBExpr
        , (\e -> (AE e, TyInt)) <$> pAExpr
        ]
    mt <- S.gets (M.lookup l)
    case mt of
        Just a
            | t == a -> return $ Assign l e
            | otherwise -> error $ "Variable " ++ show l ++ " of type " ++ show a ++ " can't be assigned value of type " ++ show t
        Nothing -> do
            S.modify (M.insert l t)
            return $ Assign l e

pMethod :: Vname -> Parser Command
pMethod l = choice
    [ UnitOp l <$> pGate
    , symbol "measure" *> parens (Measure l <$> pAExpr <*> (symbol "," *> pAExpr))
    , symbol "compose" *> parens (Compose l <$> pQExpr)
    , symbol "addQbits" *> parens (AddQ l <$> pAExpr)
    , symbol "addBits" *> parens (AddB l <$> pAExpr)
    ]

pGate :: Parser (QuantumGate AExpr)
pGate = choice
    [ QG1 <$> pQG1name <*> parens pAExpr
    , QG2 <$> pQG2name <*> (symbol "(" *> pAExpr) <*> (symbol "," *> pAExpr <* symbol ")")
    ]

pIf :: Parser Command
pIf = do
    _ <- symbol "if"
    cond <- pBExpr
    _ <- symbol ":"
    thenCom <- pCommand
    elseCom <- optional (symbol "else" *> symbol ":" *> pCommand)
    return (If cond thenCom elseCom)

pWhile :: Parser Command
pWhile = do
    _ <- symbol "while"
    cond <- pBExpr
    _ <- symbol ":"
    _ <- symbol "["
    annot <- pBExpr
    _ <- symbol "]"
    While cond annot <$> pCommand


-- Main Parser

pProgram :: Parser HoareTriple
pProgram = do
    _ <- symbol "["
    pre <- pAssertion
    _ <- symbol "]"
    com <- pBlock
    _ <- symbol "["
    post <- pAssertion
    _ <- symbol "]"
    return $ HoareTriple pre com post

parseProgram :: String -> String -> Either (ParseErrorBundle String Void) HoareTriple
parseProgram input filepath =
    let parser = runParserT (sc *> pProgram <* eof) input filepath
        res = S.runState parser M.empty
    in case res of
        (Left err, _) -> Left err
        (Right x, _) -> Right x
