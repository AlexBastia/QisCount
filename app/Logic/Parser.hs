{-| 
Module      : Logic.Parser
Description : Parses logic formulas

Implements parsers for logical expressions (preconditions, postconditions and assertions). Uses
the same parser monad transformer and state map as the program parser.
-}

module Logic.Parser where

import Logic.Syntax
import Program.Lexer
import qualified Program.Syntax as P

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import qualified Data.Map as M
import qualified Control.Monad.State as S

-- Boolean Parser (parses the whole formula)
pBExpr :: Parser BExpr
pBExpr = makeExprParser pBTerm bOpTable

pBTerm :: Parser BExpr
pBTerm = choice
    [ try pComparison
    , BoolLit <$> (True <$ symbol "True" <|> False <$ symbol "False")
    , pBIde
    , parens pBExpr
    ]

pBIde :: Parser BExpr
pBIde = do
    ide <- identifier
    mt <- S.gets (M.lookup ide)
    case mt of
        Just P.TyBool -> return $ BIde ide
        Just t -> fail ("Variable '" ++ show ide ++ "' has wrong type '" ++ show t ++ "', expected '" ++ show P.TyBool ++ "'")
        Nothing -> do
            S.modify (M.insert ide P.TyBool)
            return $ BIde ide

pComparison :: Parser BExpr
pComparison = do
    left <- pAExpr
    op <- pCOpName
    Comparison op left <$> pAExpr

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
    [ pAIde
    , NumLit <$> integer
    , parens pAExpr
    , QOp <$> pQOpName <*> (symbol "(" *> pQExpr) <*> many (symbol "," *> pAExpr) <* symbol ")"
    ]

pAIde :: Parser AExpr
pAIde = do
    ide <- identifier
    mt <- S.gets (M.lookup ide)
    case mt of
        Just P.TyInt -> return $ AIde ide
        Just t -> fail ("Variable '" ++ show ide ++ "' has wrong type '" ++ show t ++ "', expected '" ++ show P.TyInt ++ "'")
        Nothing -> do
            S.modify (M.insert ide P.TyInt)
            return $ AIde ide

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
    [ pQIde
    , symbol "QuantumCircuit" *> parens (QC <$> pAExpr <*> (symbol "," *> pAExpr))
    , symbol "compose" *> parens (Compose <$> pQExpr <*> (symbol "," *> pQExpr))
    , QF1 <$> pQF1Name <*> (symbol "(" *> pQExpr) <*> pAExpr <* symbol ")"
    , QF2 <$> pQF2Name <*> (symbol "(" *> pQExpr) <*> (symbol "," *> pAExpr) <*> (symbol "," *> pAExpr <* symbol ")")
    ]

pQIde :: Parser QExpr
pQIde = do
    ide <- identifier
    mt <- S.gets (M.lookup ide)
    case mt of
        Just P.TyQC -> return $ QIde ide
        Just t -> fail ("Variable '" ++ show ide ++ "' has wrong type '" ++ show t ++ "', expected '" ++ show P.TyQC ++ "'")
        Nothing -> do
            S.modify (M.insert ide P.TyQC)
            return $ QIde ide

pQF1Name :: Parser QF1name
pQF1Name = choice
    [ symbol "append" *> lookAhead pGname1
    , ADQ <$ symbol "addQbits"
    , ADB <$ symbol "addBits"
    ]

pQF2Name :: Parser QF2name
pQF2Name = choice
    [ symbol "append" *> pGname2
    , MES <$ symbol "measure"
    ]

pGname1 :: Parser QF1name
pGname1 = choice
    [ H <$ symbol "H"
    , X <$ symbol "X"
    ]

pGname2 :: Parser QF2name
pGname2 = choice
    [ RX <$ symbol "RX"
    , CX <$ symbol "CX"]