module AssertLogic.Parser where

import AssertLogic.Syntax
    ( ExtendedAExpr,
      ExtendedQExpr(QIde, QC, Measure, Compose, AddQ, AddB, UnitOp), Assertion, QOpExt (Wid, GC, Dep) )
import Program.Lexer
import Program.QuantumGates
import Program.Syntax (Type(TyQC))
import Text.Megaparsec (choice, sepBy1, (<|>), MonadParsec (try))
import qualified Data.Map as M
import qualified Control.Monad.State as S
import Control.Monad.Combinators.Expr (makeExprParser)
import Expression.Parser (pAIde, aOpTable, bOpTable, pBIde, pCOpName)
import Expression.Syntax (ArithmeticFormula(NumLit, Ext), BooleanFormula (BoolLit, Comparison))

pAssertion :: Parser Assertion
pAssertion = makeExprParser pAssertionTerm bOpTable

pAssertionTerm :: Parser Assertion
pAssertionTerm = choice
    [ try pExtComparison
    , BoolLit <$> (True <$ symbol "true" <|> False <$ symbol "false")
    , pBIde
    , parens pAssertion]

pExtComparison :: Parser Assertion
pExtComparison = do
    left <- pExtAExpr
    op <- pCOpName
    Comparison op left <$> pExtAExpr

-- Arithmetic Parser
pExtAExpr :: Parser ExtendedAExpr
pExtAExpr = makeExprParser pExtATerm aOpTable

pExtATerm :: Parser ExtendedAExpr
pExtATerm = choice
    [ pAIde
    , NumLit <$> integer
    , pQOp ]

pQOp :: Parser ExtendedAExpr
pQOp = choice
    [ Ext . Wid <$> (symbol "width" *> parens pExtQExpr)
    , Ext . GC <$> (symbol "gatecount" *> parens pExtQExpr)
    , symbol "depth" *> (Ext <$> parens (Dep <$> pExtQExpr <*> (symbol "," *> pExtAExpr)))]

-- Quantum Circuit Parser
pExtQExpr :: Parser ExtendedQExpr
pExtQExpr = choice
    [ pQIde
    , symbol "QuantumCircuit" *> parens (QC <$> pExtAExpr <*> (symbol "," *> pExtAExpr))
    , symbol "measure" *> parens (Measure <$> pExtQExpr <*> (symbol "," *> pExtAExpr) <*> (symbol "," *> pExtAExpr))
    , symbol "compose" *> parens (Compose <$> pExtQExpr <*> (symbol "," *> pExtQExpr))
    , symbol "addQbits" *> parens (AddQ <$> pExtQExpr <*> (symbol "," *> pExtAExpr))
    , symbol "addBits" *> parens (AddB <$> pExtQExpr <*> (symbol "," *> pExtAExpr))
    , pUnitOp
    ]

pQIde :: Parser ExtendedQExpr
pQIde = do
    ide <- identifier
    mt <- S.gets (M.lookup ide)
    case mt of
        Just TyQC -> return $ QIde ide
        Just t -> fail ("Variable '" ++ show ide ++ "' has wrong type '" ++ show t ++ "', expected '" ++ show TyQC ++ "'")
        Nothing -> do
            S.modify (M.insert ide TyQC)
            return $ QIde ide

pUnitOp :: Parser ExtendedQExpr
pUnitOp = do
    gateName <- pGateName

    (qc, args) <- parens $ do
        q <- pExtQExpr
        _ <- symbol ","
        as <- sepBy1 pExtAExpr (symbol ",")
        return (q, as)

    gate <- pQG gateName args
    return $ UnitOp qc gate

pGateName :: Parser QGname
pGateName = choice
    [ G1 <$> pQG1name
    , G2 <$> pQG2name
    ]


pQG :: QGname -> [ExtendedAExpr] -> Parser (QuantumGate ExtendedAExpr)
pQG (G1 name) [a]          = return $ QG1 name a
pQG (G2 name) [a1, a2]     = return $ QG2 name a1 a2

pQG (G1 name) args = fail $ "Gate " ++ show name ++ " expects 1 parameter, but got " ++ show (length args)
pQG (G2 name) args = fail $ "Gate " ++ show name ++ " expects 2 parameters, but got " ++ show (length args)