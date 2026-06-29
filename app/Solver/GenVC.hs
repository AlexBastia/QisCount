{-| 
Module      : Logic.GenVC
Description : Generates VCs for a given triple

Implements Hoare rules and backpropagation to generate the Verification Conditions
of a minimaly annotated program (only While-loop assertions) given pre and post conditions.
-}

module Solver.GenVC where

import Program.Syntax
import Expression.Syntax
import Solver.Solver (verifyVC)

import qualified Data.SBV as S

isProved :: S.ThmResult -> Bool
isProved (S.ThmResult (S.Unsatisfiable _ _)) = True
isProved _ = False

-- | Generates the final Verification Condition of a given Hoare triple
genVC :: HoareTriple -> IO BExpr
genVC (HoareTriple p c q) = do
    wpResult <- wp c q
    return $ Imp p wpResult

---------------------
-- * VC generators --
---------------------

{-| Calculates the Weakest Precondition given a post-condition and a command.
    Operates in the IO monad to support on-the-fly SMT verification. -}
wp :: Command -> BExpr -> IO BExpr
wp (Assign ide e) q                  = return $ subBool q ide e
wp (UnitOp ide (MonGate g e)) q      = return $ subBool q ide (QE $ QF1 g (QIde ide) e)
wp (UnitOp ide (BinGate g e1 e2)) q  = return $ subBool q ide (QE $ QF2 g (QIde ide) e1 e2)
wp (Measure ide e1 e2) q             = return $ subBool q ide (QE $ QF2 MES (QIde ide) e1 e2)
wp (Program.Syntax.Compose ide qc) q = return $ subBool q ide (QE qc)
wp (AddQ ide e) q                    = return $ subBool q ide (QE $ QF1 ADQ (QIde ide) e)
wp (AddB ide e) q                    = return $ subBool q ide (QE $ QF1 ADB (QIde ide) e)

wp (If cond c1 mc2) q = do
    wp1 <- wp c1 q
    case mc2 of
        (Just c2) -> do
            wp2 <- wp c2 q
            return $ And (Imp cond wp1) (Imp (Not cond) wp2)
        Nothing -> 
            return $ And (Imp cond wp1) (Imp (Not cond) q)

wp (While cond inv c) q = do
    bodyWp <- wp c inv
    
    res1 <- verifyVC (Imp (And inv cond) bodyWp)
    checkResult res1
    
    res2 <- verifyVC (Imp (And inv (Not cond)) q)
    checkResult res2
    
    return inv
  where
    checkResult res = 
        if isProved res 
            then return () 
            else error $ "Verification failed! The following VC regarding a while loop invariant failed: " ++ show res

wp (Seq (s:ss)) q = do
    lastWp <- wp (last (s:ss)) q
    wp (Seq (init (s:ss))) lastWp
wp (Seq []) q = return q


---------------------
-- * Substitutions --
---------------------

-- | Performs boolean variable name substitutions
subBool :: BExpr -> Vname -> Expr -> BExpr
subBool (BIde be) ide (BE val)
    | be == ide = val
    | otherwise = BIde be
subBool (Comparison op ae1 ae2) _ (BE _) = Comparison op ae1 ae2
subBool (Comparison op ae1 ae2) ide val = Comparison op (subArithm ae1 ide val) (subArithm ae2 ide val)
subBool (And be1 be2) ide val = And (subBool be1 ide val) (subBool be2 ide val)
subBool (Or be1 be2) ide val = Or (subBool be1 ide val) (subBool be2 ide val)
subBool (Imp be1 be2) ide val = Imp (subBool be1 ide val) (subBool be2 ide val)
subBool (Not be) ide val = Not (subBool be ide val)
subBool e _ _ = e

-- | Performs integer variable name substitutions
subArithm :: AExpr -> Vname -> Expr -> AExpr
subArithm (AIde ae) ide (AE val)
    | ae == ide = val
    | otherwise = AIde ae
subArithm e _ (BE _) = e
subArithm (BinOp op ae1 ae2) ide val = BinOp op (subArithm ae1 ide val) (subArithm ae2 ide val)
subArithm (MonOp op ae) ide val = MonOp op (subArithm ae ide val)
subArithm (QOp op qe aes) ide val = QOp op (subQCirc qe ide val) (subAL aes ide val)
subArithm e _ _ = e

-- | Performs quantum cirucit variable name substitutions
subQCirc :: QExpr -> Vname -> Expr -> QExpr
subQCirc (QIde qe) ide (QE val)
    | qe == ide = val
    | otherwise = QIde qe
subQCirc e _ (BE _) = e
subQCirc (QC ae1 ae2) ide val = QC (subArithm ae1 ide val) (subArithm ae2 ide val)
subQCirc (Expression.Syntax.Compose qe1 qe2) ide val = Expression.Syntax.Compose (subQCirc qe1 ide val) (subQCirc qe2 ide val)
subQCirc (QF1 op qe ae) ide val = QF1 op (subQCirc qe ide val) (subArithm ae ide val)
subQCirc (QF2 op qe ae1 ae2) ide val = QF2 op (subQCirc qe ide val) (subArithm ae1 ide val) (subArithm ae2 ide val)
subQCirc e _ _ = e

{-| Substitutes same variable name for same value in list of
    arithmetic expressions -}
subAL :: [AExpr] -> String -> Expr -> [AExpr]
subAL [] _ _ = []
subAL (ae:aes) ide val = subArithm ae ide val : subAL aes ide val