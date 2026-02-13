{-| 
Module      : Logic.GenVC
Description : Generates VCs for a given triple

Implements Hoare rules and backpropagation to generate the Verification Conditions
of a minimaly annotated program (only While-loop assertions) given pre and post conditions.
-}

module Solver.GenVC where

import Program.Syntax
import Expression.Syntax

-- | Generates array of VCs for a given Hoare triple
genVCs :: HoareTriple -> [BExpr]
genVCs (HoareTriple p (Seq cs) q) = genRSeq p (reverse cs) q
genVCs _ = error "Command isn't a Sequence"

---------------------
-- * VC generators --
---------------------

{-| Generates array of VCs given pre and post conditions formulas 
and a list of commands in reverse order -}
genRSeq :: BExpr -> [Command] -> BExpr -> [BExpr]

genRSeq p [] q = [Imp p q]

genRSeq p (c:cs) q = do
    case c of
        Assign ide e -> genRSeq p cs (subBool q ide e)
        UnitOp ide (MonGate g e) -> 
            genRSeq p cs (subBool q ide (QE $ QF1 g (QIde ide) e))
        UnitOp ide (BinGate g e1 e2) -> 
            genRSeq p cs (subBool q ide (QE $ QF2 g (QIde ide) e1 e2))
        Measure ide e1 e2 -> 
            genRSeq p cs (subBool q ide (QE $ QF2 MES (QIde ide) e1 e2))
        Program.Syntax.Compose ide qc -> genRSeq p cs (subBool q ide (QE qc))
        AddQ ide e -> genRSeq p cs (subBool q ide (QE $ QF1 ADQ (QIde ide) e))
        AddB ide e -> genRSeq p cs (subBool q ide (QE $ QF1 ADB (QIde ide) e))

        If cond c1 mc2 ->
            case mc2 of
                (Just c2) -> genRSeq p (c1:cs) (Imp cond q) ++ genRSeq p (c2:cs) (Imp (Not cond) q)
                Nothing -> genRSeq p (c1:cs) (Imp cond q) ++ genRSeq p cs (Imp (Not cond) q)

        While cond assert com -> 
            genRSeq p cs assert ++ genRSeq (And cond assert) [com] assert ++ genRSeq (And assert (Not cond)) [] q

        Seq s -> genRSeq p (reverse s ++ cs) q


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