module Logic.GenVC where

import Program.Syntax
import qualified Logic.Syntax as L

genVCs :: HoareTriple -> [L.BExpr]
genVCs (HoareTriple p (Seq cs) q) = genRSeq p (reverse cs) q
genVCs _ = error "Command isn't a Sequence"

genRSeq :: L.BExpr -> [Command] -> L.BExpr -> [L.BExpr]

genRSeq p [] q = [L.Imp p q]

genRSeq p (c:cs) q = do
    case c of
        Assign ide e TyBool -> genRSeq p cs (subBool q ide (BE $ makeLogicBool e))
        Assign ide e TyInt -> genRSeq p cs (subBool q ide (AE $ makeLogicArithm e))
        Assign ide e TyQC -> genRSeq p cs (subBool q ide (QE $ makeLogicQCirc e))
        UnitOp ide (MonGate g e) -> genRSeq p cs (subBool q ide (QE $ L.QF1 (translateMonoGate g) (L.QIde ide) (makeLogicArithm e)))
        UnitOp ide (BinGate g e1 e2) -> genRSeq p cs (subBool q ide (QE $ L.QF2 (translateBinGate g) (L.QIde ide) (makeLogicArithm e1) (makeLogicArithm e2)))
        Measure ide e1 e2 -> genRSeq p cs (subBool q ide (QE $ L.QF2 L.MES (L.QIde ide) (makeLogicArithm e1) (makeLogicArithm e2)))
        Compose ide qc -> genRSeq p cs (subBool q ide (QE $ L.Compose (L.QIde ide) (makeLogicQCirc qc)))
        AddQ ide e -> genRSeq p cs (subBool q ide (QE $ L.QF1 L.ADQ (L.QIde ide) (makeLogicArithm e)))
        AddB ide e -> genRSeq p cs (subBool q ide (QE $ L.QF1 L.ADB (L.QIde ide) (makeLogicArithm e)))

        If cond c1 mc2 ->
            let be = makeLogicBool cond
            in case mc2 of
                (Just c2) -> genRSeq p (c1:cs) (L.Imp be q) ++ genRSeq p (c2:cs) (L.Imp (L.Not be) q)
                Nothing -> genRSeq p (c1:cs) (L.Imp be q) ++ genRSeq p cs (L.Imp (L.Not be) q)

-- TODO: does the second check on the invariant guarantee correctness?
        While cond assert com -> genRSeq p cs assert ++ genRSeq (L.And be assert) [com] assert ++ genRSeq (L.And assert (L.Not be)) [] q
            where
              be = makeLogicBool cond

        Seq s -> genRSeq p (reverse s ++ cs) q

data LExpr =
    BE L.BExpr | AE L.AExpr | QE L.QExpr

subBool :: L.BExpr -> Vname -> LExpr -> L.BExpr
subBool (L.BIde be) ide (BE val)
    | be == ide = val
    | otherwise = L.BIde be
subBool (L.Comparison op ae1 ae2) _ (BE _) = L.Comparison op ae1 ae2
subBool (L.Comparison op ae1 ae2) ide val = L.Comparison op (subArithm ae1 ide val) (subArithm ae2 ide val)
subBool (L.And be1 be2) ide val = L.And (subBool be1 ide val) (subBool be2 ide val)
subBool (L.Or be1 be2) ide val = L.Or (subBool be1 ide val) (subBool be2 ide val)
subBool (L.Imp be1 be2) ide val = L.Imp (subBool be1 ide val) (subBool be2 ide val)
subBool (L.Not be) ide val = L.Not (subBool be ide val)
subBool e _ _ = e

subArithm :: L.AExpr -> Vname -> LExpr -> L.AExpr
subArithm (L.AIde ae) ide (AE val)
    | ae == ide = val
    | otherwise = L.AIde ae
subArithm e _ (BE _) = e
subArithm (L.BinOp op ae1 ae2) ide val = L.BinOp op (subArithm ae1 ide val) (subArithm ae2 ide val)
subArithm (L.MonOp op ae) ide val = L.MonOp op (subArithm ae ide val)
subArithm (L.QOp op qe aes) ide val = L.QOp op (subQCirc qe ide val) (subAL aes ide val)
subArithm e _ _ = e

subQCirc :: L.QExpr -> Vname -> LExpr -> L.QExpr
subQCirc (L.QIde qe) ide (QE val)
    | qe == ide = val
    | otherwise = L.QIde qe
subQCirc e _ (BE _) = e
subQCirc (L.QC ae1 ae2) ide val = L.QC (subArithm ae1 ide val) (subArithm ae2 ide val)
subQCirc (L.Compose qe1 qe2) ide val = L.Compose (subQCirc qe1 ide val) (subQCirc qe2 ide val)
subQCirc (L.QF1 op qe ae) ide val = L.QF1 op (subQCirc qe ide val) (subArithm ae ide val)
subQCirc (L.QF2 op qe ae1 ae2) ide val = L.QF2 op (subQCirc qe ide val) (subArithm ae1 ide val) (subArithm ae2 ide val)
subQCirc e _ _ = e

subAL :: [L.AExpr] -> String -> LExpr -> [L.AExpr]
subAL [] _ _ = []
subAL (ae:aes) ide val = subArithm ae ide val : subAL aes ide val

makeLogicBool :: Expr -> L.BExpr
makeLogicBool (BoolLit b) = L.BoolLit b
makeLogicBool (Ide ide t) =
    case t of
        TyBool -> L.BIde ide
        ty -> error ("Variable '" ++ show ide ++ "' is of type '" ++ show ty ++ "', expected " ++ show TyBool)
makeLogicBool (MonOp Not e) = L.Not $ makeLogicBool e
makeLogicBool (BinOp And e1 e2) = L.And (makeLogicBool e1) (makeLogicBool e2)
makeLogicBool (BinOp Or e1 e2) = L.Or (makeLogicBool e1) (makeLogicBool e2)
makeLogicBool (BinOp Eq e1 e2) = L.Comparison L.Eq (makeLogicArithm e1) (makeLogicArithm e2)
makeLogicBool (BinOp Ne e1 e2) = L.Comparison L.Ne (makeLogicArithm e1) (makeLogicArithm e2)
makeLogicBool (BinOp Gt e1 e2) = L.Comparison L.Gt (makeLogicArithm e1) (makeLogicArithm e2)
makeLogicBool (BinOp Lt e1 e2) = L.Comparison L.Lt (makeLogicArithm e1) (makeLogicArithm e2)
makeLogicBool (BinOp Ge e1 e2) = L.Comparison L.Ge (makeLogicArithm e1) (makeLogicArithm e2)
makeLogicBool (BinOp Le e1 e2) = L.Comparison L.Le (makeLogicArithm e1) (makeLogicArithm e2)
makeLogicBool _ = error "Impossible to translate given expression into a BExpr"

makeLogicArithm :: Expr -> L.AExpr
makeLogicArithm (NumLit a) = L.NumLit a
makeLogicArithm (Ide ide t) =
    case t of
        TyInt -> L.AIde ide
        ty -> error ("Variable '" ++ show ide ++ "' is of type '" ++ show ty ++ "', expected " ++ show TyInt)
makeLogicArithm (MonOp Neg e) = L.MonOp L.Neg $ makeLogicArithm e
makeLogicArithm (MonOp Pos e) = L.MonOp L.Pos $ makeLogicArithm e
makeLogicArithm (BinOp Add e1 e2) = L.BinOp L.Add (makeLogicArithm e1) (makeLogicArithm e2)
makeLogicArithm (BinOp Sub e1 e2) = L.BinOp L.Sub (makeLogicArithm e1) (makeLogicArithm e2)
makeLogicArithm (BinOp Mult e1 e2) = L.BinOp L.Mult (makeLogicArithm e1) (makeLogicArithm e2)
makeLogicArithm (BinOp Div e1 e2) = L.BinOp L.Div (makeLogicArithm e1) (makeLogicArithm e2)
makeLogicArithm _ = error "Impossible to translate given expression into an AExpr"

makeLogicQCirc :: Expr -> L.QExpr
makeLogicQCirc (QC e1 e2) = L.QC (makeLogicArithm e1) (makeLogicArithm e2)
makeLogicQCirc (Ide ide t) =
    case t of
        TyQC -> L.QIde ide
        ty -> error ("Variable '" ++ show ide ++ "' is of type '" ++ show ty ++ "', expected " ++ show TyQC)
makeLogicQCirc _ = error "Impossible to translate given expression into a QExpr"

data GateName
    = MG Gate1 | BG Gate2

translateMonoGate :: Gate1 -> L.QF1name
translateMonoGate H = L.H
translateMonoGate X = L.X

translateBinGate :: Gate2 -> L.QF2name
translateBinGate RX = L.RX
translateBinGate CX = L.CX