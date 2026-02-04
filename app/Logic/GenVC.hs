module Logic.GenVC where

import Program.Syntax
import qualified Logic.Syntax as L
import Solver.Solver (verify)


genVCs :: HoareTriple -> IO ()
genVCs (HoareTriple p (Seq cs) q) = genRSeq p (reverse cs) q
genVCs (HoareTriple p _ q) = error "Command isn't a Sequence"

genRSeq :: L.BExpr -> [Command] -> L.BExpr -> IO ()

genRSeq p [] q = do
    _ <- putStrLn ("hy: " ++ show p ++ "\nth: " ++ show q)
    res <- verify p q
    print res

genRSeq p (c:cs) q = do
    case c of
        Assign ide e -> genRSeq p cs (substitute q ide (makeLogic e))

        UnitOp ide (MonGate g e) ->
            let le = makeLogic e
                lg = translateGate (MG g)
            in case (le, lg) of
                (AE ae, L.M mg) -> genRSeq p cs (substitute q ide (QE $ L.QF1 mg (L.QIde ide) ae))
                _ -> error "Incorrect type"

        UnitOp ide (BinGate g e1 e2) ->
            let le1 = makeLogic e1
                le2 = makeLogic e2
                lg = translateGate (BG g)
            in case (le1, le2, lg) of
                (AE ae1, AE ae2, L.B bg) -> genRSeq p cs (substitute q ide (QE $ L.QF2 bg (L.QIde ide) ae1 ae2))
                _ -> error "Incorrect type"

        Measure ide e1 e2 ->
            let le1 = makeLogic e1
                le2 = makeLogic e2
            in case (le1, le2) of
                (AE ae1, AE ae2) -> genRSeq p cs (substitute q ide (QE $ L.QF2 L.MES (L.QIde ide) ae1 ae2))
                _ -> error "Incorrect type"

        Compose ide qc ->
            let lqc = makeLogic qc
            in case lqc of
                QE qe -> genRSeq p cs (substitute q ide (QE $ L.Compose (L.QIde ide) qe))
                _ -> error "Incorrect type"

        AddQ ide e ->
            let le = makeLogic e
            in case le of
                AE ae -> genRSeq p cs (substitute q ide (QE $ L.QF1 L.ADQ (L.QIde ide) ae))
                _ -> error "Incorrect type"

        AddB ide e ->
            let le = makeLogic e
            in case le of
                AE ae -> genRSeq p cs (substitute q ide (QE $ L.QF1 L.ADB (L.QIde ide) ae))
                _ -> error "Incorrect type"

        If cond c1 mc2 ->
            let lc = makeLogic cond
            in case (lc, mc2) of
                (BE be, Just c2) -> do
                    genRSeq p (c1:cs) (L.Imp be q)
                    genRSeq p (c2:cs) (L.Imp (L.Not be) q)
                (BE be, Nothing) -> do
                    genRSeq p (c1:cs) (L.Imp be q)
                    genRSeq p cs (L.Imp (L.Not be) q)
                _ -> error "Incorrect type"

        While cond assert com ->
            let lc = makeLogic cond
            in case lc of
                BE be -> do
                    -- There's also a possible check that ((lc and assert) com assert) is valid, but this seems kinda useless
                    -- as it doesn't guarantee that the invariant is valid
                    genRSeq p cs assert
                    genRSeq (L.And assert (L.Not be)) [] q
                _ -> error "Incorrect type"

        Seq s -> genRSeq p (reverse s ++ cs) q

data LExpr =
    BE L.BExpr | AE L.AExpr | QE L.QExpr

substitute :: L.BExpr -> Vname -> LExpr -> L.BExpr
substitute be ide val =
    let e = sub (BE be) ide val
    in case e of
        (BE sbe) -> sbe
        _ -> error "Incorrect type"

sub :: LExpr -> Vname -> LExpr -> LExpr

-- Boolean
sub (BE (L.BIde be)) ide (BE val)
    | be == ide = BE val
    | otherwise = BE (L.BIde be)

sub (BE (L.Comparison op ae1 ae2)) ide val =
    case val of
        BE _ -> BE (L.Comparison op ae1 ae2)
        aqVal ->
            let se1 = sub (AE ae1) ide aqVal
                se2 = sub (AE ae2) ide aqVal
            in case (se1, se2) of
                (AE sae1, AE sae2) -> BE (L.Comparison op sae1 sae2)
                _ -> error "Incorrect type"

sub (BE (L.And be1 be2)) ide val =
    let se1 = sub (BE be1) ide val
        se2 = sub (BE be2) ide val
    in case (se1, se2) of
        (BE sbe1, BE sbe2) -> BE (L.And sbe1 sbe2)
        _ -> error "Incorrect type"

sub (BE (L.Or be1 be2)) ide val =
    let se1 = sub (BE be1) ide val
        se2 = sub (BE be2) ide val
    in case (se1, se2) of
        (BE sbe1, BE sbe2) -> BE (L.Or sbe1 sbe2)
        _ -> error "Incorrect type"

sub (BE (L.Imp be1 be2)) ide val =
    let se1 = sub (BE be1) ide val
        se2 = sub (BE be2) ide val
    in case (se1, se2) of
        (BE sbe1, BE sbe2) -> BE (L.Imp sbe1 sbe2)
        _ -> error "Incorrect type"

sub (BE (L.Not be)) ide val =
    let se = sub (BE be) ide val
    in case se of
        BE sbe -> BE (L.Not sbe)
        _ -> error "Incorrect type"

-- Arithmetic
sub (AE (L.AIde ae)) ide (AE val)
    | ae == ide = AE val
    | otherwise = AE (L.AIde ae)

sub (AE (L.BinOp op ae1 ae2)) ide val =
    case val of
        BE _ -> AE (L.BinOp op ae1 ae2)
        aqVal ->
            let se1 = sub (AE ae1) ide aqVal
                se2 = sub (AE ae2) ide aqVal
            in case (se1, se2) of
                (AE sae1, AE sae2) -> AE (L.BinOp op sae1 sae2)
                _ -> error "Incorrect type"

sub (AE (L.MonOp op ae)) ide val =
    case val of
        BE _ -> AE (L.MonOp op ae)
        aqVal ->
            let se = sub (AE ae) ide aqVal
            in case se of
                AE sbe -> AE (L.MonOp op sbe)
                _ -> error "Incorrect type"

sub (AE (L.QOp op qe ae)) ide val =
    case val of
        BE _ -> AE (L.QOp op qe ae)
        aqVal ->
            let se = sub (QE qe) ide aqVal
                sae = subAL ae ide aqVal
            in case se of
                QE sqe -> AE (L.QOp op sqe sae)
                _ -> error "Incorrect type"

-- Quantum
sub (QE (L.QIde qe)) ide (QE val)
    | qe == ide = QE val
    | otherwise = QE (L.QIde qe)

sub (QE (L.QC ae1 ae2)) ide val =
    case val of
        BE _ -> QE (L.QC ae1 ae2)
        aqVal ->
            let se1 = sub (AE ae1) ide aqVal
                se2 = sub (AE ae2) ide aqVal
            in case (se1, se2) of
                (AE sae1, AE sae2) -> QE (L.QC sae1 sae2)
                _ -> error "Incorrect type"

sub (QE (L.Compose qe1 qe2)) ide val =
    case val of
        BE _ -> QE (L.Compose qe1 qe2)
        aqVal ->
            let se1 = sub (QE qe1) ide aqVal
                se2 = sub (QE qe2) ide aqVal
            in case (se1, se2) of
                (QE sqe1, QE sqe2) -> QE (L.Compose sqe1 sqe2)
                _ -> error "Incorrect type"

sub (QE (L.QF1 op qe ae)) ide val =
    case val of
        BE _ -> QE (L.QF1 op qe ae)
        aqVal ->
            let se1 = sub (QE qe) ide aqVal
                se2 = sub (AE ae) ide aqVal
            in case (se1, se2) of
                (QE sqe, AE sae) -> QE (L.QF1 op sqe sae)
                _ -> error "Incorrect type"

sub (QE (L.QF2 op qe ae1 ae2)) ide val =
    case val of
        BE _ -> QE (L.QF2 op qe ae1 ae2)
        aqVal ->
            let se1 = sub (QE qe) ide aqVal
                se2 = sub (AE ae1) ide aqVal
                se3 = sub (AE ae2) ide aqVal
            in case (se1, se2, se3) of
                (QE sqe, AE sae1, AE sae2) -> QE (L.QF2 op sqe sae1 sae2)
                _ -> error "Incorrect type"

sub e _ _ = e -- for literals

subAL :: [L.AExpr] -> String -> LExpr -> [L.AExpr]
subAL [] _ _ = []
subAL (ae:aes) ide val =
    let se = sub (AE ae) ide val
    in case se of
        (AE sae) -> sae : subAL aes ide val
        _ -> error "Incorrect type"

-- PROBLEMS:
-- 1) When translating identifiers, the type of the variable must be known -> Solved with a state (X)
-- 2) Translation of operators is kinda tedius -> Match the structure of the AST's (_)
makeLogic :: Expr -> LExpr
makeLogic (BoolLit b) = BE (L.BoolLit b)
makeLogic (NumLit b) = AE (L.NumLit b)
makeLogic (Ide ide t) =
    case t of
        TyBool -> BE (L.BIde ide)
        TyInt -> AE (L.AIde ide)
        TyQC -> QE (L.QIde ide)
makeLogic (MonOp op e) =
    let le = makeLogic e
    in case (op, le) of
        (Not, BE be) -> BE (L.Not be)
        (Neg, AE ae) -> AE (L.MonOp L.Neg ae)
        (Pos, AE ae) -> AE (L.MonOp L.Pos ae)
        _ -> error "Incorrect type"
makeLogic (BinOp op e1 e2) =
    let le1 = makeLogic e1
        le2 = makeLogic e2
    in case (op, le1, le2) of
        (Add, AE ae1, AE ae2) -> AE (L.BinOp L.Add ae1 ae2)
        (Sub, AE ae1, AE ae2) -> AE (L.BinOp L.Sub ae1 ae2)
        (Mult, AE ae1, AE ae2) -> AE (L.BinOp L.Mult ae1 ae2)
        (Div, AE ae1, AE ae2) -> AE (L.BinOp L.Div ae1 ae2)
        (And, BE be1, BE be2) -> BE (L.And be1 be2)
        (Or, BE be1, BE be2) -> BE (L.Or be1 be2)
        (Eq, AE ae1, AE ae2) -> BE (L.Comparison L.Eq ae1 ae2)
        (Ne, AE ae1, AE ae2) -> BE (L.Comparison L.Ne ae1 ae2)
        (Gt, AE ae1, AE ae2) -> BE (L.Comparison L.Gt ae1 ae2)
        (Lt, AE ae1, AE ae2) -> BE (L.Comparison L.Lt ae1 ae2)
        (Ge, AE ae1, AE ae2) -> BE (L.Comparison L.Ge ae1 ae2)
        (Le, AE ae1, AE ae2) -> BE (L.Comparison L.Le ae1 ae2)
        _ -> error "Incorrect type"
makeLogic (QC gs e1 e2) =
    let le1 = makeLogic e1
        le2 = makeLogic e2
    in case (gs, le1, le2) of
        ([], AE ae1, AE ae2) -> QE (L.QC ae1 ae2)
        _ -> error "Incorrect type"

data GateName
    = MG Gate1 | BG Gate2

translateGate :: GateName -> L.QFname
translateGate (MG H) = L.M L.H
translateGate (MG X) = L.M L.X
translateGate (BG RX) = L.B L.RX
translateGate (BG CX) = L.B L.CX