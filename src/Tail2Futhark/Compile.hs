module Tail2Futhark.Compile (compile, getType) where

import Tail2Futhark.TAIL.AST as T -- the TAIL AST
import Tail2Futhark.Futhark.AST as F -- the futhark AST
import Tail2Futhark.Futhark.Pretty as F -- the futhark AST

import Prelude

import Data.Char
import Options (Options(..))

--------------------------
-- THE MAIN FUNCTION --
--------------------------

compile :: Options -> T.Program -> F.Program
compile opts e = F.Program $ includes ++ [F.FunDecl F32T "main" signature $ compileExp rootExp]
  where includes = if includeLibs opts then builtins else []
        (signature, rootExp) = compileReads e

-------------------------
-- HELPER FUNCTIONS --
-------------------------

compileReads :: T.Exp -> ([(F.Type, String)], T.Exp)
compileReads (T.Let v  _ (T.Op "readIntVecFile" _ _) e2) = ((F.ArrayT F.IntT , "t_" ++ v):sig,e')
  where (sig,e') = compileReads e2
compileReads (T.Let v  _ (T.Op "readDoubleVecFile" _ _) e2) = ((F.ArrayT F.F32T , "t_" ++ v):sig,e')
  where (sig,e') = compileReads e2
compileReads e = ([],e)

----------------------------------------
-- AUX FUNCTIONS OF LIBRARY FUNCTIONS --
----------------------------------------

absFloatExp :: F.Exp -> F.Exp
absFloatExp e = IfThenElse Inline (BinApp LessEq e (Constant (F32 0))) (F.Neg e) e

absExp :: F.Exp -> F.Exp
absExp e = IfThenElse Inline (BinApp LessEq e (Constant (Int 0))) (F.Neg e) e

maxExp :: F.Exp -> F.Exp -> F.Exp
maxExp e1 e2 = IfThenElse Inline (BinApp LessEq e1 e2) e2 e1

minExp :: F.Exp -> F.Exp -> F.Exp
minExp e1 e2 = IfThenElse Inline (BinApp LessEq e1 e2) e1 e2

signiExp, signdExp :: F.Exp -> F.Exp
signdExp e = IfThenElse Indent (BinApp Less (Constant (F32 0)) e) (Constant (Int 1)) elseBranch
  where elseBranch = IfThenElse Indent (BinApp Eq (Constant (F32 0)) e) (Constant (Int 0)) (Constant (Int (-1)))

signiExp e = IfThenElse Indent (BinApp Less (Constant (Int 0)) e) (Constant (Int 1)) elseBranch
  where elseBranch = IfThenElse Indent (BinApp Eq (Constant (Int 0)) e) (Constant (Int 0)) (Constant (Int (-1)))

nandExp :: F.Exp -> F.Exp -> F.Exp
nandExp e1 e2 = F.FunCall "!" [BinApp F.LogicAnd e1 e2] 

norExp :: F.Exp -> F.Exp -> F.Exp
norExp e1 e2 = F.FunCall "!" [BinApp F.LogicOr e1 e2]

resiExp :: F.Exp -> F.Exp -> F.Exp
resiExp y x = F.IfThenElse F.Indent (y `eq` izero) x $ F.IfThenElse F.Indent cond (x % y) (x % y `plus` y)
  where cond = ((x % y) `eq` izero) `lor` ((x `gr` izero) `land` (y `gr` izero)) `lor` ((x `less` izero) `land` (y `less` izero))
        infix 1 %; (%) = F.BinApp F.Mod
        izero = Constant (Int 0)
        plus = F.BinApp F.Plus
        gr = F.BinApp F.Greater
        less = F.BinApp F.Less
        eq = F.BinApp F.Eq
        lor = F.BinApp F.LogicOr
        land = F.BinApp F.LogicAnd

-- reshape1 --
-- create split part of reshape1 function -- 
mkSplit :: F.Ident -> F.Ident -> F.Exp -> F.Exp -> F.Exp -> F.Exp
mkSplit id1 id2 dims e =
  F.Let Inline (TouplePat [Ident id1,Ident id2]) (F.FunCall2 "split" [dims] e)

makeLets :: [(F.Ident, F.Exp)] -> F.Exp -> F.Exp
makeLets ((v,e) : rest) body = F.Let Indent (Ident v) e (makeLets rest body)
makeLets [] body = body

reshape1Body :: F.Type -> F.Exp
reshape1Body _ = makeLets (zip ["roundUp","extend"] [desired,reshapeCall]) split 
  where split = mkSplit "v1" "_" (F.Var "l") (F.Var "extend") (F.Var "v1")
        desired = F.Var "l" `fplus` (size `fminus` Constant (Int 1)) `fdiv` size
        reshapeCall = F.FunCall2 "reshape" [BinApp Mult size len] (F.FunCall "replicate" [len,F.Var "x"])
        size = F.FunCall "size" [Constant (Int 0),F.Var "x"]
        len = F.Var "roundUp"
        fdiv = BinApp Div
        fplus = BinApp Plus
        fminus = BinApp Minus

-- drop --
-- make body for drop1 function --
dropBody :: F.Type -> F.Exp
dropBody tp = IfThenElse Indent (size `less` absExp len) emptArr elseBranch
    where izero = Constant (Int 0)
          less = BinApp LessEq
          len = F.Var "l"
          size = F.FunCall "size" [izero, F.Var "x"]
          plus = BinApp Plus len size
          emptArr = F.Empty tp
          elseBranch = IfThenElse Indent (len `less` izero) negDrop posDrop
          negDrop = mkSplit "v1" "_" plus (F.Var "x") (F.Var "v1")
          posDrop = mkSplit "_" "v2" len (F.Var "x") (F.Var "v2")

-- take1 --
-- make body for take1 function --
takeBody :: F.Exp -> F.Exp
takeBody padElement = IfThenElse Indent (izero `less` len) posTake negTake
    where less = BinApp LessEq
          izero = Constant (Int 0)
          plus  = BinApp Plus len size
          len  = F.Var "l"
          size = F.FunCall "size" [izero, F.Var "x"]
          padRight = F.FunCall "concat" [F.Var "x", padding]
          padLeft = F.FunCall "concat" [padding, F.Var "x"]
          padding = F.FunCall "replicate" [BinApp Minus len size, padElement]
          posTake = IfThenElse Indent (len `less` size) (mkSplit "v1" "_" (F.Var "l") (F.Var "x") (F.Var "v1")) padRight
          negTake = IfThenElse Indent (izero `less` plus) (mkSplit "_" "v2" plus (F.Var "x") (F.Var "v2")) padLeft 


------------------------------------------
-- AUX FUNCTIONS FOR SPECIFIC FUNCTIONS --
------------------------------------------

-- AUX shape --
makeShape :: Integer -> [T.Exp] -> [F.Exp]
makeShape r args
  | [e] <- args = map (\x -> FunCall "size" [Constant (Int x), compileExp e]) [0..r-1]
  | otherwise = error "shape takes one argument"

-- AUX transp --
makeTransp :: Integer -> F.Exp -> F.Exp
makeTransp r = makeTransp2 (map (Constant . Int) (reverse [0..r-1]))

-- AUX transp2 --
makeTransp2 :: [F.Exp] -> F.Exp -> F.Exp
makeTransp2 = F.FunCall2 "rearrange"

---------------------------
-- GENERAL AUX FUNCTIONS --
---------------------------

-- make Futhark basic type from string representation --
readBType :: String -> F.Type
readBType tp = case tp of
  "int" -> F.IntT
  "f32" -> F.F32T
  "f64" -> F.F64T
  "bool" -> F.BoolT
  "char" -> F.IntT
  _ -> error $ "readBType: unhandled " ++ show tp

-- make Futhark type from string representation --
-- i.e., takes 2int and gives [[int]] --
getType :: String -> Maybe F.Type
getType s 
  | suffix `elem` ["int","f32","f64","bool","char"] = fmap (makeArrTp (readBType suffix)) r
  | otherwise = Nothing
  where (prefix,suffix) = span isDigit s
        r | [] <- prefix = Nothing 
          | otherwise = Just (read prefix :: Integer)

-- make list of Futhark basic types --
btypes :: [F.Type]
btypes = map readBType ["int","f32","f64","bool"]

-- return zero expression of basic type --
zero :: F.Type -> F.Exp
zero F.IntT = Constant (Int 0)
zero F.F32T = Constant (F32 0)
zero F.F64T = Constant (F64 0)
zero F.BoolT = Constant (Bool False)
zero tp = error $ "take for type " ++ pretty tp ++ " not supported"

-- make Futhark function expression from ident
makeKernel :: String -> Kernel
makeKernel ident
  | Just fun <- convertFun ident = F.Fun fun []
  | Just op  <- convertBinOp ident = F.Op op
  | otherwise = error $ "not supported operation " ++ ident

-- make Futhark basic type from Tail basic type --
makeBTp :: BType -> F.Type
makeBTp T.IntT = F.IntT
makeBTp T.DoubleT = F.F32T -- XXX - we turn doubles into singles!
makeBTp T.BoolT = F.BoolT
makeBTp T.CharT = F.Int8T
makeBTp (T.Btyv v) = error $ "makeBTp: cannot transform type variable " ++ v

-- make Futhark array type from Futhark basic type --
mkType :: (BType, Integer) -> F.Type
mkType (tp, r) = makeArrTp (makeBTp tp) r

-- aux for mkType --
makeArrTp :: F.Type -> Integer -> F.Type
makeArrTp btp 0 = btp
makeArrTp btp n = F.ArrayT (makeArrTp btp (n-1))

-- make curried Futhark function that have 1 as basic element and folds with times
multExp :: [F.Exp] -> F.Exp
multExp = foldr (BinApp Mult) (Constant (Int 1))

-- make Futhark kernel expression with type
compileKernel :: T.Exp -> F.Type -> Kernel
compileKernel (T.Var ident) _ = makeKernel ident
compileKernel (T.Fn ident tp (T.Fn ident2 tp2 e)) rtp = F.Fn rtp [(compileTp tp,"t_" ++ ident),(compileTp tp2,"t_" ++ ident2)] (compileExp e)
compileKernel (T.Fn ident tp e) rtp = F.Fn rtp [(compileTp tp,"t_" ++ ident)] (compileExp e)
compileKernel e t = error $ unwords ["compileKernel, invalid args:", show e, show t]

-- AUX for compileKernel --
compileTp :: T.Type -> F.Type
compileTp (ArrT bt (R r)) = makeArrTp (makeBTp bt) r
compileTp (VecT bt (R _)) = makeArrTp (makeBTp bt) 1
compileTp (SV bt (R _)) = makeArrTp (makeBTp bt) 1
compileTp (S bt _) = makeBTp bt

-----------------------
-- LIBRARY FUNCTIONS --
-----------------------

-- list containing ompl of all library functions -- 
builtins :: [F.FunDecl]
builtins = [boolToInt,negi,negd,absi,absd,mini,mind,signd,signi,maxi,maxd,eqb,xorb,nandb,norb,neqi,neqd,resi]
        ++ reshapeFuns 
        ++ takeFuns
        ++ dropFuns

boolToInt :: FunDecl
boolToInt = F.FunDecl F.IntT "boolToInt" [(F.BoolT, "x")] $
  F.IfThenElse Inline (F.Var "x") (Constant (Int 1)) (Constant (Int 0))

negi, negd, absi, absd, mini, mind, signd, signi,
  maxi, maxd, nandb, norb, eqb, xorb, neqi, neqd, resi :: FunDecl

negi = F.FunDecl F.IntT "negi" [(F.IntT,"x")] $ F.Neg (F.Var "x")

negd = F.FunDecl F.F32T "negd" [(F.F32T,"x")] $ F.Neg (F.Var "x")

absi = F.FunDecl F.IntT "absi" [(F.IntT,"x")] $ absExp (F.Var "x")

absd = F.FunDecl F.F32T "absd" [(F.F32T,"x")] $ absFloatExp (F.Var "x")

mini = F.FunDecl F.IntT "mini" [(F.IntT, "x"), (F.IntT, "y")] $ minExp (F.Var "x") (F.Var "y")

mind = F.FunDecl F.F32T "mind" [(F.F32T, "x"), (F.F32T, "y")] $ minExp (F.Var "x") (F.Var "y")

signd = F.FunDecl F.IntT "signd" [(F.F32T, "x")] $ signdExp (F.Var "x")

signi = F.FunDecl F.IntT "signi" [(F.IntT, "x")] $ signiExp (F.Var "x")

maxi = F.FunDecl F.IntT "maxi" [(F.IntT, "x"), (F.IntT, "y")] $ maxExp (F.Var "x") (F.Var "y")

maxd = F.FunDecl F.F32T "maxd" [(F.F32T, "x"), (F.F32T, "y")] $ maxExp (F.Var "x") (F.Var "y")

nandb = F.FunDecl F.BoolT "nandb" [(F.BoolT, "x"), (F.BoolT, "y")] $ nandExp (F.Var "x") (F.Var "y")

norb = F.FunDecl F.BoolT "norb" [(F.BoolT, "x"), (F.BoolT, "y")] $ norExp (F.Var "x") (F.Var "y")

eqb = F.FunDecl F.BoolT "eqb" [(F.BoolT, "x"), (F.BoolT, "y")] $ boolEquals (F.Var "x") (F.Var "y")
  where boolEquals e1 e2 = BinApp F.LogicOr (norExp e1 e2) (BinApp F.LogicAnd e1 e2)

xorb = F.FunDecl F.BoolT "xorb" [(F.BoolT, "x"), (F.BoolT, "y")] $ boolXor (F.Var "x") (F.Var "y")
  where boolXor e1 e2 = BinApp F.LogicAnd (nandExp e1 e2) (BinApp F.LogicOr e1 e2)

neqi = F.FunDecl F.BoolT "neqi" [(F.IntT, "x"), (F.IntT, "y")] $ notEq (F.Var "x") (F.Var "y")

neqd = F.FunDecl F.BoolT "neqd" [(F.F32T, "x"), (F.F32T, "y")] $ notEq (F.Var "x") (F.Var "y")

notEq :: F.Exp -> F.Exp -> F.Exp
notEq e1 e2 = FunCall "!" [BinApp F.Eq e1 e2]

resi = F.FunDecl F.IntT "resi" [(F.IntT, "x"),(F.IntT, "y")] $ resiExp (F.Var "x") (F.Var "y")

-- AUX: make FunDecl by combining signature and body (aux function that create function body)
makeFun :: [F.Arg] -> F.Ident -> F.Exp -> F.Type -> FunDecl
makeFun args name body tp = F.FunDecl (ArrayT tp) (name ++ "_" ++ pretty tp) args body

stdArgs :: F.Type -> [(F.Type, String)]
stdArgs tp = [(F.IntT,"l"),(ArrayT tp, "x")]

reshapeFun :: F.Type -> FunDecl
reshapeFun tp = makeFun (stdArgs tp) "reshape1" (reshape1Body tp) tp
takeFun :: F.Type -> F.FunDecl
takeFun tp = makeFun (stdArgs tp) "take1" (takeBody (zero tp)) tp
dropFun :: F.Type -> F.FunDecl
dropFun tp = makeFun (stdArgs tp) "drop1" (dropBody tp) tp

reshapeFuns, takeFuns, dropFuns :: [FunDecl]
reshapeFuns = map reshapeFun btypes
takeFuns = map takeFun btypes
dropFuns = map dropFun btypes

-----------------
-- EXPRESSIONS --
-----------------

-- general expressions --
compileExp :: T.Exp -> F.Exp
compileExp (T.Var ident) | ident == "pi" = Constant(F32 3.14159265359) | otherwise = F.Var ("t_" ++ ident)
compileExp (I x) = Constant (Int x)
compileExp (D d) = Constant (F32 $ fromRational $ toRational d)
compileExp (C char)   = Constant (Char char)
compileExp (B bool)   = Constant (Bool bool)
compileExp Inf = Constant (F32 (read "Infinity"))
compileExp (T.Neg e) = F.Neg (compileExp e)
compileExp (T.Let v _ e1 e2) = F.Let Indent (Ident ("t_" ++ v)) (compileExp e1) (compileExp e2) -- Let
compileExp (T.Op ident instDecl args) = compileOpExp ident instDecl args
compileExp T.Fn{} = error "Fn not supported"
compileExp (Vc exps) = Array(map compileExp exps)

-- operators --
compileOpExp :: String -> Maybe ([BType], [Integer]) -> [T.Exp] -> F.Exp
compileOpExp ident instDecl args = case ident of
  "reduce" -> compileReduce instDecl args
  "eachV"  -> compileEachV instDecl args
  "each"   -> compileEach instDecl args
  "power"  -> compilePower instDecl args
  "powerScl" -> compilePower instDecl args
  "firstV" -> compileFirstV instDecl args
  "first" -> compileFirst instDecl args
  "shapeV" -> F.Array $ makeShape 1 args 
  "shape"  -> compileShape instDecl args
  "reshape" -> compileReshape instDecl args
  "take" -> compileTake instDecl args
  "takeV" -> compileTakeV instDecl args
  "zipWith" -> compileZipWith instDecl args
  "cat" -> compileCat instDecl args
  "reverse" -> compileReverse instDecl args
  "reverseV" -> compileVReverseV instDecl args
  "vreverse" -> compileVReverse instDecl args
  "vreverseV" -> compileVReverseV instDecl args
  "transp" -> compileTransp instDecl args
  "transp2" -> compileTransp2 instDecl args
  "drop" -> compileDrop instDecl args
  "dropV" -> compileDropV instDecl args
  "iota" -> compileIota instDecl args
  "iotaV" -> compileIota instDecl args
  "vrotate" -> compileVRotate instDecl args
  "rotate" -> compileRotate instDecl args
  "vrotateV" -> compileVRotateV instDecl args
  "rotateV" -> compileVRotateV instDecl args
  "snoc" -> compileSnoc instDecl args
  "snocV" -> compileSnocV instDecl args
  "cons" -> compileCons instDecl args
  "consV" -> compileConsV instDecl args
  "b2iV" | [T.Var "tt"] <- args -> Constant (Int 1)
         | [T.Var "ff"] <- args -> Constant (Int 0)
  "idxS" | [T.I 1, i, arr] <- args ->
           F.Index (compileExp arr) [compileExp i]
  _
    | [e1,e2]  <- args
    , Just op  <- convertBinOp ident
    -> F.BinApp op (compileExp e1) (compileExp e2)
    | Just fun <- convertFun ident
    -> F.FunCall fun $ map compileExp args
    | ident `elem` idFuns
    -> F.FunCall ident $ map compileExp args
    | otherwise       -> error $ ident ++ " not supported"

-- snocV --
compileSnocV :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileSnocV (Just([_],[_])) [a,e] = F.FunCall "concat" [compileExp a, F.Array [compileExp e]]
compileSnocV Nothing _ = error "snocV needs instance declaration"
compileSnocV _ _ = error "snocV take two aguments"

-- snoc --
compileSnoc :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileSnoc (Just([_],[r])) [a,e] = makeTransp2 (map (Constant . Int) (reverse [0..r])) (F.FunCall "concat" [arr,e'])
  where e' = F.Array [makeTransp r (compileExp e)]
        arr = makeTransp (r+1) (compileExp a)
compileSnoc _ _ = error "compileSnoc: invalid arguments"

-- consV --
compileConsV :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileConsV (Just([_],[_])) [e,a] = F.FunCall "concat" [F.Array [compileExp e], compileExp a]
compileConsV Nothing _ = error "consV needs instance declaration"
compileConsV _ _ = error "consV take two aguments"


-- cons --
compileCons :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileCons (Just([_],[r])) [e,a] = makeTransp2 (map (Constant . Int) (reverse [0..r])) (F.FunCall "concat" [e', arr])
  where e' = F.Array [makeTransp r (compileExp e)]
        arr = makeTransp (r+1) (compileExp a)
compileCons _ _ = error "compileCons: invalid arguments"

-- first --
compileFirst :: Maybe (t, [Integer]) -> [T.Exp] -> F.Exp
compileFirst (Just(_,[r])) [a] = F.Let Inline (Ident "x") (compileExp a) $ F.Index (F.Var "x") (replicate rInt (F.Constant (F.Int 0)))
  where rInt = fromInteger r :: Int
compileFirst Nothing _ = error "first needs instance declaration"
compileFirst _ _ = error "first take one argument"

-- iota --
compileIota :: t -> [T.Exp] -> F.Exp
compileIota _ [a] = Map (F.Fn F.IntT [(F.IntT, "x")] (F.BinApp Plus (F.Var "x") (Constant (F.Int 1)))) (FunCall "iota" [compileExp a])
compileIota _ _ = error "Iota take one argument"

-- vreverse --
compileVReverse :: Maybe ([BType], [Integer]) -> [T.Exp] -> F.Exp
compileVReverse (Just([tp],[r])) [a] = makeVReverse tp r (compileExp a)
compileVReverse _ _ = error "compileVReverse: invalid arguments"

compileReverse :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileReverse (Just([tp],[r])) [a] = makeTransp r $ makeVReverse tp r $ makeTransp r $ compileExp a
compileReverse _ _ = error "compileReverse: invalid arguments"

compileVReverseV :: Maybe ([BType], [t]) -> [T.Exp] -> F.Exp
compileVReverseV (Just([tp],[_])) [a] = makeVReverse tp 1 (compileExp a)
compileVReverseV _ _ = error "compileVReverseC: invalid arguments"

makeVReverse :: BType -> Integer -> F.Exp -> F.Exp
makeVReverse tp r a = F.Let Inline (Ident "a") a $ Map kernelExp (FunCall "iota" [FunCall "size" [F.Constant (F.Int 0), a]])
  where
    kernelExp = F.Fn (mkType (tp,r-1)) [(F.IntT,"x")] (F.Index (F.Var "a") [F.BinApp F.Minus minusIndex ione])
    sizeCall = F.FunCall "size" [izero, a]
    minusIndex = F.BinApp F.Minus sizeCall (F.Var "x")
    izero = F.Constant (F.Int 0)
    ione = F.Constant (F.Int 1)

-- rotate --
compileVRotate :: Maybe ([BType], [Integer]) -> [T.Exp] -> F.Exp
compileVRotate (Just([tp],[r])) [i,a] = makeVRotate tp r i (compileExp a)
compileVRotate Nothing _ = error "Need instance declaration for vrotate"
compileVRotate _ _ = error "vrotate needs 2 arguments"

compileRotate :: Maybe ([BType], [Integer]) -> [T.Exp] -> F.Exp
compileRotate (Just([tp],[r])) [i,a] = makeTransp r $ makeVRotate tp r i $ makeTransp r $ compileExp a
compileRotate Nothing _ = error "Need instance declaration for rotate"
compileRotate _ _ = error "rotate needs 2 arguments"

-- vrotateV --
compileVRotateV :: Maybe ([BType], [t]) -> [T.Exp] -> F.Exp
compileVRotateV (Just([tp],[_])) [i,a] = makeVRotate tp 1 i (compileExp a)
compileVRotateV Nothing _ = error "Need instance declaration for vrotateV"
compileVRotateV _ _ = error "vrotateV needs 2 arguments"

-- vrotate --
makeVRotate :: BType -> Integer -> T.Exp -> F.Exp -> F.Exp
makeVRotate tp r i a = F.Let Inline (Ident "a") a $ Map kernelExp (FunCall "iota" [size])
  where
    kernelExp = F.Fn (mkType (tp, r-1)) [(F.IntT, "x")] (F.Index (F.Var "a") [F.BinApp F.Mod add size])
    add = F.BinApp F.Plus (F.Var "x") (compileExp i)
    size = FunCall "size" [F.Constant (F.Int 0), a]

-- cat --
compileCat :: Maybe ([BType], [Integer]) -> [T.Exp] -> F.Exp
compileCat (Just([tp],[r])) [a1,a2] = makeCat tp r (compileExp a1) (compileExp a2)
  where
    makeCat _tp 1 a1' a2' = FunCall "concat" [a1', a2']
    makeCat _tp _r a1' a2' = Map kernelExp (FunCall "zip" [a1', a2'])
      where
        kernelExp = F.Fn (mkType (tp,r-1)) [(mkType (tp,r-1),"x"), (mkType(tp,r-1),"y")] recursiveCall
        recursiveCall = makeCat tp (r-1) (F.Var "x") (F.Var "y")
compileCat _ _ = error "compileCat: invalid arguments"

-- takeV --
compileTakeV :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileTakeV (Just([tp],_)) [len,e] = F.FunCall fname [compileExp len,compileExp e]
    where fname = "take1_" ++ pretty (makeBTp tp)
compileTakeV Nothing _ = error "Need instance declaration for takeV"
compileTakeV _ _ = error "TakeV needs 2 arguments"

-- dropV --
compileDropV :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileDropV (Just([tp],_)) [len,e] = F.FunCall fname [compileExp len,compileExp e]
    where fname = "drop1_" ++ pretty (makeBTp tp)
compileDropV Nothing _ = error "Need instance declaration for dropV"
compileDropV _ _ = error "DropV needs 2 arguments"

-- take --
compileTake :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileTake (Just([tp],[r])) [len,e] = F.FunCall2 "reshape" dims $ F.FunCall fname [sizeProd,resh]
    where dims = absExp (compileExp len) : tail shape
          sizeProd = multExp $ compileExp len : tail shape
          fname = "take1_" ++ pretty (makeBTp tp)
          resh = F.FunCall2 "reshape" [multExp shape] (compileExp e)
          shape = makeShape r [e]
compileTake Nothing _args = error "Need instance declaration for take"
compileTake _ _ = error "Take needs 2 arguments"

-- drop --
compileDrop :: Maybe ([BType], [Integer]) -> [T.Exp] -> F.Exp
compileDrop (Just([tp],[r])) [len,e] = F.FunCall2 "reshape" dims $ F.FunCall fname [sizeProd,resh]
    where dims = maxExp (Constant (Int 0)) (F.BinApp F.Minus (F.FunCall "size" [Constant (Int 0), compileExp e])  (absExp (compileExp len))) : tail shape
          resh = F.FunCall2 "reshape" [multExp shape] (compileExp e)
          sizeProd = multExp $ compileExp len : tail shape
          fname = "drop1_" ++ pretty (makeBTp tp)
          shape = makeShape r [e]
compileDrop _ _ = error "compileDrop: invalid arguments"

-- reshape --
compileReshape :: Maybe ([BType], [Integer]) -> [T.Exp] -> F.Exp
compileReshape (Just([tp],[r1,r2])) [dims,array] = F.FunCall2 "reshape" dimsList $ F.FunCall fname [dimProd, resh]
    where dimsList | F.Array l <- dimsExp = l
                   | F.Var dimsVar <- dimsExp = map (\i -> F.Index (F.Var dimsVar) [Constant (Int i)]) [0..r2-1]
                   | otherwise = error $ "reshape needs literal or variable as shape argument, not " ++ show dimsExp
          dimsExp = compileExp dims
          fname = "reshape1_" ++ pretty (makeBTp tp)
          dimProd = multExp dimsList
          resh = F.FunCall2 "reshape" [shapeProd] (compileExp array)
          shapeProd = multExp (makeShape r1 [array])
compileReshape Nothing _args = error "Need instance declaration for reshape"
compileReshape _ _ = error "Reshape needs 2 arguments"

-- transp --
compileTransp :: Maybe (t, [Integer]) -> [T.Exp] -> F.Exp
compileTransp (Just(_,[r])) [e] = makeTransp2 (map (Constant . Int) (reverse [0..r-1])) (compileExp e)
compileTransp Nothing _args = error "Need instance declaration for transp"
compileTransp _ _ = error "Transpose takes 1 argument"

-- transp2 --
compileTransp2 :: t -> [T.Exp] -> F.Exp
compileTransp2 _ [Vc dims,e] = makeTransp2 (map compileExp dimsExps) (compileExp e)
    where dimsExps = map (I . (\x -> x - 1) . getInt) dims
          getInt (I i) = i
          getInt _ = error "transp2 expects number literals in it's first argument"
compileTransp2 _ e = case e of [_,_] -> error "transp2 needs litaral as first argument"
                               _     -> error "transp2 takes 2 arguments"

-- shape --
compileShape :: Maybe (t, [Integer]) -> [T.Exp] -> F.Exp
compileShape (Just(_,[len])) args = F.Array $ makeShape len args
compileShape Nothing _args = error "Need instance declaration for shape"
compileShape _ _ = error "compileShape: invalid arguments"

-- firstV --
compileFirstV :: t -> [T.Exp] -> F.Exp
compileFirstV _ args
  | [e] <- args = F.Let Inline (Ident "x") (compileExp e) $ F.Index (F.Var "x")[F.Constant (F.Int 0)]
  | otherwise = error "firstV takes one argument"

-- eachV --
compileEachV :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileEachV Nothing _ = error "Need instance declaration for eachV"
compileEachV (Just ([_intp,outtp],[_len])) [kernel,array] = Map kernelExp (compileExp array)
   where kernelExp = compileKernel kernel (makeBTp outtp)
compileEachV _ _ = error "compileEachV: invalid arguments"

-- each --
compileEach :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileEach (Just ([intp,outtp],[orig_r])) [okernel,orig_array] = makeEach intp outtp orig_r okernel (compileExp orig_array)
  where makeEach tp1 tp2 r kernel array
          | r == 1 = Map (compileKernel kernel (makeBTp tp2)) array
          | otherwise = Map (F.Fn (mkType (tp2,r-1)) [(mkType (tp1,r-1),"x")] (makeEach tp1 tp2 (r-1) kernel (F.Var "x"))) array
compileEach Nothing _ = error "Need instance declaration for each"
compileEach _ _ = error "each takes two arguments"

-- power --
compilePower :: Maybe InstDecl -> [T.Exp] -> F.Exp
compilePower (Just ([tp],_)) [kernel,num,arr] =
  Power (compileKernel kernel (makeBTp tp)) (compileExp num) (compileExp arr)
compilePower (Just (_,_)) _ = error "power takes one type argument"
compilePower Nothing [_,_,_] = error "Need instance declaration for power"
compilePower _ _ = error "power takes three arguments"

-- zipWith --
compileZipWith :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileZipWith (Just([tp1,tp2,rtp],[rk])) [orig_kernel,orig_a1,orig_a2] = makeZipWith rk orig_kernel (compileExp orig_a1) (compileExp orig_a2)
  where
  makeZipWith r kernel a1 a2
    | r == 1 = Map (compileKernel kernel (makeBTp rtp)) (FunCall "zip" [a1,a2])
    | otherwise = Map (F.Fn (mkType (rtp,r-1)) [(mkType(tp1,r-1),"x"),(mkType(tp2,r-1),"y")] (makeZipWith (r-1) kernel (F.Var "x") (F.Var "y"))) (FunCall "zip" [a1, a2])
    --Map kernelExp $ F.FunCall "zip" [(compileExp a1),(compileExp a2)] -- F.Map kernelExp $ F.FunCall "zip" [a1,a2]
compileZipWith Nothing _ = error "Need instance declaration for zipWith"
compileZipWith _ _ = error "zipWith takes 3 arguments"

-- reduce --
compileReduce :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileReduce Nothing _ = error "Need instance declaration for reduce"
compileReduce (Just ([orig_tp],[orig_rank]))[orig_kernel,v,array] =
  makeReduce orig_tp orig_rank kernelExp (compileExp v) (compileExp array)
  where
  kernelExp = compileKernel orig_kernel (makeBTp orig_tp)
  makeReduce :: BType -> Integer -> Kernel -> F.Exp -> F.Exp -> F.Exp
  makeReduce tp r kernel idExp arrayExp
    | r == 0 = Reduce kernel idExp arrayExp
    | otherwise = Map (F.Fn (mkType(tp,r-1)) [(mkType(tp,r),"x")] (makeReduce tp (r-1) kernel idExp (F.Var "x"))) arrayExp
compileReduce _ _ = error "reduce needs 3 arguments"


-- operators that are 1:1  --
-- (library functions) --
idFuns :: [String]
idFuns = ["negi",
          "negd",
          "absi",
          "absd",
          "mini",
          "mind",
          "signd",
          "signi",
          "maxi",
          "maxd",
          "eqb",
          "xorb",
          "nandb", 
          "norb",
          "neqi",
          "neqd",
          "resi"]

-- operators that are 1:1 with Futhark functions --
convertFun :: String -> Maybe String
convertFun fun = case fun of
  "i2d"    -> Just "f32"
  "catV"   -> Just "concat"
  "b2i"    -> Just "boolToInt"
  "b2iV"   -> Just "boolToInt"
  "ln"     -> Just "log32"
  "expd"   -> Just "exp32"
  "notb"   -> Just "!"
  "floor"  -> Just "int"
  "mem"    -> Just "copy"
  _         | fun `elem` idFuns -> Just fun
            | otherwise -> Nothing


-- binary operators --
convertBinOp :: String -> Maybe Operator
convertBinOp op = case op of
  "addi" -> Just F.Plus
  "addd" -> Just F.Plus
  "subi" -> Just F.Minus
  "subd" -> Just F.Minus
  "muli" -> Just F.Mult
  "muld" -> Just F.Mult
  "ltei" -> Just F.LessEq
  "lted" -> Just F.LessEq
  "eqi"  -> Just F.Eq
  "eqd"  -> Just F.Eq
  "gti"  -> Just F.Greater
  "gtd"  -> Just F.Greater
  "gtei" -> Just F.GreaterEq
  "gted" -> Just F.GreaterEq
  "andb" -> Just F.LogicAnd
  "orb"  -> Just F.LogicOr
  "divi" -> Just F.Div
  "divd" -> Just F.Div
  "powd" -> Just F.Pow
  "powi" -> Just F.Pow
  "lti"  -> Just F.Less
  "ltd"  -> Just F.Less
  "andi" -> Just F.And
  "andd" -> Just F.And
  "xori" -> Just F.Xor
  "ori"  -> Just F.Or
  "shli" -> Just F.Shl
  "shri" -> Just F.Shr
  _      -> Nothing
