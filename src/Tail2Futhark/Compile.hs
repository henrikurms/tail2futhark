module Tail2Futhark.Compile (compile) where

import APLAcc.TAIL.AST as T -- the TAIL AST
import Tail2Futhark.Futhark.AST as F -- the futhark AST
--import GHC.Float (double2Float)
import Data.List
import Data.Maybe
import Data.Char
import Options (Options(..))

compile :: Options -> T.Program -> F.Program
compile opts e = includes ++ [(RealT, "main", [], (compileExp e))]
  where takes = nub $ getFunCalls "take" (compileExp e)
        --takeFuns = map makeTake . catMaybes . map getType $ takes
        includes = (if includeLibs opts then builtins else [])

-- get a list of function names from the program tree (with duplicates)
-- by recursively going throught the tree.
-- removes function name from list fx. take2int => 2int
getFunCalls :: F.Ident -> F.Exp -> [[Char]]
getFunCalls name exp = getFuns exp
  where getFuns (FunCall ident _) = maybeToList $ stripPrefix name ident
        getFuns (F.Let _ _ e1 e2) = getFuns e1 ++ getFuns e2
        getFuns (Index e es) = getFuns e ++ concat (map getFuns es)
        getFuns (IfThenElse _ e1 e2 e3) = getFuns e1 ++ getFuns e2 ++ getFuns e3
        getFuns (F.Neg e) = getFuns e
        getFuns (Array es) = concat $ map getFuns es
        getFuns (BinApp _ e1 e2) = getFuns e1 ++ getFuns e2
        getFuns (Map _ e) = getFuns e
--        getFuns (Zip e1 e2) = getFuns e1 ++ getFuns e2
        getFuns (Filter _ e) = getFuns e
        getFuns (Scan _ e1 e2) = getFuns e1 ++ getFuns e2
        getFuns (Reduce _ e1 e2) = getFuns e1 ++ getFuns e2
        getFuns (F.Var _) = []
        getFuns (Constant _) = []
        getFuns (F.FunCall2 _ _ exp) = getFuns exp
        --getFuns (Reshape _ exp) = getFuns exp -- reshape does not supports functions in shape arguments

-- list of builtin fuctions (EXPERIMENT) 
builtins :: [F.FunDecl]
builtins = [boolToInt,negi]
        ++ reshapeFuns 
        ++ takeFuns
        ++ dropFuns

-- AUX: makes FunDecl out of type by adding  signature + return and argument type (that are the same)
--makeTake :: F.Type -> F.FunDecl
--makeTake tp = (tp,name,[(ArrayT F.IntT, "dims"),(tp,"x")],takeBody)
--  where name = "take" ++ show (rank tp :: Integer) ++ showTp (baseType tp)

-- AUX: takes type and gives string representation of type
showTp tp  = case baseType tp of 
  F.IntT -> "int"
  F.RealT -> "real"
  F.BoolT -> "bool"
  F.CharT -> "char"

-- AUX: takes fx 2int and gives [[int]]
getType :: [Char] -> Maybe F.Type
getType s 
  | suffix `elem` ["int","real","bool","char"] = fmap (makeArrTp (readBType suffix)) $ rank
  | otherwise = Nothing
  where (prefix,suffix) = span isDigit s
        rank | [] <- prefix = Nothing 
             | otherwise = Just (read prefix :: Integer)

-- AUX: takes string representation of type and return Maybe F.Type
readBType tp = case tp of
  "int" -> F.IntT
  "real" -> F.RealT
  "bool" -> F.BoolT
  "char" -> F.CharT

btypes = map readBType ["int","real","bool","char"]

-- AUX reshape: create split part of reshape function 
mkSplit id1 id2 dims exp retExp = F.Let Inline (TouplePat [(Ident id1),(Ident id2)]) (F.FunCall2 "split" [dims] exp) retExp
takeLessBody = mkSplit "v1" "_" (F.Var "l") (F.Var "x") (F.Var "v1")
reshape1Body tp = F.FunCall name $ F.Var "l" : F.FunCall extend [F.Var "l",F.Var "x"] : []
  where name = "takeLess_" ++ showTp tp
        extend = "extend_" ++ showTp tp

-- AUX reshape: create extend part of reshape function
extendBody = F.FunCall2 "reshape" [BinApp Mult size length] (F.FunCall "replicate" [length,F.Var "x"])
  where length = (F.Var "l" `fdiv` size) `fplus` Constant (Int 1)
        size = F.FunCall "size" [Constant (Int 0),F.Var "x"]
        fdiv = BinApp Div
        fplus = BinApp Plus

-- AUX: make FunDecl by combining signature and body (aux function that create function body)
makeFun :: [F.Arg] -> F.Type -> (F.Ident,F.Exp) -> FunDecl
makeFun args tp (name,body) = (ArrayT tp,name ++ "_" ++ showTp tp,args,body)

boolToInt :: FunDecl
boolToInt = (F.IntT, "boolToInt", [(F.BoolT, "x")], F.IfThenElse Inline (F.Var "x") (Constant (Int 1)) (Constant (Int 0)))

negi :: FunDecl
negi = (F.IntT, "negi", [(F.IntT,"x")], F.Neg (F.Var "x"))

-- AUX: brainfart (Henrik)
reshapeArgs :: F.Type -> [F.Arg]
reshapeArgs tp = [(F.IntT,"l"),(ArrayT tp, "x")]
--takeLessFun tp = makeFun tp "takeLess" 

-- AUX: create a list of reshape functions for all basic types
reshapeFuns :: [FunDecl]
reshapeFuns = let
  reshapeFuns tp = map (makeFun (reshapeArgs tp) tp) [("takeLess", takeLessBody),("reshape1",reshape1Body tp),("extend",extendBody)]
  in concat $ map reshapeFuns btypes

dropBody :: F.Type -> F.Exp
dropBody tp = IfThenElse Indent (size `less` absExp len) emptArr elseBranch
    where zero = Constant (Int 0)
          less = BinApp LessEq
          len = F.Var "l"
          size = F.FunCall "size" [zero, F.Var "x"]
          sum = BinApp Plus len size
          emptArr = F.Empty tp
          elseBranch = IfThenElse Indent (len `less` zero) negDrop posDrop
          negDrop = mkSplit "v1" "_" sum (F.Var "x") (F.Var "v1")
          posDrop = mkSplit "_" "v2" len (F.Var "x") (F.Var "v2")

-- AUX: create the body for take
takeBody :: F.Exp -> F.Exp
takeBody padElement = IfThenElse Indent (zero `less` len) posTake negTake
    where less = BinApp LessEq
          zero = Constant (Int 0)
          sum  = BinApp Plus len size
          len  = F.Var "l"
          size = F.FunCall "size" [zero, F.Var "x"]
          padRight = F.FunCall "concat" [F.Var "x", padding]
          padLeft = F.FunCall "concat" [padding, F.Var "x"]
          padding = F.FunCall "replicate" [(BinApp Minus len size), padElement]
          posTake = IfThenElse Indent (len `less` size) takeLessBody padRight
          negTake = IfThenElse Indent (zero `less` sum) (mkSplit "_" "v2" sum (F.Var "x") (F.Var "v2")) padLeft 

zero :: F.Type -> F.Exp
zero F.IntT = Constant (Int 0)
zero F.RealT = Constant (Real 0)
zero F.BoolT = Constant (Bool False)
zero F.CharT = Constant (Char ' ')
zero tp = error $ "take for type " ++ showTp tp ++ " not supported"

takeFuns :: [F.FunDecl]
takeFuns = map (\tp -> makeFun (reshapeArgs tp) tp ("take1",takeBody (zero tp))) btypes

dropFuns :: [F.FunDecl]
dropFuns = map (\tp -> makeFun (reshapeArgs tp) tp ("drop1", dropBody tp)) btypes

-- Expressions --
compileExp :: T.Exp -> F.Exp
compileExp (T.Var ident) = F.Var ("t_" ++ ident)
compileExp (I int) = Constant (Int int)
compileExp (D double) = Constant (Real double) --(Float (double2Float double)) -- isn't this supporsed to be real??????????
compileExp (C char)   = Constant (Char char)
compileExp Inf = Constant (Real (read "Infinity"))
compileExp (T.Neg exp) = F.Neg (compileExp exp)
compileExp (T.Let id _ e1 e2) = F.Let Indent (Ident ("t_" ++ id)) (compileExp e1) (compileExp e2) -- Let
compileExp (T.Op ident instDecl args) = compileOpExp ident instDecl args
compileExp (T.Fn _ _ _) = error "Fn not supported"
compileExp (Vc exps) = Array(map compileExp exps)

-- Operation expressions --
compileOpExp :: [Char] -> Maybe ([BType], [Integer]) -> [T.Exp] -> F.Exp
compileOpExp ident instDecl args = case ident of
  "reduce" -> compileReduce instDecl args
  "eachV"  -> compileEachV instDecl args
  "each"   -> compileEach instDecl args
  "firstV" -> compileFirstV instDecl args
  "first" -> compileFirst instDecl args
  "shapeV" -> F.Array $ makeShape 1 args 
  "shape"  -> compileShape instDecl args
  "reshape" -> compileReshape instDecl args
  "take" -> compileTake instDecl args
  "takeV" -> compileTakeV instDecl args
  "zipWith" -> compileZipWith instDecl args
  "cat" -> compileCat instDecl args
  "vreverse" -> compileVReverse instDecl args
  "vreverseV" -> compileVReverseV instDecl args
  "transp" -> compileTransp instDecl args
  "transp2" -> compileTransp2 instDecl args
  "drop" -> compileDrop instDecl args
  "dropV" -> compileDropV instDecl args
  "iota" -> compileIota instDecl args
  "iotaV" -> compileIota instDecl args
  "vrotate" -> compileVRotate instDecl args
  "vrotateV" -> compileVRotateV instDecl args
  "snoc" -> compileSnoc instDecl args
  "snocV" -> compileSnocV instDecl args
  "cons" -> compileCons instDecl args
  "consV" -> compileConsV instDecl args
  _
    | [e1,e2]  <- args
    , Just op  <- convertBinOp ident
    -> F.BinApp op (compileExp e1) (compileExp e2)
    | Just fun <- convertFun ident
    -> F.FunCall fun $ map compileExp args
    | otherwise       -> error $ ident ++ " not supported"

-- Operations that are 1:1 --
convertFun fun = case fun of
  "i2d"    -> Just "toFloat"
  "catV"   -> Just "concat"
  "b2i"    -> Just "boolToInt"
  "negi"   -> Just "negi"
  _     -> Nothing

-- Convert string to Maybe futhark  binary operation --
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
  _      -> Nothing

-- AUX shape --
makeShape rank args
  | [e] <- args = map (\x -> FunCall "size" [Constant (Int x), compileExp e]) [0..rank-1]
  | otherwise = error "shape takes one argument"

multExp :: [F.Exp] -> F.Exp
multExp = foldr (BinApp Mult) (Constant (Int 1))

absExp :: F.Exp -> F.Exp
absExp e = IfThenElse Inline (BinApp LessEq e (Constant (Int 0))) (F.Neg e) e

maxExp :: F.Exp -> F.Exp -> F.Exp
maxExp e1 e2 = IfThenElse Inline (BinApp LessEq e1 e2) e2 e1


-- compileSnocV :: Maybe InstDecl -> [T.Exp] -> F.Exp
-- compileSnocV (Just([tp],[r])) [a,e] = makeSnoc tp 1 (compileExp a) (compileExp e)
-- compileSnocV Nothing _ = error "snoc needs instance declaration"
-- compileSnocV _ _ = error "snoc take two aguments"
-- 
-- compileSnoc :: Maybe InstDecl -> [T.Exp] -> F.Exp
-- compileSnoc (Just([tp],[r])) [a,e] = makeSnoc tp r (compileExp a) (compileExp e)
-- compileSnoc Nothing _ = error "snoc needs instance declaration"
-- compileSnoc _ _ = error "snoc take two aguments"

-- makeSnoc tp 1 a e = F.FunCall "concat" [a, F.Array [e]]
-- makeSnoc tp r a e = Map (F.Fn (mkType (tp,r-1)) [(mkType (tp,r-1), "x"), (mkType (tp,r-1), "y")] recursiveCall) arr 
--   where arr = F.FunCall "zip" [a, e]
--         recursiveCall = makeSnoc tp (r-1) (F.Var "x") (F.Var "y")

compileSnocV :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileSnocV (Just([tp],[r])) [a,e] = F.FunCall "concat" [compileExp a, F.Array [compileExp e]]
compileSnocV Nothing _ = error "snocV needs instance declaration"
compileSnocV _ _ = error "snocV take two aguments"

compileSnoc :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileSnoc (Just([tp],[r])) [a,e] = makeTransp2 (map (Constant . Int) (reverse [0..r])) (F.FunCall "concat" [arr,exp])
  where exp = F.Array [makeTransp e r]
        arr = makeTransp a (r+1)

compileConsV :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileConsV (Just([tp],[r])) [e,a] = F.FunCall "concat" [F.Array [compileExp e], compileExp a]
compileConsV Nothing _ = error "consV needs instance declaration"
compileConsV _ _ = error "consV take two aguments"

compileCons :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileCons (Just([tp],[r])) [e,a] = makeTransp2 (map (Constant . Int) (reverse [0..r])) (F.FunCall "concat" [exp, arr])
  where exp = F.Array [makeTransp e r]
        arr = makeTransp a (r+1)

compileFirst (Just(_,[r])) [a] = F.Let Inline (Ident "x") (compileExp a) $ F.Index (F.Var "x") (replicate rInt (F.Constant (F.Int 0)))
  where rInt = fromInteger r :: Int
compileFirst Nothing _ = error "first needs instance declaration"
compileFirst _ _ = error "first take one argument"

compileIota _ [a] = Map (F.Fn F.IntT [(F.IntT, "x")] (F.BinApp Plus (F.Var "x") (Constant (F.Int 1)))) (FunCall "iota" [compileExp a])
compileIota _ _ = error "Iota take one argument"

compileVReverse (Just([tp],[r])) [a] = makeVReverse tp r a
compileVReverseV (Just([tp],[l])) [a] = makeVReverse tp 1 a

makeVReverse tp r a = F.Let Inline (Ident "a") (compileExp a) $ Map kernelExp (FunCall "iota" [FunCall "size" [F.Constant (F.Int 0) ,compileExp a]])
  where
    kernelExp = F.Fn (mkType (tp,r-1)) [(F.IntT,"x")] (F.Index (F.Var "a") [F.BinApp F.Minus minusIndex one])
    sizeCall = F.FunCall "size" [zero, compileExp a] 
    minusIndex = F.BinApp F.Minus sizeCall (F.Var "x")
    zero = F.Constant (F.Int 0)
    one = F.Constant (F.Int 1)
    mkType (tp,rank) = makeArrTp (makeBTp tp) rank

compileVRotate (Just([tp],[r])) [i,a] = makeVRotate tp r i a
compileVRotate Nothing _ = error "Need instance declaration for vrotate"
compileVRotate _ _ = error "vrotate needs 2 arguments"


compileVRotateV (Just([tp],[r])) [i,a] = makeVRotate tp 1 i a
compileVRotateV Nothing _ = error "Need instance declaration for vrotateV"
compileVRotateV _ _ = error "vrotateV needs 2 arguments"

makeVRotate tp r i a = F.Let Inline (Ident "a") (compileExp a) $ Map kernelExp (FunCall "iota" [size])
  where
    kernelExp = F.Fn (mkType (tp, r-1)) [(F.IntT, "x")] (F.Index (F.Var "a") [F.BinApp F.Mod sum size])
    sum = F.BinApp F.Plus (F.Var "x") (compileExp i)
    size = FunCall "size" [F.Constant (F.Int 0), compileExp a]

compileCat (Just([tp],[r])) [a1,a2] = makeCat tp r (compileExp a1) (compileExp a2) 
  where
    makeCat tp 1 a1 a2 = FunCall "concat" [a1, a2]
    makeCat tp r a1 a2 = Map kernelExp (FunCall "zip" [a1, a2])
      where
        kernelExp = F.Fn (mkType (tp,r-1)) [(mkType (tp,r-1),"x"), (mkType(tp,r-1),"y")] recursiveCall
        recursiveCall = makeCat tp (r-1) (F.Var "x") (F.Var "y")
    mkType (tp,rank) = makeArrTp (makeBTp tp) rank

compileTakeV :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileTakeV (Just([tp],_)) [len,exp] = F.FunCall fname [compileExp len,compileExp exp]
    where fname = "take1_" ++ showTp (makeBTp tp)
compileTakeV Nothing _ = error "Need instance declaration for takeV"
compileTakeV _ _ = error "TakeV needs 2 arguments"

compileDropV :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileDropV (Just([tp],_)) [len,exp] = F.FunCall fname [compileExp len,compileExp exp]
    where fname = "drop1_" ++ showTp (makeBTp tp)
compileDropV Nothing _ = error "Need instance declaration for dropV"
compileDropV _ _ = error "DropV needs 2 arguments"

-- Compilation of take --
compileTake :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileTake (Just([tp],[r])) [len,exp] = F.FunCall2 "reshape" dims $ F.FunCall fname [sizeProd,resh]
    where dims = absExp (compileExp len) : tail shape
          sizeProd = multExp $ compileExp len : tail shape
          fname = "take1_" ++ showTp (makeBTp tp)
          resh = F.FunCall2 "reshape" [multExp shape] (compileExp exp) 
          shape = makeShape r [exp]
compileTake Nothing args = error "Need instance declaration for take"
compileTake _ _ = error "Take needs 2 arguments"

isExp :: F.Exp -> F.Exp
isExp = id

compileDrop (Just([tp],[r])) [len,exp] = F.FunCall2 "reshape" dims $ F.FunCall fname [sizeProd,resh] 
    where dims = maxExp (Constant (Int 0)) (F.BinApp F.Minus (F.FunCall "size" [Constant (Int 0), compileExp exp])  (absExp (compileExp len))) : tail shape
          resh = F.FunCall2 "reshape" [multExp shape] (compileExp exp)
          sizeProd = multExp $ compileExp len : tail shape
          fname = "drop1_" ++ showTp (makeBTp tp)
          shape = makeShape r [exp]

-- Compilation of reshape --
compileReshape (Just([tp],[r1,r2])) [dims,array] = F.FunCall2 "reshape" dimsList $ F.FunCall fname [dimProd, resh]
    where dimsList | F.Array dimsList <- dimsExp = dimsList
                   | F.Var dimsVar <- dimsExp = map (\i -> F.Index (F.Var dimsVar) [Constant (Int i)]) [0..r2-1]
                   | otherwise = error "reshape needs literal or variable as shape argument"
          dimsExp = compileExp dims
          fname = "reshape1_" ++ showTp (makeBTp tp)
          dimProd = multExp dimsList
          resh = F.FunCall2 "reshape" [shapeProd] (compileExp array)
          shapeProd = multExp (makeShape r1 [array])
compileReshape Nothing args = error "Need instance declaration for reshape"
compileReshape _ _ = error "Reshape needs 2 arguments"

-- Compilation of Transp --
compileTransp (Just(_,[r])) [exp] = makeTransp2 (map (Constant . Int) (reverse [0..r-1])) (compileExp exp)
compileTransp Nothing args = error "Need instance declaration for transp"
compileTransp _ _ = error "Transpose takes 1 argument"

makeTransp e r = makeTransp2 (map (Constant . Int) (reverse [0..r-1])) (compileExp e)

makeTransp2 dims exp = F.FunCall2 "rearrange" dims exp

--compileTransp2 (Just(_,[r])) [dims,e] = F.Let Inline (Ident "x") (compileExp e) $ makeTransp2 indices (F.Var "x")
--    where indices = map (F.Index (F.Var "x") . (:[]) . Constant . Int) [0..r-1]

compileTransp2 _ [Vc dims,e] = makeTransp2 (map compileExp dimsExps) (compileExp e)
    where dimsExps = map (I . (\x -> x - 1) . getInt) dims
          getInt (I i) = i
          getInt _ = error "transp2 expects number literals in it's first argument"
compileTransp2 _ e = case e of [_,_] -> error "transp2 needs litaral as first argument"
                               _     -> error "transp2 takes 2 arguments"

compileShape (Just(_,[len])) args = F.Array $ makeShape len args
compileShape Nothing args = error "Need instance declaration for shape"

compileFirstV _ args
  | [e] <- args = F.Let Inline (Ident "x") (compileExp e) $ F.Index (F.Var "x")[F.Constant (F.Int 0)]
  | otherwise = error "firstV takes one argument"

compileEachV :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileEachV Nothing _ = error "Need instance declaration for eachV"
compileEachV (Just ([intp,outtp],[len])) [kernel,array] = Map kernelExp (compileExp array)
   where kernelExp = compileKernel kernel (makeBTp outtp) 

compileEach :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileEach (Just ([intp,outtp],[rank])) [kernel,array] = makeEach intp outtp rank kernel (compileExp array) 
  where makeEach tp1 tp2 r kernel array
          | r == 1 = Map (compileKernel kernel (makeBTp tp2)) array
          | otherwise = Map (F.Fn (mkType (tp2,r-1)) [(mkType (tp1,r-1),"x")] (makeEach tp1 tp2 (r-1) kernel (F.Var "x"))) array
compileEach Nothing _ = error "Need instance declaration for each"
compileEach _ _ = error "each takes two arguments"

-- Nested maps with one argument in the lambda --
nestMaps :: Integer -> F.Type -> F.Type -> Kernel -> Kernel
nestMaps depth itp otp kernel = mkMapNest 1 itp otp kernel
  where mkMapNest n itp otp kernel 
          | n == depth = kernel
          | otherwise = mkMapNest (n+1) (ArrayT itp) (ArrayT otp)$ F.Fn (ArrayT otp) [(ArrayT itp,"x")] (Map kernel (F.Var "x"))

mkType (tp,rank) = makeArrTp (makeBTp tp) rank

compileZipWith :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileZipWith (Just([tp1,tp2,rtp],[rk])) [kernel,a1,a2] = makeZipWith rk kernel (compileExp a1) (compileExp a2)
  where
  makeZipWith r kernel a1 a2
    | r == 1 = Map (compileKernel kernel (makeBTp rtp)) (FunCall "zip" [a1,a2])
    | otherwise = Map (F.Fn (mkType (rtp,r-1)) [(mkType(tp1,r-1),"x"),(mkType(tp2,r-1),"y")] (makeZipWith (r-1) kernel (F.Var "x") (F.Var "y"))) (FunCall "zip" [a1, a2])
    --Map kernelExp $ F.FunCall "zip" [(compileExp a1),(compileExp a2)] -- F.Map kernelExp $ F.FunCall "zip" [a1,a2]
compileZipWith Nothing _ = error "Need instance declaration for zipWith"
compileZipWith _ _ = error "zipWith takes 3 arguments"

compileReduce :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileReduce Nothing _ = error "Need instance declaration for reduce"
compileReduce (Just ([tp],[rank]))[kernel,id,array] = makeReduce tp rank kernelExp (compileExp id) (compileExp array)
  where
  mkType (tp,rank) = makeArrTp (makeBTp tp) rank
  kernelExp = compileKernel kernel (makeBTp tp)
  makeReduce :: BType -> Integer -> Kernel -> F.Exp -> F.Exp -> F.Exp
  makeReduce tp rank kernel idExp arrayExp
    | rank == 0 = Reduce kernel idExp arrayExp
    | otherwise = Map (F.Fn (mkType(tp,rank-1)) [(mkType(tp,rank),"x")] (makeReduce tp (rank-1) kernel idExp (F.Var "x"))) arrayExp
compileReduce _ _ = error "reduce needs 3 arguments"


compileKernel :: T.Exp -> F.Type -> Kernel
compileKernel (T.Var ident) rtp = makeKernel ident
compileKernel (T.Fn ident tp (T.Fn ident2 tp2 exp)) rtp = F.Fn rtp [(compileTp tp,"t_" ++ ident),(compileTp tp2,"t_" ++ ident2)] (compileExp exp)
compileKernel (T.Fn ident tp exp) rtp = F.Fn rtp [(compileTp tp,"t_" ++ ident)] (compileExp exp)

makeKernel ident
  | Just fun <- convertFun ident = F.Fun fun []
  | Just op  <- convertBinOp ident = F.Op op
  | otherwise = error $ "not supported operation " ++ ident

compileTp (ArrT bt (R rank)) = makeArrTp (makeBTp bt) rank
compileTp (VecT bt (R rank)) = makeArrTp (makeBTp bt) 1
compileTp (SV bt (R rank)) = makeArrTp (makeBTp bt) 1
compileTp (S bt _) = makeBTp bt

makeBTp T.IntT = F.IntT
makeBTp T.DoubleT = F.RealT
makeBTp T.BoolT = F.BoolT
makeBTp T.CharT = F.CharT

makeArrTp :: F.Type -> Integer -> F.Type
makeArrTp btp 0 = btp
makeArrTp btp n = F.ArrayT (makeArrTp btp (n-1))
