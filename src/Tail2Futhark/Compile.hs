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
        getFuns (F.Let _ e1 e2) = getFuns e1 ++ getFuns e2
        getFuns (Index e es) = getFuns e ++ concat (map getFuns es)
        getFuns (IfThenElse e1 e2 e3) = getFuns e1 ++ getFuns e2 ++ getFuns e3
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
builtins = []
        ++ reshapeFuns 
        ++ takeFuns

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
mkSplit id1 id2 dims exp retExp = F.Let (TouplePat [(Ident id1),(Ident id2)]) (F.FunCall2 "split" [dims] exp) retExp
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

-- AUX: brainfart (Henrik)
reshapeArgs :: F.Type -> [F.Arg]
reshapeArgs tp = [(F.IntT,"l"),(ArrayT tp, "x")]
--takeLessFun tp = makeFun tp "takeLess" 

-- AUX: create a list of reshape functions for all basic types
reshapeFuns :: [FunDecl]
reshapeFuns = let
  reshapeFuns tp = map (makeFun (reshapeArgs tp) tp) [("takeLess", takeLessBody),("reshape1",reshape1Body tp),("extend",extendBody)]
  in concat $ map reshapeFuns btypes

-- AUX: create the body for take
takeBody :: F.Exp -> F.Exp
takeBody padElement = IfThenElse (zero `less` len) posTake negTake
    where less = BinApp LessEq
          zero = Constant (Int 0)
          sum  = BinApp Plus len size
          len  = F.Var "l"
          size = F.FunCall "size" [zero, F.Var "x"]
          padRight = F.FunCall "concat" [F.Var "x", padding]
          padLeft = F.FunCall "concat" [padding, F.Var "x"]
          padding = F.FunCall "replicate" [(BinApp Minus len size), padElement]
          posTake = IfThenElse (len `less` size) takeLessBody padRight
          negTake = IfThenElse (zero `less` sum) (mkSplit "_" "v2" sum (F.Var "x") (F.Var "v2")) padLeft 

zero :: F.Type -> F.Exp
zero F.IntT = Constant (Int 0)
zero F.RealT = Constant (Real 0)
zero F.BoolT = Constant (Bool False)
zero F.CharT = Constant (Char ' ')
zero tp = error $ "take for type " ++ showTp tp ++ " not supported"

takeFuns = map (\tp -> makeFun (reshapeArgs tp) tp ("take1",takeBody (zero tp))) btypes

-- Expressions --
compileExp :: T.Exp -> F.Exp
compileExp (T.Var ident) = F.Var ("t_" ++ ident)
compileExp (I int) = Constant (Int int)
compileExp (D double) = Constant (Real double) --(Float (double2Float double)) -- isn't this supporsed to be real??????????
compileExp (C char)   = Constant (Char char)
compileExp Inf = Constant (Real (read "Infinity"))
compileExp (T.Neg exp) = F.Neg (compileExp exp)
compileExp (T.Let id _ e1 e2) = F.Let (Ident ("t_" ++ id)) (compileExp e1) (compileExp e2) -- Let
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
  "shapeV" -> F.Array $ makeShape 1 args 
  "shape"  -> compileShape instDecl args
  "reshape" -> compileReshape instDecl args
  "take" -> compileTake instDecl args
  "takeV" -> compileTakeV instDecl args
  "zipWith" -> compileZipWith instDecl args
  "cat" -> compileCat instDecl args
  _
    | [e1,e2]  <- args
    , Just op  <- convertBinOp ident
    -> F.BinApp op (compileExp e1) (compileExp e2)
    | Just fun <- convertFun ident
    -> F.FunCall fun $ map compileExp args
    | otherwise       -> error $ ident ++ " not supported"

-- Operations that are 1:1 --
convertFun fun = case fun of
  "i2d"    -> Just "toReal"
  "iotaV"  -> Just "iota"
  "iota"   -> Just "iota"
--  "cat"    -> Just "concat"
--  "catV"   -> Just "concat"
  _     -> Nothing

-- Convert string to Maybe futhark  binary operation --
convertBinOp op = case op of
  "addi" -> Just F.Plus
  "addd" -> Just F.Plus
  "subi" -> Just F.Minus
  "subd" -> Just F.Minus
  "multi" -> Just F.Mult
  "multd" -> Just F.Mult
  "ltei" -> Just F.LessEq
  "lted" -> Just F.LessEq
  _      -> Nothing

-- AUX shape --
makeShape rank args
  | [e] <- args = map (\x -> FunCall "size" [Constant (Int x), compileExp e]) [0..rank-1]
  | otherwise = error "shape takes one argument"

multExp :: [F.Exp] -> F.Exp
multExp = foldr (BinApp Mult) (Constant (Int 1))

absExp :: F.Exp -> F.Exp
absExp e = IfThenElse (BinApp LessEq e (Constant (Int 0))) (F.Neg e) e

compileCat (Just([tp],[0])) [a1,a2] = FunCall "concat" [compileExp a1, compileExp a2]
compileCat (Just([tp],[r])) [a1,a2] = Map kernelExp (FunCall "zip" [compileExp a1, compileExp a2])
    where
       kernelExp = F.Fn (mkType (tp,r-1)) [(mkType (tp,r),"x"), (mkType(tp,r),"y")] recursiveCall
       recursiveCall = compileCat (Just([tp],[r-1])) [T.Var "x",T.Var "y"]
       mkType (tp,rank) = makeArrTp (makeBTp tp) rank

compileTakeV :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileTakeV (Just([tp],_)) [len,exp] = F.FunCall fname [compileExp len,compileExp exp]
    where fname = "take1_" ++ showTp (makeBTp tp)
compileTakeV Nothing _ = error "Need instance declaration for takeV"
compileTakeV _ _ = error "TakeV needs 2 arguments"

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

-- Compilation of reshape --
compileReshape (Just([tp],[r1,r2])) [dims,array] = F.FunCall2 "reshape" dimsList $ F.FunCall fname [dimProd, resh]
    where dimsList | F.Array dimsList <- dimsExp = dimsList
                   | F.Var dimsVar <- dimsExp = map (\i -> F.Index (F.Var dimsVar) [Constant (Int i)]) [0..r1-1]
                   | otherwise = error "reshape needs literal or variable as shape argument"
          dimsExp = compileExp dims
          fname = "reshape1_" ++ showTp (makeBTp tp)
          dimProd = multExp dimsList
          resh = F.FunCall2 "reshape" [shapeProd] (compileExp array)
          shapeProd = multExp (makeShape r1 [array])
compileReshape Nothing args = error "Need instance declaration for reshape"
compileReshape _ _ = error "Reshape needs 2 arguments"

-- Compilation of Transp --
compileTransp (Just(_,_)) args = F.FunCall "transpose" $ map compileExp args
compileTransp Nothing args = error "Need instance declaration for transp"

compileShape (Just(_,[len])) args = F.Array $ makeShape len args
compileShape Nothing args = error "Need instance declaration for shape"

compileFirstV _ args
  | [e] <- args = F.Index (compileExp e) [F.Constant (F.Int 0)]
  | otherwise = error "firstV takes one argument"

compileEachV :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileEachV Nothing _ = error "Need instance declaration for eachV"
compileEachV (Just ([intp,outtp],[len])) [kernel,array] = Map kernelExp (compileExp array)
   where kernelExp = compileKernel kernel (makeBTp outtp) 

compileEach :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileEach (Just ([intp,outtp],[rank])) [kernel,array] = Map kernelExp (compileExp array)
  -- | rank == 1 = Map (compileKernel kernel (makeBTp outtp)) (compileExp array)
  -- | otherwise = Map kernelExp (compileExp array)
  where kernelExp = nestMaps rank (makeBTp outtp) (makeBTp outtp) (compileKernel kernel (makeBTp outtp))
compileEach Nothing _ = error "Need instance declaration for each"
compileEach _ _ = error "each takes two arguments"

-- Nested maps with one argument in the lambda --
nestMaps :: Integer -> F.Type -> F.Type -> Kernel -> Kernel
nestMaps depth itp otp kernel = mkMapNest 1 itp otp kernel
  where mkMapNest n itp otp kernel 
          | n == depth = kernel
          | otherwise = mkMapNest (n+1) (ArrayT itp) (ArrayT otp)$ F.Fn (ArrayT otp) [(ArrayT itp,"x")] (Map kernel (F.Var "x"))

-- Nested maps with two arguments in the lambda --
nestMapsZip :: Integer -> F.Type -> F.Type -> Kernel -> Kernel
nestMapsZip depth itp otp kernel = mkMapNest 1 itp otp kernel
    where mkMapNest n itp otp kernel 
            | n == depth = kernel
            | otherwise =  mkMapNest (n+1) (ArrayT itp) (ArrayT otp) $ F.Fn (ArrayT otp) [(ArrayT itp,"x"), (ArrayT itp, "y")] (Map kernel (F.FunCall "zip" [F.Var "x", F.Var "y"]))

compileZipWith :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileZipWith (Just([a1tp,a2tp,rtp],[rk])) [kernel,a1,a2] = Map kernelExp $ F.FunCall "zip" [(compileExp a1),(compileExp a2)] -- F.Map kernelExp $ F.FunCall "zip" [a1,a2]
    where kernelExp = nestMapsZip rk (makeBTp rtp) (makeBTp rtp) (compileKernel kernel (makeBTp rtp))  
compileZipWith Nothing _ = error "Need instance declaration for zipWith"

-- compileReduce :: Maybe InstDecl -> [T.Exp] -> F.Exp
-- compileReduce Nothing _ = error "Need instance declaration for reduce"
-- compileReduce (Just ([tp],[rank])) [kernel,id,array]
--   | rank == 0 = Reduce kernelExp idExp arrayExp
--   -- | rank == 1 = Map (F.Fn ftp [(ArrayT ftp, "x")] (Reduce kernelExp idExp (F.Var "x"))) arrayExp
--   | otherwise = Map (nestMaps rank (ArrayT ftp) ftp (F.Fn ftp [(ArrayT ftp,"x")] (Reduce kernelExp idExp (F.Var "x")))) arrayExp
--     where kernelExp = compileKernel kernel (makeArrTp (makeBTp tp) rank)
--           idExp = compileExp id
--           arrayExp = compileExp array
--           ftp = makeBTp tp --makeArrTp (makeBTp tp) (rank)
-- compileReduce _ _ = error "reduce needs 3 arguments" 

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
