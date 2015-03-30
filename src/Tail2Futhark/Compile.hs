module Tail2Futhark.Compile (compile) where

import APLAcc.TAIL.AST as T -- the TAIL AST
import Tail2Futhark.Futhark.AST as F -- the futhark AST
import GHC.Float (double2Float)
import Data.List
import Data.Maybe
import Data.Char
import Options (Options(..))

compile :: Options -> T.Program -> F.Program
compile opts e = includes ++ [(RealT, "main", [], (compileExp e))]
  where takes = nub $ getFunCalls "take" (compileExp e)
        takeFuns = map makeTake . catMaybes . map getType $ takes
        includes = (if includeLibs opts then builtins else []) ++ takeFuns

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
        getFuns (Filter _ e) = getFuns e
        getFuns (Scan _ e1 e2) = getFuns e1 ++ getFuns e2
        getFuns (Reduce _ e1 e2) = getFuns e1 ++ getFuns e2
        getFuns (F.Var _) = []
        getFuns (Constant _) = []
        getFuns (F.FunCall2 _ _ exp) = getFuns exp
        --getFuns (Reshape _ exp) = getFuns exp -- reshape does not supports functions in shape arguments

-- list of builtin fuctions (EXPERIMENT) 
builtins :: [F.FunDecl]
builtins = reshapeFuns -- reshapeFuns = the functions needed to make reshape

-- AUX: makes FunDecl out of type by adding  signature + return and argument type (that are the same)
makeTake :: F.Type -> F.FunDecl
makeTake tp = (tp,name,[(ArrayT F.IntT, "dims"),(tp,"x")],takeBody)
  where name = "take" ++ show (rank tp :: Integer) ++ showTp (baseType tp)

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
  in concat $ map (reshapeFuns . readBType) $ ["int","real","bool","char"]

-- AUX: HELP US!!!!
takeBody :: F.Exp
takeBody = IfThenElse (BinApp LessEq (Constant (Int 0)) (F.Var "dims")) posTake negTake
  where posTake = IfThenElse (BinApp LessEq (F.Var "dims") (FunCall "size" [Constant (Int 0),F.Var "x"])) letExp elseBranch
         where letExp = F.Let (TouplePat [(Ident "v1"),(Ident "v2")]) split (F.Var "v1")
               split = FunCall "split" [F.Var "dims", F.Var "x"]
               elseBranch = FunCall "concat" [F.Var "x",FunCall "replicate" [BinApp Minus (F.Var "dims") sizeExp,Constant (Int 0)]]
               sizeExp = FunCall "size" [Constant (Int 0), F.Var "x"]
        negTake = IfThenElse (BinApp LessEq (F.Neg (F.Var "dims")) (FunCall "size" [Constant (Int 0),F.Var "x"])) letExp elseBranch
         where letExp = F.Let (TouplePat [Ident "v1",Ident "v2"]) split (F.Var "v1")
               split = FunCall "split" [BinApp Plus (FunCall "size" [Constant (Int 0),F.Var "x"]) (F.Var "dims"),F.Var "x"]
               elseBranch = FunCall "concat" [FunCall "replicate" [BinApp Plus sizeExp (F.Var "dims"),Constant (Int 0)],F.Var "x"]
               sizeExp = F.Neg $ FunCall "size" [Constant (Int 0), F.Var "x"]

-- Expressionis --
compileExp :: T.Exp -> F.Exp
compileExp (T.Var ident) = F.Var ("t_" ++ ident)
compileExp (I int) = Constant (Int int)
compileExp (D double) = Constant (Float (double2Float double)) -- isn't this supporsed to be real??????????
compileExp (C char)   = Constant (Char char)
compileExp Inf = Constant (Float (read "Infinity"))
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
  "firstV" -> compileFirstV instDecl args
  "shapeV" -> F.Array $ makeShape 1 args 
  "shape"  -> compileShape instDecl args
  "reshape" -> compileReshape instDecl args
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
  "cat"    -> Just "concat"
  "catV"   -> Just "concat"
  _     -> Nothing

-- Convert string to Maybe futhark  binary operation --
convertBinOp op = case op of
  "addi" -> Just F.Plus
  "addd" -> Just F.Plus
  _      -> Nothing

-- AUX shape --
makeShape rank args
  | [e] <- args = map (\x -> FunCall "size" [Constant (Int x), compileExp e]) [0..rank-1]
  | otherwise = error "shape takes one argument"

-- Compilation of reshape --
compileReshape (Just([tp],[r1,r2])) [dims,array] = F.FunCall2 "reshape" dimsList $ F.FunCall fname [dimProd, resh]
    where dimsList | F.Array dimsList <- dimsExp = dimsList
                   | F.Var dimsVar <- dimsExp = map (\i -> F.Index (F.Var dimsVar) [Constant (Int i)]) [0..r1-1]
                   | otherwise = error "reshape needs literal or variable as shape argument"
          dimsExp = compileExp dims
          fname = "reshape1_" ++ showTp (makeBTp tp)
          dimProd = foldr (BinApp Mult) (Constant (Int 1)) dimsList
          resh = F.FunCall2 "reshape" [shapeProd] (compileExp array)
          shapeProd = foldr (BinApp Mult) (Constant (Int 1)) (makeShape r1 [array])
compileReshape Nothing args = error "Need instance declaration for reshape"
compileReshape _ _ = error "Reshape nedds 2 arguments"

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

compileReduce :: Maybe InstDecl -> [T.Exp] -> F.Exp
compileReduce Nothing _ = error "Need instance declaration for reduce"
compileReduce (Just ([tp],[rank])) [kernel,id,array]
  | rank == 0 = Reduce kernelExp idExp arrayExp
  | otherwise = error "Reduce operation - Not supported" 
    where kernelExp = compileKernel kernel (makeArrTp (makeBTp tp) rank)
          idExp = compileExp id
          arrayExp = compileExp array
compileReduce _ _ = error "reduce needs 3 arguments" 
-- compileReduce for arrays

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
