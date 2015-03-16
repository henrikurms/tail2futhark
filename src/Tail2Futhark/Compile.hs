module Tail2Futhark.Compile (compile) where

import APLAcc.TAIL.AST as T -- the TAIL AST
import Tail2Futhark.Futhark.AST as F -- the futhark AST
import GHC.Float (double2Float)

compile :: T.Program -> F.Program
compile e = [(RealT, "main", [], (compileExp e))] 

-- Expressionis--
compileExp :: T.Exp -> F.Exp
compileExp (T.Var ident) = F.Var ("t_" ++ ident)
compileExp (I int) = Constant (Int int)
compileExp (D double) = Constant (Float (double2Float double)) -- isn't this supporsed to be real??????????
compileExp (C char)   = Constant (Char char)
compileExp Inf = Constant (Float (read "Infinity"))
compileExp (T.Neg exp) = F.Neg (compileExp exp)
compileExp (T.Let id t e1 e2) = F.Let (Ident ("t_" ++ id)) (compileExp e1) (compileExp e2) -- Let
compileExp (T.Op ident instDecl args) = compileOpExp ident instDecl args
compileExp (T.Fn _ _ _) = error "Fn not supported"
compileExp (Vc exps) = Array(map compileExp exps)


compileOpExp ident instDecl args = case ident of
  "reduce" -> compileReduce instDecl args
  "eachV"  -> compileEachV instDecl args
  _
    | [e]      <- args
    , Just fun <- convertFun ident
    -> F.FunCall fun [compileExp e]
    | [e1,e2]  <- args
    , Just op  <- convertBinOp ident
    -> F.BinApp op (compileExp e1) (compileExp e2)
    | otherwise       -> error $ ident ++ " not supported"
--compileOpExp "addi" _ [e1,e2] = F.BinApp F.Plus (compileExp e1) (compileExp e2)
--compileOpExp "addd" _ [e1,e2] = F.BinApp F.Plus (compileExp e1) (compileExp e2)
--compileOpExp "i2d" _ [e]      = F.FunCall "toReal" [compileExp e]
--compileOpExp ident instDecl args = error $ ident ++ " not supported"

convertFun fun = case fun of
  "i2d"    -> Just "toReal"
  "iotaV"  -> Just "iota"
  _     -> Nothing

convertBinOp op = case op of
  "addi" -> Just F.Plus
  "addd" -> Just F.Plus
  _      -> Nothing

-- AUX functions--
--compileEachV :: Maybe InstDecl -> T.Exp -> F.Exp -> F.Exp
compileEachV Nothing _ = error "Need instance declaration for eachV"
compileEachV (Just ([intp,outtp],[len])) [kernel,array] = Map kernelExp (compileExp array)
   where kernelExp = compileKernel kernel (makeBTp outtp) 

--compileReduce :: Maybe InstDecl -> T.Exp -> F.Exp -> F.Exp -> F.Exp
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
compileKernel (T.Fn ident tp (T.Fn ident2 tp2 exp)) rtp = F.Fn rtp [(compileTp tp,ident),(compileTp tp2,ident2)] (compileExp exp)
compileKernel (T.Fn ident tp exp) rtp = F.Fn rtp [(compileTp tp,ident)] (compileExp exp)

makeKernel ident
  | Just fun <- convertFun ident = F.Fun fun []
  | Just op  <- convertBinOp ident = F.Op op
  | otherwise = error $ "not supported operation " ++ ident

compileTp (ArrT bt (R rank)) = makeArrTp (makeBTp bt) rank
compileTp (VecT bt (R rank)) = makeArrTp (makeBTp bt) 1
compileTp (SV bt (R rank)) = makeArrTp (makeBTp bt) 1
compileTp (S bt (R rank)) = makeBTp bt

makeBTp T.IntT = F.IntT
makeBTp T.DoubleT = F.RealT
makeBTp T.BoolT = F.BoolT
makeBTp T.CharT = F.CharT

makeArrTp :: F.Type -> Integer -> F.Type
makeArrTp btp 0 = btp
makeArrTp btp n = F.ArrayT (makeArrTp btp (n-1))
