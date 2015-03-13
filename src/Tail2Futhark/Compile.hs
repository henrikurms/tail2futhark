module Tail2Futhark.Compile (compile) where

import APLAcc.TAIL.AST as T -- the TAIL AST
import Tail2Futhark.Futhark.AST as F -- the futhark AST
import GHC.Float (double2Float)

compile :: T.Program -> F.Program
compile e = [(RealT, "main", [], (compileExp $ removeFinali2d e))] -- This is only temporary

removeFinali2d (T.Let id t e1 (T.Op "i2d" _ [e])) = T.Let id t e1 e
removeFinali2d (T.Let id t (T.Op "i2d" _ [e]) e2) = T.Let id t e e2
removeFinali2d (T.Op "i2d" _ [e]) = e
removeFinali2d rest = rest

-- Expressionis--
compileExp :: T.Exp -> F.Exp

compileExp (T.Let id t e1 e2) = F.Let (Ident id) (compileExp e1) (compileExp e2) -- Let

-- simple types --
compileExp (I int) = Constant (Int int)
compileExp (D double) = Constant (Float (double2Float double)) -- isn't this supporsed to be real??????????
compileExp (C char)   = Constant (Char char)
compileExp (T.Var ident) = F.Var ident
compileExp (Vc exps) = Array(map compileExp exps)
--compileExp Inf = Constant (Float (read "Infinity"))
--compileExp (T.Neg exp) = F.Neg (compileExp exp)
--compileExp (T.Op ident instDecl ((T.Var op):args)) = Map (Fun op []) (Array (map compileExp args))
compileExp (T.Op ident instDecl args) = compileOpExp ident instDecl args
compileExp rest = F.Var $ show rest -- catch all DUMMY!!!


compileOpExp "reduce" instDecl [kernel,id,array] = compileReduce instDecl kernel (compileExp id) (compileExp array)
compileOpExp "eachV" instDecl [kernel,array] = compileEachV instDecl kernel (compileExp array)
--compileOpExp "reduce" instDecl [T.Fn ident tp exp,identity,array] = Reduce (F.Fn ) identity array
--compileOpExp ident instDecl args = F.Var $ show ident ++ show args
compileOpExp "addi" _ [e1,e2] = F.BinApp F.Plus (compileExp e1) (compileExp e2)
compileOpExp "addd" _ [e1,e2] = F.BinApp F.Plus (compileExp e1) (compileExp e2)
compileOpExp ident instDecl args = error $ ident ++ " not supported"

-- AUX functions--
compileEachV :: Maybe InstDecl -> T.Exp -> F.Exp -> F.Exp
compileEachV Nothing _ _ = error "Need instance declaration for eachV"
compileEachV (Just ([intp,outtp],[len])) kernel array = undefined --Map kernelExp array
   where kernelExp = compileKernel kernel (makeBTp outtp) 

compileReduce :: Maybe InstDecl -> T.Exp -> F.Exp -> F.Exp -> F.Exp
compileReduce Nothing _ _ _ = error "Need instance declaration for reduce"
compileReduce (Just ([tp],[rank])) kernel id array = if rank == 0 then Reduce kernelExp id array else F.Var "puha" 
    where kernelExp = compileKernel kernel (makeArrTp (makeBTp tp) rank)
-- compileReduce for arrays

compileKernel :: T.Exp -> F.Type -> Kernel
compileKernel (T.Var ident) rtp = compileBinOp ident
compileKernel (T.Fn ident tp (T.Fn ident2 tp2 exp)) rtp = F.Fn rtp [(ident,compileTp tp),(ident2,compileTp tp2)] (compileExp exp)
compileKernel (T.Fn ident tp exp) rtp = F.Fn rtp [(ident,compileTp tp)] (compileExp exp)

compileBinOp ident = case ident of
                       "addi" -> F.Op Plus
                       "addd" -> F.Op Plus
                       _      -> F.Fun ident []

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
