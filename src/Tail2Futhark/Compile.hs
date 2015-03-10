module Tail2Futhark.Compile (compile) where

import APLAcc.TAIL.AST as T -- the TAIL AST
import Tail2Futhark.Futhark.AST as F -- the futhark AST
import GHC.Float (double2Float)

compile :: T.Program -> F.Program
compile e = [(RealT, "main", [], (compileExp e))]

-- Expressionis--
compileExp :: T.Exp -> F.Exp

compileExp (T.Let id t e1 e2) = F.Let (Ident id) (compileExp e1) (compileExp e2) -- Let

-- simple types --
compileExp (I int) = Constant (Int int)
compileExp (D double) = Constant (Float (double2Float double)) -- isn't this supporsed to be real??????????
compileExp (C char)   = Constant (Char char)
compileExp (T.Var ident) = F.Var ident

-- operators --

--compileExp (Vc exps) = Array(map compileExp exps)
--compileExp Inf = Constant (Float (read "Infinity"))
--compileExp (T.Neg exp) = F.Neg (compileExp exp)
--compileExp (T.Op ident instDecl ((T.Var op):args)) = Map (Fun op []) (Array (map compileExp args))
--compileExp (T.Op ident instDecl args) = Array (F.Var ident : map compileExp args)
compileExp rest = F.Var $ show rest -- catch all DUMMY!!!
