module Tail2Futhark.Compile (compile) where

import APLAcc.TAIL.AST as T -- the TAIL AST
import Tail2Futhark.Futhark.AST as F -- the futhark AST

compile :: T.Program -> F.Program
compile e = [(RealT, "main", [], (compileExp e))]

-- Expression --
compileExp :: T.Exp -> F.Exp
compileExp (T.Let id t e1 e2) = F.Let (Ident id) (compileExp e1) (compileExp e2) -- Let
compileExp (I int) = Constant (Int int)
compileExp rest = F.Var $ show rest -- catch all
