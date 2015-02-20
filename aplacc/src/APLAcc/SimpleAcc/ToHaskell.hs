module APLAcc.SimpleAcc.ToHaskell (
  toHs,
  OutputOpts(..), defaultOpts,
) where

import Prelude hiding (exp)

import Language.Haskell.Exts.Syntax as Hs
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified APLAcc.SimpleAcc.AST as A

data OutputOpts = ToHsOpts { toCUDA :: Bool }

defaultOpts = ToHsOpts { toCUDA = False }

toHs :: OutputOpts -> A.Program -> String
toHs opts = prettyPrint . outputProgram opts

instance Show A.Type where
  show = prettyPrint . outputType

instance Show A.Name where
  show = prettyPrint . name

instance Show A.QName where
  show = prettyPrint . qname

instance Show A.Exp where
  show = prettyPrint . outputExp


qualAcc :: Name -> QName
qualAcc = Qual (ModuleName "Acc")

qualPrelude :: Name -> QName
qualPrelude = Qual (ModuleName "P")

name :: A.Name -> Name
name (A.Ident n) = Ident n
name (A.Symbol n) = Symbol n

qname :: A.QName -> QName
qname (A.UnQual n)     = UnQual $ name n
qname (A.Prelude (A.Symbol "+")) = UnQual $ Symbol "+"
qname (A.Prelude (A.Symbol "-")) = UnQual $ Symbol "-"
qname (A.Prelude (A.Symbol "*")) = UnQual $ Symbol "*"
qname (A.Prelude (A.Symbol "/")) = UnQual $ Symbol "/"
qname (A.Prelude n)    = Qual (ModuleName "P") $ name n
qname (A.Accelerate n) = UnQual $ name n
qname (A.Primitive n)  = Qual (ModuleName "Prim") $ name n

infixOp :: A.QName -> QOp
infixOp = QVarOp . qname

acc    = TyApp $ TyCon $ qname $ A.Accelerate $ A.Ident "Acc"
exp    = TyApp $ TyCon $ qname $ A.Accelerate $ A.Ident "Exp"
scalar = TyApp $ TyCon $ qname $ A.Accelerate $ A.Ident "Scalar"
vector = TyApp $ TyCon $ qname $ A.Accelerate $ A.Ident "Vector"
array d = TyApp (TyApp (TyCon $ qname $ A.Accelerate $ A.Ident "Array") d)
dim n  = TyCon $ qname $ A.Accelerate $ A.Ident $ "DIM" ++ show n
int    = TyCon $ qname $ A.Prelude $ A.Ident "Int"
double = TyCon $ qname $ A.Prelude $ A.Ident "Double"

snocList :: (Integral a) => [a] -> Exp
snocList ns =
  foldl (\e e' -> InfixApp e (infixOp $ A.Accelerate $ A.Symbol ":.") e')
        (Var $ qname $ A.Accelerate $ A.Ident "Z")
        (map (Lit . Int . toInteger) ns)

outputProgram :: OutputOpts -> A.Program -> Module
outputProgram opts p =
  Module noLoc (ModuleName "Main") [] Nothing Nothing imports [progSig, prog, main]
  where backend = if toCUDA opts
                  then "Data.Array.Accelerate.CUDA"
                  else "Data.Array.Accelerate.Interpreter"
        imports =
          [ ImportDecl { importLoc       = noLoc
                       , importModule    = ModuleName "Prelude"
                       , importQualified = True
                       , importSrc       = False
                       , importSafe      = False
                       , importPkg       = Nothing
                       , importAs        = Just $ ModuleName "P"
                       , importSpecs     = Nothing }
          , ImportDecl { importLoc       = noLoc
                       , importModule    = ModuleName "Prelude"
                       , importQualified = False
                       , importSrc       = False
                       , importSafe      = False
                       , importPkg       = Nothing
                       , importAs        = Nothing
                       , importSpecs     = Just (False, map (IAbs . Symbol) ["+", "-", "*", "/"]) }
          , ImportDecl { importLoc       = noLoc
                       , importModule    = ModuleName "Data.Array.Accelerate"
                       , importQualified = False
                       , importSrc       = False
                       , importSafe      = False
                       , importPkg       = Nothing
                       , importAs        = Nothing
                       , importSpecs     = Nothing }
          , ImportDecl { importLoc       = noLoc
                       , importModule    = ModuleName backend
                       , importQualified = True
                       , importSrc       = False
                       , importSafe      = False
                       , importPkg       = Nothing
                       , importAs        = Just $ ModuleName "Backend"
                       , importSpecs     = Nothing }
          , ImportDecl { importLoc       = noLoc
                       , importModule    = ModuleName "APLAcc.Primitives"
                       , importQualified = True
                       , importSrc       = False
                       , importSafe      = False
                       , importPkg       = Nothing
                       , importAs        = Just $ ModuleName "Prim"
                       , importSpecs     = Nothing }
          ]
        -- Assume result is always scalar double for now
        progSig = TypeSig noLoc [Ident "program"] $ acc (scalar double)
        prog = FunBind
          [Match noLoc (Ident "program") [] Nothing
                 (UnGuardedRhs $ outputExp p) (BDecls [])]
        main = FunBind
          [Match noLoc (Ident "main") [] Nothing
                 (UnGuardedRhs mainBody) (BDecls [])]
        mainBody = App (Var $ qualPrelude $ Ident "print") $
          App (Var $ Qual (ModuleName "Backend") $ Ident "run") (Var $ UnQual $ Ident "program")

outputExp :: A.Exp -> Exp
outputExp (A.Var n) = Var $ qname n 
outputExp (A.I i) = Lit $ Int i
outputExp (A.D d) = Lit $ Frac $ toRational d
outputExp (A.Shape is) = snocList is
outputExp (A.TypSig e t) = ExpTypeSig noLoc (outputExp e) (outputType t)
outputExp (A.Neg e) = NegApp $ outputExp e
outputExp (A.List es) = List $ map outputExp es
outputExp (A.InfixApp n [e]) = outputExp e
outputExp (A.InfixApp n (e1:e2:es)) =
  foldl op (op (outputExp e1) (outputExp e2)) (map outputExp es)
  where op = flip InfixApp (infixOp n)
outputExp (A.InfixApp n []) = error "invalid infix application"
outputExp (A.App n es) = foldl App (Var $ qname n) (map outputExp es)
outputExp (A.Let ident typ e1 e2) =
  let e1' = case e1 of
              (A.TypSig _ _) -> outputExp e1
              _              -> ExpTypeSig noLoc (outputExp e1) (outputType typ)
  in Let (BDecls [ PatBind noLoc (PVar $ Ident ident) (UnGuardedRhs e1') (BDecls []) ])
         (outputExp e2)
outputExp (A.Fn ident _ e) =
  Lambda noLoc [PVar $ Ident ident] (outputExp e)

outputType :: A.Type -> Type
outputType (A.Exp btyp) = exp (outputBType btyp)
outputType (A.Acc r btyp) = acc $ array (dim r) (outputBType btyp)
outputType (A.Plain btyp) = outputBType btyp

outputBType :: A.BType -> Type
outputBType A.IntT = int
outputBType A.DoubleT = double
