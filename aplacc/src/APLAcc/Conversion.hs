module APLAcc.Conversion (convertProgram) where

import Control.Monad.Reader
import qualified Data.Map as Map

import qualified APLAcc.TAIL.AST as T
import qualified APLAcc.SimpleAcc.AST as A
import APLAcc.SimpleAcc.AST (Type(..), BType(..), Name(..), QName(..))
import APLAcc.SimpleAcc.ToHaskell () -- Show instances for SimpleAcc.AST


type Env = Map.Map T.Ident A.Type

emptyEnv :: Env
emptyEnv = Map.empty

type Convert a = Reader Env a

runConvert = runReader

convertProgram :: T.Program -> A.Program
convertProgram p = runConvert (convertExp p (Acc 0 DoubleT)) emptyEnv

typeCast :: A.Type -- from
         -> A.Type -- to
         -> A.Exp -> A.Exp
typeCast (Plain t1)   (Exp t2)        | t1 == t2 = A.constant . flip A.TypSig (Plain t1)
typeCast (Plain t1)   (Acc r t2)      | t1 == t2 = typeCast (Exp t1) (Acc r t1) . typeCast (Plain t1) (Exp t1)

typeCast (Exp t1)    (Acc 0 t2)       = A.unit . typeCast (Exp t1) (Exp t2)
typeCast (Exp t1)    (Acc 1 t2)       = A.unitvec . typeCast (Exp t1) (Acc 0 t2)

typeCast (Acc 0 t1)  (Plain t2)       = typeCast (Exp t1) (Plain t2) . A.the
typeCast (Acc 0 t1)  (Exp t2)         | t1 == t2 = A.the
typeCast (Acc 1 t1)  (Exp t2)         | t1 == t2 = A.first
typeCast (Acc 0 t1)  (Acc 1 t2)       | t1 == t2 = A.unitvec
typeCast (Acc 1 t1)  (Acc 0 t2)       = typeCast (Exp t1) (Acc 0 t2) . A.first

typeCast t1 t2 | t1 == t2 = id
typeCast t1 t2 = \e -> error $ "cannot type cast " ++ show e ++ " from " ++ show t1 ++ " to " ++ show t2


cancelLift :: A.Type -> A.Exp -> (A.Type, A.Exp)
cancelLift (Exp t1) (A.App (Accelerate (Ident "constant")) [A.TypSig e (Plain t2)]) | t1 == t2 = (Plain t1, e)
cancelLift t e = (t, e)


convertExp :: T.Exp -> A.Type -> Convert A.Exp
convertExp (T.Var "zilde") (Acc 1 _) = return $ A.Var $ Primitive $ Ident "zilde"
convertExp (T.Var name) t = do
  env <- ask
  return $ case Map.lookup name env of
    Nothing -> error $ name ++ " not found in env"
    Just t2 -> typeCast t2 t $ A.Var $ UnQual $ Ident name

convertExp (T.I i) t = return $ typeCast (Plain IntT) t $ A.I i
convertExp (T.D d) t = return $ typeCast (Plain DoubleT) t $ A.D d
convertExp (T.Inf) t = return $ typeCast (Plain DoubleT) t $ A.Var $ Primitive $ Ident "infinity"

convertExp (T.Neg e) t = do
  let t' = Exp $ A.baseType t
  e' <- convertExp e t'
  let (t2, e2) = cancelLift t' e'
  return $ typeCast t2 t $ A.Neg e2

convertExp (T.Let x t1 e1 e2) t2 = do
  let t1' = convertType t1
  e1' <- convertExp e1 t1'
  -- If the e1 has been lifted to Exp then drop the lift and store as Plain
  let (t3, e3) = cancelLift t1' e1'
  e2' <- local (Map.insert x t3) $ convertExp e2 t2
  return $ A.Let x t3 e3 e2'

convertExp (T.Op name instDecl args) t = do
  (e, t2) <- convertOp name instDecl args t
  return $ typeCast t2 t e

convertExp (T.Fn x t1 e) t2 = do
  let t1' = convertType t1
  e' <- local (Map.insert x t1') (convertExp e t2)
  return $ A.Fn x t1' e'

convertExp (T.Vc es) (Acc 1 t) = do
  es' <- mapM (`convertExp` Plain t) es
  return $ A.TypSig (A.use $ A.fromList (length es') (A.List es')) (Acc 1 t)

convertExp e t = error $ "failed to convert exp " ++ show e ++ " to type " ++ show t

convertType :: T.Type -> A.Type
convertType (T.ArrT t (T.R 0)) = Exp t
convertType (T.ArrT t (T.R r)) = Acc r t
convertType (T.ShT _) = Acc 1 IntT
convertType (T.SiT _) = Exp IntT
convertType (T.ViT _) = Acc 1 IntT
convertType _ = error "convertType - not implemented"

functions :: Map.Map String (Maybe T.InstDecl -> A.Type -> ([A.Exp] -> A.Exp, [T.Exp -> Convert A.Exp], A.Type))
functions = Map.fromList
  [ ( "addi",    \Nothing                    t -> binOp (symb "+")   IntT    t )
  , ( "subi",    \Nothing                    t -> binOp (symb "-")   IntT    t )
  , ( "muli",    \Nothing                    t -> binOp (symb "*")   IntT    t )
  , ( "mini",    \Nothing                    t -> binOp (prel "min") IntT    t )
  , ( "maxi",    \Nothing                    t -> binOp (prel "max") IntT    t )
  , ( "addd",    \Nothing                    t -> binOp (symb "+")   DoubleT t )
  , ( "subd",    \Nothing                    t -> binOp (symb "-")   DoubleT t )
  , ( "muld",    \Nothing                    t -> binOp (symb "*")   DoubleT t )
  , ( "divd",    \Nothing                    t -> binOp (symb "/")   DoubleT t )
  , ( "mind",    \Nothing                    t -> binOp (prel "min") DoubleT t )
  , ( "maxd",    \Nothing                    t -> binOp (prel "max") DoubleT t )
  , ( "i2d",     \Nothing                    t -> unaryOp (prim "i2d")      IntT    DoubleT t )
  , ( "negi",    \Nothing                    t -> unaryOp (\[a] -> A.Neg a) IntT    IntT    t )
  , ( "negd",    \Nothing                    t -> unaryOp (\[a] -> A.Neg a) DoubleT DoubleT t )
  , ( "resi",    \Nothing                    _ -> (prim "residue",  [expArg IntT, expArg IntT], Exp IntT) )
  , ( "each",    \(Just ([t1, t2], [r]))     _ -> (prim "each",     [funcArg $ Exp t1, accArg r t1], Acc r t2) )
  , ( "reduce",  \(Just ([t], [r]))          _ -> (prim "reduce",   [funcArg $ Exp t, expArg t, accArg (r+1) t], Acc r t) )
  , ( "cat",     \(Just ([t], [r]))          _ -> (prim "cat",      [accArg r t, accArg r t], Acc r t) )
  , ( "catSh",   \Nothing                    _ -> (prim "catSh",    [accArg 1 IntT, accArg 1 IntT], Acc 1 IntT) )
  , ( "iota",    \Nothing                    t -> (prim "iota",     [expArg IntT], Acc 1 (A.baseType t)) )
  , ( "iotaSh",  \Nothing                    _ -> (prim "iotaSh",   [expArg IntT], Acc 1 IntT) )
  , ( "drop",    \(Just ([t], [r]))          _ -> (prim "drop",     [expArg t, accArg r t], Acc r t) )
  , ( "dropSh",  \Nothing                    _ -> (prim "dropSh",   [expArg IntT, accArg 1 IntT], Acc 1 IntT) )
  -- FIXME: take should take a default element
  , ( "take",    \(Just ([t], [r]))          _ -> (prim "take",     [expArg t, accArg r t], Acc r t) )
  , ( "takeSh",  \Nothing                    t -> (prim "takeSh",   [expArg IntT, accArg 1 IntT], t) )
  , ( "shape",   \(Just ([t], [r]))          _ -> (prim "shape",    [accArg r t], Acc 1 IntT) )
  , ( "shapeSh", \Nothing                    _ -> (prim "shapeSh",  [accArg 1 IntT], Exp IntT) )
  -- FIXME: APLT currently uses the reshape name for reshape0
  , ( "reshape", \(Just ([t], [r1, r2]))     _ -> (prim "reshape0", [shapeArg, accArg r1 t], Acc r2 t) )
  , ( "reverse", \(Just ([t], [r]))          _ -> (prim "reverse",  [accArg r t], Acc r t) )
  , ( "cons",    \(Just ([t], [r]))          _ -> (prim "cons",     [accArg r t, accArg (r+1) t], Acc (r+1) t) )
  , ( "consSh",  \Nothing                    _ -> (prim "consSh",   [expArg IntT, accArg 1 IntT], Acc 1 IntT) )
  , ( "snoc",    \(Just ([t], [r]))          _ -> (prim "snoc",     [accArg (r+1) t, accArg r t], Acc (r+1) t) )
  , ( "snocSh",  \Nothing                    _ -> (prim "snocSh",   [accArg 1 IntT, expArg IntT], Acc 1 IntT) )
  , ( "zipWith", \(Just ([t1, t2, t3], [r])) _ -> (prim "zipWith",  [funcArg $ Exp t1, accArg r t1, accArg r t2], Acc r t3) )
  , ( "rotate",  \(Just ([t], [r]))          _ -> (prim "rotate",   [expArg IntT, accArg r t], Acc r t) )
  , ( "rotateSh",\Nothing                    _ -> (prim "rotateSh", [expArg IntT, accArg 1 IntT], Acc 1 IntT) )
  , ( "transp",  \(Just ([t], [r]))          _ -> (prim "transp",   [accArg r t], Acc r t) )
  --, ( "transp2", \(Just ([t], [r]))          _ -> (prim "transp",   [shapeArg, accArg r t], Acc r t) )
  -- FIXME: first should take a default element
  , ( "first",   \(Just ([t], [r]))          _ -> (prim "first",    [accArg r t], Exp t) )
  , ( "firstSh", \Nothing                    _ -> (prim "firstSh",  [accArg 1 IntT], Exp IntT) )
  ]
  where symb = A.InfixApp . Prelude . Symbol
        prim = A.App . Primitive . Ident
        prel = A.App . Prelude . Ident

        unaryOp f bty retbty (Plain _) = (f, [plainArg bty], Plain retbty)
        unaryOp f bty retbty _         = (f, [expArg bty],   Exp retbty)

        binOp f bty (Plain _) = (f, [plainArg bty, plainArg bty], Plain bty)
        binOp f bty _         = (f, [expArg bty,   expArg bty],   Exp bty)

        plainArg :: A.BType -> T.Exp -> Convert A.Exp
        plainArg t = flip convertExp (Plain t)

        expArg :: A.BType -> T.Exp -> Convert A.Exp
        expArg t = flip convertExp (Exp t)

        accArg :: Integer -> A.BType -> T.Exp -> Convert A.Exp
        accArg n t = flip convertExp (Acc n t)

        shapeArg :: T.Exp -> Convert A.Exp
        shapeArg (T.Vc es) =
          return $ A.lift $ A.InfixApp (Accelerate $ Symbol ":.") ((A.Var $ Accelerate $ Ident "Z") : map toInt es)
          where toInt (T.I i) = A.TypSig (A.I i) (Plain IntT)
                toInt _ = error "shape must be list of ints"
        shapeArg e = error $ "shape argument " ++ show e ++ " not supported"

        funcArg :: A.Type -> T.Exp -> Convert A.Exp
        funcArg (Exp IntT) (T.Var "i2d") = return $ A.Var $ Primitive $ Ident "i2d"
        funcArg (Exp IntT) (T.Var "addi") = return $ A.Var $ Prelude $ Symbol "+"
        funcArg (Exp IntT) (T.Var "subi") = return $ A.Var $ Prelude $ Symbol "-"
        funcArg (Exp IntT) (T.Var "muli") = return $ A.Var $ Prelude $ Symbol "*"
        funcArg (Exp IntT) (T.Var "mini") = return $ A.Var $ Prelude $ Ident "min"
        funcArg (Exp IntT) (T.Var "maxi") = return $ A.Var $ Prelude $ Ident "max"
        funcArg (Exp IntT) (T.Var "resi") = return $ A.Var $ Primitive $ Ident "residue"
        funcArg (Exp DoubleT) (T.Var "addd") = return $ A.Var $ Prelude $ Symbol "+"
        funcArg (Exp DoubleT) (T.Var "subd") = return $ A.Var $ Prelude $ Symbol "-"
        funcArg (Exp DoubleT) (T.Var "muld") = return $ A.Var $ Prelude $ Symbol "*"
        funcArg (Exp DoubleT) (T.Var "divd") = return $ A.Var $ Prelude $ Symbol "/"
        funcArg (Exp DoubleT) (T.Var "mind") = return $ A.Var $ Prelude $ Ident "min"
        funcArg (Exp DoubleT) (T.Var "maxd") = return $ A.Var $ Prelude $ Ident "max"
        funcArg t e@(T.Fn{}) = convertExp e t
        funcArg t name = error $ show name ++ " not implemented as function for " ++ show t


convertOp :: T.Ident -> Maybe T.InstDecl -> [T.Exp] -> A.Type -> Convert (A.Exp, A.Type)
convertOp name inst args t =
  case Map.lookup name functions of
    Just f  -> do let (g, argTyps, retTyp) = f inst t
                  e <- liftM g (convertArgs argTyps args)
                  return (e, retTyp)
    Nothing -> error $ name ++ "{" ++ show inst ++ "} not implemented"


convertArgs :: [T.Exp -> Convert A.Exp] -> [T.Exp] -> Convert [A.Exp]
convertArgs = zipWithM ($)
