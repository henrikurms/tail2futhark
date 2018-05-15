{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tail2Futhark.Compile (compile, getType) where

import           Tail2Futhark.Futhark.AST    as F
import           Tail2Futhark.Futhark.Pretty as F
import           Tail2Futhark.TAIL.AST       as T

import           Prelude

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Char
import           Data.List
import qualified Data.Map                    as M
import           Options                     (Options (..))

data Env = Env { floatType :: F.Type }

newEnv :: Env
newEnv = Env F.F64T

newtype CompilerM a = CompilerM (ReaderT Env (WriterT (M.Map String FunDecl) (Except String)) a)
                    deriving (Applicative, Functor, Monad,
                              MonadReader Env, MonadError String,
                              MonadWriter (M.Map String FunDecl))

runCompilerM :: CompilerM a -> Env -> Either String (a, M.Map String FunDecl)
runCompilerM (CompilerM m) = runExcept . runWriterT . runReaderT m

--------------------------
-- THE MAIN FUNCTION --
--------------------------

compile :: Options -> [(String, T.Program)] -> F.Program
compile opts progs =
  case runCompilerM (mapM (uncurry $ compileProg opts) progs) env of
    Left e -> error e
    Right (entry_funs, genfuns) ->
      F.Program $
      includes ++
      map snd (M.toList genfuns) ++
      entry_funs
  where includes = if includeLibs opts then fbuiltins else []
        fbuiltins = if floatAsSingle opts then f32Builtins else f64Builtins
        float = if floatAsSingle opts then F.F32T else F.F64T
        env = newEnv { floatType = float }

compileProg :: Options -> String -> T.Program -> CompilerM F.FunDecl
compileProg opts entry_name prog = do
  mainbody <- compileExp rootExp
  return $ F.FunDecl True ret entry_name [] signature' $ maybeUnsafe mainbody
  where (signature, ret, rootExp) = inputsAndOutputs float prog
        signature' = case signature of [] -> []
                                       _  -> signature
        float = if floatAsSingle opts then F.F32T else F.F64T
        maybeUnsafe
          | unsafe opts = Unsafe
          | otherwise   = id


-------------------------
-- HELPER FUNCTIONS --
-------------------------

inputsAndOutputs :: F.Type -> T.Exp -> ([(F.Type, String)], F.Type, T.Exp)
inputsAndOutputs float = inputsAndOutputs' []
  where complex = F.TupleT [float, float]
        inputsAndOutputs' outs (T.Let v  _ (T.Op "readIntVecFile" _ _) e2) =
          ((F.ArrayT F.IntT F.AnyDim, "t_" ++ v):sig, ret, e')
          where (sig, ret, e') = inputsAndOutputs' outs e2

        inputsAndOutputs' outs (T.Let v  _ (T.Op "readDoubleVecFile" _ _) e2) =
          ((F.ArrayT float F.AnyDim, "t_" ++ v):sig, ret, e')
          where (sig, ret, e') = inputsAndOutputs' outs e2

        inputsAndOutputs' outs (T.Let v (T.ArrT _ (R r)) (T.Op "prArrC" _ [e]) e2) =
          output outs v e T.IntT F.IntT r e2
        inputsAndOutputs' outs (T.Let v (T.ArrT _ (R r)) (T.Op "prArrB" _ [e]) e2) =
          output outs v e T.BoolT F.BoolT r e2
        inputsAndOutputs' outs (T.Let v (T.ArrT _ (R r)) (T.Op "prArrI" _ [e]) e2) =
          output outs v e T.IntT F.IntT r e2
        inputsAndOutputs' outs (T.Let v (T.ArrT _ (R r)) (T.Op "prArrD" _ [e]) e2) =
          output outs v e T.DoubleT float r e2
        inputsAndOutputs' outs (T.Let v (T.ArrT _ (R r)) (T.Op "prArrX" _ [e]) e2) =
          output outs v e T.ComplexT complex r e2

        inputsAndOutputs' outs (T.Let v (T.ArrT _ (R r)) (T.Op "prSclI" _ [e]) e2) =
          output outs v e T.IntT F.IntT r e2
        inputsAndOutputs' outs (T.Let v (T.ArrT _ (R r)) (T.Op "prSclD" _ [e]) e2) =
          output outs v e T.DoubleT float r e2
        inputsAndOutputs' outs (T.Let v (T.ArrT _ (R r)) (T.Op "prSclX" _ [e]) e2) =
          output outs v e T.ComplexT complex r e2


        inputsAndOutputs' outs (T.Let v  t e body) =
          (sig, ret, T.Let v t e body')
          where (sig, ret, body') = inputsAndOutputs' outs body

        inputsAndOutputs' [(e,t)] _ =
          ([], t, e)

        inputsAndOutputs' outs@(_:_) _ =
          ([], F.TupleT ret, T.Op "tuple" Nothing es)
          where (es, ret) = unzip $ reverse outs

        inputsAndOutputs' [] e =
          ([], float, e)

        output outs v e bt t r rest =
          let (outs'', ret, rest') = inputsAndOutputs' outs' rest
          in (outs'', ret, T.Let v (T.ArrT bt (R r)) e rest')
          where outs' = (e, foldr (const (`F.ArrayT` F.AnyDim)) t [0..r-1]) : outs

----------------------------------------
-- AUX FUNCTIONS OF LIBRARY FUNCTIONS --
----------------------------------------

absExp :: F.Exp -> F.Exp
absExp e = IfThenElse (BinApp LessEq e (Constant (Int 0))) (F.Neg e) e

maxExp :: F.Exp -> F.Exp -> F.Exp
maxExp e1 e2 = IfThenElse (BinApp LessEq e1 e2) e2 e1

minExp :: F.Exp -> F.Exp -> F.Exp
minExp e1 e2 = IfThenElse (BinApp LessEq e1 e2) e1 e2

-- reshape1 --
-- create split part of reshape1 function --
mkSplit :: F.Ident -> F.Ident -> F.Exp -> F.Exp -> F.Exp -> F.Exp
mkSplit id1 id2 dims e =
  F.Let (TouplePat [Ident id1,Ident id2]) (F.FunCall (F.Var "split") [dims, e])

makeLets :: [(F.Ident, F.Exp)] -> F.Exp -> F.Exp
makeLets ((v,e) : rest) body = F.Let (Ident v) e (makeLets rest body)
makeLets [] body             = body

-- Fully flatten an array of known rank.
fullFlatten :: Int -> F.Exp -> F.Exp
fullFlatten r e | r > 1 = fullFlatten (r-1) $ F.FunCall (F.Var "flatten") [e]
                | otherwise = e

fullUnflatten :: [F.Exp] -> F.Exp -> F.Exp
fullUnflatten dims arr = foldr unflatten arr $
                         zip (drop 1 $ map fProduct $ inits dims) (drop 1 dims)
  where unflatten (n,m) arr' = F.FunCall (F.Var "unflatten") [n, m, arr']
        fProduct [] = Constant $ Int 0
        fProduct (x:xs) = foldl (BinApp Mult) x xs

reshape1Body :: F.Type -> F.Exp
reshape1Body t = F.IfThenElse (BinApp Eq size (Constant (Int 0)))
                 (F.FunCall (F.Var "replicate") [F.Var "l", blank t])
                 (makeLets (zip ["roundUp","extend"] [desired,reshapeCall]) split)
  where split = mkSplit "v1" "_" (F.Var "l") (F.Var "extend") (F.Var "v1")
        desired = F.Var "l" `fplus` (size `fminus` Constant (Int 1)) `fdiv` size
        reshapeCall = F.FunCall (F.Var "flatten") [F.FunCall (F.Var "replicate") [len,F.Var "x"]]
        size = F.FunCall (F.Var "length") [F.Var "x"]
        len = F.Var "roundUp"
        fdiv = BinApp Div
        fplus = BinApp Plus
        fminus = BinApp Minus

        blank (F.ArrayT et _) = blank et
        blank F.F32T = Constant $ F32 0
        blank F.F64T = Constant $ F64 0
        blank F.BoolT = Constant $ Bool False
        blank (F.TupleT ts) = Tuple $ map blank ts
        blank _ = Constant $ Int 0

-- drop --
-- make body for drop1 function --
dropBody :: [F.Exp] -> F.Type -> F.Exp
dropBody _dims tp = IfThenElse (size `less` absExp len) emptArr elseBranch
    where izero = Constant (Int 0)
          less = BinApp LessEq
          len = F.Var "l"
          size = F.FunCall (F.Var "length") [F.Var "x"]
          plus = BinApp Plus len size
          emptArr = F.Ascript (F.Array []) $
                    foldr (flip ArrayT) (baseType tp) $
                    ConstDim 0 :
                    map (NamedDim . ("d"++) . show) [(0::Int)..rank tp-1]
          elseBranch = IfThenElse (len `less` izero) negDrop posDrop
          negDrop = mkSplit "v1" "_" plus (F.Var "x") (F.Var "v1")
          posDrop = mkSplit "_" "v2" len (F.Var "x") (F.Var "v2")

-- take1 --
-- make body for take1 function --
takeBody :: F.Exp -> F.Exp
takeBody padElement = IfThenElse (izero `less` len) posTake negTake
    where less = BinApp LessEq
          izero = Constant (Int 0)
          plus  = BinApp Plus len size
          len  = F.Var "l"
          size = F.FunCall (F.Var "length") [F.Var "x"]
          padRight = F.FunCall (F.Var "concat") [F.Var "x", padding]
          padLeft = F.FunCall (F.Var "concat") [padding, F.Var "x"]
          padding = F.FunCall (F.Var "replicate")
                    [BinApp Minus (F.FunCall (F.Var "i32.abs") [len]) size, padElement]
          posTake = IfThenElse (len `less` size) (mkSplit "v1" "_" (F.Var "l") (F.Var "x") (F.Var "v1")) padRight
          negTake = IfThenElse (izero `less` plus) (mkSplit "_" "v2" plus (F.Var "x") (F.Var "v2")) padLeft

------------------------------------------
-- AUX FUNCTIONS FOR SPECIFIC FUNCTIONS --
------------------------------------------

-- AUX transp --
makeTransp :: Integer -> F.Exp -> F.Exp
makeTransp r = makeTransp2 $ reverse [0..r-1]

-- APL uses inverse indexing for dyadic transpose; invIndexing has a
-- number of properties:
--   (1) invIndexing(iota n) = iota n;
--   (2) invIndexing(rev(iota n)) = rev(iota n);
--   (3) invIndexing o invIndexing = id;
--   (4) invIndexing [2,0,1] = [1,2,0]
--   ...
invIndexing :: [Integer] -> [Integer] 
invIndexing idxs = inv idxs 0
  where inv xs n = if n == toInteger(length xs) then [] else findIdx xs n : inv xs (n+1)
        findIdx [] _ = -1
        findIdx (x:xs) n = if n == x then 0 else 1 + findIdx xs n

-- AUX transp2 --
makeTransp2 :: [Integer] -> F.Exp -> F.Exp
makeTransp2 xs y = Rearrange (invIndexing xs) y

---------------------------
-- GENERAL AUX FUNCTIONS --
---------------------------

-- make Futhark basic type from string representation --
readBType :: String -> F.Type
readBType tp = case tp of
  "int"  -> F.IntT
  "f32"  -> F.F32T
  "f64"  -> F.F64T
  "bool" -> F.BoolT
  "char" -> F.IntT
  _      -> error $ "readBType: unhandled " ++ show tp

-- make Futhark type from string representation --
-- i.e., takes 2int and gives [[int]] --
getType :: String -> Maybe F.Type
getType s
  | suffix `elem` ["int","f32","f64","bool","char"] = fmap (makeArrTp (readBType suffix)) r
  | otherwise = Nothing
  where (prefix,suffix) = span isDigit s
        r | [] <- prefix = Nothing
          | otherwise = Just (read prefix :: Integer)

-- return zero expression of basic type --
zero :: (Integer, F.Ident) -> F.Type -> CompilerM F.Exp
zero _ F.IntT = return $ Constant (Int 0)
zero _ F.F32T = return $ Constant (F32 0)
zero _ F.F64T = return $ Constant (F64 0)
zero _ F.BoolT = return $ Constant (Bool False)
zero (i,x) at@(F.ArrayT t _) = do
  zero' <- zero (i+1,x) t
  dim_fn <- genFun $ DimFun $ foldl ArrayT at $
            genericReplicate i AnyDim
  return $ F.FunCall (F.Var "replicate")
    [FunCall (F.Var dim_fn) [F.Constant (F.Int i), F.Var x], zero']
zero _ tp = fail $ "take for type " ++ pretty tp ++ " not supported"

-- make Futhark function expression from ident
makeKernel :: String -> F.Exp
makeKernel ident
  | Just fun <- convertFun ident = F.Fun fun []
  | Just op  <- convertBinOp ident = F.Op op
  | otherwise = error $ "not supported operation " ++ ident

-- make Futhark basic type from Tail basic type --
makeBTp :: BType -> CompilerM F.Type
makeBTp T.IntT = return F.IntT
makeBTp T.DoubleT = asks floatType
makeBTp T.BoolT = return F.BoolT
makeBTp T.CharT = return F.IntT
makeBTp T.ComplexT = do t <- makeBTp T.DoubleT
                        return $ F.TupleT [t,t]
makeBTp (T.Btyv _) = return F.IntT   -- just pick an arbitrary type

-- make Futhark array type from Futhark basic type --
mkType :: (BType, Integer) -> CompilerM F.Type
mkType (tp, r) = makeArrTp <$> makeBTp tp <*> pure r

-- aux for mkType --
makeArrTp :: F.Type -> Integer -> F.Type
makeArrTp btp 0 = btp
makeArrTp btp n = F.ArrayT (makeArrTp btp (n-1)) F.AnyDim

-- make curried Futhark function that have 1 as basic element and folds with times
multExp :: [F.Exp] -> F.Exp
multExp = foldr (BinApp Mult) (Constant (Int 1))

-- make Futhark kernel expression with type
compileKernel :: T.Exp -> F.Type -> CompilerM F.Exp
compileKernel (T.Var ident) _ =
  return $ makeKernel ident
compileKernel (T.Fn ident tp (T.Fn ident2 tp2 e)) rtp = do
  tp' <- compileTp tp
  tp2' <- compileTp tp2
  F.Fn rtp [(tp',"t_" ++ ident),(tp2',"t_" ++ ident2)] <$> compileExp e
compileKernel (T.Fn ident tp e) rtp = do
  tp' <- compileTp tp
  F.Fn rtp [(tp',"t_" ++ ident)] <$> compileExp e
compileKernel e t = throwError $ unwords ["compileKernel, invalid args:", show e, show t]

-- AUX for compileKernel --
compileTp :: T.Type -> CompilerM F.Type
compileTp (ArrT bt (R r)) = makeArrTp <$> makeBTp bt <*> pure r
compileTp (VecT bt (R _)) = makeArrTp <$> makeBTp bt <*> pure 1
compileTp (TupT ts)       = F.TupleT <$> mapM compileTp ts
compileTp (SV bt (R _))   = makeArrTp <$> makeBTp bt <*> pure 1
compileTp (S bt _)        = makeBTp bt

-----------------------
-- LIBRARY FUNCTIONS --
-----------------------

f32Builtins, f64Builtins :: [F.FunDecl]
(f32Builtins, f64Builtins) = (funs F.F32T (F.Var . ("f32."++)) (F.Var . (++"32")) tof32,
                              funs F.F64T (F.Var . ("f64."++)) (F.Var . (++"64")) tof64)
  where
    tof32, tof64 :: Double -> F.Exp
    tof32 = Constant . F32 . fromRational . toRational
    tof64 = Constant . F64

    funs t pref suff constant =
      [i2dt, sqrtf, ln, absd, negd, maxd, mind, expd, signd, ceil, floorf,
       sinf, cosf, tanf, atan2f,
       d2x, addx, mulx, injx, subx, negx, conjx, expx,
       rex, imx, magnx]
      where
        complex = TupleT [t, t]
        x = F.Var "x"
        y = F.Var "y"
        i2dt = F.FunDecl False t "i2d" [] [(F.IntT, "x")] $ F.FunCall (suff "r") [x]
        sqrtf = F.FunDecl False t "sqrt" [] [(t, "x")] $ F.FunCall (pref "sqrt") [x]
        ln = F.FunDecl False t "ln" [] [(t, "x")] $ F.FunCall (pref "log") [x]
        sinf = F.FunDecl False t "sin" [] [(t, "x")] $ F.FunCall (pref "sin") [x]
        cosf = F.FunDecl False t "cos" [] [(t, "x")] $ F.FunCall (pref "cos") [x]
        tanf = F.FunDecl False t "tan" [] [(t, "x")] $ F.BinApp F.Div (F.FunCall (pref "sin") [x]) (F.FunCall (pref "cos") [x])
        atan2f = F.FunDecl False t "atan2" [] [(t, "x"), (t, "y")] $ F.FunCall (pref "atan2") [x,y]
        absd = F.FunDecl False t "absd" [] [(t,"x")] $
          IfThenElse (BinApp LessEq x (constant 0)) (F.Neg x) x
        negd = F.FunDecl False t "negd" [] [(t,"x")] $ F.Neg x
        maxd = F.FunDecl False t "maxd" [] [(t, "x"), (t, "y")] $ maxExp x y
        mind = F.FunDecl False t "mind" [] [(t, "x"), (t, "y")] $ minExp x y
        expd = F.FunDecl False t "expd" [] [(t, "x")] $ F.FunCall (pref "exp") [x]
        signd = F.FunDecl False F.IntT "signd" [] [(t, "x")] $
          IfThenElse (BinApp Less (constant 0) x)
          (Constant (Int 1)) $
          IfThenElse (BinApp Eq (constant 0) x)
          (Constant (Int 0)) (Constant (Int (-1)))
        ceil = F.FunDecl False F.IntT "ceil" [] [(t, "x")] $
          IfThenElse (F.BinApp F.Eq (F.FunCall (F.Var "i2d") [F.FunCall (suff "t") [x]]) x)
          (F.FunCall (suff "t") [x]) $
          IfThenElse (F.BinApp F.LessEq (F.Var "x") (constant 0))
          (F.FunCall (suff "t") [x])
          (F.FunCall (suff "t") [F.BinApp Plus x (constant 1)])
        floorf = F.FunDecl False F.IntT "floor" [] [(t, "x")] $
          IfThenElse (F.BinApp F.Less (F.Var "x") (constant 0))
          (F.FunCall (suff "t") [F.BinApp Minus x (constant 1)])
          (F.FunCall (suff "t") [x])
        d2x = F.FunDecl False complex "d2x" [] [(t, "x")] $
          F.Tuple [x, constant 0]
        injx = F.FunDecl False complex "injx" [] [(t, "x")] $
          F.Tuple [constant 0, x]
        addx = F.FunDecl False complex "addx" [] [(complex, "x"), (complex, "y")] $
          F.Tuple [BinApp Plus (Project "1" x) (Project "1" y),
                   BinApp Plus (Project "2" x) (Project "2" y)]
        subx = F.FunDecl False complex "subx" [] [(complex, "x"), (complex, "y")] $
          F.Tuple [BinApp Minus (Project "1" x) (Project "1" y),
                   BinApp Minus (Project "2" x) (Project "2" y)]
        mulx = F.FunDecl False complex "mulx" [] [(complex, "x"), (complex, "y")] $
          let a = Project "1" x
              b = Project "2" x
              c = Project "1" y
              d = Project "2" y
          in F.Tuple [BinApp Minus (BinApp Mult a c) (BinApp Mult b d),
                      BinApp Plus (BinApp Mult a d) (BinApp Mult b c)]
        negx = F.FunDecl False complex "negx" [] [(complex, "x")] $
          F.Tuple [F.Neg (Project "1" x),
                   F.Neg (Project "2" x)]
        conjx = F.FunDecl False complex "conjx" [] [(complex, "x")] $
          F.Tuple [Project "1" x,
                   F.Neg (Project "2" x)]
        expx = F.FunDecl False complex "expx" [] [(complex, "x")] $
          FunCall (F.Var "mulx")
          [F.Tuple [constant 0,
                    FunCall (F.Var "expd") [Project "1" x]],
           F.Tuple [FunCall (F.Var "cos") [Project "2" x],
                    FunCall (F.Var "sin") [Project "2" x]]]
        rex = F.FunDecl False t "rex" [] [(complex, "x")] $
          Project "1" x
        imx = F.FunDecl False t "imx" [] [(complex, "x")] $
          Project "2" x
        magnx = F.FunDecl False t "magnx" [] [(complex, "x")] $
          let a = Project "1" x
              b = Project "2" x
          in FunCall (F.Var "sqrt") [BinApp F.Plus (BinApp F.Mult a a) (BinApp F.Mult b b)]

-- AUX: make FunDecl by combining signature and body (aux function that create function body)
makeFun :: [F.TypeSizeParam] -> [F.Arg] -> F.Ident -> F.Exp -> F.Type -> FunDecl
makeFun tsparams args name body tp = F.FunDecl False (ArrayT tp AnyDim) (name ++ "_" ++ annot tp) tsparams args body
  where annot (TupleT ts)  = intercalate "_" $ map annot ts
        annot (ArrayT t _) = "arr" ++ annot t
        annot t            = pretty t

stdArgs :: F.Type -> ([TypeSizeParam], [(F.Type, String)])
stdArgs tp = (map SizeParam sizes, [(F.IntT,"l"),(ArrayT arr AnyDim, "x")])
  where sizes = map (("d"++) . show) [(0::Int)..rank tp-1]
        arr = foldr (\d t -> ArrayT t d) (baseType tp) $
              map NamedDim sizes

-- Generic library functions
data GenFun = TakeFun F.Type
            | DropFun F.Type
            | ReshapeFun F.Type
            | DimFun F.Type
            deriving (Eq, Ord, Show)

generateGenFun :: GenFun -> CompilerM FunDecl
generateGenFun (TakeFun t) = do
  body <- takeBody <$> zero (1, "x") t
  return $ uncurry makeFun (stdArgs t) "take" body t
generateGenFun (DropFun t) = do
  dim_fn <- genFun $ DimFun $ ArrayT t AnyDim
  let dims = [ FunCall (F.Var dim_fn) [Constant $ Int i, F.Var "x"] | i <- [0..F.rank t] ]
  return $ uncurry makeFun (stdArgs t) "drop" (dropBody dims t) t
generateGenFun (ReshapeFun t) =
  return $ uncurry makeFun (stdArgs t) "reshape" (reshape1Body t) t
generateGenFun (DimFun t) =
  return $
  F.FunDecl False F.IntT ("dim_" ++ show (F.rank t::Int))
  (TypeParam "t":size_params) value_params body
  where dim_names = map (\i -> "d" ++ show (i::Int)) [0..F.rank t-1]
        size_params = map SizeParam dim_names
        arr_t = foldl ArrayT (VarT "t") $ reverse $ map NamedDim dim_names
        value_params = [(F.IntT,"i"), (arr_t, "x")]
        body = foldl branch (F.Var "d0") $
               reverse $ drop 1 $ zip [0..] dim_names
        branch else_branch (i,d) =
          F.IfThenElse (F.BinApp Eq (F.Var "i") (Constant $ Int i)) (F.Var d) else_branch

genFun :: GenFun -> CompilerM F.Ident
genFun gf = do
  ff@(FunDecl False _ name _ _ _) <- generateGenFun gf
  tell $ M.singleton name ff
  return name

-----------------
-- EXPRESSIONS --
-----------------

-- general expressions --
compileExp :: T.Exp -> CompilerM F.Exp
compileExp (T.Var ident) = return $ F.Var $ "t_" ++ ident
compileExp (I x) = return $ Constant (Int x)
compileExp (D d) = do
  float <- asks floatType
  case float of
    F.F32T ->
      return $ Constant (F32 $ fromRational $ toRational d)
    _ ->
      return $ Constant (F64 d)
compileExp (X a b) = do
  a' <- compileExp $ D a
  b' <- compileExp $ D b
  return $ Tuple [a',b']
compileExp (C char)   = return $ Constant (Char char)
compileExp (B bool)   = return $ Constant (Bool bool)
compileExp Inf = do
  float <- asks floatType
  case float of
    F.F32T -> return $ F.Var "f32.inf"
    _      -> return $ F.Var "f64.inf"
compileExp (T.Neg e) = F.Neg <$> compileExp e
compileExp (T.Let v _ e1 e2) =
  F.Let (Ident ("t_" ++ v)) <$>
  compileExp e1 <*> compileExp e2
compileExp (T.Prj _ i e) = compilePrj i e
compileExp (T.Op "tuple" Nothing args) = F.Tuple <$> mapM compileExp args
compileExp (T.Op ident instDecl args) = compileOpExp ident instDecl args
compileExp (Ts es) = F.Tuple <$> mapM compileExp es
compileExp T.Fn{} = throwError "Fn not supported"
compileExp (Vc exps) = Array <$> mapM compileExp exps

-- operators --
compileOpExp :: String -> Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileOpExp ident instDecl args = case ident of
  "pi" -> compilePi instDecl args
  "rav" -> compileRav instDecl args
  "zilde" -> compileZilde instDecl args
  "reduce" -> compileReduce instDecl args
  "compress" -> compileCompress instDecl args
  "scan" -> compileScan instDecl args
  "eachV"  -> compileEachV instDecl args
  "each"   -> compileEach instDecl args
  "power"  -> compilePower instDecl args
  "powerScl" -> compilePower instDecl args
  "firstV" -> compileFirstV instDecl args
  "first" -> compileFirst instDecl args
  "shapeV" -> compileShape instDecl args
  "shape"  -> compileShape instDecl args
  "reshape" -> compileReshape instDecl args
  "take" -> compileTake instDecl args
  "takeV" -> compileTakeV instDecl args
  "zipWith" -> compileZipWith instDecl args
  "cat" -> compileCat instDecl args
  "gradeUp" -> compileGradeUp instDecl args
  "gradeDown" -> compileGradeDown instDecl args
  "replicate" -> compileReplicate instDecl args  
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
  "b2iV" | [T.Var "tt"] <- args -> return $ Constant (Int 1)
         | [T.Var "ff"] <- args -> return $ Constant (Int 0)
  "idxS" | [T.I 1, i, arr] <- args ->
           F.Index <$>
           compileExp arr <*>
           sequence [F.BinApp F.Minus <$> compileExp i <*> compileExp (I 1) ]
         | [T.I d, i, arr] <- args -> compileIdxS instDecl d i arr
  "bench" -> compileBench instDecl args

  _
    | [e1,e2]  <- args
    , Just op  <- convertBinOp ident ->
      F.BinApp op <$> compileExp e1 <*> compileExp e2
    | Just fun <- convertFun ident ->
      F.FunCall (F.Var fun) <$> mapM compileExp args
    | ident `elem` idFuns ->
      F.FunCall (F.Var ident) <$> mapM compileExp args
    | otherwise       -> throwError $ ident ++ " not supported"

-- snocV --
compileSnocV :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileSnocV (Just([_],[_])) [a,e] = do
  a' <- compileExp a
  e' <- compileExp e
  return $ F.FunCall (F.Var "concat") [a', F.Array [e']]
compileSnocV Nothing _ = throwError "snocV needs instance declaration"
compileSnocV _ _ = throwError "snocV take two aguments"

-- snoc --
compileSnoc :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileSnoc (Just([_],[r])) [a,e] = compileSnoc' <$> compileExp a <*> compileExp e
  where compileSnoc' a' e' =
          F.FunCall (F.Var concatf) [a',e'']
          where e'' = makeTransp (r+1) $ F.Array [e']
        concatf = "concat@" ++ show r
compileSnoc _ _ = throwError "compileSnoc: invalid arguments"

-- consV --
compileConsV :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileConsV (Just([_],[_])) [e,a] =
  compileConsV' <$> compileExp e <*> compileExp a
  where compileConsV' e' a' = F.FunCall (F.Var "concat") [F.Array [e'], a']
compileConsV Nothing _ = throwError "consV needs instance declaration"
compileConsV _ _ = throwError "consV take two aguments"


-- cons --
compileCons :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileCons (Just([_],[r])) [e,a] = compileCons' <$> compileExp e <*> compileExp a
  where compileCons' e' a' =
          F.FunCall (F.Var concatf) [e'',a']
            where e'' = makeTransp (r+1) $ F.Array [e']
        concatf = "concat@" ++ show r
compileCons _ _ = throwError "compileCons: invalid arguments"

-- first --
compileFirst :: Maybe (t, [Integer]) -> [T.Exp] -> CompilerM F.Exp
compileFirst (Just(_,[r])) [a] = compileFirst' <$> compileExp a
  where compileFirst' a' =
          F.Let (Ident "x") a' $
          F.Index (F.Var "x") (replicate rInt (F.Constant (F.Int 0)))
        rInt = fromInteger r :: Int
compileFirst Nothing _ = throwError "first needs instance declaration"
compileFirst _ _ = throwError "first take one argument"

-- iota --
compileIota :: t -> [T.Exp] -> CompilerM F.Exp
compileIota _ [a] = compileIota' <$> compileExp a
  where compileIota' a' =
          Map (F.Fn F.IntT [(F.IntT, "x")] (F.BinApp Plus (F.Var "x") (Constant (F.Int 1))))
          [FunCall (F.Var "iota") [a']]
compileIota _ _ = throwError "Iota take one argument"

compileGradeUp :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileGradeUp (Just([tp],[])) [a] =
  if tp == T.IntT || tp == T.CharT then
    do a' <- compileExp a
       return $ FunCall (F.Var "grade_up") [a']
  else throwError "compileGradeUp: expecting sortable type instance"
compileGradeUp _ _ = throwError "compileGradeUp: expecting singleton type instance list and one argument"

compileGradeDown :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileGradeDown (Just([tp],[])) [a] =
  if tp == T.IntT || tp == T.CharT then
    do a' <- compileExp a
       return $ FunCall (F.Var "grade_down") [a']
  else throwError "compileGradeUp: expecting sortable type instance"
compileGradeDown _ _ = throwError "compileGradeDown: expecting singleton type instance list and one argument"

-- replicate --
compileReplicate' :: BType -> Integer -> F.Exp -> F.Exp -> F.Exp -> CompilerM F.Exp
compileReplicate' _ 0 _ reps a = return $ F.FunCall (F.Var "replicate") [reps,a]
compileReplicate' tp 1 def reps a =
   do t' <- mkType(tp,0)
      return $ F.Let (Ident "vals2") (Map (F.Fn t' [(F.IntT,"r"),(t',"v")] (F.IfThenElse (F.BinApp F.Less (F.Var "r") zer) def (F.Var "v"))) [reps,a])
             $ F.Let (Ident "reps2") (F.Map (absfn t') [reps])
             $ F.Let (Ident "idxs") (F.FunCall (F.Var "replIdx") [F.Var "reps2"])
             $ Map (F.Fn t' [(F.IntT,"i")] (F.Unsafe(F.Index(F.Var"vals2")[F.Var"i"]))) [F.Var "idxs"]
      where zer = F.Constant (Int 0)
            absfn t' = F.Fn t' [(t',"i")] (F.IfThenElse (F.BinApp F.Less (F.Var "i") zer) (F.Neg (F.Var "i")) (F.Var "i"))
compileReplicate' _ _ _ _ _ = throwError "compileReplicate': multi-dimensional replicate not supported"

compileReplicate :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileReplicate (Just([tp],[r])) [def,reps,a] =
           do def' <- compileExp def
              reps' <- compileExp reps
              a' <- compileExp a
              compileReplicate' tp r def' reps' a'
compileReplicate _ _ = throwError "compileReplicate: expects two singleton instance lists and three arguments"

-- vreverse --
compileVReverse :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileVReverse (Just([tp],[r])) [a] = makeVReverse tp r =<< compileExp a
compileVReverse _ _ = throwError "compileVReverse: invalid arguments"

compileReverse :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileReverse (Just([tp],[r])) [a] =
  makeTransp r <$> (makeVReverse tp r =<< (makeTransp r <$> compileExp a))
compileReverse _ _ = throwError "compileReverse: invalid arguments"

compileVReverseV :: Maybe ([BType], [t]) -> [T.Exp] -> CompilerM F.Exp
compileVReverseV (Just([tp],[_])) [a] = makeVReverse tp 1 =<< compileExp a
compileVReverseV _ _ = throwError "compileVReverseC: invalid arguments"

makeVReverse :: BType -> Integer -> F.Exp -> CompilerM F.Exp
makeVReverse tp r a = F.Let (Ident "a") a <$>
  (Map <$> kernelExp <*> pure [FunCall (F.Var "iota") [FunCall (F.Var "length") [a]]])
  where
    kernelExp = F.Fn <$>
      mkType (tp,r-1) <*>
      pure [(F.IntT,"x")] <*>
      pure (F.Index (F.Var "a") [F.BinApp F.Minus minusIndex ione])
    sizeCall = F.FunCall (F.Var "length") [a]
    minusIndex = F.BinApp F.Minus sizeCall (F.Var "x")
    ione = F.Constant (F.Int 1)

-- rotate --
compileVRotate :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileVRotate (Just([tp],[r])) [i,a] = makeVRotate tp r i =<< compileExp a
compileVRotate Nothing _ = throwError "Need instance declaration for vrotate"
compileVRotate _ _ = throwError "vrotate needs 2 arguments"

compileRotate :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileRotate (Just([tp],[r])) [i,a] =
  makeRotate tp r i =<< compileExp a
compileRotate Nothing _ = throwError "Need instance declaration for rotate"
compileRotate _ _ = throwError "rotate needs 2 arguments"

-- vrotateV --
compileVRotateV :: Maybe ([BType], [t]) -> [T.Exp] -> CompilerM F.Exp
compileVRotateV (Just([tp],[_])) [i,a] = makeVRotate tp 1 i =<< compileExp a
compileVRotateV Nothing _ = throwError "Need instance declaration for vrotateV"
compileVRotateV _ _ = throwError "vrotateV needs 2 arguments"

-- vrotate --
makeVRotate :: BType -> Integer -> T.Exp -> F.Exp -> CompilerM F.Exp
makeVRotate t _ = makeRotate t 1

makeRotate :: BType -> Integer -> T.Exp -> F.Exp -> CompilerM F.Exp
makeRotate _ r i a = do
  i' <- compileExp i
  return $ F.FunCall (F.Var rotatef) [i', a]
  where rotatef = "rotate@" ++ show (r-1)

-- cat --
compileCat :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileCat (Just([_],[r])) [a1,a2] = do
  a1' <- compileExp a1
  a2' <- compileExp a2
  return $ FunCall (F.Var concatf) [a1', a2']
  where concatf = "concat@" ++ show (r-1)
compileCat _ _ = throwError "compileCat: invalid arguments"

-- takeV --
compileTakeV :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileTakeV (Just([tp],_)) [len,e] = do
  tp' <- mkType (tp, 0)
  fname <- genFun $ TakeFun tp'
  F.FunCall (F.Var fname) <$> sequence [compileExp len, compileExp e]
compileTakeV Nothing _ = throwError "Need instance declaration for takeV"
compileTakeV _ _ = throwError "TakeV needs 2 arguments"

-- dropV --
compileDropV :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileDropV (Just([tp],_)) [len,e] = do
  tp' <- mkType (tp, 0)
  fname <- genFun $ DropFun tp'
  F.FunCall (F.Var fname) <$> sequence [compileExp len,compileExp e]
compileDropV Nothing _ = throwError "Need instance declaration for dropV"
compileDropV _ _ = throwError "DropV needs 2 arguments"

-- take --
compileTake :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileTake (Just([tp],[r])) [len,e] = do
  tp' <- mkType (tp, r-1)
  fname <- genFun $ TakeFun tp'
  takeDropHelper (compileTake' fname) len e
  where compileTake' fname len' e' =
          pure $ F.FunCall (F.Var fname) [len',e']
compileTake Nothing _args = throwError "Need instance declaration for take"
compileTake _ _ = throwError "Take needs 2 arguments"

-- drop --
compileDrop :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileDrop (Just([tp],[r])) [len,e] = do
  tp' <- mkType (tp, r-1)
  fname <- genFun $ DropFun tp'
  takeDropHelper (compileDrop' fname) len e
  where compileDrop' fname len' e' =
          pure $ F.FunCall (F.Var fname) [len',e']
compileDrop _ _ = throwError "compileDrop: invalid arguments"

takeDropHelper :: (F.Exp -> F.Exp -> CompilerM b)
               -> T.Exp -> T.Exp -> CompilerM b
takeDropHelper f len e = do
  len' <- compileExp len
  e' <- compileExp e
  f len' e'

-- reshape --
compileReshape :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileReshape (Just([tp],[r1,r2])) [dims,array] = do
  dimsExp <- compileExp dims
  fname <- genFun =<< ReshapeFun <$> mkType (tp, 0)
  (dimsList, wrap) <- case dimsExp of
    F.Array l ->
      return (l, id)
    F.Var dimsVar ->
      return (map (\i -> F.Index (F.Var dimsVar) [Constant (Int i)]) [0..r2-1], id)
    _ -> do
      let name = "dim_tmp"
      return ([F.Index (F.Var name) [Constant (Int i)] | i <- [0..r2-1]],
              F.Let (F.Ident name) dimsExp)
  tp_r1 <- mkType (tp,r1)
  array' <- compileExp array
  let resh = fullFlatten (rank tp_r1) array'
  return $ wrap $ fullUnflatten dimsList $ F.FunCall (F.Var fname) [multExp dimsList, resh]
compileReshape Nothing _args = throwError "Need instance declaration for reshape"
compileReshape _ _ = throwError "Reshape needs 2 arguments"

-- transp --
compileTransp :: Maybe (t, [Integer]) -> [T.Exp] -> CompilerM F.Exp
compileTransp (Just(_,[r])) [e] =
  makeTransp2 (reverse [0..r-1]) <$> compileExp e
compileTransp Nothing _args = throwError "Need instance declaration for transp"
compileTransp _ _ = throwError "Transpose takes 1 argument"

-- transp2 --
compileTransp2 :: t -> [T.Exp] -> CompilerM F.Exp
compileTransp2 _ [Vc dims,e] = makeTransp2 perm <$> compileExp e
    where perm = map ((subtract 1) . getInt) dims
          getInt (I i) = i
          getInt _ = error "transp2 expects number literals in it's first argument"
compileTransp2 _ e = case e of [_,_] -> throwError "transp2 needs litaral as first argument"
                               _     -> throwError "transp2 takes 2 arguments"

-- shape --
compileShape :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileShape (Just([tp],[r])) [arg] = do
  fname <- genFun =<< DimFun <$> mkType (tp, r)
  arg' <- compileExp arg
  return $ Array [ F.FunCall (F.Var fname) [Constant $ Int i, arg'] | i <- [0..r-1] ]
compileShape Nothing _args = throwError "Need instance declaration for shape"
compileShape _ _ = throwError "Shape takes 1 argument"

-- firstV --
compileFirstV :: t -> [T.Exp] -> CompilerM F.Exp
compileFirstV _ args
  | [e] <- args =
      F.Let (Ident "x") <$> compileExp e <*>
      pure (F.Index (F.Var "x")[F.Constant (F.Int 0)])
  | otherwise = throwError "firstV takes one argument"

-- eachV --
compileEachV :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileEachV Nothing _ = throwError "Need instance declaration for eachV"
compileEachV (Just ([_intp,outtp],[_len])) [kernel,array] =
  Map <$> (compileKernel kernel =<< makeBTp outtp) <*> mapM compileExp [array]
compileEachV _ _ = throwError "compileEachV: invalid arguments"

-- each --
compileEach :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileEach (Just ([intp,outtp],[orig_r])) [okernel,orig_array] = do
  array <- compileExp orig_array
  F.Let (Ident "array") array <$>
    makeEach intp outtp orig_r okernel (F.Var "array")
  where makeEach _ tp2 1 kernel array =
          Map <$> (compileKernel kernel =<< makeBTp tp2) <*> pure [array]
        makeEach tp1 tp2 r kernel array = do
          rtp  <- mkType (tp2,r-1)
          tp1' <- mkType (tp1,r-1)
          body <- makeEach tp1 tp2 (r-1) kernel (F.Var "x")
          return $ Map (F.Fn rtp [(tp1',"x")] body) [array]
compileEach Nothing _ = throwError "Need instance declaration for each"
compileEach _ _ = throwError "each takes two arguments"

-- power --
compilePower :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compilePower (Just (ts,rs)) [kernel,num,arr] = do
  ts' <- zipWithM (curry mkType) ts rs
  fn <- compileKernel kernel $ F.TupleT ts'
  num' <- compileExp num
  arr' <- compileExp arr
  case fn of
    F.Fn _ [(_,var)] body ->
      return $ F.ForLoop var arr' "i" num' body
    F.Fn{} ->
      fail "expecting exactly one argument in function to power-function"
    _  ->
      fail "expecting anonymous function as argument to power-function"
compilePower (Just (_,_)) _ = throwError $ "power takes one type argument"
compilePower Nothing [_,_,_] = throwError "Need instance declaration for power"
compilePower _ _ = throwError "power takes three arguments"

-- zipWith --
compileZipWith :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileZipWith (Just([tp1,tp2,rtp],[rk])) [orig_kernel,orig_a1,orig_a2] = do
  a1 <- compileExp orig_a1
  a2 <- compileExp orig_a2
  F.Let (Ident "x") a1 <$>
    F.Let (Ident "y") a2 <$>
    makeZipWith rk orig_kernel (F.Var "x") (F.Var "y")
  where makeZipWith r kernel a1 a2
          | r == 1 =
            Map <$> (compileKernel kernel =<< makeBTp rtp) <*> pure [a1,a2]
          | otherwise = do
              rtp' <- mkType (rtp,r-1)
              tp1' <- mkType (tp1,r-1)
              tp2' <- mkType (tp2,r-1)
              body <- makeZipWith (r-1) kernel (F.Var "x") (F.Var "y")
              return $ Map (F.Fn rtp' [(tp1',"x"),(tp2',"y")] body) [a1, a2]
    --Map kernelExp $ F.FunCall "zip" [(compileExp a1),(compileExp a2)] -- F.Map kernelExp $ F.FunCall "zip" [a1,a2]
compileZipWith Nothing _ = throwError "Need instance declaration for zipWith"
compileZipWith _ _ = throwError "zipWith takes 3 arguments"

-- zilde --
compileZilde' :: BType -> Integer -> CompilerM F.Exp
compileZilde' t _ = do
  bt <- makeBTp t
  return $ F.Empty bt

compileZilde :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileZilde (Just([t],[r])) [] = compileZilde' t r
compileZilde _ _ = throwError "zilde takes two singleton instance lists and no arguments"

-- idxS --
extractDim :: Integer -> Integer -> [Integer]
extractDim r j = (j-1):elim (j-1) [0..r-1]
  where elim _ [] = []
        elim i (x:xs) = if x==i then xs else x:elim i xs

compileIdxS' :: Integer -> Integer -> T.Exp -> T.Exp -> CompilerM F.Exp
compileIdxS' r d i arr = do
  arr1 <- compileExp arr
  idx <- compileExp i
  return $ F.Index (Rearrange (extractDim r d) arr1) [F.BinApp F.Minus idx (F.Constant (F.Int 1))]

compileIdxS :: Maybe InstDecl -> Integer -> T.Exp -> T.Exp -> CompilerM F.Exp
compileIdxS (Just([_],[r])) d i a = compileIdxS' (r+1) d i a
compileIdxS _ _ _ _ = throwError "idxS takes two singleton instance lists and three arguments"

-- rav --
compileRav' :: F.Type -> T.Exp -> CompilerM F.Exp
compileRav' t xs = fullFlatten (rank t) <$> compileExp xs

compileRav :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileRav (Just([tp],[r])) [xs] = do
  tp' <- mkType (tp,r)
  compileRav' tp' xs
compileRav _ _ = throwError "rav takes two singleton instance lists and one argument"

-- pi --
compilePi :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compilePi Nothing [] = compileExp $ D 3.14159265359
compilePi _ _ = throwError "pi takes no instance lists and no arguments"

-- compress --
compileCompress' :: BType -> T.Exp -> T.Exp -> CompilerM F.Exp
compileCompress' bt bs xs = do
  bs' <- compileExp bs
  xs' <- compileExp xs
  t <- mkType (bt,0)
  return $ F.Let (Ident "zs") (Map (zipWithFn t) [bs',xs']) $
           F.Let (Ident "rs") (Filter (filterFn t) (F.Var "zs")) $
           Map (mapFn t) [F.Var "rs"]
  where zipWithFn t = F.Fn (pt t) [(F.BoolT,"b"),(t,"x")] (F.Tuple[F.Var "b",F.Var "x"])
        filterFn t = F.Fn F.BoolT [(pt t,"p")] (F.Let (TouplePat [Ident "b",Ident "_"]) (F.Var "p") (F.Var "b"))
        mapFn t = F.Fn t [(pt t,"p")] (F.Let (TouplePat [Ident "_",Ident "x"]) (F.Var "p") (F.Var "x"))
        pt t = F.TupleT[F.BoolT,t]

compileCompress :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileCompress (Just([t],[_])) [bs,xs] = compileCompress' t bs xs
compileCompress Nothing _ = throwError "compress needs instance declaration"
compileCompress _ _ = throwError "compress takes two arguments"

-- reduce --
compileReduce :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileReduce Nothing _ = throwError "Need instance declaration for reduce"
compileReduce (Just ([orig_tp],[orig_rank]))[orig_kernel,v,array] = do
  kernel <- compileKernel orig_kernel =<< makeBTp orig_tp
  v' <- compileExp v
  array' <- compileExp array
  makeReduce orig_tp orig_rank kernel v' array'
  where makeReduce tp r kernel idExp arrayExp
          | r == 0 = return $ Reduce kernel idExp arrayExp
          | otherwise = do
              rt <- mkType (tp,r-1)
              t <- mkType (tp,r)
              body <- makeReduce tp (r-1) kernel idExp (F.Var "x")
              return $ Map (F.Fn rt [(t,"x")] body) [arrayExp]
compileReduce _ _ = throwError "reduce needs 3 arguments"

-- scan --
compileScan :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileScan Nothing _ = throwError "Need instance declaration for scan"
compileScan (Just ([orig_tp],[orig_rank]))[orig_kernel,v,array] = do
  kernel <- compileKernel orig_kernel =<< makeBTp orig_tp
  v' <- compileExp v
  array' <- compileExp array
  makeScan orig_tp orig_rank kernel v' array'
  where makeScan tp r kernel idExp arrayExp
          | r == 0 = return $ arrayExp
          | r == 1 = return $ Scan kernel idExp arrayExp
          | otherwise = do
              rt <- mkType (tp,r-1)
              -- t <- mkType (tp,r)
              body <- makeScan tp (r-1) kernel idExp (F.Var "x")
              return $ Map (F.Fn rt [(rt,"x")] body) [arrayExp]
compileScan _ _ = throwError "scan needs 3 arguments"

-- Prj ---
compilePrj :: Integer -> T.Exp -> CompilerM F.Exp
compilePrj i tup =
  -- TAIL tuples are 0-indexed and Futhark tuples are 1-indexed.
  F.Project (show (i+1)) <$> compileExp tup

-- bench --
--
-- The bench function is semantically identity.  Since Futhark
-- contains its own benchmarking infrastructure, we compile it away.
compileBench :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileBench _ [(T.Fn ident _tp e), _nruns, arg] =
  F.Let (Ident ident) <$> compileExp arg <*> compileExp e
compileBench _ _ = throwError "compileBench: invalid arguments"


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
          "maxi",
          "maxd",
          "eqb",
          "xorb",
          "nandb",
          "norb",
          "neqi",
          "neqd",
          "resi",

          -- Complex number functions
         "addx",
         "mulx",
         "d2x",
         "rex",
         "imx",
         "injx",
         "subx",
         "negx",
         "conjx",
         "expx",
         "magnx"]

-- operators that are 1:1 with Futhark functions or prefix operators --
convertFun :: String -> Maybe String
convertFun fun = case fun of
  "i2d"    -> Just "i2d"
  "catV"   -> Just "concat"
  "b2i"    -> Just "i32.bool"
  "b2iV"   -> Just "i32.bool"
  "ln"     -> Just "ln"
  "ceil"   -> Just "ceil"
  "expd"   -> Just "expd"
  "sin"    -> Just "sin"
  "cos"    -> Just "cos"
  "tan"    -> Just "tan"
  "atan2"  -> Just "atan2"
  "notb"   -> Just "!"
  "floor"  -> Just "floor"
  "mem"    -> Just "copy"
  "signi"  -> Just "i32.sgn"
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
  "eqc"  -> Just F.Eq
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
