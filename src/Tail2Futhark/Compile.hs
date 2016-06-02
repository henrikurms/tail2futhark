{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tail2Futhark.Compile (compile, getType) where

import Tail2Futhark.TAIL.AST as T -- the TAIL AST
import Tail2Futhark.Futhark.AST as F -- the futhark AST
import Tail2Futhark.Futhark.Pretty as F -- the futhark AST

import Prelude

import Control.Monad.Reader
import Control.Monad.Except
import Data.Char
import Options (Options(..))

data Env = Env { floatType :: F.Type }

newEnv :: Env
newEnv = Env F.F64T

newtype CompilerM a = CompilerM (ReaderT Env (Except String) a)
                    deriving (Applicative, Functor, Monad,
                              MonadReader Env, MonadError String)

runCompilerM :: CompilerM a -> Env -> Either String a
runCompilerM (CompilerM m) = runExcept . runReaderT m

--------------------------
-- THE MAIN FUNCTION --
--------------------------

compile :: Options -> T.Program -> F.Program
compile opts prog =
  case runCompilerM (compileExp rootExp) env of
    Left e -> error e
    Right mainbody ->
      F.Program $ includes ++ [F.FunDecl ret "main" signature $ maybeUnsafe mainbody]
  where includes = if includeLibs opts then builtins ++ fbuiltins else []
        fbuiltins = if floatAsSingle opts then f32Builtins else f64Builtins
        (signature, ret, rootExp) = inputsAndOutputs float prog
        float = if floatAsSingle opts then F.F32T else F.F64T
        env = newEnv { floatType = float }
        maybeUnsafe
          | unsafe opts = Unsafe
          | otherwise   = id

-------------------------
-- HELPER FUNCTIONS --
-------------------------

inputsAndOutputs :: F.Type -> T.Exp -> ([(F.Type, String)], F.Type, T.Exp)
inputsAndOutputs float = inputsAndOutputs' []
  where inputsAndOutputs' outs (T.Let v  _ (T.Op "readIntVecFile" _ _) e2) =
          ((F.ArrayT F.IntT , "t_" ++ v):sig, ret, e')
          where (sig, ret, e') = inputsAndOutputs' outs e2

        inputsAndOutputs' outs (T.Let v  _ (T.Op "readDoubleVecFile" _ _) e2) =
          ((F.ArrayT float , "t_" ++ v):sig, ret, e')
          where (sig, ret, e') = inputsAndOutputs' outs e2

        inputsAndOutputs' outs (T.Let _ (T.ArrT _ (R r)) (T.Op "prArrC" _ [e]) e2) =
          inputsAndOutputs' outs' e2
          where outs' = (e, foldr (const F.ArrayT) F.IntT [0..r-1]) : outs

        inputsAndOutputs' outs (T.Let _ (T.ArrT _ (R r)) (T.Op "prSclI" _ [e]) e2) =
          inputsAndOutputs' outs' e2
          where outs' = (e, foldr (const F.ArrayT) F.IntT [0..r-1]) : outs


        inputsAndOutputs' outs (T.Let v  t e body) =
          (sig, ret, T.Let v t e body')
          where (sig, ret, body') = inputsAndOutputs' outs body

        inputsAndOutputs' [(e,t)] _ =
          ([], t, e)

        inputsAndOutputs' outs@(_:_) _ =
          ([], F.TupleT ret, T.Op "tuple" Nothing $ reverse es)
          where (es, ret) = unzip outs

        inputsAndOutputs' [] e =
          ([], float, e)

----------------------------------------
-- AUX FUNCTIONS OF LIBRARY FUNCTIONS --
----------------------------------------

absExp :: F.Exp -> F.Exp
absExp e = IfThenElse (BinApp LessEq e (Constant (Int 0))) (F.Neg e) e

maxExp :: F.Exp -> F.Exp -> F.Exp
maxExp e1 e2 = IfThenElse (BinApp LessEq e1 e2) e2 e1

minExp :: F.Exp -> F.Exp -> F.Exp
minExp e1 e2 = IfThenElse (BinApp LessEq e1 e2) e1 e2

signiExp :: F.Exp -> F.Exp

signiExp e = IfThenElse (BinApp Less (Constant (Int 0)) e) (Constant (Int 1)) elseBranch
  where elseBranch = IfThenElse (BinApp Eq (Constant (Int 0)) e) (Constant (Int 0)) (Constant (Int (-1)))

nandExp :: F.Exp -> F.Exp -> F.Exp
nandExp e1 e2 = F.FunCall "!" [BinApp F.LogicAnd e1 e2] 

norExp :: F.Exp -> F.Exp -> F.Exp
norExp e1 e2 = F.FunCall "!" [BinApp F.LogicOr e1 e2]

resiExp :: F.Exp -> F.Exp -> F.Exp
resiExp y x = F.IfThenElse (y `eq` izero) x $ F.IfThenElse cond (x % y) (x % y `plus` y)
  where cond = ((x % y) `eq` izero) `lor` ((x `gr` izero) `land` (y `gr` izero)) `lor` ((x `less` izero) `land` (y `less` izero))
        infix 1 %; (%) = F.BinApp F.Mod
        izero = Constant (Int 0)
        plus = F.BinApp F.Plus
        gr = F.BinApp F.Greater
        less = F.BinApp F.Less
        eq = F.BinApp F.Eq
        lor = F.BinApp F.LogicOr
        land = F.BinApp F.LogicAnd

-- reshape1 --
-- create split part of reshape1 function -- 
mkSplit :: F.Ident -> F.Ident -> F.Exp -> F.Exp -> F.Exp -> F.Exp
mkSplit id1 id2 dims e =
  F.Let (TouplePat [Ident id1,Ident id2]) (F.FunCall2 "split" [dims] e)

makeLets :: [(F.Ident, F.Exp)] -> F.Exp -> F.Exp
makeLets ((v,e) : rest) body = F.Let (Ident v) e (makeLets rest body)
makeLets [] body = body

reshape1Body :: F.Type -> F.Exp
reshape1Body _ = makeLets (zip ["roundUp","extend"] [desired,reshapeCall]) split 
  where split = mkSplit "v1" "_" (F.Var "l") (F.Var "extend") (F.Var "v1")
        desired = F.Var "l" `fplus` (size `fminus` Constant (Int 1)) `fdiv` size
        reshapeCall = F.FunCall2 "reshape" [BinApp Mult size len] (F.FunCall "replicate" [len,F.Var "x"])
        size = F.FunCall "size" [Constant (Int 0),F.Var "x"]
        len = F.Var "roundUp"
        fdiv = BinApp Div
        fplus = BinApp Plus
        fminus = BinApp Minus

-- drop --
-- make body for drop1 function --
dropBody :: F.Type -> F.Exp
dropBody tp = IfThenElse (size `less` absExp len) emptArr elseBranch
    where izero = Constant (Int 0)
          less = BinApp LessEq
          len = F.Var "l"
          size = F.FunCall "size" [izero, F.Var "x"]
          plus = BinApp Plus len size
          emptArr = F.Empty tp
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
          size = F.FunCall "size" [izero, F.Var "x"]
          padRight = F.FunCall "concat" [F.Var "x", padding]
          padLeft = F.FunCall "concat" [padding, F.Var "x"]
          padding = F.FunCall "replicate" [BinApp Minus (F.FunCall "abs" [len]) size, padElement]
          posTake = IfThenElse (len `less` size) (mkSplit "v1" "_" (F.Var "l") (F.Var "x") (F.Var "v1")) padRight
          negTake = IfThenElse (izero `less` plus) (mkSplit "_" "v2" plus (F.Var "x") (F.Var "v2")) padLeft 


------------------------------------------
-- AUX FUNCTIONS FOR SPECIFIC FUNCTIONS --
------------------------------------------

-- AUX shape --
makeShape :: Integer -> [T.Exp] -> CompilerM [F.Exp]
makeShape r args
  | [e] <- args = do
      e' <- compileExp e
      return $ map (\x -> FunCall "size" [Constant (Int x), e']) [0..r-1]
  | otherwise = throwError "shape takes one argument"

-- AUX transp --
makeTransp :: Integer -> F.Exp -> F.Exp
makeTransp r = makeTransp2 (map (Constant . Int) (reverse [0..r-1]))

-- AUX transp2 --
makeTransp2 :: [F.Exp] -> F.Exp -> F.Exp
makeTransp2 = F.FunCall2 "rearrange"

---------------------------
-- GENERAL AUX FUNCTIONS --
---------------------------

-- make Futhark basic type from string representation --
readBType :: String -> F.Type
readBType tp = case tp of
  "int" -> F.IntT
  "f32" -> F.F32T
  "f64" -> F.F64T
  "bool" -> F.BoolT
  "char" -> F.IntT
  _ -> error $ "readBType: unhandled " ++ show tp

-- make Futhark type from string representation --
-- i.e., takes 2int and gives [[int]] --
getType :: String -> Maybe F.Type
getType s 
  | suffix `elem` ["int","f32","f64","bool","char"] = fmap (makeArrTp (readBType suffix)) r
  | otherwise = Nothing
  where (prefix,suffix) = span isDigit s
        r | [] <- prefix = Nothing 
          | otherwise = Just (read prefix :: Integer)

-- make list of Futhark basic types --
btypes :: [F.Type]
btypes = map readBType ["int","f32","f64","bool"]

-- return zero expression of basic type --
zero :: F.Type -> F.Exp
zero F.IntT = Constant (Int 0)
zero F.F32T = Constant (F32 0)
zero F.F64T = Constant (F64 0)
zero F.BoolT = Constant (Bool False)
zero tp = error $ "take for type " ++ pretty tp ++ " not supported"

-- make Futhark function expression from ident
makeKernel :: String -> Kernel
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
makeBTp (T.Btyv v) = throwError $ "makeBTp: cannot transform type variable " ++ v

-- make Futhark array type from Futhark basic type --
mkType :: (BType, Integer) -> CompilerM F.Type
mkType (tp, r) = makeArrTp <$> makeBTp tp <*> pure r

-- aux for mkType --
makeArrTp :: F.Type -> Integer -> F.Type
makeArrTp btp 0 = btp
makeArrTp btp n = F.ArrayT (makeArrTp btp (n-1))

-- make curried Futhark function that have 1 as basic element and folds with times
multExp :: [F.Exp] -> F.Exp
multExp = foldr (BinApp Mult) (Constant (Int 1))

-- make Futhark kernel expression with type
compileKernel :: T.Exp -> F.Type -> CompilerM Kernel
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
compileTp (SV bt (R _)) = makeArrTp <$> makeBTp bt <*> pure 1
compileTp (S bt _) = makeBTp bt

-----------------------
-- LIBRARY FUNCTIONS --
-----------------------

-- list containing ompl of all library functions -- 
builtins :: [F.FunDecl]
builtins = [boolToInt,negi,absi,mini,signi,maxi,eqb,xorb,nandb,norb,neqi,neqd,resi]
        ++ reshapeFuns 
        ++ takeFuns
        ++ dropFuns

f32Builtins, f64Builtins :: [F.FunDecl]
(f32Builtins, f64Builtins) = (funs F.F32T (++"32") tof32,
                              funs F.F64T (++"64") tof64)
  where
    tof32, tof64 :: Double -> F.Exp
    tof32 = Constant . F32 . fromRational . toRational
    tof64 = Constant . F64

    funs t suff constant = [i2dt, sqrtf, ln, absd, negd, maxd, mind, expd, signd, ceil]
      where
        x = F.Var "x"
        y = F.Var "y"
        i2dt = F.FunDecl t "i2d" [(F.IntT, "x")] $ F.FunCall (suff "f") [x]
        sqrtf = F.FunDecl t "sqrt" [(t, "x")] $ F.FunCall (suff "sqrt") [x]
        ln = F.FunDecl t "ln" [(t, "x")] $ F.FunCall (suff "log") [x]
        absd = F.FunDecl t "absd" [(t,"x")] $
          IfThenElse (BinApp LessEq x (constant 0)) (F.Neg x) x
        negd = F.FunDecl t "negd" [(t,"x")] $ F.Neg x
        maxd = F.FunDecl t "maxd" [(t, "x"), (t, "y")] $ maxExp x y
        mind = F.FunDecl t "mind" [(t, "x"), (t, "y")] $ minExp x y
        expd = F.FunDecl t "expd" [(t, "x")] $ F.FunCall (suff "exp") [x]
        signd = F.FunDecl F.IntT "signd" [(t, "x")] $
          IfThenElse (BinApp Less (constant 0) x)
          (Constant (Int 1)) $
          IfThenElse (BinApp Eq (constant 0) x)
          (Constant (Int 0)) (Constant (Int (-1)))
        ceil = F.FunDecl F.IntT "ceil" [(t, "x")] $
          IfThenElse (F.BinApp F.Eq (F.FunCall "i2d" [F.FunCall "int" [x]]) x)
          (F.FunCall "int" [x])
          (F.FunCall "int" [F.BinApp Plus x (constant 1)])

boolToInt :: FunDecl
boolToInt = F.FunDecl F.IntT "boolToInt" [(F.BoolT, "x")] $
  F.IfThenElse (F.Var "x") (Constant (Int 1)) (Constant (Int 0))

negi, absi, mini, signi,
  maxi, nandb, norb, eqb, xorb, neqi, neqd, resi :: FunDecl

negi = F.FunDecl F.IntT "negi" [(F.IntT,"x")] $ F.Neg (F.Var "x")

absi = F.FunDecl F.IntT "absi" [(F.IntT,"x")] $ absExp (F.Var "x")

mini = F.FunDecl F.IntT "mini" [(F.IntT, "x"), (F.IntT, "y")] $ minExp (F.Var "x") (F.Var "y")

signi = F.FunDecl F.IntT "signi" [(F.IntT, "x")] $ signiExp (F.Var "x")

maxi = F.FunDecl F.IntT "maxi" [(F.IntT, "x"), (F.IntT, "y")] $ maxExp (F.Var "x") (F.Var "y")

nandb = F.FunDecl F.BoolT "nandb" [(F.BoolT, "x"), (F.BoolT, "y")] $ nandExp (F.Var "x") (F.Var "y")

norb = F.FunDecl F.BoolT "norb" [(F.BoolT, "x"), (F.BoolT, "y")] $ norExp (F.Var "x") (F.Var "y")

eqb = F.FunDecl F.BoolT "eqb" [(F.BoolT, "x"), (F.BoolT, "y")] $ boolEquals (F.Var "x") (F.Var "y")
  where boolEquals e1 e2 = BinApp F.LogicOr (norExp e1 e2) (BinApp F.LogicAnd e1 e2)

xorb = F.FunDecl F.BoolT "xorb" [(F.BoolT, "x"), (F.BoolT, "y")] $ boolXor (F.Var "x") (F.Var "y")
  where boolXor e1 e2 = BinApp F.LogicAnd (nandExp e1 e2) (BinApp F.LogicOr e1 e2)

neqi = F.FunDecl F.BoolT "neqi" [(F.IntT, "x"), (F.IntT, "y")] $ notEq (F.Var "x") (F.Var "y")

neqd = F.FunDecl F.BoolT "neqd" [(F.F32T, "x"), (F.F32T, "y")] $ notEq (F.Var "x") (F.Var "y")

notEq :: F.Exp -> F.Exp -> F.Exp
notEq e1 e2 = FunCall "!" [BinApp F.Eq e1 e2]

resi = F.FunDecl F.IntT "resi" [(F.IntT, "x"),(F.IntT, "y")] $ resiExp (F.Var "x") (F.Var "y")

-- AUX: make FunDecl by combining signature and body (aux function that create function body)
makeFun :: [F.Arg] -> F.Ident -> F.Exp -> F.Type -> FunDecl
makeFun args name body tp = F.FunDecl (ArrayT tp) (name ++ "_" ++ pretty tp) args body

stdArgs :: F.Type -> [(F.Type, String)]
stdArgs tp = [(F.IntT,"l"),(ArrayT tp, "x")]

reshapeFun :: F.Type -> FunDecl
reshapeFun tp = makeFun (stdArgs tp) "reshape1" (reshape1Body tp) tp
takeFun :: F.Type -> F.FunDecl
takeFun tp = makeFun (stdArgs tp) "take1" (takeBody (zero tp)) tp
dropFun :: F.Type -> F.FunDecl
dropFun tp = makeFun (stdArgs tp) "drop1" (dropBody tp) tp

reshapeFuns, takeFuns, dropFuns :: [FunDecl]
reshapeFuns = map reshapeFun btypes
takeFuns = map takeFun btypes
dropFuns = map dropFun btypes

-----------------
-- EXPRESSIONS --
-----------------

-- general expressions --
compileExp :: T.Exp -> CompilerM F.Exp
compileExp (T.Var ident)
  | ident == "pi" = compileExp $ D 3.14159265359
  | otherwise = return $ F.Var $ "t_" ++ ident
compileExp (I x) = return $ Constant (Int x)
compileExp (D d) = do
  float <- asks floatType
  case float of
    F.F32T ->
      return $ Constant (F32 $ fromRational $ toRational d)
    _ ->
      return $ Constant (F64 d)
compileExp (C char)   = return $ Constant (Char char)
compileExp (B bool)   = return $ Constant (Bool bool)
compileExp Inf = return $ Constant (F32 (read "Infinity"))
compileExp (T.Neg e) = F.Neg <$> compileExp e
compileExp (T.Let v _ e1 e2) =
  F.Let (Ident ("t_" ++ v)) <$>
  compileExp e1 <*> compileExp e2
compileExp (T.Op "tuple" Nothing args) = F.Tuple <$> mapM compileExp args
compileExp (T.Op ident instDecl args) = compileOpExp ident instDecl args
compileExp T.Fn{} = throwError "Fn not supported"
compileExp (Vc exps) = Array <$> mapM compileExp exps

-- operators --
compileOpExp :: String -> Maybe ([BType], [Integer]) -> [T.Exp] -> CompilerM F.Exp
compileOpExp ident instDecl args = case ident of
  "reduce" -> compileReduce instDecl args
  "eachV"  -> compileEachV instDecl args
  "each"   -> compileEach instDecl args
  "power"  -> compilePower instDecl args
  "powerScl" -> compilePower instDecl args
  "firstV" -> compileFirstV instDecl args
  "first" -> compileFirst instDecl args
  "shapeV" -> F.Array <$> makeShape 1 args 
  "shape"  -> compileShape instDecl args
  "reshape" -> compileReshape instDecl args
  "take" -> compileTake instDecl args
  "takeV" -> compileTakeV instDecl args
  "zipWith" -> compileZipWith instDecl args
  "cat" -> compileCat instDecl args
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
  _
    | [e1,e2]  <- args
    , Just op  <- convertBinOp ident ->
      F.BinApp op <$> compileExp e1 <*> compileExp e2
    | Just fun <- convertFun ident ->
      F.FunCall fun <$> mapM compileExp args
    | ident `elem` idFuns ->
      F.FunCall ident <$> mapM compileExp args
    | otherwise       -> throwError $ ident ++ " not supported"

-- snocV --
compileSnocV :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileSnocV (Just([_],[_])) [a,e] = do
  a' <- compileExp a
  e' <- compileExp e
  return $ F.FunCall "concat" [a', F.Array [e']]
compileSnocV Nothing _ = throwError "snocV needs instance declaration"
compileSnocV _ _ = throwError "snocV take two aguments"

-- snoc --
compileSnoc :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileSnoc (Just([_],[0])) [a,e] = compileSnoc' <$> compileExp a <*> compileExp e
  where compileSnoc' a' e' =
          F.FunCall "concat" [a',F.Array [e']]
compileSnoc (Just([_],[r])) [a,e] = compileSnoc' <$> compileExp a <*> compileExp e
  where compileSnoc' a' e' =
          makeTransp2 (map (Constant . Int) (reverse [0..r])) (F.FunCall "concat" [arr,e''])
          where e'' = F.Array [makeTransp r e']
                arr = makeTransp (r+1) a'
compileSnoc _ _ = throwError "compileSnoc: invalid arguments"

-- consV --
compileConsV :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileConsV (Just([_],[_])) [e,a] =
  compileConsV' <$> compileExp e <*> compileExp a
  where compileConsV' e' a' = F.FunCall "concat" [F.Array [e'], a']
compileConsV Nothing _ = throwError "consV needs instance declaration"
compileConsV _ _ = throwError "consV take two aguments"


-- cons --
compileCons :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileCons (Just([_],[r])) [e,a] = compileCons' <$> compileExp e <*> compileExp a
  where compileCons' e' a' =
          makeTransp2 (map (Constant . Int) (reverse [0..r])) (F.FunCall "concat" [e'', arr])
          where e'' = F.Array [makeTransp r e']
                arr = makeTransp (r+1) a'
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
          (FunCall "iota" [a'])
compileIota _ _ = throwError "Iota take one argument"

-- vreverse --
compileVReverse :: Maybe ([BType], [Integer]) -> [T.Exp] -> CompilerM F.Exp
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
  (Map <$> kernelExp <*> pure (FunCall "iota" [FunCall "size" [F.Constant (F.Int 0), a]]))
  where
    kernelExp = F.Fn <$>
      mkType (tp,r-1) <*>
      pure [(F.IntT,"x")] <*>
      pure (F.Index (F.Var "a") [F.BinApp F.Minus minusIndex ione])
    sizeCall = F.FunCall "size" [izero, a]
    minusIndex = F.BinApp F.Minus sizeCall (F.Var "x")
    izero = F.Constant (F.Int 0)
    ione = F.Constant (F.Int 1)

-- rotate --
compileVRotate :: Maybe ([BType], [Integer]) -> [T.Exp] -> CompilerM F.Exp
compileVRotate (Just([tp],[r])) [i,a] = makeVRotate tp r i =<< compileExp a
compileVRotate Nothing _ = throwError "Need instance declaration for vrotate"
compileVRotate _ _ = throwError "vrotate needs 2 arguments"

compileRotate :: Maybe ([BType], [Integer]) -> [T.Exp] -> CompilerM F.Exp
compileRotate (Just([tp],[r])) [i,a] =
  makeTransp r <$> (makeVRotate tp r i =<< (makeTransp r <$> compileExp a))
compileRotate Nothing _ = throwError "Need instance declaration for rotate"
compileRotate _ _ = throwError "rotate needs 2 arguments"

-- vrotateV --
compileVRotateV :: Maybe ([BType], [t]) -> [T.Exp] -> CompilerM F.Exp
compileVRotateV (Just([tp],[_])) [i,a] = makeVRotate tp 1 i =<< compileExp a
compileVRotateV Nothing _ = throwError "Need instance declaration for vrotateV"
compileVRotateV _ _ = throwError "vrotateV needs 2 arguments"

-- vrotate --
makeVRotate :: BType -> Integer -> T.Exp -> F.Exp -> CompilerM F.Exp
makeVRotate tp r i a =
  F.Let (Ident "a") a <$> (Map <$> kernelExp <*> pure (FunCall "iota" [size]))
  where
    kernelExp = F.Fn <$>
      mkType (tp, r-1) <*>
      pure [(F.IntT, "x")] <*>
      (F.Index (F.Var "a") <$> sequence [F.BinApp F.Mod <$> add <*> pure size])
    add = F.BinApp F.Plus (F.Var "x") <$> compileExp i
    size = FunCall "size" [F.Constant (F.Int 0), a]

-- cat --
compileCat :: Maybe ([BType], [Integer]) -> [T.Exp] -> CompilerM F.Exp
compileCat (Just([tp],[r])) [a1,a2] = do
  a1' <- compileExp a1
  a2' <- compileExp a2
  makeCat tp r a1' a2'
  where makeCat _ 1 a1' a2' = return $ FunCall "concat" [a1', a2']
        makeCat tp' r' a1' a2' = Map <$> kernelExp <*> pure (FunCall "zip" [a1', a2'])
          where kernelExp = do
                  t <- mkType (tp',r'-1)
                  F.Fn t [(t,"x"),(t,"y")] <$> makeCat tp' (r'-1) (F.Var "x") (F.Var "y")
compileCat _ _ = throwError "compileCat: invalid arguments"

-- takeV --
compileTakeV :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileTakeV (Just([tp],_)) [len,e] =
  F.FunCall <$> fname <*> sequence [compileExp len, compileExp e]
  where fname = ("take1_" ++) . pretty <$> makeBTp tp
compileTakeV Nothing _ = throwError "Need instance declaration for takeV"
compileTakeV _ _ = throwError "TakeV needs 2 arguments"

-- dropV --
compileDropV :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileDropV (Just([tp],_)) [len,e] =
  F.FunCall <$> fname <*> sequence [compileExp len,compileExp e]
    where fname = ("drop1_" ++) . pretty <$> makeBTp tp
compileDropV Nothing _ = throwError "Need instance declaration for dropV"
compileDropV _ _ = throwError "DropV needs 2 arguments"

-- take --
compileTake :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileTake (Just([tp],[r])) [len,e] = takeDropHelper compileTake' r len e
  where compileTake' shape len' e' =
          F.FunCall2 "reshape" dims <$> (F.FunCall <$> fname <*> pure [sizeProd,resh])
          where dims =  absExp len':tail shape
                sizeProd = multExp $ len':tail shape
                fname = ("take1_" ++) . pretty <$> makeBTp tp
                resh = F.FunCall2 "reshape" [multExp shape] e'
compileTake Nothing _args = throwError "Need instance declaration for take"
compileTake _ _ = throwError "Take needs 2 arguments"

-- drop --
compileDrop :: Maybe ([BType], [Integer]) -> [T.Exp] -> CompilerM F.Exp
compileDrop (Just([tp],[r])) [len,e] = takeDropHelper compileDrop' r len e
  where compileDrop' shape len' e' =
          F.FunCall2 "reshape" dims <$> (F.FunCall <$> fname <*> pure [sizeProd,resh])
          where dims =
                  maxExp (Constant (Int 0))
                  (F.BinApp F.Minus
                   (F.FunCall "size" [Constant (Int 0), e'])
                   (absExp len')) : tail shape
                resh = F.FunCall2 "reshape" [multExp shape] e'
                sizeProd = multExp $ len' : tail shape
                fname = ("drop1_" ++) . pretty <$> makeBTp tp
compileDrop _ _ = throwError "compileDrop: invalid arguments"

takeDropHelper :: ([F.Exp] -> F.Exp -> F.Exp -> CompilerM b)
               -> Integer -> T.Exp -> T.Exp -> CompilerM b
takeDropHelper f r len e = do
  shape <- makeShape r [e]
  len' <- compileExp len
  e' <- compileExp e
  f shape len' e'

-- reshape --
compileReshape :: Maybe ([BType], [Integer]) -> [T.Exp] -> CompilerM F.Exp
compileReshape (Just([tp],[r1,r2])) [dims,array] = do
  dimsExp <- compileExp dims
  fname <- ("reshape1_" ++) . pretty <$> makeBTp tp
  (dimsList, wrap) <- case dimsExp of
    F.Array l ->
      return (l, id)
    F.Var dimsVar ->
      return (map (\i -> F.Index (F.Var dimsVar) [Constant (Int i)]) [0..r2-1], id)
    _ -> do
      let name = "shape_tmp"
      return ([F.Index (F.Var name) [Constant (Int i)] | i <- [0..r2-1]],
              F.Let (F.Ident name) dimsExp)
  shapeProd <- multExp <$> makeShape r1 [array]
  resh <- F.FunCall2 "reshape" [shapeProd] <$> compileExp array
  return $ wrap $ F.FunCall2 "reshape" dimsList $ F.FunCall fname [multExp dimsList, resh]
compileReshape Nothing _args = throwError "Need instance declaration for reshape"
compileReshape _ _ = throwError "Reshape needs 2 arguments"

-- transp --
compileTransp :: Maybe (t, [Integer]) -> [T.Exp] -> CompilerM F.Exp
compileTransp (Just(_,[r])) [e] =
  makeTransp2 (map (Constant . Int) (reverse [0..r-1])) <$> compileExp e
compileTransp Nothing _args = throwError "Need instance declaration for transp"
compileTransp _ _ = throwError "Transpose takes 1 argument"

-- transp2 --
compileTransp2 :: t -> [T.Exp] -> CompilerM F.Exp
compileTransp2 _ [Vc dims,e] = makeTransp2 <$> mapM compileExp dimsExps <*> compileExp e
    where dimsExps = map (I . (\x -> x - 1) . getInt) dims
          getInt (I i) = i
          getInt _ = error "transp2 expects number literals in it's first argument"
compileTransp2 _ e = case e of [_,_] -> throwError "transp2 needs litaral as first argument"
                               _     -> throwError "transp2 takes 2 arguments"

-- shape --
compileShape :: Maybe (t, [Integer]) -> [T.Exp] -> CompilerM F.Exp
compileShape (Just(_,[len])) args = F.Array <$> makeShape len args
compileShape Nothing _args = throwError "Need instance declaration for shape"
compileShape _ _ = throwError "compileShape: invalid arguments"

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
  Map <$> (compileKernel kernel =<< makeBTp outtp) <*> compileExp array
compileEachV _ _ = throwError "compileEachV: invalid arguments"

-- each --
compileEach :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileEach (Just ([intp,outtp],[orig_r])) [okernel,orig_array] =
  makeEach intp outtp orig_r okernel =<< compileExp orig_array
  where makeEach _ tp2 1 kernel array =
          Map <$> (compileKernel kernel =<< makeBTp tp2) <*> pure array
        makeEach tp1 tp2 r kernel array = do
          tp2' <- mkType (tp2,r-1)
          tp1' <- mkType (tp1,r-1)
          body <- makeEach tp1 tp2 (r-1) kernel (F.Var "x")
          return $ Map (F.Fn tp2' [(tp1',"x")] body) array
compileEach Nothing _ = throwError "Need instance declaration for each"
compileEach _ _ = throwError "each takes two arguments"

-- power --
compilePower :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compilePower (Just ([tp],_)) [kernel,num,arr] = do
  fn <- compileKernel kernel =<< makeBTp tp
  num' <- compileExp num
  arr' <- compileExp arr
  case fn of
    F.Fn _ [(_,var)] body ->
      return $ F.ForLoop var arr' "i" num' body $ F.Var var
    F.Fn{} ->
      fail "expecting exactly one argument in function to power-function"
    _  ->
      fail "expecting anonymous function as argument to power-function"
compilePower (Just (_,_)) _ = throwError "power takes one type argument"
compilePower Nothing [_,_,_] = throwError "Need instance declaration for power"
compilePower _ _ = throwError "power takes three arguments"

-- zipWith --
compileZipWith :: Maybe InstDecl -> [T.Exp] -> CompilerM F.Exp
compileZipWith (Just([tp1,tp2,rtp],[rk])) [orig_kernel,orig_a1,orig_a2] = do
  a1 <- compileExp orig_a1
  a2 <- compileExp orig_a2
  makeZipWith rk orig_kernel a1 a2
  where makeZipWith r kernel a1 a2
          | r == 1 =
            Map <$> (compileKernel kernel =<< makeBTp rtp) <*> pure (FunCall "zip" [a1,a2])
          | otherwise = do
              rtp' <- mkType (rtp,r-1)
              tp1' <- mkType (tp1,r-1)
              tp2' <- mkType (tp2,r-1)
              body <- makeZipWith (r-1) kernel (F.Var "x") (F.Var "y")
              return $ Map (F.Fn rtp' [(tp1',"x"),(tp2',"y")] body) (FunCall "zip" [a1, a2])
    --Map kernelExp $ F.FunCall "zip" [(compileExp a1),(compileExp a2)] -- F.Map kernelExp $ F.FunCall "zip" [a1,a2]
compileZipWith Nothing _ = throwError "Need instance declaration for zipWith"
compileZipWith _ _ = throwError "zipWith takes 3 arguments"

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
              return $ Map (F.Fn rt [(t,"x")] body) arrayExp
compileReduce _ _ = throwError "reduce needs 3 arguments"


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
          "signi",
          "maxi",
          "maxd",
          "eqb",
          "xorb",
          "nandb",
          "norb",
          "neqi",
          "neqd",
          "resi"]

-- operators that are 1:1 with Futhark functions --
convertFun :: String -> Maybe String
convertFun fun = case fun of
  "i2d"    -> Just "i2d"
  "catV"   -> Just "concat"
  "b2i"    -> Just "boolToInt"
  "b2iV"   -> Just "boolToInt"
  "ln"     -> Just "ln"
  "ceil"   -> Just "ceil"
  "expd"   -> Just "expd"
  "notb"   -> Just "!"
  "floor"  -> Just "int"
  "mem"    -> Just "copy"
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
