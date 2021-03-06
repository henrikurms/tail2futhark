{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tail2Futhark.Futhark.Pretty (pretty)  where

import           Tail2Futhark.Futhark.AST
import           Text.PrettyPrint.Mainland.Class
import           Text.PrettyPrint.Mainland hiding (pretty)
import qualified Text.PrettyPrint.Mainland as PP

-- | Prettyprint a value, wrapped to 80 characters.
pretty :: Pretty a => a -> String
pretty = PP.pretty 80 . ppr

instance Pretty Program where
  ppr (Program fundecs) =
    stack . map ppr $ fundecs

instance Pretty FunDecl where
  ppr (FunDecl entry tp ident tsparams args body) =
    fun'
    <+> text ident
    <+> tsparams' <+> args'
    <> text ":" <+> ppr tp
    <+> equals </>
    indent 2 (ppr body)
    where fun' = text $ if entry then "entry" else "let"
          args' = case args of
                    [] -> parens mempty
                    _  -> spread (map (parens . ppArg) args)
          tsparams' = spread $ map ppr tsparams

instance Pretty TypeSizeParam where
  ppr (SizeParam x) = brackets $ text x
  ppr (TypeParam x) = text "'" <> text x

commaList :: [Doc] -> Doc
commaList = parens . commasep
brackList :: [Doc] -> Doc
brackList = brackets . commasep
brackExps :: [Exp] -> Doc
brackExps = brackList . map ppr

instance Pretty Type where
  ppr IntT          = text "i32"
  ppr Int8T         = text "i8"
  ppr F32T          = text "f32"
  ppr F64T          = text "f64"
  ppr BoolT         = text "bool"
  ppr (VarT s)      = text s
  ppr (ArrayT at d) = brackets (ppr d) <> ppr at
  ppr (TupleT ts)   = parens $ commasep $ map ppr ts

instance Pretty DimDecl where
  ppr AnyDim       = mempty
  ppr (NamedDim d) = ppr d
  ppr (ConstDim d) = ppr d

instance Pretty Exp where
  ppr (Var ident) = text ident
  ppr (Ascript e t) = ppr e <+> colon <+> ppr t
  ppr (Let pat exp1 exp2) = text "let" <+> ppPat pat <+> equals <+> align (ppr exp1) <+> text "in" </> ppr exp2
  ppr (IfThenElse e1 e2 e3) = text "if" <+> ppr e1 </> text "then" <+> ppr e2 </> text "else" <+> ppr e3
  ppr (Unsafe e) = text "unsafe" <+> ppr e
  ppr (ForLoop merge merge_init i bound loopbody) =
    text "loop" <+> text merge <+> text "=" <+> ppr merge_init <+> text "for" <+> text i <+> text "<" <+> ppr bound <+> text "do" </>
    indent 2 (ppr loopbody)
  ppr (Constant c) = ppr c
  ppr (Neg e)    = text "-" <> ppr e
  ppr (Index (Var v) exps) = ppr v <> brackExps exps
  ppr (Index e exps) = parens (ppr e) <> brackExps exps
  ppr (Array exps) = brackExps exps
  ppr (Tuple exps) = parens $ commasep $ map ppr exps
  ppr (Project f e) = ppr e <> text "." <> text f
  ppr (BinApp op e1 e2) = parens $ ppr e1 <+> ppOp op <+> ppr e2
  ppr (FunCall f exps) = ppr f <+> spread (map (parens . ppr) exps)
  ppr (Rearrange perm e) = text "rearrange" <+> parens (commasep $ map ppr perm) <+> parens (ppr e)
  ppr (Empty tp) = text "empty" <> parens (ppr tp)
  ppr (Map k es) = ppSOAC1 ("map" ++ show (length es)) k es
  ppr (Filter k e) = ppSOAC1 "filter" k [e]
  ppr (Scan k e1 e2) = ppSOAC2 "scan" k e1 e2
  ppr (Reduce k e1 e2) = ppSOAC2 "reduce" k e1 e2

  ppr (Fn tp args body) =
    parens $
    text "\\" <+> (spread $ map (parens . ppArg) args) <>
    text ":" <+> ppr tp <+> text "->" </>
    ppr body
  ppr (Fun ident []) = text ident
  ppr (Fun ident exps) = parens $ text ident <+> (commaList . map ppr $ exps)
  ppr (Op op) = parens $ ppOp op

ppSOAC1 :: String -> Exp -> [Exp] -> Doc
ppSOAC1 v k es = text v <+> ppr k <+> spread (map (parens . ppr) es)

ppSOAC2 :: String -> Exp -> Exp -> Exp -> Doc
ppSOAC2 v k e1 e2 = text v <+> ppr k <+> parens (ppr e1) <+> parens (ppr e2)

ppOp :: Operator -> Doc
ppOp op = text $ case op of
  Plus      -> "+"
  Minus     -> "-"
  LessEq    -> "<="
  Mult      -> "*"
  Div       -> "/"
  Eq        -> "=="
  Mod       -> "%"
  Greater   -> ">"
  Less      -> "<"
  GreaterEq -> ">="
  LogicAnd  -> "&&"
  LogicOr   -> "||"
  Pow       -> "**"
  Or        -> "|"
  Xor       -> "^"
  And       -> "&"
  Shl       -> ">>"
  Shr       -> "<<"
  Concat    -> "++"
  --XOr -> "^"

instance Pretty Constant where
  ppr (Int x)             = integer x
  ppr (F32 f)             = text (show f) <> text "f32"
  ppr (F64 f)             = text (show f) <> text "f64"
  ppr (Char c)            = text $ show c
  ppr (Bool b)            = text (if b then "true" else "false")
  ppr (ArrayConstant arr) = brackets . commasep . map ppr $ arr

-- Arguments --
ppArg :: (Type, String) -> Doc
ppArg (tp,ident) = text ident <> text ":" <+> ppr tp

-- Pattern --
ppPat :: Pattern -> Doc
ppPat (Ident ident)   = text ident
ppPat (TouplePat pat) = parens . commasep . map ppPat $ pat
