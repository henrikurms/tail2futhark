{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tail2Futhark.Futhark.Pretty (pretty)  where

import Text.PrettyPrint.Mainland hiding (pretty)
import qualified Text.PrettyPrint.Mainland as PP
import Tail2Futhark.Futhark.AST

-- | Prettyprint a value, wrapped to 80 characters.
pretty :: Pretty a => a -> String
pretty = PP.pretty 80 . ppr

instance Pretty Program where
  ppr (Program fundecs) =
    stack . map ppr $ fundecs

instance Pretty FunDecl where
  ppr (FunDecl tp ident args body) =
    text "fun"
    <+> ppr tp
    <+> text ident <> (commaList . map ppArg) args
    <+> equals </>
    indent 2 (ppr body)

commaList :: [Doc] -> Doc
commaList = parens . commasep
ppKernel :: Kernel -> Doc
commaExps :: [Exp] -> Doc
commaExps = commaList . map ppr
brackList :: [Doc] -> Doc
brackList = brackets . commasep
brackExps :: [Exp] -> Doc
brackExps = brackList . map ppr

instance Pretty Type where
  ppr IntT = text "int"
  ppr Int8T = text "i8"
  ppr F32T = text "f32"
  ppr F64T = text "f64"
  ppr BoolT = text "bool"
  ppr (ArrayT at AnyDim) = brackets (ppr at)
  ppr (ArrayT at (NamedDim v)) = brackets (ppr at <> comma <> ppr v)
  ppr (ArrayT at (ConstDim k)) = brackets (ppr at <> comma <> ppr k)
  ppr (TupleT ts) = parens $ commasep $ map ppr ts

instance Pretty Exp where
  ppr (Var ident) = text ident
  ppr (Let pat exp1 exp2) = text "let" <+> ppPat pat <+> equals <+> ppr exp1 <+> text "in" </> ppr exp2
  ppr (IfThenElse e1 e2 e3) = text "if" <+> ppr e1 </> text "then" <+> ppr e2 </> text "else" <+> ppr e3
  ppr (Unsafe e) = text "unsafe" <+> ppr e
  ppr (ForLoop merge merge_init i bound loopbody letbody) =
    text "loop" <+> parens (text merge <+> text "=" <+> ppr merge_init) <+>
    text "=" <+> text "for" <+> text i <+> text "<" <+> ppr bound <+> text "do" </>
    ppr loopbody <+> text "in" <+> ppr letbody
  ppr (Constant c) = ppr c
  ppr (Neg e)    = parens $ text "-" <> ppr e
  ppr (Index e exps) = ppr e <> brackExps exps
  ppr (Array exps) = brackExps exps
  ppr (Tuple exps) = parens $ commasep $ map ppr exps
  ppr (BinApp op e1 e2) = parens $ ppr e1 <+> ppOp op <+> ppr e2
  ppr (FunCall ident exps) = text ident <> commaExps exps
  ppr (FunCall2 ident exps e) = text ident <> parens (commaExps exps <> comma <> ppr e)
  --ppr (Reshape exps exp) = text "reshape" <> parens (commaExps exps <> comma <> ppr exp)
  ppr (Empty tp) = text "empty" <> parens (ppr tp)
  ppr (Map k e) = ppSOAC1 "map" k e
  ppr (Filter k e) = ppSOAC1 "filter" k e
  ppr (Scan k e1 e2) = ppSOAC2 "scan" k e1 e2
  ppr (Reduce k e1 e2) = ppSOAC2 "reduce" k e1 e2

ppSOAC1 :: String -> Kernel -> Exp -> Doc
ppSOAC1 v k e = text v <> parens (ppKernel k <> comma <> ppr e)

ppSOAC2 :: String -> Kernel -> Exp -> Exp -> Doc
ppSOAC2 v k e1 e2 = text v <> parens (ppKernel k <> comma <> ppr e1 <> comma <> ppr e2)

ppKernel (Fn tp args body) =
  text "fn" <+> ppr tp <+> (commaList . map ppArg $ args) <+> text "=>" </>
  ppr body
ppKernel (Fun ident []) = text ident
ppKernel (Fun ident exps) = text ident <+> (commaList . map ppr $ exps)
ppKernel (Op op) = ppOp op

ppOp :: Operator -> Doc
ppOp op = text $ case op of
  Plus -> "+"
  Minus -> "-"
  LessEq -> "<="
  Mult -> "*"
  Div -> "/"
  Eq -> "=="
  Mod -> "%"
  Greater -> ">"
  Less -> "<"
  GreaterEq -> ">="
  LogicAnd -> "&&"
  LogicOr -> "||"
  Pow -> "**"
  Or -> "|"
  Xor -> "^"
  And -> "&"
  Shl -> ">>"
  Shr -> "<<"
  --XOr -> "^"

instance Pretty Constant where
  ppr (Int x) = integer x
  ppr (F32 f) = text (show f) <> text "f32"
  ppr (F64 f) = text (show f) <> text "f64"
  ppr (Char c) = text $ show c
  ppr (Bool b) = text (if b then "True" else "False")
  ppr (ArrayConstant arr) = brackets . commasep . map ppr $ arr

-- Arguments --
ppArg :: (Type, String) -> Doc
ppArg (tp,ident) = ppr tp <+> text ident

-- Pattern --
ppPat :: Pattern -> Doc
ppPat (Ident ident) = text ident
ppPat (TouplePat pat) = parens . commasep . map ppPat $ pat
