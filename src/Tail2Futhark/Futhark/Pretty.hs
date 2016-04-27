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
    indent 2 (ppExp body)

commaList :: [Doc] -> Doc
commaList = parens . commasep
ppKernel :: Kernel -> Doc
commaExps :: [Exp] -> Doc
commaExps = commaList . map ppExp
brackList :: [Doc] -> Doc
brackList = brackets . commasep
brackExps :: [Exp] -> Doc
brackExps = brackList . map ppExp

instance Pretty Type where
  ppr IntT = text "int"
  ppr Int8T = text "i8"
  ppr F32T = text "f32"
  ppr F64T = text "f64"
  ppr BoolT = text "bool"
  ppr (ArrayT at) = brackets (ppr at)

ppExp :: Exp -> Doc
ppExp (Var ident) = text ident
ppExp (Let Indent pat exp1 exp2) = text "let" <+> ppPat pat <+> equals <+> ppExp exp1 <+> text "in" </> ppExp exp2
ppExp (Let Inline pat exp1 exp2) = text "let" <+> ppPat pat <+> equals <+> ppExp exp1 <+> text "in" <+> ppExp exp2
ppExp (IfThenElse Indent e1 e2 e3) = text "if" <+> ppExp e1 </> text "then" <+> ppExp e2 </> text "else" <+> ppExp e3
ppExp (IfThenElse Inline e1 e2 e3) = text "if" <+> ppExp e1 <+> text "then" <+> ppExp e2 <+> text "else" <+> ppExp e3
ppExp (Constant c) = ppr c
ppExp (Neg e)    = text "-" <> ppExp e
ppExp (Index e exps) = ppExp e <> brackExps exps
ppExp (Array exps) = brackExps exps
ppExp (BinApp op e1 e2) = parens $ ppExp e1 <+> ppOp op <+> ppExp e2
ppExp (FunCall ident exps) = text ident <> commaExps exps
ppExp (FunCall2 ident exps e) = text ident <> parens (commaExps exps <> comma <> ppExp e)
--ppExp (Reshape exps exp) = text "reshape" <> parens (commaExps exps <> comma <> ppExp exp)
ppExp (Empty tp) = text "empty" <> parens (ppr tp)
ppExp (Map k e) = ppSOAC1 "map" k e
ppExp (Filter k e) = ppSOAC1 "filter" k e
ppExp (Scan k e1 e2) = ppSOAC2 "scan" k e1 e2
ppExp (Reduce k e1 e2) = ppSOAC2 "reduce" k e1 e2

ppSOAC1 :: String -> Kernel -> Exp -> Doc
ppSOAC1 v k e = text v <> parens (ppKernel k <> comma <> ppExp e)

ppSOAC2 :: String -> Kernel -> Exp -> Exp -> Doc
ppSOAC2 v k e1 e2 = text v <> parens (ppKernel k <> comma <> ppExp e1 <> comma <> ppExp e2)

ppKernel (Fn tp args body) =
  text "fn" <+> ppr tp <+> (commaList . map ppArg $ args) <+> text "=>" </>
  ppExp body
ppKernel (Fun ident []) = text ident
ppKernel (Fun ident exps) = text ident <+> (commaList . map ppExp $ exps)
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
  ppr (ArrayConstant arr) = braces . commasep . map ppr $ arr

-- Arguments --
ppArg :: (Type, String) -> Doc
ppArg (tp,ident) = ppr tp <+> text ident

-- Pattern --
ppPat :: Pattern -> Doc
ppPat (Ident ident) = text ident
ppPat (TouplePat pat) = braces . commasep . map ppPat $ pat 
