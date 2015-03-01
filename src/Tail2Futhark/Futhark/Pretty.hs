module Tail2Futhark.Futhark.Pretty (prettyPrint)  where

import Text.PrettyPrint
import Tail2Futhark.Futhark.AST

prettyPrint :: Program -> String
prettyPrint = render . vcat . map ppFun 

ppFun :: FunDecl -> Doc
ppFun (tp, ident, args, exp) = text "fun" 
                               <+> ppType tp 
                               <+> text ident 
                               <> (parens . hcat . punctuate comma . map ppArg) args 
                               <+> equals $+$ nest 2 (ppExp exp)

ppType tp = text . show $ tp

ppExp (Let pat exp1 exp2) = text "let" <+> ppPat pat <+> equals <+> ppExp exp1 <+> text "in" $+$ ppExp exp2
ppExp rest = text . show $ rest

-- Arguments --
ppArg arg = text . show $ arg

-- Pattern --
ppPat :: Pattern -> Doc
ppPat (Ident ident) = text ident
ppPat (TouplePat pat) = braces . hcat . punctuate comma . map ppPat $ pat 
