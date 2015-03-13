module APLAcc.TAIL.Parser (parseFile) where

import System.IO (Handle, hGetContents)
import Control.Monad (liftM, liftM2)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Pos
import qualified Text.Parsec.Token as Token

import APLAcc.TAIL.AST


parseFile :: Handle -> String -> IO Program
parseFile handle filename =
  do str <- hGetContents handle
     case parse program filename str of
       Left e  -> error $ show e
       Right r -> return r


tailDef = Token.LanguageDef {
                Token.commentStart     = "(*"
              , Token.commentEnd       = "*)"
              , Token.commentLine      = ""
              , Token.nestedComments   = False
              , Token.identStart       = letter
              , Token.identLetter      = alphaNum <|> char '_'
              , Token.opStart          = oneOf ""
              , Token.opLetter         = oneOf ""
              , Token.reservedOpNames  = []
              , Token.reservedNames    = [ "let", "in", "int", "double", "fn", "inf" ]
              , Token.caseSensitive    = True
  }

lexer = Token.makeTokenParser tailDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
stringlit  = Token.stringLiteral lexer
charlit    = Token.charLiteral lexer
parens     = Token.parens     lexer
brackets   = Token.brackets   lexer
angles     = Token.angles     lexer
braces     = Token.braces     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
comma      = Token.comma      lexer
colon      = Token.colon      lexer
symbol     = Token.symbol     lexer
whitespace = Token.whiteSpace lexer
decimal    = Token.decimal    lexer
float      = Token.float      lexer
lexeme     = Token.lexeme     lexer

withPrefix :: Parser a -> Parser b -> (a -> b -> b) -> Parser b
withPrefix pre p f =
  do x <- optionMaybe pre
     y <- p
     return $ case x of
       Just x' -> f x' y
       Nothing -> y

program :: Parser Program
program =
  do whitespace
     prog <- expr
     eof
     return prog

-----------------
-- Expression

expr :: Parser Exp
expr = opExpr
   <|> arrayExpr
   <|> letExpr
   <|> fnExpr
   <|> valueExpr
   <?> "expression"

valueExpr :: Parser Exp
valueExpr = try (liftM D $ lexeme float)
         <|> liftM I (lexeme decimal)
         <|> try (reserved "inf" >> return Inf)
         <|> (char '~' >> liftM Neg valueExpr)
         <|> liftM C charlit 
         <|> liftM Var identifier
         <?> "number or identifier"

arrayExpr :: Parser Exp
arrayExpr = liftM Vc $ brackets (sepBy (opExpr <|> valueExpr) comma)

letExpr :: Parser Exp
letExpr =
  do reserved "let"
     (ident, typ) <- typedIdent
     symbol "="
     e1 <- expr
     reserved "in"
     e2 <- expr
     return $ Let ident typ e1 e2

instanceDecl :: Parser InstDecl
instanceDecl = braces $
  do btyps <- brackets $ sepBy basicType comma
     comma
     ranks <- brackets $ sepBy (lexeme decimal) comma
     return (btyps, ranks)

opExpr :: Parser Exp
opExpr =
  do ident <- try $ do { i <- identifier; lookAhead $ oneOf "({"; return i }
     instDecl <- optionMaybe instanceDecl
     args <- parens $ sepBy expr comma
     return $ Op ident instDecl args

fnExpr :: Parser Exp
fnExpr =
  do reserved "fn"
     (ident, typ) <- typedIdent
     symbol "=>"
     e <- expr
     return $ Fn ident typ e

typedIdent :: Parser (Ident, Type)
typedIdent =
  do ident <- identifier
     colon
     typ <- typeExpr
     return (ident, typ)

------------------
-- Types

typeExpr :: Parser Type
typeExpr = arrayType <|> vectorType <?> "type"
--typeExpr = liftM (foldr1 FunT) $
--  sepBy1 (arrayType <|> vectorType <?> "type") (symbol "->")

arrayType :: Parser Type
arrayType = liftM2 ArrT (brackets basicType) rank

-- vectortype as replacement for shapeType 
vectorType :: Parser Type
vectorType = liftM2 VecT (angles basicType) rank
         <|> (try (symbol "SV") >> parens (do {t <- basicType ; comma ; r <- rank ; return $ SV t r}))
         <|> (try (symbol "S") >> parens (do {t <- basicType ; comma ; r <- rank ; return $ S t r }))
         <?> "vector type"

--shapeType :: Parser Type
--shapeType = shape "Sh" ShT
--        <|> shape "Si" SiT
--        <|> shape "Vi" ViT
--        <?> "shape type"
--  where shape name con = try (symbol name) >> liftM con (parens rank)

rank :: Parser Rank
rank = liftM R (lexeme decimal)
   -- <|> (liftM Rv identifier)  Unsupported
   <?> "rank"

basicType :: Parser BType
basicType = (reserved "int" >> return IntT)
        <|> (reserved "double" >> return DoubleT)
        <|> (reserved "bool" >> return BoolT)
        <|> (reserved "char" >> return CharT)
        <|> (char '\'' >> many1 alphaNum  >>= return . Btyv)
        <?> "basic type"

-------------------
-- Debug functions

parseString :: Parser a -> String -> a
parseString parser str =
  case parse parser "" str of
    Left e  -> error $ show e
    Right r -> r

