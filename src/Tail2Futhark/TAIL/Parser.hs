-- | Originally from APLAcc by Michael Budde
module Tail2Futhark.TAIL.Parser (parseFile, parseString) where

import Control.Applicative
import System.IO (Handle, hGetContents)
import Control.Monad (void)
import Text.Parsec hiding (Empty, (<|>))
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import Prelude

import Tail2Futhark.TAIL.AST


parseFile :: Handle -> String -> IO Program
parseFile handle filename =
  do str <- hGetContents handle
     case parse program filename str of
       Left e  -> error $ show e
       Right r -> return r


tailDef :: Token.LanguageDef u
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
              , Token.reservedNames    = [ "let", "in", "int", "double", "fn", "inf" , "tt", "ff"]
              , Token.caseSensitive    = True
  }

lexer :: Token.TokenParser u
lexer = Token.makeTokenParser tailDef

charlit :: Parser Char
identifier :: Parser String
reserved, symbol :: String -> Parser ()
parens, brackets, angles, braces, lexeme :: Parser a -> Parser a
decimal :: Parser Integer
float :: Parser Double
whitespace, comma, colon :: Parser ()

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
charlit    = Token.charLiteral lexer
parens     = Token.parens     lexer
brackets   = Token.brackets   lexer
angles     = Token.angles     lexer
braces     = Token.braces     lexer
whitespace = Token.whiteSpace lexer
decimal    = Token.decimal    lexer
float      = Token.float      lexer
lexeme     = Token.lexeme     lexer
comma      = void $ Token.comma  lexer
colon      = void $ Token.colon  lexer
symbol     = void . Token.symbol lexer

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
valueExpr = try (D <$> lexeme float)
         <|> I <$> lexeme decimal
         <|> try (reserved "inf" >> return Inf)
         <|> (char '-' >> Neg <$> valueExpr)
         <|> C <$> charlit
         <|> B <$> ((reserved "tt" >> return True) <|> (reserved "ff" >> return False))
         <|> Var <$> identifier
         <|> Ts <$> try (parens (expr `sepBy1` comma))
         <?> "number or identifier"

arrayExpr :: Parser Exp
arrayExpr = Vc <$> brackets (sepBy (opExpr <|> valueExpr) comma)

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
  do ident <- try $ do i <- identifier
                       void $ lookAhead $ oneOf "({"
                       return i
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
typeExpr = arrayType <|> vectorType <|> tupleType <?> "type"
--typeExpr = liftM (foldr1 FunT) $
--  sepBy1 (arrayType <|> vectorType <?> "type") (symbol "->")

arrayType :: Parser Type
arrayType = ArrT <$> brackets basicType <*> rank

-- vectortype as replacement for shapeType 
vectorType :: Parser Type
vectorType = VecT <$> angles basicType <*> rank
         <|> (try (symbol "SV") >> parens (SV <$> basicType <* comma <*> rank))
         <|> (try (symbol "S") >> parens (S <$> basicType <* comma <*> rank))
         <?> "vector type"

tupleType :: Parser Type
tupleType = TupT <$> parens (typeExpr `sepBy1` symbol "*")

--shapeType :: Parser Type
--shapeType = shape "Sh" ShT
--        <|> shape "Si" SiT
--        <|> shape "Vi" ViT
--        <?> "shape type"
--  where shape name con = try (symbol name) >> liftM con (parens rank)

rank :: Parser Rank
rank = R <$> lexeme decimal
   -- <|> (liftM Rv identifier)  Unsupported
   <?> "rank"

basicType :: Parser BType
basicType = (reserved "int" >> return IntT)
        <|> (reserved "double" >> return DoubleT)
        <|> (reserved "bool" >> return BoolT)
        <|> (reserved "char" >> return CharT)
        <|> (char '\'' >> Btyv <$> many1 alphaNum)
        <?> "basic type"

-------------------
-- Debug functions

parseString :: Parser a -> String -> a
parseString parser str =
  case parse parser "" str of
    Left e  -> error $ show e
    Right r -> r

