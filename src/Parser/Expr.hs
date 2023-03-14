{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Parser.Expr (
  module Parser.Expr,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Hyper

import AST.Expr
import Lexer

type instance LocType Expr = Span
type instance LocType Multirep = Span
type instance LocType Str = ()

target :: Parser a -> Parser (Target a # Ann Loc)
target p =
  RefTarget <$ symbolic '{' <*> expr <* symbolic '}'
    <|> DirectTarget <$> p

var :: Parser Var
var = V <$> ident

bool :: Parser Bool
bool = False <$ reserve "false" <|> True <$ reserve "true"

constant :: Parser Constant
constant = Int <$> int <|> Bool <$> bool

inter :: Parser (StrPart # Ann Loc)
inter =
  Inter <$> try (char '$' *> capMode <* symbolic '{') <*> expr <* char '}'

capMode :: Parser CapMode
capMode =
  Upper <$ string "!!"
    <|> Cap <$ string "!"
    <|> pure NoCap

multirep :: Parser (Multirep # Ann Loc)
multirep =
  Multirep <$ symbol "@{" <*> term <*> strLit `sepByNonEmpty` symbolic '|' <* char '}'

special :: Parser (StrPart # Ann Loc)
special = inter <|> SPMultirep <$> withSpan multirep

normal :: Char -> Parser (StrPart # Ann Loc)
normal end = Normal <$> (escapedChar <|> someNormals end)

normalChar :: Char -> Parser Char
normalChar end = satisfy $ \c -> c /= '\\' && c /= end

escapedChar :: Parser ByteString
escapedChar = C.singleton <$ char '\\' <*> anyChar

someNormals :: Char -> Parser ByteString
someNormals end = sliced . skipSome $ notFollowedBy special *> normalChar end

str :: [StrPart # Ann Loc] -> Ann Loc # Str
str = Ann (Loc ()) . S . compress

strLit :: Parser (Ann Loc # Str)
strLit = str <$ char '"' <*> many (normal '"' <|> special) <* char '"'

compress :: [StrPart h] -> [StrPart h]
compress [] = []
compress (Normal s1 : xs) = case compress xs of
  (Normal s2 : ys) -> Normal (s1 <> s2) : ys
  ys -> Normal s1 : ys
compress (x : xs) = x : compress xs

unOp :: Parser UnOp
unOp =
  Neg <$ op "-"
    <|> Not <$ reserve "not"

binOp :: Parser BinOp
binOp =
  Add <$ op "+"
    <|> Sub <$ op "-"
    <|> Mul <$ op "*"
    <|> Div <$ op "/"
    <|> Mod <$ reserve "modulo"
    <|> FAdd <$ op "%+"
    <|> FSub <$ op "%-"
    <|> Eq <$ op "="
    <|> Neq <$ op "!="
    <|> Gt <$ op ">"
    <|> Lt <$ op "<"
    <|> And <$ reserve "and"
    <|> Or <$ reserve "or"
    <|> Get <$ op "#"
    <|> Cat <$ op "&"

fun :: Parser Fun
fun = Length <$ string "length"

term :: Parser (Ann Loc # Expr)
term =
  withSpan
    ( Constant <$> constant
        <|> Str <$> strLit
        <|> Fun <$> try (fun <* symbolic '(') <*> expr <* symbolic ')'
        <|> Var <$> target var
    )
    <|> symbolic '(' *> expr <* symbolic ')'

expr :: Parser (Ann Loc # Expr)
expr =
  withSpan
    ( UnOp <$> unOp <*> term
        <|> try (BinOp <$> term <*> binOp) <*> term
    )
    <|> term
