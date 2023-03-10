{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Parser where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (some1)
import Hyper

import AST
import Lexer
import Parser.Expr

type instance LocType (PLine simple) = Caret
type instance LocType OptionMod = Caret
type instance LocType (Option body) = Caret
type instance LocType (Else simple) = Caret

indent :: Parser ()
indent =
  lookAhead (void (char '\n') <|> eof) <|> do
    n <- get
    replicateM_ n $ char ' '

newline :: Parser ()
newline = try $ char '\n' *> indent

indented :: Parser a -> Parser a
indented p = do
  newline
  save <- get
  skipSome $ char ' ' *> modify succ
  res <- p
  put save
  pure res

mbIndented :: Parser a -> Parser a
mbIndented p = indented p <|> p

body :: ParseSimple simple => Parser (PBody simple # Ann Loc)
body = PBody <$> withCaret line `sepBy` newline

line :: ParseSimple simple => Parser (PLine simple # Ann Loc)
line =
  PFlat <$> flatLine
    <|> PIf <$> pIf "if"
    <|> PChoice <$> choice

flatLine :: ParseSimple simple => Parser (FlatLine simple # Ann Loc)
flatLine =
  Simple <$> simple
    <|> cmd Label "label" <*> label
    <|> EmptyLine <$ lookAhead (char '\n')
    <|> Text <$> text

text :: Parser (Ann Loc # Str)
text = str <$> some (normal '\n' <|> special)

pIf :: ParseSimple simple => String -> Parser (If simple # Ann Loc)
pIf q = cmd If q <*> expr <*> indented body <*> optional (newline *> withCaret pElse)
{-# INLINE pIf #-}

pElse :: ParseSimple simple => Parser (Else simple # Ann Loc)
pElse =
  Elseif <$> pIf "elseif"
    <|> cmd Else "else" <*> indented body

choice :: ParseSimple simple => Parser (Choice (PBody simple) # Ann Loc)
choice = Choice <$> choiceMode <*> indented (withCaret option `sepByNonEmpty` newline)

choiceMode :: Parser ChoiceMode
choiceMode =
  cmd ChoiceMode "choice"
    <|> cmd FakeChoiceMode "fake_choice"

option :: ParseSimple simple => Parser (Option (PBody simple) # Ann Loc)
option = optional (withCaret optionMod) >>= maybe noMods addMod

noMods :: ParseSimple simple => Parser (Option (PBody simple) # Ann Loc)
noMods = do
  optionText <- text
  optionBody <- indented body
  pure $ Option{optionMods = [], ..}

addMod :: ParseSimple simple => Ann Loc # OptionMod -> Parser (Option (PBody simple) # Ann Loc)
addMod mod = do
  opt <- mbIndented option
  pure $ opt{optionMods = mod : optionMods opt}

optionMod :: Parser (OptionMod # Ann Loc)
optionMod =
  cmd IfMod "if" <*> expr
    <|> cmd SelectableIfMod "selectable_if" <*> expr
    <|> cmd DisableReuseMod "disable_reuse"
    <|> cmd HideReuseMod "hide_reuse"
    <|> cmd AllowReuseMod "allow_reuse"

class ParseSimple simple where
  simple :: Parser (simple # Ann Loc)

instance ParseSimple SimpleCommand where
  simple =
    cmd HideReuse "hide_reuse"
      <|> cmd Temp "temp" <*> var <*> expr
      <|> cmd Set "set" <*> target var <*> setExpr
      <|> cmd Delete "delete" <*> var
      <|> cmd InputNumber "input_number" <*> var <*> int <*> int
      <|> cmd InputText "input_text" <*> var
      <|> cmd Print "print" <*> var
      <|> cmd Rand "rand" <*> var <*> int <*> int
      <|> cmd Goto "goto" <*> target label
      <|> cmd GotoScene "goto_scene" <*> sceneName <*> optional (target label)
      <|> cmd Gosub "gosub" <*> subArgs
      <|> cmd GosubScene "gosub_scene" <*> sceneName <*> optional subArgs
      <|> cmd Params "params" <*> some1 var
      <|> cmd Return "return"
      <|> cmd GotoRandomScene "goto_random_scene" <*> indented (sceneName `sepByNonEmpty` newline)
      <|> cmd Finish "finish" <*> optional text
      <|> cmd LineBreak "line_break"
      <|> cmd PageBreak "page_break" <*> optional text
      <|> cmd StatChart "stat_chart" <*> indented (stat `sepByNonEmpty` newline)
      <|> cmd Achieve "achieve" <*> achievement
      <|> cmd CheckAchievements "check_achievements"
      <|> cmd Ending "ending"

instance ParseSimple StartupSimpleCommand where
  simple =
    cmd Create "create" <*> var <*> expr
      <|> cmd SceneList "scene_list" <*> indented (sceneName `sepByNonEmpty` newline)
      <|> cmd Title "title" <*> text
      <|> cmd Author "author" <*> text
      <|> cmd Achievement "achievement" <*> achievement <*> achData
      <|> NormalSimple <$> simple

target :: Parser a -> Parser (Target a # Ann Loc)
target p =
  RefTarget <$ symbolic '{' <*> expr <* symbolic '}'
    <|> DirectTarget <$> p

setExpr :: Parser (SetExpr # Ann Loc)
setExpr =
  ModSet <$> binOp <*> expr
    <|> NormalSet <$> expr

label :: Parser Label
label = L <$> ident

sceneName :: Parser SceneName
sceneName = SN <$> ident

achievement :: Parser Achievement
achievement = A <$> ident

subArgs :: Parser (SubArgs # Ann Loc)
subArgs = SubArgs <$> target label <*> many term

stat :: Parser Stat
stat =
  TextStat <$ reserve "text" <*> statName
    <|> PercentStat <$ reserve "percent" <*> namedStat
    <|> reserve "opposed_pair" *> (opposedNamed <|> opposedSingle)

statName :: Parser ByteString
statName = sliced . skipSome . satisfy $ \c -> c /= '\n'

namedStat :: Parser NamedStat
namedStat = Named <$> var <*> statName <|> JustVar <$> statName

opposedNamed :: Parser Stat
opposedNamed = do
  left <- var
  indented $ do
    leftName <- statName
    newline
    OpposedStat (Named left leftName) <$> statName

opposedSingle :: Parser Stat
opposedSingle = do
  leftName <- statName
  OpposedStat (JustVar leftName) <$> indented statName

achData :: Parser (AchData # Ann Loc)
achData = do
  visible <- True <$ reserve "visible" <|> False <$ reserve "hidden"
  points <- int
  title <- text
  indented $ do
    preDesc <- text
    newline
    postDesc <- text
    pure $ AchData{..}