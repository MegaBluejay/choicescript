{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lexer (
  module Lexer,
  module Control.Applicative,
  module Control.Monad.State,
  module Text.Parser.LookAhead,
  module Text.Trifecta,
  module Text.Trifecta.Delta,
) where

import Control.Applicative
import Control.Monad.State
import Data.HashSet qualified as HS
import Data.String
import Generics.Constraints
import Hyper
import Hyper.Recurse
import Text.Parser.LookAhead
import Text.Parser.Token.Highlight
import Text.Trifecta hiding (
  Parser,
  choice,
  ident,
  line,
  newline,
  option,
  reserve,
  reserveText,
  text,
 )
import Text.Trifecta qualified as Trifecta
import Text.Trifecta.Delta

identStyle :: TokenParsing m => IdentifierStyle m
identStyle =
  IdentifierStyle
    { _styleName = "identifier"
    , _styleStart = letter <|> char '_'
    , _styleLetter = alphaNum <|> char '_'
    , _styleReserved = HS.fromList ["false", "true", "and", "or", "not", "modulo"]
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }

opStyle :: TokenParsing m => IdentifierStyle m
opStyle =
  IdentifierStyle
    { _styleName = "operator"
    , _styleStart = _styleLetter opStyle
    , _styleLetter = oneOf "+-*/%=!<>&#"
    , _styleReserved = HS.fromList ["+", "-", "*", "/", "%+", "%-", "=", "!=", "<", ">", "&", "#"]
    , _styleHighlight = BadInput
    , _styleReservedHighlight = ReservedOperator
    }

cmdStyle :: TokenParsing m => IdentifierStyle m
cmdStyle =
  IdentifierStyle
    { _styleName = "command"
    , _styleStart = char '*'
    , _styleLetter = letter <|> char '_'
    , _styleReserved =
        HS.fromList
          [ "*hide_reuse"
          , "*temp"
          , "*set"
          , "*delete"
          , "*input_number"
          , "*input_text"
          , "*print"
          , "*rand"
          , "*goto"
          , "*goto_scene"
          , "*gosub"
          , "*gosub_scene"
          , "*params"
          , "*return"
          , "*goto_random_scene"
          , "*finish"
          , "*line_break"
          , "*page_break"
          , "*link"
          , "*stat_chart"
          , "*achieve"
          , "*check_achievements"
          , "*ending"
          , "*create"
          , "*scene_list"
          , "*title"
          , "*author"
          , "*achievement"
          ]
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }

ident :: (TokenParsing m, Monad m, IsString s) => m s
ident = Trifecta.ident identStyle
{-# INLINE ident #-}

reserve :: (TokenParsing m, Monad m) => String -> m ()
reserve = Trifecta.reserve identStyle
{-# INLINE reserve #-}

op :: (TokenParsing m, Monad m) => String -> m ()
op = Trifecta.reserve opStyle
{-# INLINE op #-}

cmd :: (TokenParsing m, Monad m) => a -> String -> m a
cmd x c = x <$ Trifecta.reserve cmdStyle ("*" <> c)
{-# INLINE cmd #-}

someTill :: Alternative m => m a -> m end -> m [a]
someTill p end = (:) <$> p <*> manyTill p end
{-# INLINE someTill #-}

type family LocType (f :: HyperType)
newtype Loc h = Loc {getLoc :: LocType (GetHyperType h)}
  deriving (Generic)

withSpan :: (LocType h ~ Span, DeltaParsing m) => m (h # Ann Loc) -> m (Ann Loc # h)
withSpan p = (\s l a e -> Ann (Loc $ Span s e l) a) <$> position <*> Trifecta.line <*> p <*> position

withCaret :: (LocType h ~ Caret, DeltaParsing m) => m (h # Ann Loc) -> m (Ann Loc # h)
withCaret p = (\m l a -> Ann (Loc $ Caret m l) a) <$> position <*> Trifecta.line <*> p

type Parser = StateT Int (MUnlined Trifecta.Parser)

newtype MUnlined m a = MUnlined {runMUnlined :: Unlined m a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadState s
    , Alternative
    , MonadPlus
    , Parsing
    , CharParsing
    , TokenParsing
    )

instance DeltaParsing m => DeltaParsing (MUnlined m) where
  line = lift Trifecta.line
  {-# INLINEABLE line #-}

  position = lift position
  {-# INLINEABLE position #-}

  slicedWith f (MUnlined (Unlined x)) = MUnlined . Unlined $ slicedWith f x
  {-# INLINEABLE slicedWith #-}

  rend = lift rend
  {-# INLINEABLE rend #-}

  restOfLine = lift restOfLine
  {-# INLINEABLE restOfLine #-}

instance LookAheadParsing m => LookAheadParsing (MUnlined m) where
  lookAhead (MUnlined (Unlined x)) = MUnlined . Unlined $ lookAhead x
  {-# INLINEABLE lookAhead #-}

int :: Parser Int
int = fromInteger <$> token decimal

unwrapParser :: Parser a -> Trifecta.Parser a
unwrapParser = runUnlined . runMUnlined . flip evalStateT 0

deAnn :: Recursively HFunctor h => Ann a # h -> Pure # h
deAnn = unwrap $ \_ (Ann _ x) -> x

makeDerivings [''Show] [''Loc]
