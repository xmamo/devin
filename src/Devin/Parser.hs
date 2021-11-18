module Devin.Parser (
  Parser,
  ParserT (..),
  parser,
  run,
  position,
  satisfy,
  oneOf,
  noneOf,
  text,
  eoi,
  separatedBy,
  separatedBy1,
  commit,
  label
) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.List

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.Trans.Class

import Devin.Parser.Input hiding (position)
import Devin.Parser.Result hiding (position)


type Parser = ParserT Identity


newtype ParserT m a = ParserT {runT :: Input -> m (Result a)}
  deriving Functor


instance (Monad m, Semigroup a) => Semigroup (ParserT m a) where
  parser1 <> parser2 = liftA2 (<>) parser1 parser2


instance (Monad m, Monoid a) => Monoid (ParserT m a) where
  mempty = parser (Success mempty)


instance Monad m => Applicative (ParserT m) where
  pure x = parser (Success x)

  parser1 <*> parser2 = ParserT $ \input -> do
    result1 <- runT parser1 input

    case result1 of
      Success f rest -> do
        result2 <- runT parser2 rest
        pure (f <$> result2)

      Failure isFatal position expectations ->
        pure (Failure isFatal position expectations)


instance Monad m => Monad (ParserT m) where
  parser >>= f = ParserT $ \input -> do
    result <- runT parser input

    case result of
      Success x rest ->
        runT (f x) rest

      Failure isFatal position expectations ->
        pure (Failure isFatal position expectations)


instance MonadFail m => MonadFail (ParserT m) where
  fail message = ParserT $ \_ -> fail message


instance Monad m => Alternative (ParserT m) where
  empty = parser $ \(Input position _) -> Failure False position []

  parser1 <|> parser2 = ParserT $ \input -> do
    result1 <- runT parser1 input

    case result1 of
      Success _ _ -> pure result1

      Failure True _ _ -> pure result1

      Failure False position1 expectations1 -> do
        result2 <- runT parser2 input

        case result2 of
          Success _ _ -> pure result2

          Failure isFatal2 position2 expectations2 -> do
            let (position, expectations) = case compare position1 position2 of
                  LT -> (position2, expectations2)
                  EQ -> (position1, expectations1 `union` expectations2)
                  GT -> (position1, expectations1)

            pure (Failure isFatal2 position expectations)


instance Monad m => MonadPlus (ParserT m)


instance MonadTrans ParserT where
  lift mx = ParserT $ \input -> do
    x <- mx
    pure (Success x input)


parser :: Applicative m => (Input -> Result a) -> ParserT m a
parser f = ParserT $ \input -> pure (f input)


run :: Parser a -> Input -> Result a
run parser input = runIdentity (runT parser input)


position :: (Applicative m, Num a) => ParserT m a
position = parser $ \input -> Success (fromIntegral input.position) input


satisfy :: Applicative m => (Char -> Bool) -> ParserT m Char
satisfy f = parser $ \(Input position rest) ->
  case Text.uncons rest of
    Just (c, suffix) | f c -> Success c (Input (position + 1) suffix)
    _ -> Failure False position []


oneOf :: Applicative m => Set Char -> ParserT m Char
oneOf chars = parser $ \(Input position rest) ->
  case Text.uncons rest of
    Just (c, suffix) | Set.member c chars -> Success c (Input (position + 1) suffix)
    _ -> Failure False position ["one of " <> Text.pack (show chars)]


noneOf :: Applicative m => Set Char -> ParserT m Char
noneOf chars = parser $ \(Input position rest) ->
  case Text.uncons rest of
    Just (c, suffix) | Set.member c chars -> Success c (Input (position + 1) suffix)
    _ -> Failure False position ["none of " <> Text.pack (show chars)]


text :: Applicative m => Text -> ParserT m Text
text t = parser $ \(Input position rest) ->
  case Text.stripPrefix t rest of
    Just suffix -> Success t (Input (position + Text.length t) suffix)
    Nothing -> Failure False position [Text.pack (show t)]


eoi :: Applicative m => ParserT m ()
eoi = parser $ \input ->
  if Text.null input.rest then
    Success () input
  else
    Failure False input.position ["end of input"]


separatedBy :: Monad m => ParserT m a -> ParserT m b -> ParserT m [a]
parser `separatedBy` separator = (parser `separatedBy1` separator) <|> pure []


separatedBy1 :: Monad m => ParserT m a -> ParserT m b -> ParserT m [a]
parser `separatedBy1` separator = liftA2 (:) parser (many (separator *> parser))


commit :: Monad m => ParserT m a -> ParserT m a
commit parser = ParserT $ \input -> do
  result <- runT parser input

  case result of
    Success x rest -> pure (Success x rest)
    Failure _ position expectations -> pure (Failure True position expectations)


label :: Monad m => Text -> ParserT m a -> ParserT m a
label expectation parser = ParserT $ \input -> do
  result <- runT parser input

  case result of
    Success x rest -> pure (Success x rest)
    Failure isFatal _ _ -> pure (Failure isFatal input.position [expectation])
