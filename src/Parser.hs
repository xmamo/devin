module Parser(
  Parser,
  ParserT (..),
  parse,
  eoi,
  satisfy,
  char,
  text,
  position,
  separatedBy,
  separatedBy1,
  label,
  commit
) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.List

import Control.Monad.Trans.Class

import Data.Text (Text)
import qualified Data.Text as Text

import Input (Input (Input))
import qualified Input
import Result (Result)
import qualified Result


type Parser = ParserT Identity


newtype ParserT m a where
  ParserT :: {parseT :: Input -> m (Result a)} -> ParserT m a


instance (Monad m, Semigroup a) => Semigroup (ParserT m a) where
  parser1 <> parser2 = (<>) <$> parser1 <*> parser2


instance (Monad m, Monoid a) => Monoid (ParserT m a) where
  mempty = pure mempty


instance Functor m => Functor (ParserT m) where
  fmap f parser = ParserT ((fmap f <$>) . parseT parser)


instance Monad m => Applicative (ParserT m) where
  pure value = ParserT $ \input -> pure (Result.Success value input)

  parser1 <*> parser2 = ParserT $ parseT parser1 >=> \case
    Result.Success f rest -> fmap f <$> parseT parser2 rest
    Result.Failure recoverable position expectations -> pure (Result.Failure recoverable position expectations)


instance Monad m => Monad (ParserT m) where
  parser >>= f = ParserT $ parseT parser >=> \case
    Result.Success value rest -> parseT (f value) rest
    Result.Failure recoverable position expectations -> pure (Result.Failure recoverable position expectations)


instance Monad m => MonadFail (ParserT m) where
  fail label = ParserT $ \(Input position _) -> pure (Result.Failure True position [Text.pack label])


instance Monad m => Alternative (ParserT m) where
  empty = ParserT $ \(Input position _) -> pure (Result.Failure True position [])

  -- TODO: This is probably not associative
  parser1 <|> parser2 = ParserT $ \input -> parseT parser1 input >>= \case
    Result.Success value rest -> pure (Result.Success value rest)

    Result.Failure True position1 expectations1 -> parseT parser2 input >>= \case
      Result.Success value rest -> pure (Result.Success value rest)

      Result.Failure recoverable2 position2 expectations2 -> case compare position1 position2 of
        LT | null expectations2 && not (null expectations1) -> pure (Result.Failure recoverable2 position1 expectations1)
        LT -> pure (Result.Failure recoverable2 position2 expectations2)
        EQ -> pure (Result.Failure recoverable2 position2 (expectations1 `union` expectations2))
        GT | null expectations1 && not (null expectations1) -> pure (Result.Failure recoverable2 position2 expectations2)
        GT -> pure (Result.Failure recoverable2 position1 expectations1)

    Result.Failure False position expectations -> pure (Result.Failure False position expectations)


instance MonadTrans ParserT where
  lift ma = ParserT $ \input -> (\a -> Result.Success a input) <$> ma


parse :: Parser a -> Input -> Result a
parse parser input = runIdentity (parseT parser input)


eoi :: Applicative m => ParserT m ()
eoi = ParserT $ \input@(Input position text) ->
  if Text.null text then
    pure (Result.Success () input)
  else
    pure (Result.Failure True position ["End of input"])


satisfy :: Applicative m => (Char -> Bool) -> ParserT m Char
satisfy f = ParserT $ \(Input position text) -> case Text.uncons text of
  Just (head, tail) | f head -> pure (Result.Success head (Input (position + 1) tail))
  _ -> pure (Result.Failure True position [])


char :: Applicative m => Char -> ParserT m Char
char c = ParserT $ \(Input position text) -> case Text.stripPrefix (Text.singleton c) text of
  Just rest -> pure (Result.Success c (Input (position + 1) rest))
  Nothing -> pure (Result.Failure True position [Text.pack (show c)])


text :: Applicative m => Text -> ParserT m Text
text t = ParserT $ \(Input position text) -> case Text.stripPrefix t text of
  Just rest -> pure (Result.Success t (Input (position + Text.length t) rest))
  Nothing -> pure (Result.Failure True position [Text.pack (show t)])


position :: Applicative m => Num a => ParserT m a
position = ParserT $ \input -> pure (Result.Success (Input.position input) input)


separatedBy :: Monad m => ParserT m a -> ParserT m b -> ParserT m [a]
separatedBy parser separator = separatedBy1 parser separator <|> pure []


separatedBy1 :: Monad m => ParserT m a -> ParserT m b -> ParserT m [a]
separatedBy1 parser separator = (:) <$> parser <*> many (separator *> parser)


label :: Monad m => Text -> ParserT m a -> ParserT m a
label l parser = ParserT $ \input@(Input position _) -> parseT parser input >>= \case
  Result.Success value rest -> pure (Result.Success value rest)
  Result.Failure recoverable _ _ -> pure (Result.Failure recoverable position [l])


commit :: Monad m => ParserT m a -> ParserT m a
commit parser = ParserT $ parseT parser >=> \case
  Result.Success value rest -> pure (Result.Success value rest)
  Result.Failure _ position expectations -> pure (Result.Failure False position expectations)
