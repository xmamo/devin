module Parser (
  Parser,
  ParserT (..),
  parse,
  position,
  satisfy,
  char,
  text,
  either,
  separatedBy,
  separatedBy1,
  label,
  commit,
  eoi
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import Input (Input (Input))
import qualified Input
import Result (Result)
import qualified Result
import Prelude hiding (either)


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
  pure value = ParserT (pure . Result.Success value)

  parser1 <*> parser2 = ParserT $ parseT parser1 >=> \case
    Result.Success f rest -> fmap f <$> parseT parser2 rest
    Result.Failure recoverable position expectations -> pure (Result.Failure recoverable position expectations)


instance Monad m => Monad (ParserT m) where
  parser >>= f = ParserT $ parseT parser >=> \case
    Result.Success value rest -> parseT (f value) rest
    Result.Failure recoverable position expectations -> pure (Result.Failure recoverable position expectations)


instance Monad m => MonadFail (ParserT m) where
  fail label = ParserT \(Input position _) -> pure (Result.Failure True position [Text.pack label])


instance Monad m => Alternative (ParserT m) where
  empty = ParserT \(Input position _) -> pure (Result.Failure True position [])

  -- TODO: This is probably not associative
  parser1 <|> parser2 = ParserT \input -> parseT parser1 input >>= \case
    Result.Success value rest -> pure (Result.Success value rest)

    Result.Failure True position1 expectations1 -> parseT parser2 input >>= \case
      Result.Success value rest -> pure (Result.Success value rest)

      Result.Failure recoverable2 position2 expectations2 -> case compare position1 position2 of
        LT | null expectations2 && not (null expectations1) -> pure (Result.Failure recoverable2 position1 expectations1)
        LT -> pure (Result.Failure recoverable2 position2 expectations2)
        EQ -> pure (Result.Failure recoverable2 position1 (expectations1 `union` expectations2))
        GT | null expectations1 && not (null expectations2) -> pure (Result.Failure recoverable2 position2 expectations2)
        GT -> pure (Result.Failure recoverable2 position1 expectations1)

    Result.Failure False position expectations -> pure (Result.Failure False position expectations)


instance MonadTrans ParserT where
  lift ma = ParserT \input -> (\a -> Result.Success a input) <$> ma


parse :: Parser a -> Input -> Result a
parse parser input = runIdentity (parseT parser input)


position :: Applicative m => Num a => ParserT m a
position = ParserT \input -> pure (Result.Success (Input.position input) input)


satisfy :: Applicative m => (Char -> Bool) -> ParserT m Char
satisfy f = ParserT \(Input position rest) -> case Text.uncons rest of
  Just (head, tail) | f head -> pure (Result.Success head (Input (position + 1) tail))
  _ -> pure (Result.Failure True position [])


char :: Applicative m => Char -> ParserT m Char
char c = ParserT \(Input position rest) -> case Text.stripPrefix (Text.singleton c) rest of
  Just rest -> pure (Result.Success c (Input (position + 1) rest))
  Nothing -> pure (Result.Failure True position [Text.pack (show c)])


text :: Applicative m => Text -> ParserT m Text
text t = ParserT \(Input position rest) -> case Text.stripPrefix t rest of
  Just rest -> pure (Result.Success t (Input (position + Text.length t) rest))
  Nothing -> pure (Result.Failure True position [Text.pack (show t)])


either :: Monad m => ParserT m a -> ParserT m b -> ParserT m (Either a b)
either parser1 parser2 = (Left <$> parser1) <|> (Right <$> parser2)


separatedBy :: Monad m => ParserT m a -> ParserT m b -> ParserT m [a]
separatedBy parser comma = separatedBy1 parser comma <|> pure []


separatedBy1 :: Monad m => ParserT m a -> ParserT m b -> ParserT m [a]
separatedBy1 parser comma = (:) <$> parser <*> many (comma *> parser)


label :: Monad m => Text -> ParserT m a -> ParserT m a
label l parser = ParserT \input @ (Input position _) -> parseT parser input >>= \case
  Result.Success value rest -> pure (Result.Success value rest)
  Result.Failure recoverable _ _ -> pure (Result.Failure recoverable position [l])


commit :: Monad m => ParserT m a -> ParserT m a
commit parser = ParserT $ parseT parser >=> \case
  Result.Success value rest -> pure (Result.Success value rest)
  Result.Failure _ position expectations -> pure (Result.Failure False position expectations)


eoi :: Applicative m => ParserT m ()
eoi = ParserT \input @ (Input position rest) ->
  if Text.null rest then
    pure (Result.Success () input)
  else
    pure (Result.Failure True position ["end of input"])
