module Parser (
  Parser (..),
  satisfy,
  char,
  text,
  position,
  label,
  commit
) where

import Data.List
import Control.Applicative

import Data.Text (Text)
import qualified Data.Text as Text

import Input (Input (Input))

import Result (Result)
import qualified Result


newtype Parser a where
  Parser :: {parse :: Input -> Result a} -> Parser a


instance Semigroup a => Semigroup (Parser a) where
  parser1 <> parser2 = liftA2 (<>) parser1 parser2


instance Monoid a => Monoid (Parser a) where
  mempty = pure mempty


instance Functor Parser where
  fmap f parser = Parser (fmap f . parse parser)


instance Applicative Parser where
  pure value = Parser (Result.Success value)


  parser1 <*> parser2 = Parser $ \input -> case parse parser1 input of
    Result.Success f rest -> f <$> parse parser2 rest
    Result.Failure recoverable position expectations -> Result.Failure recoverable position expectations


  liftA2 f parser1 parser2 = Parser $ \input -> case parse parser1 input of
    Result.Success value1 rest -> case parse parser2 rest of
      Result.Success value2 rest -> Result.Success (f value1 value2) rest
      Result.Failure recoverable position expectations -> Result.Failure recoverable position expectations

    Result.Failure recoverable position expectations -> Result.Failure recoverable position expectations


instance Monad Parser where
  parser >>= f = Parser $ \input -> case parse parser input of
    Result.Success value rest -> parse (f value) rest
    Result.Failure recoverable position expectations -> Result.Failure recoverable position expectations


instance MonadFail Parser where
  fail label = Parser $ \(Input position _) -> Result.Failure True position [Text.pack label]


instance Alternative Parser where
  empty = Parser $ \(Input position _) -> Result.Failure True position []


  -- TODO: This is probably not associative
  parser1 <|> parser2 = Parser $ \input -> case parse parser1 input of
    Result.Success value rest -> Result.Success value rest

    Result.Failure True position1 expectations1 -> case parse parser2 input of
      Result.Success value rest -> Result.Success value rest

      Result.Failure recoverable2 position2 expectations2 -> case compare position1 position2 of
        LT | null expectations2 && not (null expectations1) -> Result.Failure recoverable2 position1 expectations1
        LT -> Result.Failure recoverable2 position2 expectations2
        EQ -> Result.Failure recoverable2 position2 (expectations1 `union` expectations2)
        GT | null expectations1 && not (null expectations1) -> Result.Failure recoverable2 position2 expectations2
        GT -> Result.Failure recoverable2 position1 expectations1

    Result.Failure False position expectations -> Result.Failure False position expectations


satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \(Input position text) -> case Text.uncons text of
  Just (head, tail) | f head -> Result.Success head (Input (position + 1) tail)
  _ -> Result.Failure True position []


char :: Char -> Parser Char
char c = Parser $ \(Input position text) -> case Text.stripPrefix (Text.singleton c) text of
  Just rest -> Result.Success c (Input (position + 1) rest)
  Nothing -> Result.Failure True position ["char " <> Text.pack (show c)]



text :: Text -> Parser Text
text t = Parser $ \(Input position text) -> case Text.stripPrefix t text of
  Just rest -> Result.Success t (Input (position + Text.length t) rest)
  Nothing -> Result.Failure True position ["text " <> Text.pack (show t)]


position :: Parser Int
position = Parser $ \(Input position text) -> Result.Success position (Input position text)


label :: Text -> Parser a -> Parser a
label l parser = Parser $ \(Input position text) -> case parse parser (Input position text) of
  Result.Success value rest -> Result.Success value rest
  Result.Failure recoverable _ _ -> Result.Failure recoverable position [l]


commit :: Parser a -> Parser a
commit parser = Parser $ \input -> case parse parser input of
  Result.Success value rest -> Result.Success value rest
  Result.Failure _ position expectations -> Result.Failure False position expectations
