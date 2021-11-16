module Parsers (
  Parser,
  devin,
  declaration,
  statement,
  expression,
  unaryOperator,
  binaryOperator,
  assignOperator,
  identifier,
  comment
) where

import Data.Functor

import Control.Monad.Trans.Writer

import Parser hiding (Parser, run)
import Syntax hiding (declaration)

import qualified Parsers.Internal as Internal


type Parser m a = ParserT m (a, [Comment])


devin :: Applicative m => Parser m Devin
devin = run Internal.devin


declaration :: Applicative m => Parser m Declaration
declaration = run Internal.declaration


statement :: Applicative m => Parser m Statement
statement = run Internal.statement


expression :: Applicative m => Parser m Expression
expression = run Internal.expression


unaryOperator :: Applicative m => Parser m UnaryOperator
unaryOperator = run Internal.unaryOperator


binaryOperator :: Applicative m => Parser m BinaryOperator
binaryOperator = run Internal.binaryOperator


assignOperator :: Applicative m => Parser m AssignOperator
assignOperator = run Internal.assignOperator


identifier :: Applicative m => Parser m Identifier
identifier = run Internal.identifier


comment :: Applicative m => Parser m Comment
comment = run Internal.comment


run :: Applicative m => Internal.Parser a -> Parser m a
run parser = ParserT $ \input -> do
  let (result, comments) = runWriter (runT parser input)
  pure (result <&> (, comments))
