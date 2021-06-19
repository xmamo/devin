module Parsers (
  Parser,
  declarations,
  declaration,
  statement,
  expression,
  unaryOperator,
  binaryOperator,
  assignOperator,
  identifier,
  comment
) where

import Control.Monad.Trans.Writer

import Parser (ParserT (ParserT))
import qualified Parser
import qualified Syntax

import qualified Parsers.Internal as Internal


type Parser m a = ParserT m (a, [Syntax.Comment])


declarations :: Applicative m => Parser m [Syntax.Declaration ()]
declarations = run Internal.declarations


declaration :: Applicative m => Parser m (Syntax.Declaration ())
declaration = run Internal.declaration


statement :: Applicative m => Parser m (Syntax.Statement ())
statement = run Internal.statement


expression :: Applicative m => Parser m (Syntax.Expression ())
expression = run Internal.expression


unaryOperator :: Applicative m => Parser m (Syntax.UnaryOperator ())
unaryOperator = run Internal.unaryOperator


binaryOperator :: Applicative m => Parser m (Syntax.BinaryOperator ())
binaryOperator = run Internal.binaryOperator


assignOperator :: Applicative m => Parser m (Syntax.AssignOperator ())
assignOperator = run Internal.assignOperator


identifier :: Applicative m => Parser m (Syntax.Identifier ())
identifier = run Internal.identifier


comment :: Applicative m => Parser m Syntax.Comment
comment = run Internal.comment


run :: Applicative m => Internal.Parser a -> Parser m a
run parser = ParserT \input ->
  let (result, comments) = runWriter (Parser.parseT parser input)
   in pure ((, comments) <$> result)
