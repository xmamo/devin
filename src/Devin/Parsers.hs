module Devin.Parsers (
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

import Devin.Parser hiding (Parser, run)
import Devin.Syntax hiding (declaration)

import qualified Devin.Parsers.Internal as Internal


type Parser m a = ParserT m (a, [Comment])


{-|
Parses Devin.

@
'devin' = {'declaration'};
@
-}
devin :: Applicative m => Parser m Devin
devin = run Internal.devin


{-|
Parses a declaration.

@
'declaration' = variableDeclaration | functionDeclaration;
variableDeclaration = "var", 'identifier', "=", 'expression' ";";
functionDeclaration = "def", 'identifier', parameters, ["->", 'identifier'], 'statement';
parameters = "(", [parameter, {",", parameter}], ")";
parameter = 'identifier', ":", 'identifier';
@
-}
declaration :: Applicative m => Parser m Declaration
declaration = run Internal.declaration


{-|
Parses a statement.

@
'statement' = 'declaration' | ifElseStatement | ifStatement | whileStatement | doWhileStatement | returnStatement | expressionStatement | blockStatement;
ifStatement = "if", 'expression', 'statement', ["else", 'statement'];
whileStatement = "while", 'expression', 'statement';
doWhileStatement = "do", 'statement', "while", 'expression', ";";
returnStatement = "return", ['expression'], ";";
expressionStatement = 'expression', ";";
blockStatement = "{", {'statement'}, "}";
@
-}
statement :: Applicative m => Parser m Statement
statement = run Internal.statement


{-|
Parses an expression.

@
'expression' = assignExpression | binaryExpression | operandExpression;
assignExpression = 'identifier', "=", 'expression';
binaryExpression = operandExpression, 'binaryOperator', 'expression';
operandExpression = numberExpression | callExpression | unaryExpression | 'identifier' | parenthesizedExpression;
numberExpression = digit, {digit}, [".", digit, {digit}];
callExpression = 'identifier', "(", ['expression', {",", 'expression'}], ")";
unaryExpression = 'unaryOperator', operandExpression;
parenthesizedExpression = "(", 'expression', ")";
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
@

For binary expressions, the syntax tree is built according to the following
precedence table:

+-----------------------------------+------------+
| Operators                         | Precedence |
+===================================+============+
| @*@, @/@, @%@                     | 6          |
+-----------------------------------+------------+
| @+@, @-@                          | 5          |
+-----------------------------------+------------+
| @\<@, @\<=@, @>@, @>=@            | 4          |
+-----------------------------------+------------+
| @==@, @!=@                        | 3          |
+-----------------------------------+------------+
| @and@, @or@                       | 2          |
+-----------------------------------+------------+
| @=@, @+=@, @-=@, @*=@, @/=@, @%=@ | 1          |
+-----------------------------------+------------+
-}
expression :: Applicative m => Parser m Expression
expression = run Internal.expression


{-|
Parses a unary operator.

@
'unaryOperator' = "+" | "-" | "not";
@
-}
unaryOperator :: Applicative m => Parser m UnaryOperator
unaryOperator = run Internal.unaryOperator


{-|
Parses a binary operator.

@
'binaryOperator' = "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "\<=" | "\<" | ">=" | ">" | "and" | "or";
@
-}
binaryOperator :: Applicative m => Parser m BinaryOperator
binaryOperator = run Internal.binaryOperator


{-|
Parses an assignment operator.

@
'assignOperator' = "=" | "+=" | "-=" | "/=" | "%=";
@
-}
assignOperator :: Applicative m => Parser m AssignOperator
assignOperator = run Internal.assignOperator


{-|
Parses an identifier.

An identifier is a sequence of characters matching the regular expression
@[\\p{L}\\p{Nl}\\p{Pc}][\\p{L}\\p{Nl}\\p{Pc}\\p{Mn}\\p{Mc}\\p{Nd}]*@. This
definition is loosely based on Unicode's recommendation for identifiers found
on [UAX #31](https://www.unicode.org/reports/tr31/#Default_Identifier_Syntax).
-}
identifier :: Applicative m => Parser m Identifier
identifier = run Internal.identifier


{-|
Parses a comment.

A comment is a sequence of characters matching the regular expression @//\\V*@.
-}
comment :: Applicative m => Parser m Comment
comment = run Internal.comment


{-|
Converts a @'ParserT' ('Writer' ['Comment'])@ to a
@'ParserT' m (a, ['Comment'])@ by calling @'runWriter'@ on the underlying monad.
-}
run :: Applicative m => Internal.Parser a -> Parser m a
run parser = ParserT $ \input -> do
  let (result, comments) = runWriter (runT parser input)
  pure (result <&> (, comments))
