module Parsers (
  identifier,
  identifierExpression,
  integerExpression,
  primaryExpression,
  unaryExpression,
  binaryExpression,
  assignExpression,
  parenthesizedExpression,
  expression,
  unaryOperator,
  binaryOperator,
  assignOperator
) where

import Prelude hiding (span)
import Data.Char
import Data.Foldable
import Data.Functor
import Control.Applicative

import Data.Text (Text)
import qualified Data.Text as Text

import Parser (Parser)
import qualified Parser

import Span (Span (Span))
import qualified Syntax


-- (?:\p{Nd}[\p{Mn}\p{Mc}]*)*[\p{L}\p{Nl}\p{Pc}][\p{L}\p{Nl}\p{Pc}\p{Mn}\p{Mc}\p{Nd}]*
identifier :: Parser Syntax.Identifier
identifier = Parser.label "identifier" . syntax $ do
  a <- Text.concat <$> many (gc [nd] <> (Text.concat <$> many (gc [mn, mc])))
  b <- gc [lu, ll, lt, lm, lo, nl, pc]
  c <- Text.concat <$> many (gc [lu, ll, lt, lm, lo, nl, pc, mn, mc, nd])
  pure (Syntax.Identifier (a <> b <> c))
  where
    gc list = Text.singleton <$> Parser.satisfy (\c -> generalCategory c `elem` list)
    lu = UppercaseLetter
    ll = LowercaseLetter
    lt = TitlecaseLetter
    lm = ModifierLetter
    lo = OtherLetter
    mn = NonSpacingMark
    mc = SpacingCombiningMark
    nd = DecimalNumber
    nl = LetterNumber
    pc = ConnectorPunctuation


integerExpression :: Parser Syntax.Expression
integerExpression = Parser.label "integer" . syntax $ do
  sign <- (Parser.char '+' $> 1) <|> (Parser.char '-' $> (-1)) <|> pure 1
  digits <- some (Parser.satisfy isDigit)
  let magnitude = foldl' (\a d -> 10 * a + toInteger (digitToInt d)) 0 digits
  pure (Syntax.IntegerExpression (sign * magnitude))


identifierExpression :: Parser Syntax.Expression
identifierExpression = syntax (Syntax.IdentifierExpression <$> identifier)


primaryExpression :: Parser Syntax.Expression
primaryExpression = identifierExpression <|> integerExpression


unaryExpression :: Parser Syntax.Expression
unaryExpression = syntax $ do
  operator <- unaryOperator
  operand <- Parser.commit(s *> (unaryExpression <|> parenthesizedExpression <|> primaryExpression))
  pure (Syntax.UnaryExpression operator operand)


binaryExpression :: Parser Syntax.Expression
binaryExpression = do
  left <- unaryExpression <|> parenthesizedExpression <|> primaryExpression
  operator <- s *> binaryOperator
  right <- Parser.commit (s *> expression)
  pure (binary left operator right)


assignExpression :: Parser Syntax.Expression
assignExpression = syntax $ do
  target <- identifier
  operator <- s *> assignOperator
  value <- Parser.commit (s *> expression)
  pure (Syntax.AssignExpression target operator value)


parenthesizedExpression :: Parser Syntax.Expression
parenthesizedExpression = syntax $ do
  open <- span (Parser.char '(')

  Parser.commit $ do
    e <- s *> expression
    close <- span (Parser.char ')')
    pure (Syntax.ParenthesizedExpression open e close)


expression :: Parser Syntax.Expression
expression = assignExpression <|> do
  left <- unaryExpression <|> parenthesizedExpression <|> primaryExpression
  operator <- optional (s *> binaryOperator)

  case operator of
    Just operator -> Parser.commit $ do
      right <- s *> expression
      pure (binary left operator right)

    Nothing -> pure left


unaryOperator :: Parser Syntax.UnaryOperator
unaryOperator = syntax $ asum
  [
    Parser.char '+' $> Syntax.PlusOperator,
    Parser.char '-' $> Syntax.MinusOperator,
    Parser.char '!' $> Syntax.NotOperator
  ]


binaryOperator :: Parser Syntax.BinaryOperator
binaryOperator = syntax $ asum
  [
    Parser.char '+' $> Syntax.AddOperator,
    Parser.char '-' $> Syntax.SubtractOperator,
    Parser.char '*' $> Syntax.MultiplyOperator,
    Parser.char '/' $> Syntax.DivideOperator,
    Parser.char '%' $> Syntax.DivideOperator,
    Parser.text "==" $> Syntax.EqualOperator,
    Parser.text "!=" $> Syntax.NotEqualOperator,
    Parser.text "<=" $> Syntax.LessOrEqualOperator,
    Parser.char '<' $> Syntax.LessOperator,
    Parser.text ">=" $> Syntax.GreaterOrEqualOperator,
    Parser.char '>' $> Syntax.GreaterOperator,
    Parser.char '&' $> Syntax.AndOperator,
    Parser.char '|' $> Syntax.OrOperator
  ]


assignOperator :: Parser Syntax.AssignOperator
assignOperator = syntax $ asum
  [
    Parser.char '=' $> Syntax.AssignOperator,
    Parser.text "+=" $> Syntax.AddAssignOperator,
    Parser.text "-=" $> Syntax.SubtractAssignOperator,
    Parser.text "*=" $> Syntax.MultiplyAssignOperator,
    Parser.text "/=" $> Syntax.DivideAssignOperator,
    Parser.text "%=" $> Syntax.RemainderAssignOperator
  ]


s :: Parser Text
s = Text.concat <$> many (Text.singleton <$> Parser.satisfy isSpace)


span :: Parser a -> Parser Span
span parser = do
  start <- Parser.position
  parser
  Span start <$> Parser.position


syntax :: Parser (Span -> a) -> Parser a
syntax parser = do
  start <- Parser.position
  f <- parser
  f . Span start <$> Parser.position


keyword :: Text -> Parser Span
keyword k = Parser.label ("keyword " <> k) $ do
  (Syntax.Identifier name span) <- identifier

  if name == k then
    pure span
  else
    empty


binary :: Syntax.Expression -> Syntax.BinaryOperator -> Syntax.Expression -> Syntax.Expression

binary Syntax.BinaryExpression {} _ _ = undefined

binary left operator right @ (Syntax.BinaryExpression rLeft rOperator rRight _)
  | Syntax.precedence operator >= Syntax.precedence rOperator =
    let left' = Syntax.BinaryExpression left operator rLeft (Span (Syntax.start left) (Syntax.end rLeft))
    in Syntax.BinaryExpression left' rOperator rRight (Span (Syntax.start left) (Syntax.end right))

binary left operator right = Syntax.BinaryExpression left operator right (Span (Syntax.start left) (Syntax.end right))
