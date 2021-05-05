module Parsers (
  expression,
  binaryExpression,
  unaryExpression,
  parenthesizedExpression,
  primaryExpression,
  identifierExpression,
  integerExpression,
  unaryOperator,
  binaryOperator
) where

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


expression :: Parser Syntax.Expression
expression = binaryExpression <|> unaryExpression <|> parenthesizedExpression <|> primaryExpression


binaryExpression :: Parser Syntax.Expression
binaryExpression = syntax $ do
  left <- unaryExpression <|> parenthesizedExpression <|> primaryExpression
  operator <- space *> binaryOperator
  right <- Parser.commit (space *> expression)

  pure $ case right of
    Syntax.BinaryExpression rLeft rOperator rRight _ | Syntax.precedence operator >= Syntax.precedence rOperator ->
      let Span start _ = Syntax.span left
          Span _ end = Syntax.span rLeft
          left' = Syntax.BinaryExpression left operator rLeft (Span start end)
      in Syntax.BinaryExpression left' rOperator rRight

    _ -> Syntax.BinaryExpression left operator right


unaryExpression :: Parser Syntax.Expression
unaryExpression = syntax $ do
  operator <- unaryOperator
  operand <- Parser.commit(space *> (unaryExpression <|> parenthesizedExpression <|> primaryExpression))
  pure (Syntax.UnaryExpression operator operand)


parenthesizedExpression :: Parser Syntax.Expression
parenthesizedExpression = syntax $ do
  Parser.char '('
  Syntax.ParenthesizedExpression <$> Parser.commit (space *> expression <* space <* Parser.char ')')


primaryExpression :: Parser Syntax.Expression
primaryExpression = identifierExpression <|> integerExpression


integerExpression :: Parser Syntax.Expression
integerExpression = Parser.label "integer" . syntax $ do
  sign <- (Parser.char '+' $> 1) <|> (Parser.char '-' $> (-1)) <|> pure 1
  digits <- some (Parser.satisfy isDigit)
  let magnitude = foldl' (\a d -> 10 * a + toInteger (digitToInt d)) 0 digits
  pure (Syntax.IntegerExpression (sign * magnitude))


-- (?:\p{Nd}[\p{Mn}\p{Mc}]*)*[\p{L}\p{Nl}\p{Pc}][\p{L}\p{Nl}\p{Pc}\p{Mn}\p{Mc}\p{Nd}]*
identifierExpression :: Parser Syntax.Expression
identifierExpression = Parser.label "identifier" . syntax $ do
  a <- Text.concat <$> many (gc [nd] <> (Text.concat <$> many (gc [mn, mc])))
  b <- gc [lu, ll, lt, lm, lo, nl, pc]
  c <- Text.concat <$> many (gc [lu, ll, lt, lm, lo, nl, pc, mn, mc, nd])
  pure (Syntax.IdentifierExpression (a <> b <> c))
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


space :: Parser Text
space = Text.concat <$> many (Text.singleton <$> Parser.satisfy isSpace)


syntax :: Parser (Span -> a) -> Parser a
syntax parser = do
  start <- Parser.position
  f <- parser
  f . Span start <$> Parser.position
