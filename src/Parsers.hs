module Parsers (
  identifier,
  declareStatement,
  declareAndAssignStatement,
  expressionStatement,
  ifStatement,
  ifElseStatement,
  whileStatement,
  doWhileStatement,
  returnStatement,
  blockStatement,
  statement,
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


declareStatement :: Parser Syntax.Statement
declareStatement = do
  varKeyword <- keyword "var"
  variable <- s *> identifier
  terminator <- Parser.commit (s *> token (Parser.char ';'))
  pure (Syntax.DeclareStatement varKeyword variable terminator)


declareAndAssignStatement :: Parser Syntax.Statement
declareAndAssignStatement = do
  varKeyword <- keyword "var"
  variable <- s *> identifier
  equalSign <- s *> token (Parser.char '=')

  Parser.commit $ do
    value <- s *> expression
    terminator <- s *> token (Parser.char ';')
    pure (Syntax.DeclareAndAssignStatement varKeyword variable equalSign value terminator)


expressionStatement :: Parser Syntax.Statement
expressionStatement = do
  value <- expression
  terminator <- Parser.commit (s *> token (Parser.char ';'))
  pure (Syntax.ExpressionStatement value terminator)


ifStatement :: Parser Syntax.Statement
ifStatement = do
  ifKeyword <- keyword "if"
  predicate <- s *> expression
  trueBranch <- Parser.commit (s *> statement)
  pure (Syntax.IfStatement ifKeyword predicate trueBranch)


ifElseStatement :: Parser Syntax.Statement
ifElseStatement = do
  ifKeyword <- keyword "if"
  predicate <- s *> expression
  trueBranch <- Parser.commit (s *> statement)
  elseKeyword <- s *> keyword "else"
  falseBranch <- Parser.commit (s *> statement)
  pure (Syntax.IfElseStatement ifKeyword predicate trueBranch elseKeyword falseBranch)


whileStatement :: Parser Syntax.Statement
whileStatement = do
  whileKeyword <- keyword "while"
  predicate <- s *> expression
  body <- Parser.commit (s *> statement)
  pure (Syntax.WhileStatement whileKeyword predicate body)


doWhileStatement :: Parser Syntax.Statement
doWhileStatement = do
  doKeyword <- keyword "do"
  body <- s *> statement

  Parser.commit $ do
    whileKeyword <- s *> keyword "while"
    predicate <- s *> expression
    terminator <- s *> token (Parser.char ';')
    pure (Syntax.DoWhileStatement doKeyword body whileKeyword predicate terminator)


returnStatement :: Parser Syntax.Statement
returnStatement = do
  returnKeyword <- keyword "return"
  value <- s*> expression
  terminator <- Parser.commit (s *> token (Parser.char ';'))
  pure (Syntax.ReturnStatement returnKeyword value terminator)


blockStatement :: Parser Syntax.Statement
blockStatement = do
  open <- token (Parser.char '{')

  Parser.commit $ do
    statements <- s *> Parser.separatedBy statement s
    close <- s *> token (Parser.char '}')
    pure (Syntax.BlockStatement open statements close)


statement :: Parser Syntax.Statement
statement = asum
  [
    do
      varKeyword <- keyword "var"
      variable <- s *> identifier <* s
      equalSign <- optional (token (Parser.char '='))

      case equalSign of
        Just equalSign -> Parser.commit $ do
          value <- s *> expression
          terminator <- s *> token (Parser.char ';')
          pure (Syntax.DeclareAndAssignStatement varKeyword variable equalSign value terminator)

        Nothing -> do
          terminator <- Parser.commit (s *> token (Parser.char ';'))
          pure (Syntax.DeclareStatement varKeyword variable terminator),

    do
      ifKeyword <- keyword "if"
      predicate <- s *> expression
      trueBranch <- Parser.commit (s *> statement)
      elseKeyword <- optional (s *> keyword "else")

      case elseKeyword of
        Just elseKeyword -> do
          falseBranch <- Parser.commit (s *> statement)
          pure (Syntax.IfElseStatement ifKeyword predicate trueBranch elseKeyword falseBranch)

        Nothing -> pure (Syntax.IfStatement ifKeyword predicate trueBranch),

    whileStatement,
    doWhileStatement,
    returnStatement,
    blockStatement,
    expressionStatement
  ]


integerExpression :: Parser Syntax.Expression
integerExpression = Parser.label "integer" . syntax $ do
  sign <- (Parser.char '+' $> 1) <|> (Parser.char '-' $> -1) <|> pure 1
  digits <- some (Parser.satisfy isDigit)
  let magnitude = foldl' (\a d -> 10 * a + toInteger (digitToInt d)) 0 digits
  pure (Syntax.IntegerExpression (sign * magnitude))


identifierExpression :: Parser Syntax.Expression
identifierExpression = Syntax.IdentifierExpression <$> identifier


primaryExpression :: Parser Syntax.Expression
primaryExpression = identifierExpression <|> integerExpression


unaryExpression :: Parser Syntax.Expression
unaryExpression = do
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
assignExpression = do
  target <- identifier
  operator <- s *> assignOperator
  value <- Parser.commit (s *> expression)
  pure (Syntax.AssignExpression target operator value)


parenthesizedExpression :: Parser Syntax.Expression
parenthesizedExpression = do
  open <- token (Parser.char '(')

  Parser.commit $ do
    value <- s *> expression
    close <- s *> token (Parser.char ')')
    pure (Syntax.ParenthesizedExpression open value close)


expression :: Parser Syntax.Expression
expression = asum
  [
    assignExpression,

    do
      left <- unaryExpression <|> parenthesizedExpression <|> primaryExpression
      operator <- optional (s *> binaryOperator)

      case operator of
        Just operator -> do
          right <- Parser.commit (s *> expression)
          pure (binary left operator right)

        Nothing -> pure left
  ]


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
    keyword "and" $> Syntax.AndOperator,
    keyword "or" $> Syntax.OrOperator
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


token :: Parser a -> Parser Syntax.Token
token parser = do
  start <- Parser.position
  parser
  end <- Parser.position
  pure (Syntax.Token (Span start end))


syntax :: Parser (Span -> a) -> Parser a
syntax parser = do
  start <- Parser.position
  f <- parser
  end <- Parser.position
  pure (f (Span start end))


keyword :: Text -> Parser Syntax.Token
keyword k = Parser.label ("keyword " <> k) $ do
  (Syntax.Identifier name span) <- identifier

  if name == k then
    pure (Syntax.Token span)
  else
    empty


binary :: Syntax.Expression -> Syntax.BinaryOperator -> Syntax.Expression -> Syntax.Expression

binary Syntax.BinaryExpression {} _ _ = undefined

binary left operator (Syntax.BinaryExpression rLeft rOperator rRight)
  | Syntax.precedence operator >= Syntax.precedence rOperator =
    Syntax.BinaryExpression (Syntax.BinaryExpression left operator rLeft) rOperator rRight

binary left operator right = Syntax.BinaryExpression left operator right
