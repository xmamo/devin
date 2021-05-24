module Parsers (
  declarations,
  variableDeclaration,
  variableAssignDeclaration,
  functionDeclaration,
  declaration,
  expressionStatement,
  ifStatement,
  ifElseStatement,
  whileStatement,
  doWhileStatement,
  returnStatement,
  blockStatement,
  statement,
  integerExpression,
  identifierExpression,
  callExpression,
  unaryExpression,
  binaryExpression,
  assignExpression,
  parenthesizedExpression,
  expression,
  unaryOperator,
  binaryOperator,
  assignOperator,
  identifier,
  comment
) where

import Data.Char
import Data.Foldable
import Data.Functor
import Control.Applicative

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer

import Data.Text (Text)
import qualified Data.Text as Text

import Parser (ParserT)
import qualified Parser

import Span (Span (Span))
import qualified Syntax

import qualified Helpers


type Parser = ParserT (Writer [Syntax.Comment])


declarations :: Parser [Syntax.Declaration]
declarations = s *> Parser.separatedBy declaration s <* s <* Parser.eoi


variableDeclaration :: Parser Syntax.Declaration
variableDeclaration = do
  t <- identifier
  variable <- s *> identifier
  terminator <- s *> charToken ';'
  pure (Syntax.VariableDeclaration t variable terminator)


variableAssignDeclaration :: Parser Syntax.Declaration
variableAssignDeclaration = do
  t <- identifier
  variable <- s *> identifier
  equalSign <- s *> symbol "="

  Parser.commit $ do
    value <- s *> expression
    terminator <- s *> charToken ';'
    pure (Syntax.VariableAssignDeclaration t variable equalSign value terminator)


functionDeclaration :: Parser Syntax.Declaration
functionDeclaration = do
  t <- identifier
  name <- s *> identifier
  open <- s *> charToken '('

  Parser.commit $ do
    first <- optional ((,) <$> (s *> identifier) <*> Parser.commit (s *> identifier))

    parameters <- case first of
      Just first -> do
        rest <- many $ do
          separator <- s *> charToken ','

          Parser.commit $ do
            t <- s *> identifier
            name <- s *> identifier
            pure (separator, t, name)

        pure (Just (first, rest))

      Nothing -> pure Nothing

    close <- s *> charToken ')'
    body <- s *> statement
    pure (Syntax.FunctionDeclaration t name open parameters close body)


declaration :: Parser Syntax.Declaration
declaration = functionDeclaration <|> variableAssignDeclaration <|> variableDeclaration


expressionStatement :: Parser Syntax.Statement
expressionStatement = do
  value <- expression
  terminator <- Parser.commit (s *> charToken ';')
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
    terminator <- s *> charToken ';'
    pure (Syntax.DoWhileStatement doKeyword body whileKeyword predicate terminator)


returnStatement :: Parser Syntax.Statement
returnStatement = do
  returnKeyword <- keyword "return"
  value <- s*> expression
  terminator <- Parser.commit (s *> charToken ';')
  pure (Syntax.ReturnStatement returnKeyword value terminator)


blockStatement :: Parser Syntax.Statement
blockStatement = do
  open <- charToken '{'

  Parser.commit $ do
    elements <- s *> Parser.separatedBy ((Left <$> declaration) <|> (Right <$> statement)) s
    close <- s *> charToken '}'
    pure (Syntax.BlockStatement open elements close)


statement :: Parser Syntax.Statement
statement = asum
  [
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


callExpression :: Parser Syntax.Expression
callExpression = do
  target <- identifier
  open <- s *> charToken '('

  Parser.commit $ do
    first <- optional (s *> expression)

    arguments <- case first of
      Just first -> do
        rest <- many ((,) <$> (s *> charToken ',') <*> Parser.commit (s *> expression))
        pure (Just (first, rest))

      Nothing -> pure Nothing

    close <- s *> charToken ')'
    pure (Syntax.CallExpression target open arguments close)


operandExpression :: Parser Syntax.Expression
operandExpression = asum
  [
    parenthesizedExpression,
    integerExpression,
    callExpression,
    unaryExpression,
    identifierExpression
  ]


unaryExpression :: Parser Syntax.Expression
unaryExpression = do
  operator <- unaryOperator

  case operator of
    Syntax.NotOperator{} -> do
      operand <- s *> operandExpression
      pure (Syntax.UnaryExpression operator operand)

    _ -> do
      operand <- Parser.commit (s *> operandExpression)
      pure (Syntax.UnaryExpression operator operand)


binaryExpression :: Parser Syntax.Expression
binaryExpression = do
  left <- operandExpression
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
parenthesizedExpression = Syntax.ParenthesizedExpression <$> charToken '(' <*> (s *> expression) <*> (s *> charToken ')')


expression :: Parser Syntax.Expression
expression = asum
  [
    assignExpression,

    do
      left <- operandExpression
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
    keyword "not" $> Syntax.NotOperator
  ]


binaryOperator :: Parser Syntax.BinaryOperator
binaryOperator = syntax $ asum
  [
    symbol "+" $> Syntax.AddOperator,
    symbol "-" $> Syntax.SubtractOperator,
    symbol "*" $> Syntax.MultiplyOperator,
    symbol "/" $> Syntax.DivideOperator,
    symbol "%" $> Syntax.DivideOperator,
    symbol "==" $> Syntax.EqualOperator,
    symbol "!=" $> Syntax.NotEqualOperator,
    symbol "<" $> Syntax.LessOperator,
    symbol "<=" $> Syntax.LessOrEqualOperator,
    symbol ">" $> Syntax.GreaterOperator,
    symbol ">=" $> Syntax.GreaterOrEqualOperator,
    keyword "and" $> Syntax.AndOperator,
    keyword "or" $> Syntax.OrOperator
  ]


assignOperator :: Parser Syntax.AssignOperator
assignOperator = syntax $ asum
  [
    symbol "=" $> Syntax.AssignOperator,
    symbol "+=" $> Syntax.AddAssignOperator,
    symbol "-=" $> Syntax.SubtractAssignOperator,
    symbol "*=" $> Syntax.MultiplyAssignOperator,
    symbol "/=" $> Syntax.DivideAssignOperator,
    symbol "%=" $> Syntax.RemainderAssignOperator
  ]


-- [\p{L}\p{Nl}\p{Pc}][\p{L}\p{Nl}\p{Pc}\p{Mn}\p{Mc}\p{Nd}]*
identifier :: Parser Syntax.Identifier
identifier = Parser.label "identifier" . syntax $ do
  start <- gc [lu, ll, lt, lm, lo, nl, pc]
  continue <- Text.pack <$> many (gc [lu, ll, lt, lm, lo, nl, pc, mn, mc, nd])
  pure (Syntax.Identifier (Text.cons start continue))
  where
    gc list = Parser.satisfy (\c -> generalCategory c `elem` list)
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


comment :: Parser Syntax.Comment
comment = do
  start <- Parser.position
  Parser.text "//"
  many (Parser.satisfy (not . Helpers.isNewline))
  end <- Parser.position

  let comment = Syntax.Comment (Span start end)
  lift (tell [comment])
  pure (Syntax.Comment (Span start end))


s :: Parser Syntax.Token
s = token (many (void (some (Parser.satisfy isSpace)) <|> void comment))


keyword :: Text -> Parser Syntax.Token
keyword k = Parser.label ("keyword " <> k) $ do
  (Syntax.Identifier name span) <- identifier

  if name == k then
    pure (Syntax.Token span)
  else
    empty


symbol :: Text -> Parser Syntax.Token
symbol s = Parser.label s . syntax $ do
  s' <- Text.pack <$> some (Parser.satisfy isSymbol)

  if s' == s then
    pure Syntax.Token
  else
    empty


token :: Parser a -> Parser Syntax.Token
token parser = do
  start <- Parser.position
  parser
  end <- Parser.position
  pure (Syntax.Token (Span start end))


charToken :: Char -> Parser Syntax.Token
charToken = token . Parser.char


syntax :: Parser (Span -> a) -> Parser a
syntax parser = do
  start <- Parser.position
  f <- parser
  end <- Parser.position
  pure (f (Span start end))


binary :: Syntax.Expression -> Syntax.BinaryOperator -> Syntax.Expression -> Syntax.Expression

binary Syntax.BinaryExpression{} _ _ = undefined

binary left operator (Syntax.BinaryExpression rLeft rOperator rRight)
  | Syntax.comparePrecedence operator rOperator >= EQ =
    Syntax.BinaryExpression (Syntax.BinaryExpression left operator rLeft) rOperator rRight

binary left operator right = Syntax.BinaryExpression left operator right
