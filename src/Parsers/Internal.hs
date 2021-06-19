module Parsers.Internal (
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

import Control.Applicative
import Data.Char
import Data.Foldable
import Data.Functor

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer

import Parser (ParserT)
import qualified Parser
import Span (Span (Span))
import qualified Syntax
import qualified Unicode


type Parser = ParserT (Writer [Syntax.Comment])


declarations :: Parser [Syntax.Declaration ()]
declarations = s *> Parser.separatedBy declaration s <* s <* Parser.eoi


variableDeclaration :: Parser (Syntax.Declaration ())
variableDeclaration = do
  varKeyword <- keyword "var"
  variableId <- s *> identifier
  typeInfo <- optional ((,) <$> (s *> charToken ':') <*> Parser.commit (s *> identifier))

  Parser.commit do
    equalSign <- s *> charToken '='
    value <- s *> expression
    semicolon <- s *> charToken ';'
    pure (Syntax.VariableDeclaration varKeyword variableId typeInfo equalSign value semicolon)


functionDeclaration :: Parser (Syntax.Declaration ())
functionDeclaration = do
  defKeyword <- keyword "def"

  Parser.commit do
    functionId <- s *> identifier
    open <- s *> charToken '('

    first <- optional do
      id <- s *> identifier
      Parser.commit ((id,,) <$> (s *> charToken ':') <*> (s *> identifier))

    parameters <- case first of
      Just (id, colon, typeId) -> do
        rest <- many do
          comma <- s *> charToken ','
          Parser.commit ((comma,,,) <$> (s *> identifier) <*> (s *> charToken ':') <*> (s *> identifier))

        pure (Just (id, colon, typeId, rest))

      Nothing -> pure Nothing

    close <- s *> charToken ')' <* s
    returnInfo <- optional ((,) <$> (s *> textToken "->") <*> Parser.commit (s *> identifier))
    body <- s *> statement
    pure (Syntax.FunctionDeclaration defKeyword functionId open parameters close returnInfo body)


declaration :: Parser (Syntax.Declaration ())
declaration = variableDeclaration <|> functionDeclaration


expressionStatement :: Parser (Syntax.Statement ())
expressionStatement = Syntax.ExpressionStatement <$> expression <*> Parser.commit (s *> charToken ';')


ifElseStatementOrIfStatement :: Parser (Syntax.Statement ())
ifElseStatementOrIfStatement = do
  ifKeyword <- keyword "if"

  Parser.commit do
    predicate <- s *> expression
    trueBranch <- s *> statement

    optional (s *> keyword "else") >>= \case
      Just elseKeyword -> do
        falseBranch <- Parser.commit (s *> statement)
        pure (Syntax.IfElseStatement ifKeyword predicate trueBranch elseKeyword falseBranch)

      Nothing -> pure (Syntax.IfStatement ifKeyword predicate trueBranch)


whileStatement :: Parser (Syntax.Statement ())
whileStatement = do
  whileKeyword <- keyword "while"

  Parser.commit do
    predicate <- s *> expression
    body <- s *> statement
    pure (Syntax.WhileStatement whileKeyword predicate body)


doWhileStatement :: Parser (Syntax.Statement ())
doWhileStatement = do
  doKeyword <- keyword "do"

  Parser.commit do
    body <- s *> statement
    whileKeyword <- s *> keyword "while"
    predicate <- s *> expression
    semicolon <- s *> charToken ';'
    pure (Syntax.DoWhileStatement doKeyword body whileKeyword predicate semicolon)


returnStatement :: Parser (Syntax.Statement ())
returnStatement = do
  returnKeyword <- keyword "return"
  result <- optional (s *> expression)
  semicolon <- Parser.commit (s *> charToken ';')
  pure (Syntax.ReturnStatement returnKeyword result semicolon)


blockStatement :: Parser (Syntax.Statement ())
blockStatement = do
  open <- charToken '{'

  Parser.commit do
    elements <- s *> Parser.separatedBy (Parser.either declaration statement) s
    close <- s *> charToken '}'
    pure (Syntax.BlockStatement open elements close)


statement :: Parser (Syntax.Statement ())
statement = asum
  [
    blockStatement,
    ifElseStatementOrIfStatement,
    whileStatement,
    doWhileStatement,
    returnStatement,
    expressionStatement
  ]


integerExpression :: Parser (Syntax.Expression ())
integerExpression = Parser.label "integer" $ syntax do
  sign <- (Parser.char '+' $> 1) <|> (Parser.char '-' $> -1) <|> pure 1
  digits <- some (Parser.satisfy isDigit)
  let magnitude = foldl' (\a d -> 10 * a + toInteger (digitToInt d)) 0 digits
  pure \span -> Syntax.IntegerExpression span (sign * magnitude) ()


rationalExpression :: Parser (Syntax.Expression ())
rationalExpression = Parser.label "rational" $ syntax do
  sign <- (Parser.char '+' $> 1) <|> (Parser.char '-' $> -1) <|> pure 1
  digits1 <- some (Parser.satisfy isDigit)
  Parser.char '.'
  digits2 <- some (Parser.satisfy isDigit)
  let magnitude = foldl' (\a d -> 10 * a + toRational (digitToInt d)) 0 (digits1 ++ digits2) / 10 ^^ length digits2
  pure \span -> Syntax.RationalExpression span (sign * magnitude) ()


literalExpression :: Parser (Syntax.Expression ())
literalExpression = rationalExpression <|> integerExpression


variableExpression :: Parser (Syntax.Expression ())
variableExpression = Syntax.VariableExpression <$> identifier <*> pure ()


callExpression :: Parser (Syntax.Expression ())
callExpression = do
  targetId <- identifier
  open <- s *> charToken '('

  Parser.commit do
    arguments <-
      optional (s *> expression) >>= \case
        Just first -> do
          rest <- many ((,) <$> (s *> charToken ',') <*> Parser.commit (s *> expression))
          pure (Just (first, rest))

        Nothing -> pure Nothing

    close <- s *> charToken ')'
    pure (Syntax.CallExpression targetId open arguments close ())


unaryExpression :: Parser (Syntax.Expression ())
unaryExpression = do
  unary <- unaryOperator

  operand <- case unary of
    Syntax.NotOperator{} -> s *> operandExpression
    _ -> Parser.commit (s *> operandExpression)

  pure (Syntax.UnaryExpression unary operand ())


operandExpression :: Parser (Syntax.Expression ())
operandExpression = literalExpression <|> callExpression <|> unaryExpression <|> variableExpression


binaryExpressionOrOperandExpression :: Parser (Syntax.Expression ())
binaryExpressionOrOperandExpression = do
  left <- operandExpression

  optional (s *> binaryOperator) >>= \case
    Just binary -> do
      right <- Parser.commit (s *> expression)
      pure (newBinary left binary right)

    Nothing -> pure left


assignExpression :: Parser (Syntax.Expression ())
assignExpression = do
  targetId <- identifier
  assign <- s *> assignOperator
  value <- Parser.commit (s *> expression)
  pure (Syntax.AssignExpression targetId assign value ())


parenthesizedExpression :: Parser (Syntax.Expression ())
parenthesizedExpression = do
  open <- charToken '('

  Parser.commit do
    inner <- s *> expression
    close <- charToken ')'
    pure (Syntax.ParenthesizedExpression open inner close ())


expression :: Parser (Syntax.Expression ())
expression = parenthesizedExpression <|> assignExpression <|> binaryExpressionOrOperandExpression


unaryOperator :: Parser (Syntax.UnaryOperator ())
unaryOperator = fmap ($ ()) . syntax $ asum
  [
    Parser.char '+' $> Syntax.PlusOperator,
    Parser.char '-' $> Syntax.MinusOperator,
    keyword "not" $> Syntax.NotOperator
  ]


binaryOperator :: Parser (Syntax.BinaryOperator ())
binaryOperator = fmap ($ ()) . syntax $ asum
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

assignOperator :: Parser (Syntax.AssignOperator ())
assignOperator = fmap ($ ()) . syntax $ asum
  [
    Parser.char '=' $> Syntax.AssignOperator,
    Parser.text "+=" $> Syntax.AddAssignOperator,
    Parser.text "-=" $> Syntax.SubtractAssignOperator,
    Parser.text "*=" $> Syntax.MultiplyAssignOperator,
    Parser.text "/=" $> Syntax.DivideAssignOperator,
    Parser.text "%=" $> Syntax.RemainderAssignOperator
  ]


-- [\p{L}\p{Nl}\p{Pc}][\p{L}\p{Nl}\p{Pc}\p{Mn}\p{Mc}\p{Nd}]*
identifier :: Parser (Syntax.Identifier ())
identifier = Parser.label "identifier" $ syntax do
  start <- category [lu, ll, lt, lm, lo, nl, pc]
  continue <- many (category [lu, ll, lt, lm, lo, nl, pc, mn, mc, nd])
  pure \span -> Syntax.Identifier span (Text.pack (start : continue)) ()
  where
    category categories = Parser.satisfy \c -> Unicode.category c `elem` categories
    lu = Unicode.UppercaseLetter
    ll = Unicode.LowercaseLetter
    lt = Unicode.TitlecaseLetter
    lm = Unicode.ModifierLetter
    lo = Unicode.OtherLetter
    mn = Unicode.NonspacingMark
    mc = Unicode.SpacingMark
    nd = Unicode.DecimalNumber
    nl = Unicode.LetterNumber
    pc = Unicode.ConnectorPunctuation


comment :: Parser Syntax.Comment
comment = do
  start <- Parser.position
  Parser.text "//"
  many (Parser.satisfy (not . Unicode.isNewline))
  end <- Parser.position

  let comment = Syntax.Comment (Span start end)
  lift (tell [comment])
  pure comment


s :: Parser Syntax.Token
s = token (many (void (some (Parser.satisfy Unicode.isSpace)) <|> void comment))


keyword :: Text -> Parser Syntax.Token
keyword k = Parser.label ("keyword " <> k) do
  Syntax.Identifier{s, name} <- identifier

  if Unicode.collate name == Unicode.collate k then
    pure (Syntax.Token s)
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


textToken :: Text -> Parser Syntax.Token
textToken = token . Parser.text


syntax :: Parser (Span -> a) -> Parser a
syntax parser = do
  start <- Parser.position
  f <- parser
  end <- Parser.position
  pure (f (Span start end))


newBinary :: Syntax.Expression () -> Syntax.BinaryOperator () -> Syntax.Expression () -> Syntax.Expression ()
newBinary left binary right = case (left, right) of
  (Syntax.BinaryExpression{}, _) -> undefined

  (_, right@Syntax.BinaryExpression{})
    | Syntax.comparePrecedence binary right.binary >= EQ ->
      Syntax.BinaryExpression (Syntax.BinaryExpression left binary right.left ()) right.binary right.right ()

  _ -> Syntax.BinaryExpression left binary right ()
