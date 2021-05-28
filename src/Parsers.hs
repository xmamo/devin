module Parsers (
  declarations,
  declaration,
  statement,
  expression,
  unaryOperator,
  binaryOperator,
  assignOperator,
  integer,
  identifier,
  comment
) where

import Control.Applicative
import Data.Char
import Data.Foldable
import Data.Functor
import System.IO.Unsafe

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer

import Data.Text (Text)
import qualified Data.Text as Text

import qualified GI.GLib as GLib

import qualified Helpers
import Parser (ParserT)
import qualified Parser
import Span (Span (Span))
import qualified Syntax


type Parser = ParserT (Writer [Syntax.Comment])


declarations :: Parser [Syntax.Declaration ()]
declarations = s *> Parser.separatedBy declaration s <* s <* Parser.eoi


variableDeclaration :: Parser (Syntax.Declaration ())
variableDeclaration = do
  varKeyword <- keyword "var"

  Parser.commit $ do
    variable <- s *> identifier
    colon <- s *> charToken ':'
    tName <- s *> identifier <* s

    Parser.either (charToken ';') (charToken '=') >>= \case
      Left semicolon -> pure (Syntax.VariableDeclaration {varKeyword, variable, colon, tName, semicolon, extra = ()})

      Right equalSign -> do
        value <- s *> expression
        semicolon <- s *> charToken ';'
        pure Syntax.VariableAssignDeclaration {varKeyword, variable, colon, tName, equalSign, value, semicolon, extra = ()}


functionDeclaration :: Parser (Syntax.Declaration ())
functionDeclaration = do
  defKeyword <- keyword "def"

  Parser.commit $ do
    name <- s *> identifier
    open <- s *> charToken '('

    first <- optional $ do
      name <- s *> identifier

      Parser.commit $ do
        colon <- s *> charToken ':'
        tName <- s *> identifier
        pure (name, colon, tName)

    parameters <- case first of
      Just first -> do
        rest <- many $ do
          comma <- s *> charToken ','

          Parser.commit $ do
            name <- s *> identifier
            colon <- s *> charToken ':'
            tName <- s *> identifier
            pure (comma, name, colon, tName)

        pure (Just (first, rest))

      Nothing -> pure Nothing

    close <- s *> charToken ')'
    arrow <- s *> textToken "->"
    tName <- s *> identifier
    body <- s *> statement
    pure Syntax.FunctionDeclaration {defKeyword, name, open, parameters, close, arrow, tName, body, extra = ()}


declaration :: Parser (Syntax.Declaration ())
declaration = variableDeclaration <|> functionDeclaration


expressionStatement :: Parser (Syntax.Statement ())
expressionStatement = do
  value <- expression
  semicolon <- Parser.commit (s *> charToken ';')
  pure Syntax.ExpressionStatement {value, semicolon, extra = ()}


ifStatement :: Parser (Syntax.Statement ())
ifStatement = do
  ifKeyword <- keyword "if"

  Parser.commit $ do
    predicate <- s *> expression
    trueBranch <- s *> statement

    optional (s *> keyword "else") >>= \case
      Just elseKeyword -> do
        falseBranch <- Parser.commit (s *> statement)
        pure Syntax.IfElseStatement {ifKeyword, predicate, trueBranch, elseKeyword, falseBranch, extra = ()}

      Nothing -> pure Syntax.IfStatement {ifKeyword, predicate, trueBranch, extra = ()}


whileStatement :: Parser (Syntax.Statement ())
whileStatement = do
  whileKeyword <- keyword "while"

  Parser.commit $ do
    predicate <- s *> expression
    body <- s *> statement
    pure Syntax.WhileStatement {whileKeyword, predicate, body, extra = ()}


doWhileStatement :: Parser (Syntax.Statement ())
doWhileStatement = do
  doKeyword <- keyword "do"

  Parser.commit $ do
    body <- s *> statement
    whileKeyword <- s *> keyword "while"
    predicate <- s *> expression
    semicolon <- s *> charToken ';'
    pure Syntax.DoWhileStatement {doKeyword, body, whileKeyword, predicate, semicolon, extra = ()}


returnStatement :: Parser (Syntax.Statement ())
returnStatement = do
  returnKeyword <- keyword "return"
  value <- optional (s *> expression)
  semicolon <- Parser.commit (s *> charToken ';')

  case value of
    Just value -> pure Syntax.ReturnStatement {returnKeyword, value, semicolon, extra = ()}
    Nothing -> pure Syntax.EmptyReturnStatement {returnKeyword, semicolon, extra = ()}


blockStatement :: Parser (Syntax.Statement ())
blockStatement = do
  open <- charToken '{'

  Parser.commit $ do
    elements <- s *> Parser.separatedBy (Parser.either declaration statement) s
    close <- s *> charToken '}'
    pure Syntax.BlockStatement {open, elements, close, extra = ()}


statement :: Parser (Syntax.Statement ())
statement = asum
  [
    blockStatement,
    ifStatement,
    whileStatement,
    doWhileStatement,
    returnStatement,
    expressionStatement
  ]


integerExpression :: Parser (Syntax.Expression ())
integerExpression = Syntax.IntegerExpression <$> integer <*> pure ()


variableExpression :: Parser (Syntax.Expression ())
variableExpression = Syntax.VariableExpression <$> identifier <*> pure ()


callExpression :: Parser (Syntax.Expression ())
callExpression = do
  target <- identifier
  open <- s *> charToken '('

  Parser.commit $ do
    arguments <- optional (s *> expression) >>= \case
      Just first -> do
        rest <- many ((,) <$> (s *> charToken ',') <*> Parser.commit (s *> expression))
        pure (Just (first, rest))

      Nothing -> pure Nothing

    close <- s *> charToken ')'
    pure Syntax.CallExpression {target, open, arguments, close, extra = ()}


unaryExpression :: Parser (Syntax.Expression ())
unaryExpression = asum
  [
    integerExpression,

    optional unaryOperator >>= \case
      Just unary @ (Syntax.NotOperator span) -> optional (s *> unaryExpression) >>= \case
        Just operand -> pure Syntax.UnaryExpression {unary, operand, extra = ()}
        Nothing -> pure Syntax.VariableExpression {variable = Syntax.Identifier span "not", extra = ()}

      Just unary -> do
        operand <- Parser.commit (s *> unaryExpression)
        pure Syntax.UnaryExpression {unary, operand, extra = ()}

      Nothing -> integerExpression <|> callExpression
  ]


binaryExpression :: Parser (Syntax.Expression ())
binaryExpression = do
  left <- unaryExpression

  optional (s *> binaryOperator) >>= \case
    Just operator -> do
      right <- Parser.commit (s *> expression)
      pure (binary left operator right)

    Nothing -> pure left


assignExpression :: Parser (Syntax.Expression ())
assignExpression = do
  target <- identifier
  assign <- s *> assignOperator
  value <- Parser.commit (s *> expression)
  pure Syntax.AssignExpression {target, assign, value, extra = ()}


parenthesizedExpression :: Parser (Syntax.Expression ())
parenthesizedExpression = do
  open <- charToken '('

  Parser.commit $ do
    inner <- s *> expression
    close <- s *> charToken ')'
    pure Syntax.ParenthesizedExpression {open, inner, close, extra = ()}


expression :: Parser (Syntax.Expression ())
expression = asum
  [
    parenthesizedExpression,
    assignExpression,
    binaryExpression,
    unaryExpression,
    variableExpression
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


integer :: Parser Syntax.Integer
integer = Parser.label "integer" . syntax $ do
  sign <- (Parser.char '+' $> 1) <|> (Parser.char '-' $> -1) <|> pure 1
  digits <- some (Parser.satisfy isDigit)
  let magnitude = foldl' (\a d -> 10 * a + toInteger (digitToInt d)) 0 digits
  pure $ \span -> Syntax.Integer span (sign * magnitude)


-- [\p{L}\p{Nl}\p{Pc}][\p{L}\p{Nl}\p{Pc}\p{Mn}\p{Mc}\p{Nd}]*
identifier :: Parser Syntax.Identifier
identifier = Parser.label "identifier" . syntax $ do
  start <- category [lu, ll, lt, lm, lo, nl, pc]
  continue <- Text.pack <$> many (category [lu, ll, lt, lm, lo, nl, pc, mn, mc, nd])
  pure $ \span -> Syntax.Identifier span (Text.cons start continue)
  where
    lu = GLib.UnicodeTypeUppercaseLetter
    ll = GLib.UnicodeTypeLowercaseLetter
    lt = GLib.UnicodeTypeTitlecaseLetter
    lm = GLib.UnicodeTypeModifierLetter
    lo = GLib.UnicodeTypeOtherLetter
    mn = GLib.UnicodeTypeNonSpacingMark
    mc = GLib.UnicodeTypeSpacingMark
    nd = GLib.UnicodeTypeDecimalNumber
    nl = GLib.UnicodeTypeLetterNumber
    pc = GLib.UnicodeTypeConnectPunctuation


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
s = token (many (void (some (Parser.satisfy (unsafePerformIO . GLib.unicharIsspace))) <|> void comment))


keyword :: Text -> Parser Syntax.Token
keyword k = Parser.label ("keyword " <> k) $ do
  (Syntax.Identifier span name) <- identifier

  if Helpers.collate name == Helpers.collate k then
    pure (Syntax.Token span)
  else
    empty


symbol :: Text -> Parser Syntax.Token
symbol s = Parser.label s . syntax $ do
  text <- Text.pack <$> ((:) <$> category [pd, po, sm, sc, so] <*> many (category [pd, po, sm, sc, so, sk]))

  if Helpers.collate text == Helpers.collate s then
    pure Syntax.Token
  else
    empty

  where
    pd = GLib.UnicodeTypeDashPunctuation
    po = GLib.UnicodeTypeOtherPunctuation
    sm = GLib.UnicodeTypeMathSymbol
    sc = GLib.UnicodeTypeCurrencySymbol
    sk = GLib.UnicodeTypeModifierSymbol
    so = GLib.UnicodeTypeOtherSymbol


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


category :: (Foldable t, Applicative m) => t GLib.UnicodeType -> ParserT m Char
category categories = Parser.satisfy $ \c -> unsafePerformIO (GLib.unicharType c) `elem` categories


syntax :: Parser (Span -> a) -> Parser a
syntax parser = do
  start <- Parser.position
  f <- parser
  end <- Parser.position
  pure (f (Span start end))


binary :: Syntax.Expression () -> Syntax.BinaryOperator -> Syntax.Expression () -> Syntax.Expression ()

binary Syntax.BinaryExpression {} _ _ = undefined

binary left operator (Syntax.BinaryExpression rLeft rOperator rRight _)
  | Syntax.comparePrecedence operator rOperator >= EQ =
    Syntax.BinaryExpression (Syntax.BinaryExpression left operator rLeft ()) rOperator rRight ()

binary left operator right = Syntax.BinaryExpression left operator right ()
