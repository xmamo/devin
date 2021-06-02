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

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer

import Data.Text (Text)
import qualified Data.Text as Text

import Parser (ParserT (ParserT))
import qualified Parser
import Span (Span (Span))
import Syntax (Syntax)
import qualified Syntax
import qualified Unicode


type ParserW = ParserT (Writer [Syntax.Comment])


declarations :: Applicative m => ParserT m ([Syntax.Declaration ()], [Syntax.Comment])
declarations = runW declarationsW


declaration :: Applicative m => ParserT m (Syntax.Declaration (), [Syntax.Comment])
declaration = runW declarationW


statement :: Applicative m => ParserT m (Syntax.Statement (), [Syntax.Comment])
statement = runW statementW


expression :: Applicative m => ParserT m (Syntax.Expression (), [Syntax.Comment])
expression = runW expressionW


unaryOperator :: Applicative m => ParserT m (Syntax.UnaryOperator, [Syntax.Comment])
unaryOperator = runW unaryOperatorW


binaryOperator :: Applicative m => ParserT m (Syntax.BinaryOperator, [Syntax.Comment])
binaryOperator = runW binaryOperatorW


assignOperator :: Applicative m => ParserT m (Syntax.AssignOperator, [Syntax.Comment])
assignOperator = runW assignOperatorW


integer :: Applicative m => ParserT m (Syntax.Integer, [Syntax.Comment])
integer = runW integerW


identifier :: Applicative m => ParserT m (Syntax.Identifier, [Syntax.Comment])
identifier = runW identifierW


comment :: Applicative m => ParserT m (Syntax.Comment, [Syntax.Comment])
comment = runW commentW


runW :: Applicative m => ParserW a -> ParserT m (a, [Syntax.Comment])
runW parser = ParserT \input ->
  let (result, comments) = runWriter (Parser.parseT parser input)
   in pure ((, comments) <$> result)


declarationsW :: ParserW [Syntax.Declaration ()]
declarationsW = sW *> Parser.separatedBy declarationW sW <* sW <* Parser.eoi


variableDeclarationW :: ParserW (Syntax.Declaration ())
variableDeclarationW = do
  varKeyword <- keywordW "var"

  Parser.commit do
    variable <- sW *> identifierW
    colon <- sW *> charTokenW ':'
    tName <- sW *> identifierW <* sW

    Parser.either (charTokenW ';') (charTokenW '=') >>= \case
      Left semicolon -> pure Syntax.EmptyVariableDeclaration {varKeyword, variable, colon, tName, semicolon, extra = ()}

      Right equalSign -> do
        value <- sW *> expressionW
        semicolon <- sW *> charTokenW ';'
        pure Syntax.VariableDeclaration {varKeyword, variable, colon, tName, equalSign, value, semicolon, extra = ()}


functionDeclarationW :: ParserW (Syntax.Declaration ())
functionDeclarationW = do
  defKeyword <- keywordW "def"

  Parser.commit do
    name <- sW *> identifierW
    open <- sW *> charTokenW '('

    first <- optional do
      name <- sW *> identifierW

      Parser.commit do
        colon <- sW *> charTokenW ':'
        tName <- sW *> identifierW
        pure (name, colon, tName)

    parameters <- case first of
      Just first -> do
        rest <- many do
          comma <- sW *> charTokenW ','

          Parser.commit do
            name <- sW *> identifierW
            colon <- sW *> charTokenW ':'
            tName <- sW *> identifierW
            pure (comma, name, colon, tName)

        pure (Just (first, rest))

      Nothing -> pure Nothing

    close <- sW *> charTokenW ')'
    arrow <- sW *> textTokenW "->"
    tName <- sW *> identifierW
    body <- sW *> statementW
    pure Syntax.FunctionDeclaration {defKeyword, name, open, parameters, close, arrow, tName, body, extra = ()}


declarationW :: ParserW (Syntax.Declaration ())
declarationW = variableDeclarationW <|> functionDeclarationW


expressionStatementW :: ParserW (Syntax.Statement ())
expressionStatementW = do
  value <- expressionW
  semicolon <- Parser.commit (sW *> charTokenW ';')
  pure Syntax.ExpressionStatement {value, semicolon, extra = ()}


ifElseStatementW :: ParserW (Syntax.Statement ())
ifElseStatementW = do
  ifKeyword <- keywordW "if"

  Parser.commit do
    predicate <- sW *> expressionW
    trueBranch <- sW *> statementW

    optional (sW *> keywordW "else") >>= \case
      Just elseKeyword -> do
        falseBranch <- Parser.commit (sW *> statementW)
        pure Syntax.IfElseStatement {ifKeyword, predicate, trueBranch, elseKeyword, falseBranch, extra = ()}

      Nothing -> pure Syntax.IfStatement {ifKeyword, predicate, trueBranch, extra = ()}


whileStatementW :: ParserW (Syntax.Statement ())
whileStatementW = do
  whileKeyword <- keywordW "while"

  Parser.commit do
    predicate <- sW *> expressionW
    body <- sW *> statementW
    pure Syntax.WhileStatement {whileKeyword, predicate, body, extra = ()}


doWhileStatementW :: ParserW (Syntax.Statement ())
doWhileStatementW = do
  doKeyword <- keywordW "do"

  Parser.commit do
    body <- sW *> statementW
    whileKeyword <- sW *> keywordW "while"
    predicate <- sW *> expressionW
    semicolon <- sW *> charTokenW ';'
    pure Syntax.DoWhileStatement {doKeyword, body, whileKeyword, predicate, semicolon, extra = ()}


returnStatementW :: ParserW (Syntax.Statement ())
returnStatementW = do
  returnKeyword <- keywordW "return"
  result <- optional (sW *> expressionW)
  semicolon <- Parser.commit (sW *> charTokenW ';')
  pure Syntax.ReturnStatement {returnKeyword, result, semicolon, extra = ()}


blockStatementW :: ParserW (Syntax.Statement ())
blockStatementW = do
  open <- charTokenW '{'

  Parser.commit do
    elements <- sW *> Parser.separatedBy (Parser.either declarationW statementW) sW
    close <- sW *> charTokenW '}'
    pure Syntax.BlockStatement {open, elements, close, extra = ()}


statementW :: ParserW (Syntax.Statement ())
statementW = asum
  [
    blockStatementW,
    ifElseStatementW,
    whileStatementW,
    doWhileStatementW,
    returnStatementW,
    expressionStatementW
  ]


integerExpressionW :: ParserW (Syntax.Expression ())
integerExpressionW = Syntax.IntegerExpression <$> integerW <*> pure ()


variableExpressionW :: ParserW (Syntax.Expression ())
variableExpressionW = Syntax.VariableExpression <$> identifierW <*> pure ()


callExpressionW :: ParserW (Syntax.Expression ())
callExpressionW = do
  target <- identifierW
  open <- sW *> charTokenW '('

  Parser.commit do
    arguments <- optional (sW *> expressionW) >>= \case
      Just first -> do
        rest <- many ((,) <$> (sW *> charTokenW ',') <*> Parser.commit (sW *> expressionW))
        pure (Just (first, rest))

      Nothing -> pure Nothing

    close <- sW *> charTokenW ')'
    pure Syntax.CallExpression {target, open, arguments, close, extra = ()}


unaryExpressionW :: ParserW (Syntax.Expression ())
unaryExpressionW = do
  unary <- unaryOperatorW

  operand <- case unary of
    Syntax.NotOperator _ -> sW *> operandExpressionW
    _ -> Parser.commit (sW *> operandExpressionW)

  pure Syntax.UnaryExpression {unary, operand, extra = ()}


operandExpressionW :: ParserW (Syntax.Expression ())
operandExpressionW = integerExpressionW <|> callExpressionW <|> unaryExpressionW <|> variableExpressionW


binaryExpressionOrOperandExpressionW :: ParserW (Syntax.Expression ())
binaryExpressionOrOperandExpressionW = do
  left <- operandExpressionW

  optional (sW *> binaryOperatorW) >>= \case
    Just binary -> do
      right <- Parser.commit (sW *> expressionW)
      pure (newBinary left binary right)

    Nothing -> pure left


assignExpressionW :: ParserW (Syntax.Expression ())
assignExpressionW = do
  target <- identifierW
  assign <- sW *> assignOperatorW
  value <- Parser.commit (sW *> expressionW)
  pure Syntax.AssignExpression {target, assign, value, extra = ()}


parenthesizedExpressionW :: ParserW (Syntax.Expression ())
parenthesizedExpressionW = do
  open <- charTokenW '('

  Parser.commit do
    inner <- sW *> expressionW
    close <- charTokenW ')'
    pure Syntax.ParenthesizedExpression {open, inner, close, extra = ()}


expressionW :: ParserW (Syntax.Expression ())
expressionW = parenthesizedExpressionW <|> assignExpressionW <|> binaryExpressionOrOperandExpressionW


unaryOperatorW :: ParserW Syntax.UnaryOperator
unaryOperatorW = syntaxW $ asum
  [
    Parser.char '+' $> Syntax.PlusOperator,
    Parser.char '-' $> Syntax.MinusOperator,
    keywordW "not" $> Syntax.NotOperator
  ]


binaryOperatorW :: ParserW Syntax.BinaryOperator
binaryOperatorW = syntaxW $ asum
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
    keywordW "and" $> Syntax.AndOperator,
    keywordW "or" $> Syntax.OrOperator
  ]


assignOperatorW :: ParserW Syntax.AssignOperator
assignOperatorW = syntaxW $ asum
  [
    Parser.char '=' $> Syntax.AssignOperator,
    Parser.text "+=" $> Syntax.AddAssignOperator,
    Parser.text "-=" $> Syntax.SubtractAssignOperator,
    Parser.text "*=" $> Syntax.MultiplyAssignOperator,
    Parser.text "/=" $> Syntax.DivideAssignOperator,
    Parser.text "%=" $> Syntax.RemainderAssignOperator
  ]


integerW :: ParserW Syntax.Integer
integerW = Parser.label "integer" $ syntaxW do
  sign <- (Parser.char '+' $> 1) <|> (Parser.char '-' $> -1) <|> pure 1
  digits <- some (Parser.satisfy isDigit)
  let magnitude = foldl' (\a d -> 10 * a + toInteger (digitToInt d)) 0 digits
  pure \span -> Syntax.Integer span (sign * magnitude)


-- [\p{L}\p{Nl}\p{Pc}][\p{L}\p{Nl}\p{Pc}\p{Mn}\p{Mc}\p{Nd}]*
identifierW ::ParserW Syntax.Identifier
identifierW = Parser.label "identifier" $ syntaxW do
  start <- category [lu, ll, lt, lm, lo, nl, pc]
  continue <- Text.pack <$> many (category [lu, ll, lt, lm, lo, nl, pc, mn, mc, nd])
  pure \span -> Syntax.Identifier span (Text.cons start continue)
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


commentW :: ParserW Syntax.Comment
commentW = do
  start <- Parser.position
  Parser.text "//"
  many (Parser.satisfy (not . Unicode.isNewline))
  end <- Parser.position

  let comment = Syntax.Comment (Span start end)
  lift (tell [comment])
  pure comment


sW :: ParserW Syntax.Token
sW = tokenW (many (void (some (Parser.satisfy Unicode.isSpace)) <|> void commentW))


keywordW :: Text -> ParserW Syntax.Token
keywordW k = Parser.label ("keyword " <> k) do
  (Syntax.Identifier span name) <- identifierW

  if Unicode.collate name == Unicode.collate k then
    pure (Syntax.Token span)
  else
    empty


tokenW :: ParserW a -> ParserW Syntax.Token
tokenW parser = do
  start <- Parser.position
  parser
  end <- Parser.position
  pure (Syntax.Token (Span start end))


charTokenW :: Char -> ParserW  Syntax.Token
charTokenW = tokenW . Parser.char


textTokenW :: Text -> ParserW Syntax.Token
textTokenW = tokenW . Parser.text


syntaxW :: Syntax a => ParserW (Span -> a) -> ParserW a
syntaxW parser = do
  start <- Parser.position
  f <- parser
  end <- Parser.position
  pure (f (Span start end))


newBinary :: Syntax.Expression () -> Syntax.BinaryOperator -> Syntax.Expression () -> Syntax.Expression ()

newBinary Syntax.BinaryExpression {} _ _ = undefined

newBinary left operator (Syntax.BinaryExpression rLeft rOperator rRight _)
  | Syntax.comparePrecedence operator rOperator >= EQ =
    Syntax.BinaryExpression (Syntax.BinaryExpression left operator rLeft ()) rOperator rRight ()

newBinary left operator right = Syntax.BinaryExpression left operator right ()
