module Devin.Parsers.Internal (
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

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Functor

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.Set as Set

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer

import qualified Devin.CallTarget as CallTarget
import Devin.Parser hiding (Parser)
import Devin.Syntax hiding (label, declaration)
import qualified Devin.Type as Type


type Parser = ParserT (Writer [Comment])


devin :: Parser Devin
devin = syntax $ do
  declarations <- s *> (declaration `separatedBy` s) <* s <* eoi
  pure (Devin declarations)


declaration :: Parser Declaration
declaration = variableDeclaration <|> functionDeclaration


variableDeclaration :: Parser Declaration
variableDeclaration = do
  varKeyword <- keyword "var"
  variableId <- s *> identifier

  commit $ do
    equalSign <- s *> tokenText "="
    right <- s *> expression
    semicolon <- s *> tokenText ";"
    pure (VariableDeclaration varKeyword variableId equalSign right semicolon)


functionDeclaration :: Parser Declaration
functionDeclaration = do
  defKeyword <- keyword "def"

  commit $ do
    functionId <- s *> identifier
    open <- s *> tokenText "("

    first <- optional $ do
      id <- s *> identifier

      commit $ do
        colon <- s *> tokenText ":"
        typeId <- s *> identifier
        pure (id, colon, typeId)

    (parameters, commas) <- case first of
      Just first -> do
        rest <- many $ do
          comma <- s *> tokenText ","

          commit $ do
            id <- s *> identifier
            colon <- s *> tokenText ":"
            typeId <- s *> s *> identifier
            pure (comma, (id, colon, typeId))

        pure (first : map snd rest, fst <$> rest)

      Nothing -> pure ([], [])

    close <- s *> tokenText ")"
    returnInfo <- optional (liftA2 (,) (s *> tokenText "->") (commit (s *> identifier)))
    body <- s *> statement
    pure (FunctionDeclaration defKeyword functionId open parameters commas close returnInfo body)


statement :: Parser Statement
statement = asum
  [
    declarationStatement,
    ifElseOrIfStatement,
    whileStatement,
    doWhileStatement,
    returnStatement,
    expressionStatement,
    blockStatement
  ]


declarationStatement :: Parser Statement
declarationStatement = DeclarationStatement <$> declaration


ifElseOrIfStatement :: Parser Statement
ifElseOrIfStatement = do
  ifKeyword <- keyword "if"

  commit $ do
    predicate <- s *> expression
    trueBranch <- s *> statement
    elseKeyword <- optional (s *> keyword "else")

    case elseKeyword of
      Just elseKeyword -> do
        falseBranch <- commit (s *> statement)
        pure (IfElseStatement ifKeyword predicate trueBranch elseKeyword falseBranch)

      Nothing -> pure (IfStatement ifKeyword predicate trueBranch)


whileStatement :: Parser Statement
whileStatement = do
  whileKeyword <- keyword "while"

  commit $ do
    predicate <- s *> expression
    body <- s *> statement
    pure (WhileStatement whileKeyword predicate body)


doWhileStatement :: Parser Statement
doWhileStatement = do
  doKeyword <- keyword "do"

  commit $ do
    body <- s *> statement
    whileKeyword <- s *> keyword "while"
    predicate <- s *> expression
    semicolon <- s *> tokenText ";"
    pure (DoWhileStatement doKeyword body whileKeyword predicate semicolon)


returnStatement :: Parser Statement
returnStatement = do
  returnKeyword <- keyword "return"
  result <- optional (s *> expression)
  semicolon <- commit (s *> tokenText ";")
  pure (ReturnStatement returnKeyword result semicolon)


expressionStatement :: Parser Statement
expressionStatement = do
  value <- expression
  semicolon <- commit (s *> tokenText ";")
  pure (ExpressionStatement value semicolon)


blockStatement :: Parser Statement
blockStatement = do
  open <- tokenText "{"

  commit $ do
    elements <- s *> separatedBy statement s
    close <- s *> tokenText "}"
    pure (BlockStatement open elements close)


expression :: Parser Expression
expression = assignExpression <|> binaryOrOperandExpression


assignExpression :: Parser Expression
assignExpression = do
  targetId <- identifier
  assign <- s *> assignOperator
  value <-  s *> expression
  pure (AssignExpression targetId assign value Type.Undefined)


binaryOrOperandExpression :: Parser Expression
binaryOrOperandExpression = do
  left <- operandExpression
  binary <- optional (s *> binaryOperator)

  case binary of
    Just binary -> do
      right <- commit (s *> expression)

      case right of
        BinaryExpression{} | precedence binary >= precedence right.binary -> do
          let left' = BinaryExpression left binary right.left Type.Undefined
          pure (BinaryExpression left' right.binary right.right Type.Undefined)

        _ -> pure (BinaryExpression left binary right Type.Undefined)

    Nothing -> pure left

  where
    precedence MultiplyOperator{} = 5
    precedence DivideOperator{} = 5
    precedence ModuloOperator{} = 5
    precedence AddOperator{} = 4
    precedence SubtractOperator{} = 4
    precedence LessOperator{} = 3
    precedence LessOrEqualOperator{} = 3
    precedence GreaterOperator{} = 3
    precedence GreaterOrEqualOperator{} = 3
    precedence EqualOperator{} = 2
    precedence NotEqualOperator{} = 2
    precedence AndOperator{} = 1
    precedence OrOperator{} = 1


operandExpression :: Parser Expression
operandExpression = asum
  [
    rationalExpression,
    integerExpression,
    callExpression,
    unaryExpression,
    variableExpression,
    parenthesizedExpression
  ]


integerExpression :: Parser Expression
integerExpression = label "integer" $ syntax $ do
  sign <- (text "+" $> 1) <|> (text "-" $> -1) <|> pure 1
  digits <- some (satisfy isDigit)
  let magnitude = foldl (\a d -> 10 * a + toInteger (digitToInt d)) 0 digits
  pure (IntegerExpression (sign * magnitude) Type.Undefined)


rationalExpression :: Parser Expression
rationalExpression = label "rational" $ syntax $ do
  sign <- (text "+" $> 1) <|> (text "-" $> -1) <|> pure 1
  digits1 <- some (satisfy isDigit)
  digits2 <- text "." *> some (satisfy isDigit)
  let digits = digits1 ++ digits2
  let mantissa = foldl (\a d -> 10 * a + toRational (digitToInt d)) 0 digits
  pure (RationalExpression (sign * mantissa * 0.1 ^^ length digits2) Type.Undefined)


variableExpression :: Parser Expression
variableExpression = do
  variableId <- identifier
  pure (VariableExpression variableId Type.Undefined)


callExpression :: Parser Expression
callExpression = do
  targetId <- identifier
  open <- s *> tokenText "("

  commit $ do
    first <- optional (s *> expression)

    (arguments, commas) <- case first of
      Just first -> do
        rest <- many (liftA2 (,) (s *> tokenText ",") (commit (s *> expression)))
        pure (first : map snd rest, fst <$> rest)

      Nothing -> pure ([], [])

    close <- s *> tokenText ")"
    pure (CallExpression targetId open arguments commas close (-1) CallTarget.Undefined Type.Undefined)


unaryExpression :: Parser Expression
unaryExpression = do
  unary <- unaryOperator
  operand <- s *> operandExpression
  pure (UnaryExpression unary operand Type.Undefined)


parenthesizedExpression :: Parser Expression
parenthesizedExpression = do
  open <- tokenText "("

  commit $ do
    inner <- s *> expression
    close <- tokenText ")"
    pure (ParenthesizedExpression open inner close Type.Undefined)


unaryOperator :: Parser UnaryOperator
unaryOperator = syntax $ asum
  [
    text "+" $> PlusOperator Type.Undefined,
    text "-" $> MinusOperator Type.Undefined,
    keyword "not" $> NotOperator Type.Undefined
  ]


binaryOperator :: Parser BinaryOperator
binaryOperator = syntax $ asum
  [
    text "+" $> AddOperator Type.Undefined,
    text "-" $> SubtractOperator Type.Undefined,
    text "*" $> MultiplyOperator Type.Undefined,
    text "/" $> DivideOperator Type.Undefined,
    text "%" $> ModuloOperator Type.Undefined,
    text "==" $> EqualOperator Type.Undefined,
    text "!=" $> NotEqualOperator Type.Undefined,
    text "<=" $> LessOrEqualOperator Type.Undefined,
    text "<" $> LessOperator Type.Undefined,
    text ">=" $> GreaterOrEqualOperator Type.Undefined,
    text ">" $> GreaterOperator Type.Undefined,
    keyword "and" $> AndOperator Type.Undefined,
    keyword "or" $> OrOperator Type.Undefined
  ]


assignOperator :: Parser AssignOperator
assignOperator = syntax $ asum
  [
    text "=" $> AssignOperator Type.Undefined,
    text "+=" $> AddAssignOperator Type.Undefined,
    text "-=" $> SubtractAssignOperator Type.Undefined,
    text "*=" $> MultiplyAssignOperator Type.Undefined,
    text "/=" $> DivideAssignOperator Type.Undefined,
    text "%=" $> ModuloAssignOperator Type.Undefined
  ]


-- [\p{L}\p{Nl}\p{Pc}][\p{L}\p{Nl}\p{Pc}\p{Mn}\p{Mc}\p{Nd}]*
identifier :: Parser Identifier
identifier = label "identifier" $ syntax $ do
  start <- satisfy $ \c -> isStart (generalCategory c)
  continue <- many (satisfy $ \c -> isContinue (generalCategory c))
  pure (Identifier (Text.pack (start : continue)) Type.Undefined)
  where
    isStart UppercaseLetter = True
    isStart LowercaseLetter = True
    isStart TitlecaseLetter = True
    isStart ModifierLetter = True
    isStart OtherLetter = True
    isStart LetterNumber = True
    isStart ConnectorPunctuation = True
    isStart _ = False

    isContinue NonSpacingMark = True
    isContinue SpacingCombiningMark = True
    isContinue DecimalNumber = True
    isContinue c = isStart c


comment :: Parser Comment
comment = do
  comment <- syntax $ do
    text "//"
    many (noneOf (Set.fromList "\n\v\r\x85\x2028\x2029"))
    pure Comment

  lift (tell [comment])
  pure comment


keyword :: Text -> Parser Token
keyword k = label ("keyword " <> k) $ do
  Identifier{range, name} <- identifier
  guard (name == k)
  pure (Token range)


tokenText :: Text -> Parser Token
tokenText t = token (text t)


token :: Parser a -> Parser Token
token parser = syntax (parser $> Token)


syntax :: Num a => Parser ((a, a) -> b) -> Parser b
syntax parser = do
  start <- position
  f <- parser
  end <- position
  pure (f (start, end))


s :: Parser Token
s = token (many (void (some (satisfy isSpace)) <|> void comment))