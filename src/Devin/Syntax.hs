module Devin.Syntax (
  Node (..),
  Devin (..),
  Declaration (..),
  Statement (..),
  Expression (..),
  UnaryOperator (..),
  BinaryOperator (..),
  AssignOperator (..),
  Identifier (..),
  Token (..),
  Comment (..),
  doesReturn,
  hasSideEffects
) where

import Data.List.Extra

import Data.Text (Text)

import Devin.CallTarget (CallTarget)
import Devin.Range
import Devin.Type (Type)


class Node a where
  label :: a -> Text

  isLeaf :: a -> Bool

  findDeclaration :: (Declaration -> Bool) -> a -> Maybe Declaration


data Devin = Devin {declarations :: [Declaration], range :: (Int, Int)}
  deriving (Eq, Show, Read)


data Declaration where
  VariableDeclaration :: {
    varKeyword :: Token,
    variableId :: Identifier,
    equalSign :: Token,
    right :: Expression,
    semicolon :: Token
  } -> Declaration

  FunctionDeclaration :: {
    defKeyword :: Token,
    functionId :: Identifier,
    open :: Token,
    parameters :: [(Identifier, Token, Identifier)],
    commas :: [Token],
    close :: Token,
    returnInfo :: Maybe (Token, Identifier),
    body :: Statement,
    depth :: Int
  } -> Declaration

  deriving (Eq, Show, Read)


data Statement where
  DeclarationStatement :: {
    declaration :: Declaration
  } -> Statement

  ExpressionStatement :: {
    value :: Expression,
    semicolon :: Token
  } -> Statement

  IfStatement :: {
    ifKeyword :: Token,
    predicate :: Expression,
    trueBranch :: Statement
  } -> Statement

  IfElseStatement :: {
    ifKeyword :: Token,
    predicate :: Expression,
    trueBranch :: Statement,
    elseKeyword :: Token,
    falseBranch :: Statement
  } -> Statement

  WhileStatement :: {
    whileKeyword :: Token,
    predicate :: Expression,
    body :: Statement
  } -> Statement

  DoWhileStatement :: {
    doKeyword :: Token,
    body :: Statement,
    whileKeyword :: Token,
    predicate :: Expression,
    semicolon :: Token
  } -> Statement

  ReturnStatement :: {
    returnKeyword :: Token,
    result :: Maybe Expression,
    semicolon :: Token
  } -> Statement

  BlockStatement :: {
    open :: Token,
    statements :: [Statement],
    close :: Token
  } -> Statement

  deriving (Eq, Show, Read)


data Expression where
  IntegerExpression :: {
    integer :: Integer,
    t :: Type,
    range :: (Int, Int)
  } -> Expression

  RationalExpression :: {
    rational :: Rational,
    t :: Type,
    range :: (Int, Int)
  } -> Expression

  VariableExpression :: {
    variableId :: Identifier,
    t :: Type
  } -> Expression

  CallExpression :: {
    targetId :: Identifier,
    open :: Token,
    arguments :: [Expression],
    commas :: [Token],
    close :: Token,
    depth :: Int,
    target :: CallTarget,
    t :: Type
  } -> Expression

  UnaryExpression :: {
    unary :: UnaryOperator,
    operand :: Expression,
    t :: Type
  } -> Expression

  BinaryExpression :: {
    left :: Expression,
    binary :: BinaryOperator,
    right :: Expression,
    t :: Type
  } -> Expression

  AssignExpression :: {
    variableId :: Identifier,
    assign :: AssignOperator,
    right :: Expression,
    t :: Type
  } -> Expression

  ParenthesizedExpression :: {
    open :: Token,
    inner :: Expression,
    close :: Token,
    t :: Type
  } -> Expression

  deriving (Eq, Show, Read)


data UnaryOperator where
  PlusOperator :: {t :: Type, range :: (Int, Int)} -> UnaryOperator
  MinusOperator :: {t :: Type, range :: (Int, Int)} -> UnaryOperator
  NotOperator :: {t :: Type, range :: (Int, Int)} -> UnaryOperator
  deriving (Eq, Show, Read)


data BinaryOperator where
  AddOperator :: {t :: Type, range :: (Int, Int)} -> BinaryOperator
  SubtractOperator :: {t :: Type, range :: (Int, Int)} -> BinaryOperator
  MultiplyOperator :: {t :: Type, range :: (Int, Int)} -> BinaryOperator
  DivideOperator :: {t :: Type, range :: (Int, Int)} -> BinaryOperator
  ModuloOperator :: {t :: Type, range :: (Int, Int)} -> BinaryOperator
  EqualOperator :: {t :: Type, range :: (Int, Int)} -> BinaryOperator
  NotEqualOperator :: {t :: Type, range :: (Int, Int)} -> BinaryOperator
  LessOperator :: {t :: Type, range :: (Int, Int)} -> BinaryOperator
  LessOrEqualOperator :: {t :: Type, range :: (Int, Int)} -> BinaryOperator
  GreaterOperator :: {t :: Type, range :: (Int, Int)} -> BinaryOperator
  GreaterOrEqualOperator :: {t :: Type, range :: (Int, Int)} -> BinaryOperator
  AndOperator :: {t :: Type, range :: (Int, Int)} -> BinaryOperator
  OrOperator :: {t :: Type, range :: (Int, Int)} -> BinaryOperator
  deriving (Eq, Show, Read)


data AssignOperator where
  AssignOperator :: {t :: Type, range :: (Int, Int)} -> AssignOperator
  AddAssignOperator :: {t :: Type, range :: (Int, Int)} -> AssignOperator
  SubtractAssignOperator :: {t :: Type, range :: (Int, Int)} -> AssignOperator
  MultiplyAssignOperator :: {t :: Type, range :: (Int, Int)} -> AssignOperator
  DivideAssignOperator :: {t :: Type, range :: (Int, Int)} -> AssignOperator
  ModuloAssignOperator :: {t :: Type, range :: (Int, Int)} -> AssignOperator
  deriving (Eq, Show, Read)


data Identifier = Identifier {name :: Text, t :: Type, range :: (Int, Int)}
  deriving (Eq, Show, Read)


newtype Token = Token {range :: (Int, Int)}
  deriving (Eq, Show, Read)


newtype Comment = Comment {range :: (Int, Int)}
  deriving (Eq, Show, Read)


instance Range Devin where
  start Devin{range} = start range

  end Devin{range} = end range


instance Node Devin where
  label Devin{} = "Devin"

  isLeaf Devin{} = False

  findDeclaration f Devin{declarations} =
    firstJust (findDeclaration f) declarations


instance Range Declaration where
  start VariableDeclaration{varKeyword} = start varKeyword
  start FunctionDeclaration{defKeyword} = start defKeyword

  end VariableDeclaration{semicolon} = end semicolon
  end FunctionDeclaration{body} = end body


instance Node Declaration where
  label VariableDeclaration{} = "VariableDeclaration"
  label FunctionDeclaration{} = "FunctionDeclaration"

  isLeaf _ = False

  findDeclaration f declaration | f declaration = Just declaration
  findDeclaration f FunctionDeclaration{body} = findDeclaration f body
  findDeclaration _ _ = Nothing


instance Range Statement where
  start DeclarationStatement{declaration} = start declaration
  start ExpressionStatement{value} = start value
  start IfStatement{ifKeyword} = start ifKeyword
  start IfElseStatement{ifKeyword} = start ifKeyword
  start WhileStatement{whileKeyword} = start whileKeyword
  start DoWhileStatement{doKeyword} = start doKeyword
  start ReturnStatement{returnKeyword} = start returnKeyword
  start BlockStatement{open} = start open

  end DeclarationStatement{declaration} = end declaration
  end ExpressionStatement{semicolon} = end semicolon
  end IfStatement{trueBranch} = end trueBranch
  end IfElseStatement{falseBranch} = end falseBranch
  end WhileStatement{body} = end body
  end DoWhileStatement{body} = end body
  end ReturnStatement{semicolon} = end semicolon
  end BlockStatement{close} = end close


instance Node Statement where
  label DeclarationStatement{} = "DeclarationStatement"
  label ExpressionStatement{} = "ExpressionStatement"
  label IfStatement{} = "IfStatement"
  label IfElseStatement{} = "IfElseStatement"
  label WhileStatement{} = "WhileStatement"
  label DoWhileStatement{} = "DoWhileStatement"
  label ReturnStatement{} = "ReturnStatement"
  label BlockStatement{} = "BlockStatement"

  isLeaf ReturnStatement{result = Nothing} = True
  isLeaf _ = False

  findDeclaration f declaration = case declaration of
    DeclarationStatement{declaration} -> findDeclaration f declaration
    IfStatement{trueBranch} -> findDeclaration f trueBranch
    IfElseStatement{trueBranch, falseBranch} -> firstJust (findDeclaration f) [trueBranch, falseBranch]
    WhileStatement{body} -> findDeclaration f body
    DoWhileStatement{body} -> findDeclaration f body
    BlockStatement{statements} -> firstJust (findDeclaration f) statements
    _ -> Nothing


instance Range Expression where
  start IntegerExpression{range} = start range
  start RationalExpression{range} = start range
  start VariableExpression{variableId} = start variableId
  start CallExpression{targetId} = start targetId
  start UnaryExpression{unary} = start unary
  start BinaryExpression{left} = start left
  start AssignExpression{variableId} = start variableId
  start ParenthesizedExpression{open} = start open

  end IntegerExpression{range} = end range
  end RationalExpression{range} = end range
  end VariableExpression{variableId} = end variableId
  end CallExpression{close} = end close
  end UnaryExpression{operand} = end operand
  end BinaryExpression{right} = end right
  end AssignExpression{right} = end right
  end ParenthesizedExpression{close} = end close


instance Node Expression where
  label IntegerExpression{} = "IntegerExpression"
  label RationalExpression{} = "RationalExpression"
  label VariableExpression{} = "VariableExpression"
  label CallExpression{} = "CallExpression"
  label UnaryExpression{} = "UnaryExpression"
  label BinaryExpression{} = "BinaryExpression"
  label AssignExpression{} = "AssignExpression"
  label ParenthesizedExpression{} = "ParenthesizedExpression"

  isLeaf IntegerExpression{} = True
  isLeaf RationalExpression{} = True
  isLeaf _ = False

  findDeclaration _ _ = Nothing


instance Range UnaryOperator where
  start PlusOperator{range} = start range
  start MinusOperator{range} = start range
  start NotOperator{range} = start range

  end PlusOperator{range} = end range
  end MinusOperator{range} = end range
  end NotOperator{range} = end range


instance Node UnaryOperator where
  label PlusOperator{} = "PlusOperator"
  label MinusOperator{} = "MinusOperator"
  label NotOperator{} = "NotOperator"

  isLeaf _ = True

  findDeclaration _ _ = Nothing


instance Range BinaryOperator where
  start AddOperator{range} = start range
  start SubtractOperator{range} = start range
  start MultiplyOperator{range} = start range
  start DivideOperator{range} = start range
  start ModuloOperator{range} = start range
  start EqualOperator{range} = start range
  start NotEqualOperator{range} = start range
  start LessOperator{range} = start range
  start LessOrEqualOperator{range} = start range
  start GreaterOperator{range} = start range
  start GreaterOrEqualOperator{range} = start range
  start AndOperator{range} = start range
  start OrOperator{range} = start range

  end AddOperator{range} = end range
  end SubtractOperator{range} = end range
  end MultiplyOperator{range} = end range
  end DivideOperator{range} = end range
  end ModuloOperator{range} = end range
  end EqualOperator{range} = end range
  end NotEqualOperator{range} = end range
  end LessOperator{range} = end range
  end LessOrEqualOperator{range} = end range
  end GreaterOperator{range} = end range
  end GreaterOrEqualOperator{range} = end range
  end AndOperator{range} = end range
  end OrOperator{range} = end range


instance Node BinaryOperator where
  label AddOperator{} = "AddOperator"
  label SubtractOperator{} = "SubtractOperator"
  label MultiplyOperator{} = "MultiplyOperator"
  label DivideOperator{} = "DivideOperator"
  label ModuloOperator{} = "ModuloOperator"
  label EqualOperator{} = "EqualOperator"
  label NotEqualOperator{} = "NotEqualOperator"
  label LessOperator{} = "LessOperator"
  label LessOrEqualOperator{} = "LessOrEqualOperator"
  label GreaterOperator{} = "GreaterOperator"
  label GreaterOrEqualOperator{} = "GreaterOrEqualOperator"
  label AndOperator{} = "AndOperator"
  label OrOperator{} = "OrOperator"

  isLeaf _ = True

  findDeclaration _ _ = Nothing


instance Range AssignOperator where
  start AssignOperator{range} = start range
  start AddAssignOperator{range} = start range
  start SubtractAssignOperator{range} = start range
  start MultiplyAssignOperator{range} = start range
  start DivideAssignOperator{range} = start range
  start ModuloAssignOperator{range} = start range

  end AssignOperator{range} = end range
  end AddAssignOperator{range} = end range
  end SubtractAssignOperator{range} = end range
  end MultiplyAssignOperator{range} = end range
  end DivideAssignOperator{range} = end range
  end ModuloAssignOperator{range} = end range


instance Node AssignOperator where
  label AssignOperator{} = "AssignOperator"
  label AddAssignOperator{} = "AddAssignOperator"
  label SubtractAssignOperator{} = "SubtractAssignOperator"
  label MultiplyAssignOperator{} = "MultiplyAssignOperator"
  label DivideAssignOperator{} = "DivideAssignOperator"
  label ModuloAssignOperator{} = "ModuloAssignOperator"

  isLeaf _ = True

  findDeclaration _ _ = Nothing


instance Range Identifier where
  start Identifier{range} = start range

  end Identifier{range} = end range


instance Node Identifier where
  label Identifier{} = "Identifier"

  isLeaf Identifier{} = True

  findDeclaration _ _ = Nothing


instance Range Token where
  start Token{range} = start range

  end Token{range} = end range


instance Node Token where
  label Token{} = "Token"

  isLeaf Token{} = True

  findDeclaration _ _ = Nothing


instance Range Comment where
  start Comment{range} = start range

  end Comment{range} = end range


instance Node Comment where
  label Comment{} = "Comment"

  isLeaf Comment{} = True

  findDeclaration _ _ = Nothing


doesReturn :: Statement -> Bool
doesReturn DeclarationStatement{} = False
doesReturn ExpressionStatement{} = False
doesReturn IfStatement{} = False
doesReturn IfElseStatement{trueBranch, falseBranch} = all doesReturn [trueBranch, falseBranch]
doesReturn WhileStatement{} = False
doesReturn DoWhileStatement{body} = doesReturn body
doesReturn ReturnStatement{} = True
doesReturn BlockStatement{statements} = any doesReturn statements


hasSideEffects :: Expression -> Bool
hasSideEffects IntegerExpression{} = False
hasSideEffects RationalExpression{} = False
hasSideEffects VariableExpression{} = False
hasSideEffects CallExpression{} = True
hasSideEffects UnaryExpression{operand} = hasSideEffects operand
hasSideEffects BinaryExpression{left, right} = any hasSideEffects [left, right]
hasSideEffects AssignExpression{} = True
hasSideEffects ParenthesizedExpression{inner} = hasSideEffects inner
