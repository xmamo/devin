module Syntax (
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
  hasSideEffects,
  comparePrecedence
) where

import Data.Ord

import Syntax.Internal


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


comparePrecedence :: BinaryOperator -> BinaryOperator -> Ordering
comparePrecedence = comparing precedence
  where
    precedence AddOperator{} = 4
    precedence SubtractOperator{} = 4
    precedence MultiplyOperator{} = 5
    precedence DivideOperator{} = 5
    precedence ModuloOperator{} = 5
    precedence EqualOperator{} = 2
    precedence NotEqualOperator{} = 2
    precedence LessOperator{} = 3
    precedence LessOrEqualOperator{} = 3
    precedence GreaterOperator{} = 3
    precedence GreaterOrEqualOperator{} = 3
    precedence AndOperator{} = 1
    precedence OrOperator{} = 1
