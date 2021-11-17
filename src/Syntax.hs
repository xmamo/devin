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
  hasSideEffects
) where

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
