module SimulationDSL.Language.UninferredEquations
  ( UninferredEquations
  , emptyUninferredEquations
  , addUninferredEquation
  , addInitialCondition
  , uninferredEquation
  , uninferredEquations
  , inferredEquations
  ) where

import Data.Maybe
import qualified Data.Map as M
import SimulationDSL.Data.Value
import SimulationDSL.Data.ExpType
import SimulationDSL.Language.Exp
import SimulationDSL.Language.Equation
import SimulationDSL.Language.Equations hiding ( addInitialCondition )
import qualified SimulationDSL.Language.Equations as E ( addInitialCondition )
import SimulationDSL.Language.UninferredEquation
import SimulationDSL.Language.InitialCondition

data UninferredEquations = UninferredEquations (M.Map String UninferredEquation)
                                               (M.Map String InitialCondition)

emptyUninferredEquations :: UninferredEquations
emptyUninferredEquations = UninferredEquations M.empty M.empty

uninferredEquation :: UninferredEquations -> String -> UninferredEquation
uninferredEquation (UninferredEquations x _) symbol
  = case M.lookup symbol x of
      Just x  -> x
      Nothing -> error "uninferredEquation: symbol not found"

uninferredEquations :: UninferredEquations -> [(String,UninferredEquation)]
uninferredEquations (UninferredEquations x _) = M.assocs x

addUninferredEquation :: UninferredEquations -> String -> Maybe ExpType -> Exp -> UninferredEquations
addUninferredEquation (UninferredEquations eqs ics) symbol t exp
  = UninferredEquations eqs' ics
    where eqs' = M.insert symbol (makeUninferredEquation symbol t exp) eqs

addInitialCondition :: UninferredEquations -> String -> InitialCondition -> UninferredEquations
addInitialCondition (UninferredEquations eqs ics) symbol ic
  = UninferredEquations eqs ics'
    where ics' = M.insert symbol ic ics

inferredEquations :: UninferredEquations -> Equations
inferredEquations eqs@(UninferredEquations xs ys) = Equations xs' ys
  where xs' = M.map (inferEquation eqs) xs

inferEquation :: UninferredEquations -> UninferredEquation -> Equation
inferEquation eqs eq = makeEquation symbol t' exp
  where symbol = uninferredEquationSymbol eq
        t      = uninferredEquationType eq
        exp    = uninferredEquationExp eq
        t'     = case t of
                   Just x  -> x
                   Nothing -> inferExpType eqs symbol exp

inferExpType :: UninferredEquations -> String -> Exp -> ExpType
inferExpType eqs s0 (Integral exp)             = inferExpType eqs s0 exp
inferExpType _   _  (Constant (ValueScalar _)) = ExpTypeScalar
inferExpType _   _  (Constant (ValueVector _)) = ExpTypeVector
inferExpType eqs s0 (Var symbol)
  | isJust t     = fromJust t
  | symbol == s0 = error "inferExpType: circular reference"
  | otherwise    = inferExpType eqs s0 exp
  where t   = uninferredEquationType $ uninferredEquation eqs symbol
        exp = uninferredEquationExp $ uninferredEquation eqs symbol
inferExpType eqs s0 (Var' symbol)
  | isJust t     = fromJust t
  | symbol == s0 = error "inferExpType: circular reference"
  | otherwise    = inferExpType eqs s0 exp
  where t   = uninferredEquationType $ uninferredEquation eqs symbol
        exp = uninferredEquationExp $ uninferredEquation eqs symbol
inferExpType eqs s0 (Add x y)
  = case (inferExpType eqs s0 x, inferExpType eqs s0 y) of
      (ExpTypeScalar, ExpTypeScalar) -> ExpTypeScalar
      (ExpTypeVector, ExpTypeVector) -> ExpTypeVector
      otherwise                      -> error "inferExpType: invalid operand(s)"
inferExpType eqs s0 (Sub x y)
  = case (inferExpType eqs s0 x, inferExpType eqs s0 y) of
      (ExpTypeScalar, ExpTypeScalar) -> ExpTypeScalar
      (ExpTypeVector, ExpTypeVector) -> ExpTypeVector
      otherwise                      -> error "inferExpType: invalid operand(s)"
inferExpType eqs s0 (Mul x y)
  = case (inferExpType eqs s0 x, inferExpType eqs s0 y) of
      (ExpTypeScalar, ExpTypeScalar) -> ExpTypeScalar
      (ExpTypeScalar, ExpTypeVector) -> ExpTypeVector
      (ExpTypeVector, ExpTypeScalar) -> ExpTypeVector
      otherwise                      -> error "inferExpType: invalid operand(s)"
inferExpType eqs s0 (Div x y)
  = case (inferExpType eqs s0 x, inferExpType eqs s0 y) of
      (ExpTypeScalar, ExpTypeScalar) -> ExpTypeScalar
      (ExpTypeVector, ExpTypeScalar) -> ExpTypeVector
      otherwise                      -> error "inferExpType: invalid operand(s)"
inferExpType eqs s0 (Norm x)
  = case inferExpType eqs s0 x of
      ExpTypeVector -> ExpTypeScalar
      otherwise     -> error "inferExpType: invalid operand"
inferExpType eqs s0 (Sigma exp) = inferExpType eqs s0 exp
