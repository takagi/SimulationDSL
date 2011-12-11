module SimulationDSL.Language.EquationsDescription
  ( EquationsDescription
  , analyzedEquationsDescription
  , define
  , defineWithType
  , initialConditionS
  , initialConditionV
  ) where

import Control.Monad.State
import SimulationDSL.Data.Scalar
import SimulationDSL.Data.Vector3
import SimulationDSL.Data.Array
import SimulationDSL.Data.ExpType
import SimulationDSL.Language.Exp
import SimulationDSL.Language.InitialCondition
import SimulationDSL.Language.Equations hiding ( addInitialCondition )
import SimulationDSL.Language.UninferredEquations

type EquationsDescription = State UninferredEquations ()

analyzedEquationsDescription :: EquationsDescription -> Equations
analyzedEquationsDescription = checkedEquations
                             . inferredEquations
                             . flip execState emptyUninferredEquations

define :: String -> Exp -> EquationsDescription
define symbol exp = modify aux
  where aux eqs = addUninferredEquation eqs symbol Nothing exp

defineWithType :: String -> Exp -> ExpType -> EquationsDescription
defineWithType symbol exp t = modify aux
  where aux eqs = addUninferredEquation eqs symbol (Just t) exp

initialConditionS :: String -> Array Scalar -> EquationsDescription
initialConditionS symbol xs = modify aux
  where aux eqs = addInitialCondition eqs symbol (InitialConditionScalar xs)

initialConditionV :: String -> Array Vector3 -> EquationsDescription
initialConditionV symbol xs = modify aux
  where aux eqs = addInitialCondition eqs symbol (InitialConditionVector xs)
