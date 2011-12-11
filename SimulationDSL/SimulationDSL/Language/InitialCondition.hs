module SimulationDSL.Language.InitialCondition
  ( InitialCondition(..)
  , initialConditionArraySize
  , initialConditionType
  ) where

import SimulationDSL.Data.Scalar
import SimulationDSL.Data.Vector3
import SimulationDSL.Data.Array
import SimulationDSL.Data.ExpType

data InitialCondition = InitialConditionScalar (Array Scalar)
                      | InitialConditionVector (Array Vector3)

instance Show InitialCondition where
  show _ = "..."

initialConditionArraySize :: InitialCondition -> Int
initialConditionArraySize (InitialConditionScalar x) = arraySize x
initialConditionArraySize (InitialConditionVector x) = arraySize x

initialConditionType :: InitialCondition -> ExpType
initialConditionType (InitialConditionScalar _) = ExpTypeScalar
initialConditionType (InitialConditionVector _) = ExpTypeVector
