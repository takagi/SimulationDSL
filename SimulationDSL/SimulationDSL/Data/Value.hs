module SimulationDSL.Data.Value ( Value(..), valueZeroS, valueZeroV
                                , addValue
                                , subValue
                                , mulValue
                                , divValue
                                , normValue
                                ) where

import SimulationDSL.Data.Scalar
import SimulationDSL.Data.Vector3

data Value = ValueScalar Scalar
           | ValueVector Vector3
  deriving Eq

instance Show Value where
  show (ValueScalar x) = show x
  show (ValueVector x) = show x

valueZeroS, valueZeroV :: Value
valueZeroS = ValueScalar 0
valueZeroV = ValueVector (0,0,0)

addValue :: Value -> Value -> Value
addValue (ValueScalar x) (ValueScalar y) = ValueScalar $ x + y
addValue (ValueVector x) (ValueVector y) = ValueVector $ addVector x y
addValue _               _               = error "addValue: invalid operand(s)"

subValue :: Value -> Value -> Value
subValue (ValueScalar x) (ValueScalar y) = ValueScalar $ x - y
subValue (ValueVector x) (ValueVector y) = ValueVector $ subVector x y
subValue _               _               = error "subValue: invalid operand(s)"

mulValue :: Value -> Value -> Value
mulValue (ValueScalar x) (ValueScalar y) = ValueScalar $ x * y
mulValue (ValueScalar a) (ValueVector x) = ValueVector $ scaleVector a x
mulValue (ValueVector x) (ValueScalar a) = ValueVector $ scaleVector a x
mulValue _               _               = error "mulValue: invalid operand(s)"

divValue :: Value -> Value -> Value
divValue (ValueScalar x) (ValueScalar y) = ValueScalar $ x / y
divValue (ValueVector x) (ValueScalar a) = ValueVector $ scaleVector (recip a) x
divValue _               _               = error "divValue: invalid operand(s)"

normValue :: Value -> Value
normValue (ValueVector x) = ValueScalar $ normVector x
normValue _               = error "normValue: invalid operand"
