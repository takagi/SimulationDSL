module SimulationDSL.Data.Vector3 ( Vector3
                                  , addVector
                                  , subVector
                                  , scaleVector
                                  , normVector
                                  ) where

import SimulationDSL.Data.Scalar

type Vector3 = ( Scalar, Scalar, Scalar )

addVector :: Vector3 -> Vector3 -> Vector3
addVector (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)

subVector :: Vector3 -> Vector3 -> Vector3
subVector (x0,y0,z0) (x1,y1,z1) = (x0-x1,y0-y1,z0-z1)

scaleVector :: Scalar -> Vector3 -> Vector3
scaleVector a (x,y,z) = (a*x, a*y, a*z)

normVector :: Vector3 -> Scalar
normVector (x,y,z) = sqrt $ x*x + y*y + z*z
