module SimulationDSL.Data.Array ( Array
                                , arraySize
                                , module Data.Vector
                                ) where

import Data.Vector ( Vector, length, fromList, toList )

type Array = Vector

arraySize :: Array a -> Int
arraySize = Data.Vector.length
