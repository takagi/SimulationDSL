module SimulationDSL.Data.ExpType ( ExpType(..)
                                  ) where

data ExpType = ExpTypeScalar
             | ExpTypeVector
  deriving Eq

instance Show ExpType where
  show ExpTypeScalar = "Scalar"
  show ExpTypeVector = "Vector"
