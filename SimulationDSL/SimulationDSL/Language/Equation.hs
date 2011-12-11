module SimulationDSL.Language.Equation
  ( Equation
  , makeEquation
  , equationSymbol
  , equationType
  , equationExp
  ) where

import SimulationDSL.Data.ExpType
import SimulationDSL.Language.Exp

data Equation = Equation { equationSymbol :: String
                         , equationType   :: ExpType
                         , equationExp    :: Exp
                         }

instance Show Equation where
  show eq = (equationSymbol eq) ++ " = " ++ show (equationExp eq) ++ " :: " ++ show (equationType eq)

makeEquation = Equation
