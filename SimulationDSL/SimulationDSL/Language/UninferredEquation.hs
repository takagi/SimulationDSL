module SimulationDSL.Language.UninferredEquation
  ( UninferredEquation
  , makeUninferredEquation
  , uninferredEquationSymbol
  , uninferredEquationType
  , uninferredEquationExp
  ) where

import SimulationDSL.Data.ExpType
import SimulationDSL.Language.Exp

data UninferredEquation = UninferredEquation
  { uninferredEquationSymbol :: String
  , uninferredEquationType   :: (Maybe ExpType)
  , uninferredEquationExp    :: Exp
  }

makeUninferredEquation :: String -> Maybe ExpType -> Exp -> UninferredEquation
makeUninferredEquation = UninferredEquation
