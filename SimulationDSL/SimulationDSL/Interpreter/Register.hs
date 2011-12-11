module SimulationDSL.Interpreter.Register
  ( Register, RegisterValue, RegisterUpdatingRule
  , makeRegister
  , registerSymbol
  , registerValue
  , registerUpdatingRule
  , updateRegister
  ) where

import qualified Data.Vector as V
import SimulationDSL.Data.Value
import {-# SOURCE #-} SimulationDSL.Interpreter.Machine

data Register = Register { registerSymbol       :: String
                         , registerValue        :: RegisterValue
                         , registerUpdatingRule :: RegisterUpdatingRule
                         }

type RegisterValue = V.Vector Value

type RegisterUpdatingRule = Machine -> Int -> Value -> Value

makeRegister :: String -> RegisterValue -> RegisterUpdatingRule -> Register
makeRegister = Register

updateRegister :: Register -> Machine -> Register
updateRegister reg machine = reg { registerValue = V.imap update val }
  where val        = registerValue reg
        update i x = (registerUpdatingRule reg) machine i x
