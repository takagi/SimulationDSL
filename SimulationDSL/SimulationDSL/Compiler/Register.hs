module SimulationDSL.Compiler.Register ( Register, RegisterValue
                                       , makeRegister
                                       , registerSymbol
                                       , registerType
                                       , registerValue
                                       , registerExp
                                       ) where

import SimulationDSL.Data.ExpType
import SimulationDSL.Interpreter.Register ( RegisterValue )
import SimulationDSL.Language.Exp


data Register = Register { registerSymbol :: String
                         , registerType   :: ExpType
                         , registerValue  :: RegisterValue
                         , registerExp    :: Exp
                         }

makeRegister :: String -> ExpType -> RegisterValue -> Exp -> Register
makeRegister = Register
