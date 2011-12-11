module SimulationDSL.Compiler.SimMachine ( compile ) where

import Language.Haskell.TH
import SimulationDSL.Compiler.Machine
import SimulationDSL.Interpreter.SimMachine ( initialValue )
import SimulationDSL.Language.Equations
import SimulationDSL.Language.EquationsDescription

makeSimMachine :: Equations -> Machine
makeSimMachine eqs = setSimMachineRegisterDependency eqs
                   $ setSimMachineRegisters eqs
                   $ makeMachine

setSimMachineRegisters :: Equations -> Machine -> Machine
setSimMachineRegisters eqs machine = foldl aux machine (equations eqs)
  where aux :: Machine -> (String,Equation) -> Machine
        aux machine (symbol,eq) = addMachineRegister symbol t val exp machine
          where t   = equationType eq
                val = initialValue eqs symbol eq
                exp = equationExp eq

setSimMachineRegisterDependency :: Equations -> Machine -> Machine
setSimMachineRegisterDependency = setMachineRegisterDependency
                                . equationsDependency

compile :: EquationsDescription -> ExpQ
compile = compileMachine
        . makeSimMachine
        . analyzedEquationsDescription
