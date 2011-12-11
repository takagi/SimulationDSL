module SimulationDSL.Interpreter.Machine ( Machine
                                         , makeMachine
                                         , machineRegister
                                         , machineRegisterValue
                                         , machineRegisterDependency
                                         , addMachineRegister
                                         , setMachineRegisterDependency
                                         , runMachine
                                         ) where

import qualified Data.Map as M
import SimulationDSL.Interpreter.Register

data Machine = Machine { machineRegisters          :: MachineRegisters
                       , machineRegisterDependency :: [String]
                       }

type MachineRegisters = M.Map String Register

makeMachine :: Machine
makeMachine = Machine M.empty []

addMachineRegister :: String -> RegisterValue -> RegisterUpdatingRule -> Machine -> Machine
addMachineRegister symbol val fn machine = machine { machineRegisters = regs' }
  where regs' = M.insert symbol (makeRegister symbol val fn) regs
        regs = machineRegisters machine

setMachineRegisterDependency :: [String] -> Machine -> Machine
setMachineRegisterDependency deps machine
  = machine { machineRegisterDependency = deps }

machineRegister :: Machine -> String -> Register
machineRegister machine symbol
  = case M.lookup symbol regs of
      Just x  -> x
      Nothing -> error ("register not found: " ++ symbol)
  where regs = machineRegisters machine

machineRegisterValue :: Machine -> String -> RegisterValue
machineRegisterValue machine = registerValue . machineRegister machine

runMachine :: Machine -> [Machine]
runMachine m
  | checkMachineRegisterDependency m = iterate updateMachine m
  | otherwise                        = error "runMachine: invalid machine register dependency"

checkMachineRegisterDependency :: Machine -> Bool
checkMachineRegisterDependency = not . null . machineRegisterDependency

updateMachine :: Machine -> Machine
updateMachine machine = foldl updateMachineRegister machine order
  where order = machineRegisterDependency machine

updateMachineRegister :: Machine -> String -> Machine
updateMachineRegister machine symbol = machine { machineRegisters = regs' }
  where regs' = let regs = machineRegisters machine
                    reg  = machineRegister machine symbol
                in M.insert symbol (updateRegister reg machine) regs
