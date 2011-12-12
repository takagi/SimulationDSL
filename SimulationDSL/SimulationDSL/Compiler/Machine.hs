module SimulationDSL.Compiler.Machine ( Machine
                                      , makeMachine
                                      , machineRegister
                                      , machineRegisterDependency
                                      , addMachineRegister
                                      , setMachineRegisterDependency
                                      , compileMachine
                                      , ) where

import Language.Haskell.TH hiding ( Exp )
import qualified Data.Map as M
import SimulationDSL.Data.ExpType
import SimulationDSL.Compiler.Register
import SimulationDSL.Compiler.CodeGen
import SimulationDSL.Language.Exp

data Machine = Machine { machineRegisters          :: MachineRegisters
                       , machineRegisterDependency :: [String]
                       }

type MachineRegisters = M.Map String Register

makeMachine :: Machine
makeMachine = Machine M.empty []

addMachineRegister :: String -> ExpType -> RegisterValue -> Exp -> Machine -> Machine
addMachineRegister symbol t val exp machine
  = machine { machineRegisters = regs' }
  where regs' = M.insert symbol (makeRegister symbol t val exp) regs
        regs  = machineRegisters machine

setMachineRegisterDependency :: [String] -> Machine -> Machine
setMachineRegisterDependency deps machine
  = machine { machineRegisterDependency = deps }

machineRegister :: Machine -> String -> Register
machineRegister machine symbol
  = case M.lookup symbol regs of
      Just x  -> x
      Nothing -> error ("register not found: " ++ symbol)
  where regs = machineRegisters machine

compileMachine :: [String] -> Machine -> ExpQ
compileMachine outs m
  | checkMachineRegisterDependency m = compile outs m
  | otherwise                        = error "compileMachine: invalid machine register dependency"

checkMachineRegisterDependency :: Machine -> Bool
checkMachineRegisterDependency = not . null . machineRegisterDependency
