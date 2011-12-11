module SimulationDSL.Compiler.Machine where
  
  import Language.Haskell.TH hiding ( Exp )
  import SimulationDSL.Data.ExpType
  import SimulationDSL.Compiler.Register
  import SimulationDSL.Language.Exp

  data Machine
  
  makeMachine                  :: Machine
  machineRegister              :: Machine -> String -> Register
  machineRegisterDependency    :: Machine -> [String]
  addMachineRegister           :: String -> ExpType -> RegisterValue -> Exp -> Machine -> Machine
  setMachineRegisterDependency :: [String] -> Machine -> Machine
  compileMachine               :: Machine -> ExpQ