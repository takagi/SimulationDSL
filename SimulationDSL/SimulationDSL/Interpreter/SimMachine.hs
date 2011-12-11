module SimulationDSL.Interpreter.SimMachine
  ( runInterpreter
  , initialValue  -- for use in Compiler/SimMachine.hs
  ) where

import qualified Data.Vector as V hiding ( (!) )

import Data.Vector ( (!) )

import SimulationDSL.Data.Value
  ( Value(..)
  , valueZeroS
  , valueZeroV
  , addValue
  , subValue
  , mulValue
  , divValue
  , normValue
  )

import SimulationDSL.Data.ExpType
  ( ExpType(..)
  )

import SimulationDSL.Interpreter.Register
  ( RegisterValue
  , RegisterUpdatingRule
  )

import SimulationDSL.Interpreter.Machine
  ( Machine
  , makeMachine
  , machineRegisterValue
  , addMachineRegister
  , setMachineRegisterDependency
  , runMachine
  )

import SimulationDSL.Language.Exp
  ( Exp(..)
  , isIntegral )

import SimulationDSL.Language.InitialCondition
  ( InitialCondition(..)
  )

import SimulationDSL.Language.Equation
  ( Equation
  , equationSymbol
  , equationType
  , equationExp
  )

import SimulationDSL.Language.Equations
  ( Equations
  , equations
  , equation
  , equationsInitialCondition
  , equationsArraySize
  , equationsDependency
  )

import SimulationDSL.Language.EquationsDescription
  ( EquationsDescription
  , analyzedEquationsDescription
  )


runInterpreter :: EquationsDescription -> [Machine]
runInterpreter = runMachine
               . makeSimMachine
               . analyzedEquationsDescription

makeSimMachine :: Equations -> Machine
makeSimMachine eqs = setSimMachineRegisterDependency eqs
                   $ setSimMachineRegisters eqs
                   $ makeMachine

setSimMachineRegisters eqs machine = foldl aux machine (equations eqs)
  where aux :: Machine -> (String,Equation) -> Machine
        aux machine (symbol,eq) = addMachineRegister symbol val fn machine
          where val = initialValue eqs symbol eq
                fn  = updatingRule eqs (equationExp eq)

setSimMachineRegisterDependency :: Equations -> Machine -> Machine
setSimMachineRegisterDependency = setMachineRegisterDependency
                                . equationsDependency

initialValue :: Equations -> String -> Equation -> RegisterValue
initialValue eqs s0 eq
  | isIntegral (equationExp eq)
      = case equationsInitialCondition eqs symbol of
          Just x  -> fromInitialCondition x
          Nothing -> initialValueExp eqs s0 symbol exp
  | otherwise = initialValueExp eqs s0 symbol exp
  where symbol = equationSymbol eq
        exp    = equationExp eq

fromInitialCondition :: InitialCondition -> RegisterValue
fromInitialCondition (InitialConditionScalar x) = V.map ValueScalar x
fromInitialCondition (InitialConditionVector x) = V.map ValueVector x

initialValueExp :: Equations -> String -> String -> Exp -> RegisterValue
initialValueExp eqs _  s1 (Integral _)
  = let n = equationsArraySize eqs
    in case equationType (equation eqs s1) of
      ExpTypeScalar -> V.replicate n valueZeroS
      ExpTypeVector -> V.replicate n valueZeroV
initialValueExp eqs _  _  (Constant x)
  = let n = equationsArraySize eqs
    in V.replicate n x
initialValueExp eqs s0 _  (Var symbol)
  | symbol == s0 = error "initialValueExp: circular reference"
  | otherwise    = initialValue eqs s0 (equation eqs symbol)
initialValueExp _   _  _  (Var' _)
  = error "initialValueExp: Var' outside Sigma"
initialValueExp eqs s0 s1 (Add x y)
  = V.zipWith addValue (initialValueExp eqs s0 s1 x)
                       (initialValueExp eqs s0 s1 y)
initialValueExp eqs s0 s1 (Sub x y)
  = V.zipWith subValue (initialValueExp eqs s0 s1 x)
                       (initialValueExp eqs s0 s1 y)
initialValueExp eqs s0 s1 (Mul x y)
  = V.zipWith mulValue (initialValueExp eqs s0 s1 x)
                       (initialValueExp eqs s0 s1 y)
initialValueExp eqs s0 s1 (Div x y)
  = V.zipWith divValue (initialValueExp eqs s0 s1 x)
                       (initialValueExp eqs s0 s1 y)
initialValueExp eqs s0 s1 (Norm x)
  = V.map normValue (initialValueExp eqs s0 s1 x)
initialValueExp eqs s0 s1 (Sigma exp)
  = let n = equationsArraySize eqs
    in V.generate n $ \i ->
         let is = filter (/= i) [0..n-1]
         in if null is
            then case equationType (equation eqs s1) of
                   ExpTypeScalar -> valueZeroS
                   ExpTypeVector -> valueZeroV
            else (foldl1 addValue $ flip map is $ \j ->
                   initialValueSigmaOperand eqs s0 exp i j)

initialValueSigmaOperand :: Equations -> String -> Exp -> Int -> Int -> Value
initialValueSigmaOperand _ _ (Integral _) _ _
  = error "initialValueSigmaOperand: integral in sigma"
initialValueSigmaOperand _ _ (Constant x) _ _
  = x
initialValueSigmaOperand eqs s0 (Var symbol) i _
  = let eq = equation eqs symbol
    in (initialValue eqs s0 eq) ! i
initialValueSigmaOperand eqs s0 (Var' symbol) _ j
  = let eq = equation eqs symbol
    in (initialValue eqs s0 eq) ! j
initialValueSigmaOperand eqs s0 (Add x y) i j
  = addValue (initialValueSigmaOperand eqs s0 x i j)
             (initialValueSigmaOperand eqs s0 y i j)
initialValueSigmaOperand eqs s0 (Sub x y) i j
  = subValue (initialValueSigmaOperand eqs s0 x i j)
             (initialValueSigmaOperand eqs s0 y i j)
initialValueSigmaOperand eqs s0 (Mul x y) i j
  = mulValue (initialValueSigmaOperand eqs s0 x i j)
             (initialValueSigmaOperand eqs s0 y i j)
initialValueSigmaOperand eqs s0 (Div x y) i j
  = divValue (initialValueSigmaOperand eqs s0 x i j)
             (initialValueSigmaOperand eqs s0 y i j)
initialValueSigmaOperand eqs s0 (Norm x) i j
  = normValue (initialValueSigmaOperand eqs s0 x i j)
initialValueSigmaOperand _ _ (Sigma _) _ _
  = error "initialValueSigmaOperand: sigma in another sigma"

updatingRule :: Equations -> Exp -> RegisterUpdatingRule
updatingRule eqs (Integral exp)
  = \m i val -> addValue val ((updatingRule eqs exp) m i val)
updatingRule _   (Constant x)
  = \_ _ _   -> x
updatingRule _   (Var symbol)
  = \m i _   -> (machineRegisterValue m symbol) ! i
updatingRule _   (Var' _)
  = error "updatingRule: var' outside of sigma"
updatingRule eqs (Add x y)
  = \m i val -> addValue ((updatingRule eqs x) m i val)
                         ((updatingRule eqs y) m i val)
updatingRule eqs (Sub x y)
  = \m i val -> subValue ((updatingRule eqs x) m i val)
                         ((updatingRule eqs y) m i val)
updatingRule eqs (Mul x y)
  = \m i val -> mulValue ((updatingRule eqs x) m i val)
                         ((updatingRule eqs y) m i val)
updatingRule eqs (Div x y)
  = \m i val -> divValue ((updatingRule eqs x) m i val)
                         ((updatingRule eqs y) m i val)
updatingRule eqs (Norm x)
  = \m i val -> normValue ((updatingRule eqs x) m i val)
updatingRule eqs (Sigma exp)
  = let n = equationsArraySize eqs
    in \m i _ -> let is = filter (/= i) [0..n-1]
                 in foldl1 addValue $ flip map is $ \j ->
                      (updatingRuleSigmaOperand exp) m i j

updatingRuleSigmaOperand :: Exp -> (Machine -> Int -> Int -> Value)
updatingRuleSigmaOperand (Integral _)
  = error "updatingRuleSigmaOperand: must not be reached"
updatingRuleSigmaOperand (Constant x)
  = \_ _ _ -> x
updatingRuleSigmaOperand (Var symbol)
  = \m i _ -> (machineRegisterValue m symbol) ! i
updatingRuleSigmaOperand (Var' symbol)
  = \m _ j -> (machineRegisterValue m symbol) ! j
updatingRuleSigmaOperand (Add x y)
  = \m i j -> addValue ((updatingRuleSigmaOperand x) m i j)
                       ((updatingRuleSigmaOperand y) m i j)
updatingRuleSigmaOperand (Sub x y)
  = \m i j -> subValue ((updatingRuleSigmaOperand x) m i j)
                       ((updatingRuleSigmaOperand y) m i j)
updatingRuleSigmaOperand (Mul x y)
  = \m i j -> mulValue ((updatingRuleSigmaOperand x) m i j)
                       ((updatingRuleSigmaOperand y) m i j)
updatingRuleSigmaOperand (Div x y)
  = \m i j -> divValue ((updatingRuleSigmaOperand x) m i j)
                       ((updatingRuleSigmaOperand y) m i j)
updatingRuleSigmaOperand (Norm x)
  = \m i j -> normValue ((updatingRuleSigmaOperand x) m i j)
updatingRuleSigmaOperand (Sigma _)
  = error "updatingRuleSigmaOperand: must not be reached"
