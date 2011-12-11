module SimulationDSL.Language.Equations
  ( Equations( .. )
  , emptyEquations
  , addEquation
  , addInitialCondition
  , equation
  , equations
  , equationsDependency
  , equationsInitialConditions
  , equationsInitialCondition
  , equationsArraySize
  , checkedEquations
  --
  , Equation
  , equationSymbol
  , equationType
  , equationExp
  ) where

import Data.List ( nub )
import qualified Data.Map as M
import Control.Monad.Error
import SimulationDSL.Data.ExpType
import SimulationDSL.Language.Exp
import SimulationDSL.Language.Equation
import SimulationDSL.Language.InitialCondition

data Equations = Equations (M.Map String Equation)
                           (M.Map String InitialCondition)

instance Show Equations where
 show eqs = unlines (["definition(s):"] ++
                     (if not $ null $ equations eqs
                      then map (indent . show . snd) (equations eqs)
                      else [indent "no definitions"])
                     ++ [indent ""] ++
                     ["initial condition(s):"] ++
                     (if (not $ null $ equationsInitialConditions eqs)
                      then map (indent . show . snd)
                               (equationsInitialConditions eqs)
                      else [indent "no initial conditions"]))
               where indent = (++) "  "

emptyEquations :: Equations
emptyEquations = Equations M.empty M.empty

addEquation :: Equations -> String -> ExpType -> Exp -> Equations
addEquation (Equations eqs ics) symbol t exp
  = Equations eqs' ics
    where eqs' = M.insert symbol (makeEquation symbol t exp) eqs

addInitialCondition :: Equations -> String -> InitialCondition -> Equations
addInitialCondition (Equations eqs ics) symbol ic
  = Equations eqs ics'
    where ics' = M.insert symbol ic ics

equation :: Equations -> String -> Equation
equation (Equations eqs _) symbol
  = case M.lookup symbol eqs of
      Just x  -> x
      Nothing -> error "equation: symbol not found"

equations :: Equations -> [(String,Equation)]
equations (Equations x _) = M.assocs x

equationsDependency :: Equations -> [String]
equationsDependency eqs = nub $ concat $ map aux $ equations eqs
  where aux (symbol,eq) = dependency eqs symbol (equationExp eq) ++ [symbol]

equationsInitialConditions :: Equations -> [(String,InitialCondition)]
equationsInitialConditions (Equations _ x) = M.assocs x

equationsInitialCondition :: Equations -> String -> Maybe InitialCondition
equationsInitialCondition (Equations _ x) symbol = M.lookup symbol x

equationsArraySize :: Equations -> Int
equationsArraySize eqs
  | null ics  = error "equationsArraySize: no initial condition"
  | otherwise = initialConditionArraySize $ snd $ head ics
  where ics = equationsInitialConditions eqs

dependency :: Equations -> String -> Exp -> [String]
dependency eqs _  (Integral exp) = dependencyOfIntegralOperand eqs exp
dependency _   _  (Constant _)   = []
dependency eqs s0 (Var symbol)
  | s0 == symbol = error "dependency: circular reference"
  | otherwise    = dependency eqs s0 exp ++ [symbol]
  where exp = equationExp $ equation eqs symbol
dependency eqs s0 (Var' symbol)
  | s0 == symbol = error "dependency: circular reference"
  | otherwise    = dependency eqs s0 exp ++ [symbol]
  where exp = equationExp $ equation eqs symbol
dependency eqs s0 (Add x y)   = dependency eqs s0 x ++ dependency eqs s0 y
dependency eqs s0 (Sub x y)   = dependency eqs s0 x ++ dependency eqs s0 y
dependency eqs s0 (Mul x y)   = dependency eqs s0 x ++ dependency eqs s0 y
dependency eqs s0 (Div x y)   = dependency eqs s0 x ++ dependency eqs s0 y
dependency eqs s0 (Norm x)    = dependency eqs s0 x
dependency eqs s0 (Sigma exp) = dependency eqs s0 exp

dependencyOfIntegralOperand :: Equations -> Exp -> [String]
dependencyOfIntegralOperand eqs (Integral _)
  = error "dependencyOfIntegralOperand: integral in another integral"
dependencyOfIntegralOperand eqs (Constant _)
  = []
dependencyOfIntegralOperand eqs (Var symbol)
  = let exp = equationExp $ equation eqs symbol
    in case exp of
         (Integral exp') -> dependencyOfIntegralOperand eqs exp' ++ [symbol]
         otherwise       -> []
dependencyOfIntegralOperand eqs (Var' symbol)
  = let exp = equationExp $ equation eqs symbol
    in case exp of
         (Integral exp') -> dependencyOfIntegralOperand eqs exp' ++ [symbol]
         otherwise       -> []
dependencyOfIntegralOperand eqs (Add x y)
  = dependencyOfIntegralOperand eqs x ++ dependencyOfIntegralOperand eqs y
dependencyOfIntegralOperand eqs (Sub x y)
  = dependencyOfIntegralOperand eqs x ++ dependencyOfIntegralOperand eqs y
dependencyOfIntegralOperand eqs (Mul x y)
  = dependencyOfIntegralOperand eqs x ++ dependencyOfIntegralOperand eqs y
dependencyOfIntegralOperand eqs (Div x y)
  = dependencyOfIntegralOperand eqs x ++ dependencyOfIntegralOperand eqs y
dependencyOfIntegralOperand eqs (Norm x)
  = dependencyOfIntegralOperand eqs x
dependencyOfIntegralOperand eqs (Sigma _)
  = error "dependencyOfIntegralOperand: sigma in integral"

-- todo: should be rewritten using appropriate error handling mechanism
checkedEquations :: Equations -> Equations
checkedEquations eqs
  = checkEquationsArraySize
  $ foldl checkEquation eqs (equations eqs)

-- todo: rewrite
checkEquationsArraySize :: Equations -> Equations
checkEquationsArraySize eqs
  = case n of
      1         -> eqs
      0         -> error "equationsArraySize: array size unspecified"
      otherwise -> error "equationsArraySize: invalid array size"
  where n = length
          $ nub 
          $ map initialConditionArraySize 
          $ map snd 
          $ equationsInitialConditions eqs

checkEquation :: Equations -> (String,Equation) -> Equations
checkEquation eqs (_,eq) = checkEquationWithInitialCondition eq
                         $ checkEquationTypeWithInitialCondition eq
                         $ checkEquationExp eq eqs

checkEquationWithInitialCondition :: Equation -> Equations -> Equations
checkEquationWithInitialCondition eq eqs
  | isIntegral (equationExp eq) = eqs
  | otherwise
      = case equationsInitialCondition eqs (equationSymbol eq) of
          Just x  -> error msg
          Nothing -> eqs
  where msg = "checkEquations: non-integral equation with initial condition"
              ++ ", which would be unused"

checkEquationTypeWithInitialCondition :: Equation -> Equations -> Equations
checkEquationTypeWithInitialCondition eq eqs
  | isIntegral (equationExp eq)
      = case equationsInitialCondition eqs (equationSymbol eq) of
          Just x  -> if equationType eq == initialConditionType x
                     then eqs
                     else error "checkEquation: type confliction"
          Nothing -> eqs
  | otherwise = eqs

checkEquationExp :: Equation -> Equations -> Equations
checkEquationExp eq eqs = case checkExp eqs exp of
                            True  -> eqs
                            False -> error "checkEquationExp: invalid Expression"
  where exp = equationExp eq

checkExp :: Equations -> Exp -> Bool
checkExp eqs (Integral exp) = not (containIntegral exp) &&
                              not (containSigma exp) &&
                              checkExp eqs exp
checkExp _   (Constant _)   = True
checkExp _   (Var _)        = True
checkExp _   (Var' _)       = True
checkExp eqs (Add x y)      = checkExpBinOp eqs x y
checkExp eqs (Sub x y)      = checkExpBinOp eqs x y
checkExp eqs (Mul x y)      = checkExpBinOp eqs x y
checkExp eqs (Div x y)      = checkExpBinOp eqs x y
checkExp eqs (Norm x)       = checkExpUnaryOp eqs x
checkExp eqs (Sigma exp)    = not (containIntegral exp) &&
                              not (containSigma exp) &&
                              checkExp eqs exp

checkExpUnaryOp :: Equations -> Exp -> Bool
checkExpUnaryOp eqs x = not (isIntegral x) && checkExp eqs x

checkExpBinOp :: Equations -> Exp -> Exp -> Bool
checkExpBinOp eqs x y = not (isIntegral x) &&
                        not (isIntegral y) &&
                        checkExp eqs x     &&
                        checkExp eqs y

