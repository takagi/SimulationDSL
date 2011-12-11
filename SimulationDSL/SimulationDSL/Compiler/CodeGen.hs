{-# LANGUAGE TemplateHaskell #-}

module SimulationDSL.Compiler.CodeGen ( compile ) where

import qualified Data.Vector as V hiding ( (!) )
import Data.Vector ( (!) )
import Data.Char ( toUpper )
import Language.Haskell.TH hiding ( Exp )
import SimulationDSL.Data.Vector3
import SimulationDSL.Data.Value
import SimulationDSL.Data.ExpType
import SimulationDSL.Compiler.Register
import {-# SOURCE #-} SimulationDSL.Compiler.Machine
import SimulationDSL.Language.Exp

compile :: Machine -> ExpQ
compile m = letE (compileLetDecs m xs)
                  compileLetExp
  where xs = machineRegisterDependency m

-- let updateAll = updateX . updateV . updateA
--     updateX (x,v,a) = ...
--     updateV (x,v,a) = ...
--     updateA (x,v,a) = ...
--     initialCondition = ...
-- in ...
compileLetDecs :: Machine -> [String] -> [DecQ]
compileLetDecs m xs = [ updateAllDec xs ] ++
                      map (updateOneDec m xs) xs ++
                      [ initialConditionDec m xs ]

-- updateAll = updateX . updateV . updateA
updateAllDec :: [String] -> DecQ
updateAllDec xs = valD (updateAllDecPat)
                       (updateAllDecBody xs)
                       []

-- updateAll = ...
updateAllDecPat :: PatQ
updateAllDecPat = varP $ mkName "updateAll"

-- ... = updateX . updateV . updateA
updateAllDecBody :: [String] -> BodyQ
updateAllDecBody xs = normalB $ foldl1 aux
                              $ map (varE . mkName . updateOneName) xs
  where aux x y = infixApp x (varE $ mkName ".") y

updateOneName :: String -> String
updateOneName = (++) "update" . capitalize

-- updateX (x,v,a) = let x' = ...
--                   in (x',v,a)
updateOneDec :: Machine -> [String] -> String -> DecQ
updateOneDec m xs x = funD (mkName $ updateOneName x)
                           [updateOneDecClause m xs x]

-- ... (x,v,a) = let x' = ...
--               in (x',v,a)
updateOneDecClause :: Machine -> [String] -> String -> ClauseQ
updateOneDecClause m xs x = clause [updateOneDecClausePat xs]
                                   (updateOneDecClauseBody m xs x)
                                   []

-- ... (x,v,a) = ...
updateOneDecClausePat :: [String] -> PatQ
updateOneDecClausePat xs = tupP $ map (varP . mkName) xs

-- ... = let x' = ...
--       in (x',v,a)
updateOneDecClauseBody :: Machine -> [String] -> String -> BodyQ
updateOneDecClauseBody m xs x = normalB exp
  where exp = letE [updateOneDecClauseBodyDec m xs x]
                   (updateOneDecClauseBodyExp xs x)

-- ... x' = V.imap (\i x -> ...) x
updateOneDecClauseBodyDec :: Machine -> [String] -> String -> DecQ
updateOneDecClauseBodyDec m xs x = valD (varP $ mkName (x ++ "'"))
                                        (normalB exp)
                                        []
  where exp = let var = varE $ mkName x
                  lam = updateOneDecClauseBodyDecLam m x
              in [| V.imap $(lam) ($var) |]

-- ... \i x -> ...
updateOneDecClauseBodyDecLam :: Machine -> String -> ExpQ
updateOneDecClauseBodyDecLam m x = lamE [ varP $ mkName "i"
                                        , varP $ mkName $ x ++ "i" ]
                                        [| $(compileExp m x exp) |]
  where exp = registerExp $ machineRegister m x

-- ... in (x',v,a)
updateOneDecClauseBodyExp :: [String] -> String -> ExpQ
updateOneDecClauseBodyExp xs x = tupE $ map (varE . mkName . aux) xs
  where aux x' | x == x'   = x' ++ "'"
               | otherwise = x'


-- initialCondition = let x0 = ...
--                        v0 = ...
--                        a0 = ...
--                    in (x0,v0,a0)
initialConditionDec :: Machine -> [String] -> DecQ
initialConditionDec m xs = valD initialConditionDecPat
                                (initialConditionDecBody m xs)
                                []

-- initialCondition = ...
initialConditionDecPat :: PatQ
initialConditionDecPat = varP $ mkName "initialCondition"

-- ... = let x0 = V.fromList ...
--           v0 = V.fromList ...
--           a0 = V.fromList ...
--       in (x0,v0,a0)
initialConditionDecBody :: Machine -> [String] -> BodyQ
initialConditionDecBody m xs = normalB exp
  where exp = letE (map (initialConditionDecBodyDec m) xs)
                   (initialConditionDecBodyExp xs)

-- ... x0 = V.fromList ...
initialConditionDecBodyDec :: Machine -> String -> DecQ
initialConditionDecBodyDec m x
  = valD (varP $ mkName $ x ++ "0")
         (normalB $ initialConditionDecBodyDecExp m x)
         []

-- ... = V.fromList ...
initialConditionDecBodyDecExp m x
  = let values = registerValue $ machineRegister m x
        xs     = listE $ map aux (V.toList values)
    in [| V.fromList $(xs) |]
  where aux (ValueScalar x)        = litE $ RationalL $ toRational x
        aux (ValueVector (x,y,z)) = tupE
                                  $ map (litE . RationalL
                                              . toRational) [x,y,z]

-- ... (x0,v0,a0)
initialConditionDecBodyExp :: [String] -> ExpQ
initialConditionDecBodyExp xs = tupE $ map (varE . mkName . flip (++) "0") xs


-- let ...
-- in iterate updateAll initialCondition
compileLetExp :: ExpQ
compileLetExp = let updateAll = varE $ mkName "updateAll"
                    initialCondition = varE $ mkName "initialCondition"
                in [| iterate $(updateAll) $(initialCondition) |]

compileExp :: Machine -> String -> Exp -> ExpQ
compileExp m s (Integral exp)
  = let xi = varE $ mkName $ s ++ "i"
        i = varE $ mkName "i"
    in case registerType $ machineRegister m s of
         ExpTypeScalar -> [| $xi + $(compileExp m s exp) |]
         ExpTypeVector -> [| addVector $xi $(compileExp m s exp) |]
compileExp _ _ (Constant val) = compileValue val
compileExp _ _ (Var symbol)   = let x = varE $ mkName symbol
                                    i = varE $ mkName "i"
                                in [| $x ! $i |]
compileExp _ _ (Var' symbol)   = let x = varE $ mkName symbol
                                     j = varE $ mkName "j"
                                 in [| $x ! $j |]
compileExp m s (Add x y)
  = case (expType m x, expType m y) of
      (ExpTypeScalar, ExpTypeScalar) -> [| $x' + $y' |]
      (ExpTypeVector, ExpTypeVector) -> [| addVector $x' $y' |]
      otherwise                      -> error "compileExp: invalid operand(s)"
  where x' = compileExp m s x
        y' = compileExp m s y
compileExp m s (Sub x y)
  = case (expType m x, expType m y) of
      (ExpTypeScalar, ExpTypeScalar) -> [| $x' - $y' |]
      (ExpTypeVector, ExpTypeVector) -> [| subVector $x' $y' |]
      otherwise                      -> error "compileExp: invalid operand(s)"
  where x' = compileExp m s x
        y' = compileExp m s y
compileExp m s (Mul x y)
  = case (expType m x, expType m y) of
      (ExpTypeScalar, ExpTypeScalar) -> [| $x' * $y' |]
      (ExpTypeScalar, ExpTypeVector) -> [| scaleVector $x' $y' |]
      (ExpTypeVector, ExpTypeScalar) -> [| scaleVector $y' $x' |]
      otherwise                      -> error "compileExp: invalid operand(s)"
  where x' = compileExp m s x
        y' = compileExp m s y
compileExp m s (Div x y)
  = case (expType m x, expType m y) of
      (ExpTypeScalar, ExpTypeScalar) -> [| $x' / $y' |]
      (ExpTypeVector, ExpTypeScalar) -> [| scaleVector (recip $y') $x' |]
      otherwise                      -> error "compileExp: invalid operand(s)"
  where x' = compileExp m s x
        y' = compileExp m s y
compileExp m s (Norm x)
  = case expType m x of
      ExpTypeVector -> [| normVector $x' |]
  where x' = compileExp m s x
compileExp m s (Sigma exp)
  = let x = varE $ mkName s
        i = varE $ mkName "i"
    in [| let n   = V.length $x
              aux = $(lam1E (varP $ mkName "j")
                            (compileExp m s exp))
          in foldl1 $(addOp) $ map aux (filter (/= $i) [0..n-1]) |]
  where addOp = case expType m exp of
                  ExpTypeScalar -> [| (+) |]
                  ExpTypeVector -> [| addVector |]

compileValue :: Value -> ExpQ
compileValue (ValueScalar v) = litE $ RationalL $ toRational v
compileValue (ValueVector v) = let (x,y,z) = v
                               in tupE $ map ( litE
                                             . RationalL
                                             . toRational) [x,y,z]

expType :: Machine -> Exp -> ExpType
expType m (Integral exp) = expType m exp
expType _ (Constant x)
  = case x of
      ValueScalar _ -> ExpTypeScalar
      ValueVector _ -> ExpTypeVector
expType m (Var symbol)   = registerType $ machineRegister m symbol
expType m (Var' symbol)  = registerType $ machineRegister m symbol
expType m (Add x y)
  = case (expType m x, expType m y) of
      (ExpTypeScalar, ExpTypeScalar) -> ExpTypeScalar
      (ExpTypeVector, ExpTypeVector) -> ExpTypeVector
      otherwise                      -> error "expType: invalid operand(s)"
expType m (Sub x y)
  = case (expType m x, expType m y) of
      (ExpTypeScalar, ExpTypeScalar) -> ExpTypeScalar
      (ExpTypeVector, ExpTypeVector) -> ExpTypeVector
      otherwise                      -> error "expType: invalid operand(s)"
expType m (Mul x y)
  = case (expType m x, expType m y) of
      (ExpTypeScalar, ExpTypeScalar) -> ExpTypeScalar
      (ExpTypeScalar, ExpTypeVector) -> ExpTypeVector
      (ExpTypeVector, ExpTypeScalar) -> ExpTypeVector
      otherwise                      -> error "expType: invalid operand(s)"
expType m (Div x y)
  = case (expType m x, expType m y) of
      (ExpTypeScalar, ExpTypeScalar) -> ExpTypeScalar
      (ExpTypeVector, ExpTypeScalar) -> ExpTypeVector
      otherwise                      -> error "expType: invalid operand(s)"
expType m (Norm x)
  = case expType m x of
      ExpTypeVector -> ExpTypeScalar
      otherwise     -> error "expType: invalid operand(s)"
expType m (Sigma exp)
  = expType m exp

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

printQ :: Ppr a => Q a -> IO ()
printQ x = fmap pprint (runQ x) >>= putStrLn
