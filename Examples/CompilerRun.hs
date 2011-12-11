{-# LANGUAGE TemplateHaskell #-}

import SimulationDSL
import CompilerExamples
import qualified Data.Vector as V

-- main = print $ take 10 $ $(compile machine19)

main = print $ map (V.toList . (\(v,x,f,a) -> x))
             $ take 100 $ $(compile machine19)
