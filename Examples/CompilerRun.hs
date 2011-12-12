{-# LANGUAGE TemplateHaskell #-}

import SimulationDSL
import CompilerExamples
import qualified Data.Vector as V

-- main = print $ take 10 $ $(compile machine19)

main = print $ take 100 $ $(compile ["x"] machine19)
