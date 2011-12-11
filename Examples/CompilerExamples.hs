module CompilerExamples where

import Data.Vector as V
import SimulationDSL

-- equationsArraySize: array size unspecified
machine1 = do define "x" (constantS 1)

-- equationsArraySize: array size unspecified
machine1' = do define "x" (constantS 1)

-- equationsArraySize: array size unspecified
machine2 = do define "x" (constantV (1,1,1))

-- checkEquations: non-integral equation with initial condition, which would be unused
machine2' = do define "x" (constantV (1,1,1))
               initialConditionS "x" x0
  where x0 = V.fromList [0]

-- ok
machine3 = do define "x" (integral (constantS 1))
              initialConditionS "x" (V.fromList[0,1])

-- ok
machine4 = do define "x" (integral (constantV (1,1,1)))
              initialConditionV "x" (V.fromList [(0,0,0), (1,1,1)])

-- checkEquation: type confliction
machine4' = do define "x" (integral (constantV (1,1,1)))
               initialConditionS "x" (V.fromList [0,1])

-- ok
machine5 = do define "x" (integral (var "v"))
              define "v" (constantS 1)
              initialConditionS "x" (V.fromList [0,1])

-- checkEquations: non-integral equation with initial condition, which would be unused
machine5' = do define "x" (integral (var "v"))
               define "v" (constantS 1)
               initialConditionS "v" (V.fromList [0,1])

-- ok
machine6 = do define "x" (integral (var "v"))
              define "v" (constantV (1,1,1))
              initialConditionV "x" (V.fromList [(0,0,0),(1,1,1)])

-- ok
machine7 = do define "x" (integral (var "v"))
              define "v" (constantV (1,1,1) <*> constantS 2)
              initialConditionV "x" (V.fromList [(0,0,0),(1,1,1)])

-- equationsArraySize: array size unspecified
machine8 = do define "x" (integral (var "v"))
              define "v" (constantV (1,1,1) <*> constantV (2,2,2))

-- inferExpType: invalid operand(s)
machine8' = do define "x" (integral (var "v"))
               define "v" (constantV (1,1,1) <*> constantV (2,2,2))
               initialConditionV "x" (fromList [(0,0,0)])

-- equationsArraySize: array size unspecified
machine9 = do define "x" (integral (var "v"))
              define "v" (integral (var "a"))
              define "a" (var "x" <+> var "v")

-- inferExpType: circular reference
machine9' = do define "x" (integral (var "v"))
               define "v" (integral (var "a"))
               define "a" (var "x" <+> var "v")
               initialConditionS "x" (V.fromList [0])

-- equationsArraySize: array size unspecified
machine10 = do define         "x" (integral (var "v"))
               define         "v" (integral (var "a"))
               defineWithType "a" (var "x" <+> var "v") ExpTypeScalar

-- ok
machine10' = do define         "x" (integral (var "v"))
                define         "v" (integral (var "a"))
                defineWithType "a" (var "x" <+> var "v") ExpTypeScalar
                initialConditionS "x" (V.fromList [0])

-- ok
machine10'' = do defineWithType "x" (integral (var "v")) ExpTypeScalar
                 defineWithType "v" (integral (var "a")) ExpTypeScalar
                 define         "a" (var "x" <+> var "v")
                 initialConditionS "x" (V.fromList [0])

-- ok
machine10''' = do define         "x" (integral (var "v"))
                  defineWithType "v" (integral (var "a")) ExpTypeScalar
                  define         "a" (var "x" <+> var "v")
                  initialConditionS "x" (V.fromList [0])

-- inferExpType: circular reference
-- 本格的な型推論をすれば型は定まるが、いまの型推論の方法では型が定まらない
machine11 = do defineWithType "x" (integral (var "v")) ExpTypeScalar
               define         "v" (integral (var "a"))
               define         "a" (var "x" <+> var "v")
               initialConditionS "x" (V.fromList [0])

-- inferExpType: circular reference
-- initialConditionの型をvの型にしてしまえば、型は定まる
machine11' = do defineWithType "x" (integral (var "v")) ExpTypeScalar
                define         "v" (integral (var "a"))
                define         "a" (var "x" <+> var "v")
                initialConditionS "v" (V.fromList [0])

-- ok
machine19 =
  do define         "x" (integral (var "v" <*> dt))
     define         "v" (integral (var "a" <*> dt))
     defineWithType "a" (var "f" </> m) ExpTypeVector
     define         "f" (let r = norm (var' "x" <-> var "x")
                             k = m <*> m <*> g </> r </> r
                             n = (var' "x" <-> var "x") </> r
                         in sigma (k <*> n))
     initialConditionV "x" x0
  where dt = constantS 0.01
        m  = constantS 1
        g  = constantS 9.8
--        x0 =  V.fromList [ (0,0,0), (2,0,0), (0,2,0) ]
        x0 =  V.fromList [ (i,i,i)
                         | i <- [0..99]]

