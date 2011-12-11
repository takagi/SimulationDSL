module SimulationDSL.Language.Exp
  ( Exp(..)
  , integral
  , constantS, constantV
  , var, var'
  , norm
  , sigma
  , (<+>), (<->), (<*>), (</>)
  , isIntegral
  , isSigma
  , containIntegral
  , containSigma
  ) where

import SimulationDSL.Data.Scalar
import SimulationDSL.Data.Vector3
import SimulationDSL.Data.Value

data Exp = Integral Exp
         | Constant Value
         | Var String
         | Var' String
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Norm Exp
         | Sigma Exp

instance Show Exp where
  show (Constant x)   = show x
  show (Integral exp) = showIntegral exp
  show (Var symbol)   = symbol
  show (Var' symbol)  = symbol ++ "'"
  show (Add x y)      = showBinaryOp "+" x y
  show (Sub x y)      = showBinaryOp "-" x y
  show (Mul x y)      = showBinaryOp "*" x y
  show (Div x y)      = showBinaryOp "/" x y
  show (Norm x)       = showNorm x
  show (Sigma exp)    = showSigma exp

showIntegral :: Exp -> String
showIntegral exp = "integral " ++ showWithParenthesisIfNotAtom exp

showSigma :: Exp -> String
showSigma exp = "Σ" ++ showWithParenthesisIfNotAtom exp

showBinaryOp :: String -> Exp -> Exp -> String
showBinaryOp op x y = x' ++ " " ++ op ++ " " ++ y'
  where x' = showWithParenthesisIfNotAtom x
        y' = showWithParenthesisIfNotAtom y

showNorm :: Exp -> String
showNorm x = "|" ++ show x ++ "|"

showWithParenthesisIfNotAtom :: Exp -> String
showWithParenthesisIfNotAtom exp
  | isAtom exp = show exp
  | otherwise  = parenthesis $ show exp

parenthesis :: String -> String
parenthesis x = "(" ++ x ++ ")"

isAtom :: Exp -> Bool
isAtom (Constant _) = True
isAtom (Var _)      = True
isAtom (Var' _)     = True
isAtom (Norm _)     = True
isAtom _            = False

integral :: Exp -> Exp
integral = Integral

constantS :: Scalar -> Exp
constantS = Constant . ValueScalar

constantV :: Vector3 -> Exp
constantV = Constant . ValueVector

var, var' :: String -> Exp
var  = Var
var' = Var'

norm :: Exp -> Exp
norm = Norm

sigma :: Exp -> Exp
sigma = Sigma

(<+>), (<->), (<*>), (</>) :: Exp -> Exp -> Exp
(<+>) = Add
(<->) = Sub
(<*>) = Mul
(</>) = Div

containIntegral :: Exp -> Bool
containIntegral (Add x y)    = containIntegral x || containIntegral y
containIntegral (Sub x y)    = containIntegral x || containIntegral y
containIntegral (Mul x y)    = containIntegral x || containIntegral y
containIntegral (Div x y)    = containIntegral x || containIntegral y
containIntegral (Norm x)     = containIntegral x
containIntegral exp          = isIntegral exp

isIntegral :: Exp -> Bool
isIntegral (Integral _) = True
isIntegral _            = False

containSigma :: Exp -> Bool
containSigma (Add x y) = containSigma x || containSigma y
containSigma (Sub x y) = containSigma x || containSigma y
containSigma (Mul x y) = containSigma x || containSigma y
containSigma (Div x y) = containSigma x || containSigma y
containSigma (Norm x)  = containSigma x
containSigma exp       = isSigma exp

isSigma :: Exp -> Bool
isSigma (Sigma _) = True
isSigma _         = False
