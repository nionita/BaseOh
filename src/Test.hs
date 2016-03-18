module Main where

import Optimisation.Stochastic.BayesOpt
import qualified Data.Vector.Storable as V

mini :: Callback ()
mini _ v = do
    let (x1:x2:_) = V.toList v
    putStrLn $ "called with: x1 = " ++ show x1 ++ ", x2 = " ++ show x2
    return $! (x1-1)*(x1-1) + (x2-2)*(x2-2)

main :: IO ()
main = do
    let vl = V.fromList [-10, -10]	-- lower limits
        vu = V.fromList [ 10,  10]	-- upper limits
    (vr, r) <- bayesOptim mini () vl vu
    let (x1:x2:_) = V.toList vr
    putStrLn "Results:"
    putStrLn $ "f = " ++ show r
    putStrLn $ "at (" ++ show x1 ++ ", " ++ show x2 ++ ")"
