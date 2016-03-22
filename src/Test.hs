module Main where

import Optimisation.Stochastic.BayesOpt
import qualified Data.Vector.Storable as V
import System.Random.MWC (GenIO, create)
import System.Random.MWC.Distributions (standard)

-- Paraboloid with noise
mini :: Callback GenIO
mini g v = do
    let (x1:x2:_) = V.toList v
    n <- standard g
    return $! (x1-1)*(x1-1) + (x2-2)*(x2-2) + n

-- Banana with noise
bana :: Callback GenIO
bana g v = do
    let (x:y:_) = V.toList v
        x1 = 1 - x
        x2 = x * x
        y1 = y - x2
        ba = x1 * x1 + 10 * y1 * y1
    n <- standard g
    putStrLn $ "Noise = " ++ show n
    return $! ba + n

main :: IO ()
main = do
    let vl = V.fromList [-10, -10]  -- lower limits
        vu = V.fromList [ 10,  10]  -- upper limits
    g <- create
    (r, vr) <- bayesOptim bana g vl vu
    let (x1:x2:_) = V.toList vr
    putStrLn "Results:"
    putStrLn $ "f = " ++ show r
    putStrLn $ "at (" ++ show x1 ++ ", " ++ show x2 ++ ")"
