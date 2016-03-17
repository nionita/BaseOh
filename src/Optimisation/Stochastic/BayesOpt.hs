{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Optimisation.Stochastic.BayesOpt (
    Callback,
    bayesOptim
) where

import           Foreign.C.Types
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Ptr
import           Data.Coerce (coerce)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Data.Monoid
import qualified Language.C.Inline as C

-- foreign import ccall "bayes_optimization" c_bayes_optimization :: CInt -> 

-- Haskell callback
type Callback a = a -> V.Vector Double -> IO Double

-- C callback:
type CCB a = CUInt -> Ptr CDouble -> Ptr CDouble -> Ptr a -> IO CDouble

-- | Possible parameters for optimisation algorithm
data Params = Kernel | Mean | Criteria | Surrogate | LogFile | LoadFile | SaveFile
            | Learning | Score

-- inline-c contexts needed for function pointers & vectors
C.context (C.baseCtx <> C.vecCtx <> C.funCtx)

C.include "bayesopt/bayesopt.h"

bayesOptim
    :: Callback a
    -> a
    -> V.Vector Double
    -> V.Vector Double
    -> IO (V.Vector Double, Double)
bayesOptim f a lv uv = do
    let dim   = V.length lv
        dim_c = fromIntegral dim
        clv   = coerce lv :: V.Vector CDouble
        cuv   = coerce uv :: V.Vector CDouble
    let funIO :: CCB a
        funIO dim_ xv gv_ a_ = do
            -- We use only the x vector, all other params we either don't need or have
            -- them already in Haskell
            ptr' <- newForeignPtr_ xv
            vec  <- V.freeze $ VM.unsafeFromForeignPtr0 ptr' dim
            val  <- coerce $ f a $ coerce vec
            [C.exp| double { $(double val) } |]
    vrx <- VM.new (dim+1) :: IO (VM.IOVector CDouble)
    ret <- [C.block| int {
            bopt_params bp;	// structure definition ??
            bp = initialize_parameters_to_default();
            set_learning(&bp, "L_MCMC");
            double *r = $vec-ptr:(double *vrx);
            bayes_optimization(
                $(int dim_c),
                $fun:(double (* funIO) (unsigned int n, const double *x, double *gradien, void *fd)),
                (void *) 0,	// we don't really need this argument, as we pass our a
                $vec-ptr:(double *clv),
                $vec-ptr:(double *cuv),
                r+1, r, bp);
        } |]
    rx <- V.freeze vrx
    return (coerce $ V.tail rx, coerce $ V.head rx)
