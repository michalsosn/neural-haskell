{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, ScopedTypeVariables #-}

module Radial where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import GHC.TypeLits
import qualified Numeric.LinearAlgebra.Data as D
import qualified Numeric.LinearAlgebra.HMatrix as H
import Numeric.LinearAlgebra.Static

import Feedback
import LinearUtils


radialBasis :: forall m n1 n2 . (KnownNat m, KnownNat n1, KnownNat n2) =>
               L n2 n1 -> L 1 n2 ->
               Circuit (L m n1) (L m n2)
radialBasis os rates = Circuit $ \xs ->
            let xos = xs <> tr os                                                       :: L m n2
                os2 = (konst 1.0 :: L m 1) <> tr ((os * os) <> (konst 1.0 :: L n1 1))   :: L m n2
                xs2 = ((xs * xs) <> (konst 1.0 :: L n1 1)) <> (konst 1.0 :: L 1 n2)     :: L m n2
                ds  = os2 - 2 * xos + xs2                                               :: L m n2
                ys  = ds * ((konst 1.0 :: L m 1) <> rates)                              :: L m n2
            in  (radialBasis os rates, mmap gauss ys)
            where
                gauss :: Double -> Double
                gauss x2 = exp (- x2)
