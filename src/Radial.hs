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
import Neural


radialBasis :: forall m n1 n2 . (KnownNat m, KnownNat n1, KnownNat n2) =>
               L n2 n1 -> L 1 n2 -> L m n1 -> L m n2
radialBasis os rates xs =
            let xos = xs <> tr os                                                       :: L m n2
                os2 = (konst 1.0 :: L m 1) <> tr ((os * os) <> (konst 1.0 :: L n1 1))   :: L m n2
                xs2 = ((xs * xs) <> (konst 1.0 :: L n1 1)) <> (konst 1.0 :: L 1 n2)     :: L m n2
                ds  = os2 - 2 * xos + xs2                                               :: L m n2
                ys  = ds * ((konst 1.0 :: L m 1) <> rates)                              :: L m n2
            in  mmap gauss ys

gauss :: Double -> Double
gauss x2 = exp (- x2)

gaussDeriv :: Double -> Double
gaussDeriv x2 = - exp (- x2)

gaussPair :: (Double -> Double, Double -> Double)
gaussPair = (gauss, gaussDeriv)

radialDescent :: forall m n1 n2 . (KnownNat m, KnownNat n1, KnownNat n2) =>
               (Double -> Double) -> (Double -> Double) -> Double -> L n2 n1 -> L 1 n2 ->
               Network m n1 n2
radialDescent av dv rate os rates = Feedback $ \xs ->
            let m = rowNum xs                                                           :: Double
                xos = xs <> tr os                                                       :: L m n2
                os2 = (konst 1.0 :: L m 1) <> tr ((os * os) <> (konst 1.0 :: L n1 1))   :: L m n2
                xs2 = ((xs * xs) <> (konst 1.0 :: L n1 1)) <> (konst 1.0 :: L 1 n2)     :: L m n2
                ds  = os2 - 2 * xos + xs2                                               :: L m n2
                as  = ds * ((konst 1.0 :: L m 1) <> rates)                              :: L m n2
                ys  = mmap av as                                                        :: L m n2
                bwd bs =
                    let osxs  = (konst (1.0/m) :: L m 1) <> (rates <> os) - xs          :: L m n1
                        core  = bs * mmap dv as                                         :: L m n2
                        bs'   = core <> os                                              :: L m n1
                        osgrad = tr core <> osxs * 2                                    :: L n2 n1
                        rtgrad = (konst (1.0/m) :: L 1 m) <> (core * ds)                :: L  1 n2
                        os'    = os - (konst rate) * osgrad                             :: L n2 n1
                        rates' = rates - (konst rate) * rtgrad                          :: L  1 n2
--                        fb'  = trace ("\nxs: " ++ show xs ++ "\nds: " ++ show ds ++ "\nas: " ++ show as ++ "\ncore: " ++ show core ++ "\nbs: " ++ show bs ++ "\nos: " ++ show os ++ "\nosgrad: " ++ show osgrad ++ "\nrates:" ++ show rates ++ "\nrtgrad:" ++ show rtgrad) $ radialDescent av dv rate os' rates'
                        fb'  = radialDescent av dv rate os' rates'                      :: Network m n1 n2
                    in  (fb', bs')
            in (bwd, ys)
