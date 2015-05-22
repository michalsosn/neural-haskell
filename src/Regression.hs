{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}

module Regression where

import GHC.TypeLits
import Numeric.LinearAlgebra.Static

import LinearUtils

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp (-x))

sigmoidDeriv :: Double -> Double
sigmoidDeriv x = let ex = exp x
                 in ex / ((1.0 + ex) * (1.0 + ex))

sigmoidPair :: (Double -> Double, Double -> Double)
sigmoidPair = (sigmoid, sigmoidDeriv)

idPair :: (Double -> Double, Double -> Double)
idPair = (id, const 1.0)

ignoreBias :: (KnownNat n, 1 <= n) => Double -> R n -> R n
ignoreBias c = uncheckedMod $ \(_bias:rest) -> c:rest

scaleFeatures :: (KnownNat m, KnownNat n, 1 <= n) => L m n -> R n -> R n
scaleFeatures xs = let m = rowNum xs
                       means = ignoreBias 0.0 $ (tr xs) #> konst (1.0 / m)
                       stddevs = ignoreBias 1.0 $ vmap sqrt (((tr xs) ** 2) #> konst (1.0 / m) - (means ** 2))
                   in \row -> (row - means) / stddevs

meanSquaredCost :: (KnownNat m, KnownNat n) => (Double -> Double) -> L m n -> R m -> R n -> Double
meanSquaredCost av xs ys os = let m = rowNum xs
                                  as = xs #> os
                              in vsum $ (vmap av as - ys) ** 2 / (2 * m)

regularizeCost :: (KnownNat m, KnownNat n) => Double -> L m n -> R n -> Double
regularizeCost reg xs os = let m = rowNum xs
                           in os <·> os * reg / (2 * m)

gradient :: (KnownNat m, KnownNat n) => (Double -> Double) -> (Double -> Double) -> L m n -> R m -> R n -> R n
gradient av dv xs ys os = let m = rowNum xs
                              as = xs #> os
                          in tr xs #> ((vmap av as - ys) * (vmap dv as)) / m

regularizeGradient :: (KnownNat m, KnownNat n, 1 <= n) => Double -> L m n -> R n -> R n
regularizeGradient reg xs os = let m = rowNum xs
                               in (ignoreBias 0.0 os) * (konst reg) / m

gradientDescent :: (KnownNat n) => (R n -> R n) -> Double -> R n -> R n
gradientDescent grad rate os = os - grad os * (konst rate)
