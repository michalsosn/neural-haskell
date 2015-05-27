{-# LANGUAGE DataKinds #-}

module LinearUtils where

import Data.Maybe
import GHC.TypeLits
import qualified Numeric.LinearAlgebra.Data as D
import qualified Numeric.LinearAlgebra.HMatrix as H
import Numeric.LinearAlgebra.Static

vsum :: (KnownNat n) => R n -> Double
vsum = dot 1

vmap :: (KnownNat n) => (Double -> Double) -> R n -> R n
vmap f = fromJust . create . H.cmap f . unwrap

msum :: (KnownNat m, KnownNat n) => L m n -> Double
msum m = (m #> konst 1.0) `dot` 1

mmap :: (KnownNat m, KnownNat n) => (Double -> Double) -> L m n -> L m n
mmap f = fromJust . create . H.cmap f . unwrap

rmap :: (KnownNat m, KnownNat n) => (R n -> R n) -> L m n -> L m n
rmap f = fromRows . fmap f . toRows

rowNum :: (KnownNat m, KnownNat n, Fractional a) => L m n -> a
rowNum = fromIntegral . fst . size

fromRows :: (KnownNat m, KnownNat n) => [R n] -> L m n
fromRows = fromJust . create . D.fromRows . fmap unwrap

toRowMs :: (KnownNat m, KnownNat n) => (L m n) -> [L 1 n]
toRowMs = fmap row . toRows

toSingleMs :: (KnownNat m, KnownNat n) => [L 1 n] -> (L m n)
toSingleMs = fromRows . fmap unrow

mfirst :: (KnownNat m, KnownNat n) => L m n -> Double
mfirst = (H.! 0) . (H.! 0) . unwrap

fromList :: (KnownNat m, KnownNat n) => [[Double]] -> L m n
fromList = fromJust . create . D.fromLists

toList :: (KnownNat m, KnownNat n) => L m n -> [[Double]]
toList = fmap (D.toList . unwrap) . toRows

toList1 :: (KnownNat m, KnownNat n) => L m n -> [Double]
toList1 = concat . fmap (D.toList . unwrap) . toRows

toPoints :: (KnownNat m) => L m 2 -> [(Double, Double)]
toPoints = fmap (\[a, b] -> (a, b)) . toList

toPoints3D :: (KnownNat m) => L m 3 -> [(Double, Double, Double)]
toPoints3D = fmap (\[a, b, c] -> (a, b, c)) . toList

uncheckedMod :: (KnownNat n) => ([Double] -> [Double]) -> R n -> R n
uncheckedMod f = fromJust . create . D.fromList . f . D.toList . unwrap

mold :: (KnownNat m1, KnownNat n1, KnownNat m2, KnownNat n2) => L m1 n1 -> L m2 n2
mold = fromJust . create . unwrap

