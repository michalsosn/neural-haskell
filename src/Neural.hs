{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, ScopedTypeVariables, ViewPatterns #-}

module Neural where

import Control.Monad.State hiding (msum)
import Data.Maybe
import Data.List
import Data.Proxy
import Debug.Trace
import GHC.TypeLits
import qualified Numeric.LinearAlgebra.Data as D
import Numeric.LinearAlgebra.Static
import System.Random

import Feedback
import LinearUtils

type Network m n1 n2 = Feedback (L m n1) (L m n2)

neuralLayer :: forall m n1 n2 . (KnownNat m, KnownNat n1, KnownNat n2) =>
               (Double -> Double) -> (Double -> Double) ->
               (Int -> Double) -> (Int -> Double) -> L n1 n2 ->
               Network m n1 n2
neuralLayer av dv rate mom ios = aux (konst 0.0) ios 1
    where
        aux :: (KnownNat m, KnownNat n1, KnownNat n2) =>
               L n1 n2 -> L n1 n2 -> Int -> Network m n1 n2
        aux pgrad os it = Feedback $ \xs ->
            let m = rowNum xs                                           :: Double
                as = xs <> os                                           :: L m n2
                ys = mmap av as                                         :: L m n2
                bwd bs =
                    let core = bs * mmap dv as                          :: L m n2
                        bs'  = core <> tr os                            :: L m n1
                        grad = -tr xs <> core * (konst $ rate it / m)   :: L n1 n2
                        os'  = os + grad + pgrad * (konst $ mom it)     :: L n1 n2
                        it'  = it + 1                                   :: Int
                        fb'  = aux grad os' it'                         :: Network m n1 n2
                    in  (fb', bs')
            in (bwd, ys)

dropBias :: (KnownNat m, KnownNat n1, KnownNat n2) => L m n1 -> L m n2
dropBias = fromJust . create . D.fromRows . fmap (D.fromList . tail . D.toList . unwrap) . toRows

bias :: forall m n1 n2 . (KnownNat m, KnownNat n1, KnownNat n2) => Network m n1 n2
bias = Feedback $ \xs ->
    let xs' = blockAt 1.0 0 1 (unwrap xs) :: L m n2
        bwd bs = let bs' = dropBias bs :: L m n1
                 in  (bias, bs')
    in  (bwd, xs')

dropChosen :: (KnownNat m, KnownNat n1, KnownNat n2) => [Bool] -> L m n1 -> L m n2
dropChosen bs = fromJust . create . D.fromRows . fmap (D.fromList . filterChosen bs . D.toList . unwrap) . toRows
    where
        filterChosen bs vs = (fmap snd . filter fst) $ zip bs vs

meanSquaredError :: (KnownNat m, KnownNat n) => L m n -> L m n -> Double
meanSquaredError ys xs = let m = rowNum xs in msum (((xs - ys) ** 2) / (2 * m))

precision :: (KnownNat m) => L m 1 -> L m 1 -> Double
precision ys xs = (sum $ zipWith hit (toList1 ys) (toList1 xs)) / rowNum ys
    where
        hit :: Double -> Double -> Double
        hit y x = if y == x then 1.0 else 0.0

trainEpoch :: (KnownNat m, KnownNat n1, KnownNat n2) => (L m n2) -> (L m n1) -> State (Network 1 n1 n2) (L m n2)
trainEpoch ys xs = fmap toSingleMs $ forM (toRowMs ys `zip` toRowMs xs) $ uncurry trainRow

trainRow :: (KnownNat n1, KnownNat n2) => (L 1 n2) -> (L 1 n1) -> State (Network 1 n1 n2) (L 1 n2)
trainRow y x = train x (\r -> r - y)

predictInRows :: (KnownNat m, KnownNat n1, KnownNat n2) => (L m n1) -> State (Network 1 n1 n2) (L m n2)
predictInRows xs = fmap toSingleMs $ forM (toRowMs xs) predict

label :: (Double -> Bool) -> Double -> Double
label f x = if f x then 1.0 else 0.0

classesToMatrix :: forall m n . (KnownNat m, KnownNat n) => (L m 1) -> (L m n)
classesToMatrix = fromRows . fmap (multiLabel (fromIntegral n)) . toList1
    where
        n = fromIntegral . natVal $ (Proxy :: Proxy n) :: Int
        multiLabel :: (KnownNat n) => Double -> Double -> R n
        multiLabel n a = fromJust . create . D.fromList $ fmap (label (==a)) [1.0..n]

matrixToClasses :: forall m n . (KnownNat m, KnownNat n) => (L m n) -> (L m 1)
matrixToClasses = fromJust . create . D.fromRows . fmap (D.fromList . (:[]) . maxIndex) . toList
    where
        maxIndex :: (Ord a) => [a] -> Double
        maxIndex (x:xs) = aux xs (2.0) x (1.0)
            where
                aux :: (Ord a) => [a] -> Double -> a -> Double -> Double
                aux [] _ _ mi = mi
                aux (x:xs) i mx mi = if x > mx
                                     then aux xs (i + 1.0) x i
                                     else aux xs (i + 1.0) mx mi
