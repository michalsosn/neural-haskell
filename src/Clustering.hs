{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

module Clustering where

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

type Network' m1 m2 n = Circuit (L m1 n) (L m2 n)
type Learning m2 n = Int -> R n -> R m2 -> L m2 n -> L m2 n

selfOrganizing :: forall m2 n . (KnownNat m2, KnownNat n) =>
                  Learning m2 n -> L m2 n -> Network' 1 m2 n
selfOrganizing learn ios = aux ios 1
    where
        aux :: L m2 n -> Int -> Network' 1 m2 n
        aux os it = Circuit $ \xs ->
            let xos = xs <> tr os                               :: L 1 m2
                os2 = tr ((os * os) <> (konst 1.0 :: L n 1))    :: L 1 m2
                ds  = os2 - 2 * xos                             :: L 1 m2
                os' = learn it (unrow xs) (unrow ds) os         :: L m2 n
                it' = it + 1                                    :: Int
            in  (aux os' it', os')


kohonen :: forall m2 n . (KnownNat m2, KnownNat n) =>
                  (Int -> Double) -> (Int -> Double) -> L m2 n ->
                  Network' 1 m2 n
kohonen rateWin rateNb = selfOrganizing learnKohonen
    where
        learnKohonen :: Learning m2 n
        learnKohonen it xs ds os =
            let wonIx = findWinner ds                                                               :: Int
                wonOs = row . fromJust . create . (D.! wonIx) . unwrap $ os                         :: L 1 n
                wonDiffs = ((konst 1.0 :: L m2 1) <> wonOs) - os                                    :: L m2 n
                wonDs = mmap (gauss $ rateNb it) $ (wonDiffs * wonDiffs) <> (konst 1.0 :: L n 1)    :: L m2 1
                xsDiffs = ((konst 1.0 :: L m2 1) <> row xs) - os                                    :: L m2 n
                xsDs = (konst (rateWin it) :: L m2 n) * (wonDs <> (konst 1.0 :: L 1 n)) * xsDiffs   :: L m2 n
            in  os + xsDs

        findWinner :: R m2 -> Int
        findWinner = D.minIndex . unwrap

        gauss :: Double -> Double -> Double
        gauss b x2 = exp (- x2 / (2 * b))
--        gauss b x2 = exp (- x2 / (2 * b * b))


neuralGas :: forall m2 n . (KnownNat m2, KnownNat n) =>
                  (Int -> Double) -> (Int -> Double) -> L m2 n ->
                  Network' 1 m2 n
neuralGas rateWin rateNb = selfOrganizing learnNeuralGas
    where
        learnNeuralGas :: Learning m2 n
        learnNeuralGas it xs ds os =
            let ixs = sortWinners ds                                                                :: R m2
                nbCoefs = mmap (nbRange $ rateNb it) . col $ ixs                                    :: L m2 1
                xsDiffs = ((konst 1.0 :: L m2 1) <> row xs) - os                                    :: L m2 n
                xsDs = (konst (rateWin it) :: L m2 n) * (nbCoefs <> (konst 1.0 :: L 1 n)) * xsDiffs :: L m2 n
            in  os + xsDs

        sortWinners :: R m2 -> R m2
        sortWinners ds =
            let sortedDs = L.sort $ (D.toList . unwrap) ds `zip` [0..]
                byPos = L.sort $ fmap snd sortedDs `zip` [0.0..]
            in  vector . fmap snd $ byPos

        nbRange :: Double -> Double -> Double
        nbRange rate i = exp (- i / rate)


kMeans :: forall m1 m2 n . (KnownNat m1, KnownNat m2, KnownNat n) =>
                  L m2 n -> Network' m1 m2 n
kMeans os = Circuit $ \xs ->
    let xos = xs <> tr os                                                               :: L m1 m2
        os2 = (konst 1.0 :: L m1 1) <> tr ((os * os) <> (konst 1.0 :: L n 1))           :: L m1 m2
--        xs2 = ((xs * xs) <> (konst 1.0 :: L n 1)) <> (konst 1.0 :: L 1 m2)              :: L m1 m2
        ds  = os2 - 2 * xos                                                             :: L m1 m2
        minIxs = (fmap (D.minIndex . unwrap) . toRows) ds                               :: [Int]

        assocXs = minIxs `zip` fmap (:[]) (toRows xs)                                   :: [(Int, [R n])]
        assocMap = foldr (uncurry $ M.insertWith (++)) M.empty assocXs                  :: M.Map Int [R n]
        avgMap = M.map (\xs -> (foldr1 (+) xs) / (fromIntegral . length) xs) assocMap   :: M.Map Int (R n)

        os' = (fromRows . replaceWithAvgs 0 . toRows) os                                :: L m2 n

        replaceWithAvgs :: Int -> [R n] -> [R n]
        replaceWithAvgs n (o:os) =
            case M.lookup n avgMap of
                (Just avg) -> avg : replaceWithAvgs (n + 1) os
                Nothing    ->   o : replaceWithAvgs (n + 1) os
        replaceWithAvgs _ [] = []

    in  (kMeans os', os')

rateFunc :: Double -> Double -> Double -> Int -> Double
rateFunc rateStart rateEnd iters it = rateStart * (rateEnd / rateStart) ** (fromIntegral it / iters)

scaleFeatures :: forall m n . (KnownNat m, KnownNat n) =>
                 L m n -> (R n -> R n, R n -> R n)
scaleFeatures xs = let m = rowNum xs                                                            :: Double
                       means = (tr xs) #> konst (1.0 / m)                                       :: R n
                       stddevs = vmap sqrt (((tr xs) ** 2) #> konst (1.0 / m) - (means ** 2))   :: R n
                       scale row = (row - means) / stddevs                                      :: R n
                       descale row = row * stddevs + means                                      :: R n
                   in (scale, descale)


quantizationError :: forall m1 m2 n . (KnownNat m1, KnownNat m2, KnownNat n) =>
                   L m1 n -> L m2 n -> Double
quantizationError xs os =
            let m = rowNum xs                                                           :: Double
                xos = xs <> tr os                                                       :: L m1 m2
                os2 = (konst 1.0 :: L m1 1) <> tr ((os * os) <> (konst 1.0 :: L n 1))   :: L m1 m2
                xs2 = ((xs * xs) <> (konst 1.0 :: L n 1)) <> (konst 1.0 :: L 1 m2)      :: L m1 m2
                ds  = os2 - 2 * xos + xs2                                               :: L m1 m2
            in  ((/m) . sum . fmap (D.minElement . unwrap) . toRows) ds


compressNearest :: forall m1 m2 n . (KnownNat m1, KnownNat m2, KnownNat n) =>
                   L m1 n -> L m2 n -> L m1 n
compressNearest xs os =
    let xos = xs <> tr os                                                               :: L m1 m2
        os2 = (konst 1.0 :: L m1 1) <> tr ((os * os) <> (konst 1.0 :: L n 1))           :: L m1 m2
        ds  = os2 - 2 * xos                                                             :: L m1 m2
        minIxs = (fmap (D.minIndex . unwrap) . toRows) ds                               :: [Int]

        uos = unwrap os
        nearest = fmap (fromJust . create . (uos H.!)) minIxs
    in fromRows nearest


peakSignalToNoiseRatio :: (KnownNat m, KnownNat n) => Double -> L m n -> L m n -> Double
peakSignalToNoiseRatio peak ys xs = let m = rowNum xs
                                        mse = msum (((xs - ys) ** 2) / m)
                                    in  10 * logBase 10 (peak * peak / mse)
