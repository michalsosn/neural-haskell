{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators, Arrows #-}

module MainIAD3 where

import Control.Arrow
import Control.Category hiding (id, (.))
import Control.Monad hiding (msum)
import Control.Monad.State hiding (msum)
import Data.Proxy
import Data.List
import Data.Ord
import GHC.TypeLits
import Graphics.EasyPlot
import Numeric.LinearAlgebra.Static hiding ((<>))
import qualified Numeric.LinearAlgebra.Data as D
import Options.Applicative
import System.Random as R
import Text.Printf

import Animation
import Feedback
import IOUtils
import LinearUtils
import Neural
import Radial
import RandomInit
import Regression

type Network'' m n1 n2 = Circuit (L m n1) (L m n2)

type RadialPts = 30

makeRadialApprox :: forall g ml mt n1 . (RandomGen g, KnownNat ml, KnownNat n1) =>
                   Double -> Double -> Double -> L ml n1 -> g ->
                   State [Double] (Circuit (L 1 n1, L 1 1) (L 1 1, L RadialPts n1))
makeRadialApprox scales rate mom xsl gen = do
    let ios1 = takeSample xsl gen
    ios2 <- takeMatrix

    let l1 = radialBasis ios1 (konst scales)                           :: Network'' 1 n1 RadialPts
        l2 = uncurry neuralLayer idPair (const rate) (const mom) ios2  :: Network   1 RadialPts  1

    return $ proc (xs1, ts) -> do
        xs2 <-       l1 -< xs1
        ys  <- close l2 -< (xs2, (\ys -> ys - ts))
        returnA -< (ys, ios1)


solveRadialApprox :: IO ()
solveRadialApprox = do
            gen <- getStdGen
            pst <- readMatrix "test/approximation_test.txt"    :: IO (L 1000 2)
            psl <- readMatrix "test/approximation_train_1.txt" :: IO (L 81 2)

            let (xst, yst) = separateYs pst :: (L 1000 1, L 1000 1)
                (xsl, ysl) = separateYs psl :: (L 81 1, L 81 1)

            let network = evalState (makeRadialApprox 1.0 0.1 0.5 xsl gen) (randomInit 0.5 gen)
                ((errorsl, errorst, centers), trained) = runState ((fmap unzip3 . replicateM 2000) $ trainEpoch xst xsl ysl) network

            void $ plot X11
                [ Data2D [Title "Mean squared error - training set", Style Lines] [] $ zip [1..] (fmap (meanSquaredError ysl) errorsl)
                , Data2D [Title "Mean squared error - test set",     Style Lines] [] $ zip [1..] (fmap (meanSquaredError yst) errorst)
                ]
            void $ plot X11
                [ Data2D     [Title "Training set",     Style Points, Color Red  ] [] $ zip (toList1 xsl) (toList1 ysl)
                , Data2D     [Title "Test set",         Style Points, Color Black] [] $ zip (toList1 xst) (toList1 yst)
                , Data2D     [Title "Radial centres",   Style Points, Color Yellow] [] $ zip (toList1 . last $ centers) (repeat 0.0)
                , Function2D [Title "Prediction",       Style Lines , Color Green] [] $ predictFunc trained
                ]
            where
                trainEpoch :: (KnownNat mt, KnownNat ml, KnownNat n1, KnownNat n2, KnownNat rads) =>
                              (L mt n1) -> (L ml n1) -> (L ml n2) ->
                              State (Circuit (L 1 n1, L 1 n2) (L 1 n2, L rads n1)) (L ml n2, L mt n2, L rads n1)
                trainEpoch xst xsl ysl = do
                    os <- (fmap . fmap) snd $ forM (toRowMs xsl `zip` toRowMs ysl) run
                    zipM3 (dryRunInRows xsl) (dryRunInRows xst) (return . last $ os)

                dryRunInRows :: (KnownNat m, KnownNat n1, KnownNat n2, KnownNat rads) =>
                                (L m n1) -> State (Circuit (L 1 n1, L 1 n2) (L 1 n2, L rads n1)) (L m n2)
                dryRunInRows xs = fmap toSingleMs $ forM (toRowMs xs `zip` (repeat $ konst 0.0)) $ fmap fst . dryRun

                predictFunc :: (KnownNat rads) => Circuit (L 1 1, L 1 1) (L 1 1, L rads 1) -> Double -> Double
                predictFunc nwk x = (mfirst . fst . snd . runCircuit nwk) (konst x, konst 0.0)

parseRadialApprox :: Parser (IO ())
parseRadialApprox = pure solveRadialApprox
