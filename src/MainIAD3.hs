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
import qualified Regression as R
import Clustering

type RadialPts = 30

makeRadialApprox :: forall g ml mt n1 . (RandomGen g, KnownNat ml, KnownNat n1) =>
                   Double -> Double -> Double -> L ml n1 -> g ->
                   State [Double] (Circuit (L 1 n1, L 1 1) (L 1 1, L RadialPts n1))
makeRadialApprox scales rate mom xsl gen = do
    let ios1 = takeSample xsl gen
    ios2 <- takeMatrix

    let l1 = radialBasis ios1 (konst scales)                             :: L 1 n1 -> L 1 RadialPts
        l2 = uncurry neuralLayer R.idPair (const rate) (const mom) ios2  :: Network   1 RadialPts  1

    return $ proc (xs1, ts) -> do
        xs2 <-   arr l1 -< xs1
        ys  <- close l2 -< (xs2, (\ys -> ys - ts))
        returnA -< (ys, ios1)


solveRadialApprox :: Double -> Double -> Double -> Int -> Double -> IO ()
solveRadialApprox scales rate mom epochs initRange = do
            gen <- getStdGen
            pst <- readMatrix "test/approximation_test.txt"    :: IO (L 1000 2)
            psl <- readMatrix "test/approximation_train_1.txt" :: IO (L 81 2)

            let (xst, yst) = separateYs pst :: (L 1000 1, L 1000 1)
                (xsl, ysl) = separateYs psl :: (L 81 1, L 81 1)

            let network = evalState (makeRadialApprox scales rate mom xsl gen) (randomInit initRange gen)
                ((errorsl, errorst, centers), trained) = runState ((fmap unzip3 . replicateM epochs) $ trainEpoch xst xsl ysl) network

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
parseRadialApprox = solveRadialApprox
    <$> option auto (short 's' <> long "scale" <> value 1.0 <> showDefault
                    <> help "Scaling coefficients of the gaussian function in the first layer.")
    <*> option auto (short 'r' <> long "rate" <> value 0.1 <> showDefault
                    <> help "Learning rate of the neurons in the second layer.")
    <*> option auto (short 'm' <> long "momentum" <> value 0.5 <> showDefault
                    <> help "Momentum of the neurons in the second layer.")
    <*> option auto (short 'e' <> long "epochs" <> value 2000 <> showDefault
                    <> help "Number of epochs of training the network will go through.")
    <*> option auto (short 'I' <> long "initRange" <> value 0.5 <> showDefault
                    <> help "The neurons will be initialized with values from (-I, I).")

--makeRadialClass1 :: forall g ml mt n1 . (RandomGen g, KnownNat ml, KnownNat n1) =>
--                   Double -> Double -> Double -> L ml n1 -> g ->
--                   State [Double] (Circuit (L 1 n1, L 1 1) (L 1 1, L RadialPts n1))
--makeRadialClass1 scales rate mom xsl gen = do
--    let ios1 = takeSample xsl gen
--    ios2 <- takeMatrix
--
--    let l1 = radialBasis ios1 (konst scales)                           :: Network'' 1 n1 RadialPts
--        l2 = uncurry neuralLayer idPair (const rate) (const mom) ios2  :: Network   1 RadialPts  1
--
--    return $ proc (xs1, ts) -> do
--        xs2 <-       l1 -< xs1
--        ys  <- close l2 -< (xs2, (\ys -> ys - ts))
--        returnA -< (ys, ios1)

makeRadialClass1Kohonen :: forall g m n r . (RandomGen g, KnownNat m, KnownNat n, KnownNat r) =>
                   L m n -> g -> Circuit (L m n) (L r n, [Double])
makeRadialClass1Kohonen xss gen = proc xs -> do
    sXs <- arr (rmap scale)   -< xs
    sOs <- network            -< sXs
    os  <- arr (rmap descale) -<  sOs
    error <- logger (quantizationError xss) -< os
    returnA -< (os, error)
    where
        (scale, descale) = scaleFeatures xss
        ios = takeSample (rmap scale xss) gen
        network = kMeans ios

makeRadialClass1Gas :: forall m n r . (KnownNat m, KnownNat n, KnownNat r) =>
               Double -> Double -> Double -> Double -> Double -> L m n ->
               State [Double] (Circuit (L 1 n) (L r n, [Double]))
makeRadialClass1Gas rateWinS rateWinE rateNbS rateNbE iters xss = do
    os <- takeMatrix
    let (scale, descale) = scaleFeatures xss
        network = neuralGas (rateFunc rateWinS rateWinE iters) (rateFunc rateNbS rateNbE iters) os :: Network' 1 r n
    return $ proc xs -> do
        sXs <- arr (rmap scale)   -< xs
        sOs <- network            -< sXs
        os  <- arr (rmap descale) -< sOs
        error <- logger (quantizationError xss) -< os
        returnA -< (os, error)

makeRadialClass2Neural :: forall m n . (KnownNat m, KnownNat n) =>
                    L 90 n -> Double -> Double -> Double -> State [Double] (Network m n 3)
makeRadialClass2Neural ios1 scales rate mom = do
    os2 <- takeMatrix
    let l1 = radialBasis ios1 (konst scales)                            :: L m n -> L m 90
        l2  = uncurry neuralLayer R.idPair (const rate) (const mom) os2 :: Network m 90 3
    return $ liftFb l1 (const $ konst 0.0) >>> l2

makeRadialClass2Inv :: forall m n . (KnownNat m, KnownNat n) =>
                    L 90 n -> Double -> L m n -> L m 3 -> Network 1 n 3
makeRadialClass2Inv ios1 scales xss ys =
    let l1 = radialBasis ios1 (konst scales)                            :: L 1 n -> L 1 90
        gs = (radialBasis ios1 (konst scales)) xss                      :: L m 90
        os2 = gs <\> ys                                                 :: L 90 3
        l2  = uncurry neuralLayer R.idPair (const 0.0) (const 0.0) os2  :: Network 1 90 3
    in  liftFb l1 (const $ konst 0.0) >>> l2

solveRadialClass :: [Bool] -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Int -> Int -> Double -> IO ()
solveRadialClass chosen =
    case length $ filter id chosen of
        0 -> aux chosen (Proxy :: Proxy 0)
        1 -> aux chosen (Proxy :: Proxy 1)
        2 -> aux chosen (Proxy :: Proxy 2)
        3 -> aux chosen (Proxy :: Proxy 3)
        4 -> aux chosen (Proxy :: Proxy 4)
    where
        aux :: forall n . (KnownNat n) =>
               [Bool] -> Proxy n -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Int -> Int -> Double -> IO ()
        aux chosen _proxy scale rate mom rateWinS rateWinE rateNbS rateNbE epochs1 epochs2 initRange = do
            (gen1:gen2:_) <- fmap splits getStdGen
            ps1 <- readMatrix "test/classification_train.txt" :: IO (L 90 5)
            pst <- readMatrix "test/classification_test.txt"  :: IO (L 93 5)

            let (xsl', csl) = separateYs ps1 :: (L 90 4, L 90 1)
                xsl = dropChosen chosen xsl' :: L 90 n
                (xst', cst) = separateYs pst :: (L 93 4, L 93 1)
                xst = dropChosen chosen xst' :: L 93 n
                ysl = classesToMatrix csl :: L 90 3
                yst = classesToMatrix cst :: L 93 3

--            let iters = fromIntegral epochs1 * rowNum xsl
--                network1 = evalState (makeRadialClass1Gas rateWinS rateWinE rateNbS rateNbE iters xsl) (randomInit initRange gen1)
--                (centers, errors) = evalState (fmap (last . concat) $ replicateM epochs1 $ forM (toRowMs xsl) run) network1
--
            let network1 = makeRadialClass1Kohonen xsl gen1
                (centers, errors) = evalState (fmap last $ replicateM epochs1 $ run xsl) network1

--            let network2 = evalState (makeRadialClass2Neural centers scale rate mom) (randomInit initRange gen2)
--                ((resLs, resTs), trained) = runState ((fmap unzip . replicateM epochs2) (trainEpoch ys1 xsl >> zipM (predictInRows xsl) (predictInRows xst))) network2

            let network2 = makeRadialClass2Inv centers scale xsl ysl
                ((resLs, resTs), trained) = runState (zipM (predictInRows xsl) (predictInRows xst)) network2

--                resLsCls = fmap matrixToClasses resLs
--                resTsCls = fmap matrixToClasses resTs
                resLsCls = matrixToClasses resLs
                resTsCls = matrixToClasses resTs

--            print $ last resTs
--            print $ last resTsCls
--            print $ cst
            let toPoints :: (KnownNat kot, KnownNat pies) => L kot pies -> [(Double, Double)]
                toPoints = fmap (\(i, row) -> (takePoint . (++[i]) . D.toList . unwrap) row) . zip [1..] . toRows

                takePoint :: [Double] -> (Double, Double)
                takePoint (a:b:_) = (a, b)
                takePoint [a] = (a, 0.0)
                ixses = toPoints xst

                filterBro :: (b -> Bool) -> [a] -> [b] -> [a]
                filterBro f (a:as) (b:bs) = if f b then a : filterBro f as bs else filterBro f as bs
                filterBro _ [] [] = []

--                [ones, twos, thrs] = (\n -> filterBro (==n) ixses (toList1 $ last resTsCls)) <$> [1.0, 2.0, 3.0]
                [ones, twos, thrs] = (\n -> filterBro (==n) ixses (toList1 resTsCls)) <$> [1.0, 2.0, 3.0]
                [wones, wtwos, wthrs] = (\n -> filterBro (==n) ixses (toList1 cst)) <$> [1.0, 2.0, 3.0]

                showBools = concat . fmap (\b -> if b then "1" else "0")

            void $ plot X11 $
                Data2D [Title "Quantization error", Style Lines] [] $ zip [1..] errors

            void $ plot X11
                [ Data2D     [Title "Ones",    Style Points, Color Red  ]  [] ones
                , Data2D     [Title "Twos",    Style Points, Color Green]  [] twos
                , Data2D     [Title "Threes",  Style Points, Color Blue ]  [] thrs
                , Data2D     [Title "Centers", Style Points, Color Black]  [] (toPoints centers)
                ]

            void $ plot X11
                [ Data2D     [Title "Correct Ones",  Style Points, Color Red  ]  [] wones
                , Data2D     [Title "Correct Twos",  Style Points, Color Green]  [] wtwos
                , Data2D     [Title "Correct Threes", Style Points, Color Blue ]  [] wthrs
                , Data2D     [Title "Centers",        Style Points, Color Black]  [] (toPoints centers)
                ]

            print $ meanSquaredError ysl resLs
            print $ meanSquaredError yst resTs
--            void $ plot X11
--                [ Data2D [Title "Mean squared error - training set", Style Lines] [] $ zip [1..]  (fmap (meanSquaredError ysl) resLs)
--                , Data2D [Title "Mean squared error - test set",     Style Lines] [] $ zip [1..]  (fmap (meanSquaredError yst) resTs)
--                ]

            print $ precision csl resLsCls
            print $ precision cst resTsCls
--            void $ plot X11
--                [ Data2D [Title "Precision - training set", Style Lines] [] $ zip [1..]  (fmap (precision csl) resLsCls)
--                , Data2D [Title "Precision - test set",     Style Lines] [] $ zip [1..]  (fmap (precision cst) resTsCls)
--                ]


parseRadialClass :: Parser (IO ())
parseRadialClass = solveRadialClass
    <$> (fmap parseFeatures . strOption)
                    (short 'f' <> long "features" <> value "1111" <> showDefault
                    <> help "A string of four 0s and 1s denoting which features to use.")
    <*> option auto (short 's' <> long "scale" <> value 1.0 <> showDefault
                    <> help "Scaling coefficients of the gaussian function in the first layer.")
    <*> option auto (short 'r' <> long "rate" <> value 0.3 <> showDefault
                    <> help "Learning rate of the neurons in the neural layer.")
    <*> option auto (short 'm' <> long "momentum" <> value 0.5 <> showDefault
                    <> help "Momentum of the neurons in the neural layer.")
    <*> option auto (short 'w' <> long "rateWinS" <> value 1.0 <> showDefault
                    <> help "Starting learning rate of the winner.")
    <*> option auto (short 'W' <> long "rateWinE" <> value 0.001 <> showDefault
                    <> help "Ending learning rate of the winner.")
    <*> option auto (short 'n' <> long "rateNbS" <> value 200.0 <> showDefault
                    <> help "Starting learning rate of the neighbourhood.")
    <*> option auto (short 'N' <> long "rateNbE" <> value 0.01 <> showDefault
                    <> help "Ending learning rate of the neighbourhood.")
    <*> option auto (short 'e' <> long "epochs1" <> value 40 <> showDefault
                    <> help "Number of epochs of training the network will go through during the clustering phase.")
    <*> option auto (short 'E' <> long "epochs2" <> value 500 <> showDefault
                    <> help "Number of epochs of training the network will go through during the classification phase.")
    <*> option auto (short 'I' <> long "initRange" <> value 1.0 <> showDefault
                    <> help "The neurons will be initialized with values from (-I, I).")
    where
        digitToBool :: Char -> Bool
        digitToBool '0' = False
        digitToBool '1' = True
        digitToBool s = error $ "Expected 0 or 1, got: " ++ [s]

        parseFeatures :: String -> [Bool]
        parseFeatures s = if length s < 5
                          then fmap digitToBool s
                          else error $ "Expected a string with length < 5, got: " ++ s


makeRadialDescentApprox :: forall g m ml . (RandomGen g, KnownNat ml, KnownNat m) =>
                     Double -> Double -> Double -> L ml 1 -> g -> State [Double] (Network m 1 1)
makeRadialDescentApprox rate1 rate2 mom2 xsl gen = do
    let os1 = takeSample xsl gen
        rates1 = konst 0.5
    os2 <- takeMatrix
    let l1  = uncurry radialDescent gaussPair rate1 os1 rates1               :: Network m 1 10
        l2  = uncurry neuralLayer   R.idPair  (const rate2) (const mom2) os2 :: Network m 10 1
    return $ l1 >>> l2

solveRadialDescentApprox :: Bool -> Double -> Double -> Double -> Int -> Double -> IO ()
solveRadialDescentApprox alt = if not alt
                         then aux "test/approximation_train_1.txt" (Proxy :: Proxy 81)
                         else aux "test/approximation_train_2.txt" (Proxy :: Proxy 15)
    where
        aux :: forall m . (KnownNat m) => String -> Proxy m -> Double -> Double -> Double -> Int -> Double -> IO ()
        aux path _proxy rate1 rate2 mom2 epochs initRange = do
            gen <- getStdGen
            pst <- readMatrix "test/approximation_test.txt" :: IO (L 1000 2)
            psl <- readMatrix path :: IO (L m 2)

            let (xst, yst) = separateYs pst :: (L 1000 1, L 1000 1)
                (xsl, ysl) = separateYs psl :: (L m 1, L m 1)

            let network = evalState (makeRadialDescentApprox rate1 rate2 mom2 xsl gen) (randomInit initRange gen)
                ((resls, rests), trained) = runState ((fmap unzip . replicateM epochs) (trainEpoch ysl xsl >> zipM (predictInRows xsl) (predictInRows xst))) network

            void $ plot X11
                [ Data2D [Title "Mean squared error - training set", Style Lines] [] $ zip [1..] (fmap (meanSquaredError ysl) resls)
                , Data2D [Title "Mean squared error - test set",     Style Lines] [] $ zip [1..] (fmap (meanSquaredError yst) rests)
                ]
            void $ plot X11
                [ Data2D     [Title "Training set", Style Points, Color Red  ] [] $ zip (toList1 xsl) (toList1 ysl)
                , Data2D     [Title "Test set",     Style Points, Color Black] [] $ zip (toList1 xst) (toList1 yst)
                , Function2D [Title "Prediction",   Style Lines , Color Green] [] $ predictFunc trained
                ]
            where
                predictFunc :: Network 1 1 1 -> Double -> Double
                predictFunc nn x = (mfirst . snd . propagate nn) (konst x :: L 1 1)

parseRadialDescentApprox :: Parser (IO ())
parseRadialDescentApprox = solveRadialDescentApprox
    <$> switch      (short 'a' <> long "alt"
                    <> help "Use an alternative data set in text/approximation_train_2.txt.")
    <*> option auto (short 'r' <> long "rate1" <> value 0.00 <> showDefault
                    <> help "Learning rate of the radial layer.")
    <*> option auto (short 'R' <> long "rate2" <> value 0.01 <> showDefault
                    <> help "Learning rate of the neurons in the output layer.")
    <*> option auto (short 'M' <> long "momentum2" <> value 0.0 <> showDefault
                    <> help "Momentum of the neurons in the output layer.")
    <*> option auto (short 'e' <> long "epochs" <> value 100 <> showDefault
                    <> help "Number of epochs of training the network will go through.")
    <*> option auto (short 'I' <> long "initRange" <> value 1.0 <> showDefault
                    <> help "The neurons will be initialized with values from (-I, I).")
