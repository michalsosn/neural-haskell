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

type RadialPts = 15
type RadialPtsB = 16

makeRadialApprox :: forall g ml mt n1 . (RandomGen g, KnownNat ml, KnownNat n1) =>
                   Double -> Double -> Double -> L ml n1 -> g ->
                   State [Double] (Circuit (L 1 n1, L 1 1) (L 1 1, L RadialPts n1))
makeRadialApprox scales rate mom xsl gen = do
    let ios1 = takeSample xsl gen
    ios2 <- takeMatrix

    let l1 = radialBasis ios1 (konst scales)                             :: L 1 n1 -> L 1 RadialPts
        l2b = bias                                                       :: Network 1 RadialPts RadialPtsB
        l2 = uncurry neuralLayer R.idPair (const rate) (const mom) ios2  :: Network 1 RadialPtsB 1

    return $ proc (xs1, ts) -> do
        xs2 <-   arr l1 -< xs1
        ys  <- close (l2b >>> l2) -< (xs2, (\ys -> ys - ts))
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

--            return (meanSquaredError ysl (last errorsl), meanSquaredError yst (last errorst))
            print $ meanSquaredError ysl (last errorsl)
            void $ plot (PNG $ printf "radial-approx-error-%f-%f-%f-%d-%f.png" scales rate mom epochs initRange)
                [ Data2D [Title "Mean squared error - training set", Style Lines] [] $ zip [1..] (fmap (meanSquaredError ysl) errorsl)
                , Data2D [Title "Mean squared error - test set",     Style Lines] [] $ zip [1..] (fmap (meanSquaredError yst) errorst)
                ]
            void $ plot (PNG $ printf "radial-approx-result-%f-%f-%f-%d-%f.png" scales rate mom epochs initRange)
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


--doRadialApprox :: IO ()
--doRadialApprox = do
--    results <- sequence $ do
--        scale <- scales
--        mom   <- moms
--        rate  <- rates
--        return $ aux scale mom rate
--
--    let noNaNs = filter (not . anyNaN) results
--
--    let minTrain = minimumBy leastTrain noNaNs
--
----    forM_ noNaNs showTuple
--    putStrLn $ showTuple minTrain
--    where
--        scales = [5.0, 2.0, 1.0, 0.1, 0.01]
--        rates = [1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001]
--        moms =  [0.9, 0.5, 0.1, 0.0]
--
--        aux :: Double -> Double -> Double -> IO (Double, Double, Double, Double, Double)
--        aux scale mom rate = do
--            (msel, mset) <- solveRadialApprox scale rate mom 250 0.5
--            return (scale, rate, mom, msel, mset)
--
--        leastTrain (_, _, _, msela, _) (_, _, _, mselb, _) = compare msela mselb
--
--        anyNaN (a, b, c, d, e) = isNaN a || isNaN b || isNaN c || isNaN d || isNaN e
--        showTuple (a, b, c, d, e) = printf "&%f&%f&%f&%.4f&%.4f\n" a b c d e

parseRadialApprox :: Parser (IO ())
--parseRadialApprox = pure doRadialApprox
parseRadialApprox = solveRadialApprox
    <$> option auto (short 's' <> long "scale" <> value 1.0 <> showDefault
                    <> help "Scaling coefficients of the gaussian function in the first layer.")
    <*> option auto (short 'r' <> long "rate" <> value 0.1 <> showDefault
                    <> help "Learning rate of the neurons in the second layer.")
    <*> option auto (short 'm' <> long "momentum" <> value 0.5 <> showDefault
                    <> help "Momentum of the neurons in the second layer.")
    <*> option auto (short 'e' <> long "epochs" <> value 250 <> showDefault
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

makeRadialClass1KMeans :: forall g m n r . (RandomGen g, KnownNat m, KnownNat n, KnownNat r) =>
                   L m n -> g -> Circuit (L m n) (L r n, [Double])
makeRadialClass1KMeans xss gen = proc xs -> do
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

type ClassNeurons = 5
type ClassNeuronsB = 6

makeRadialClass2Neural :: forall m n . (KnownNat m, KnownNat n) =>
                    L ClassNeurons n -> Double -> Double -> Double -> State [Double] (Network m n 3)
makeRadialClass2Neural ios1 scales rate mom = do
    os2 <- takeMatrix
    let l1 = radialBasis ios1 (konst scales)                            :: L m n -> L m ClassNeurons
        l2  = uncurry neuralLayer R.idPair (const rate) (const mom) os2 :: Network m ClassNeuronsB 3
    return $ liftFb l1 (const $ konst 0.0) >>> bias >>> l2

makeRadialClass2Inv :: forall m n . (KnownNat m, KnownNat n) =>
                    L ClassNeurons n -> Double -> L m n -> L m 3 -> Network 1 n 3
makeRadialClass2Inv ios1 scales xss ys =
    let l1  = radialBasis ios1 (konst scales)                           :: L 1 n -> L 1 ClassNeurons
        gs  = (radialBasis ios1 (konst scales)) xss                     :: L m ClassNeurons
        gs' =  blockAt 1.0 0 1 (unwrap gs)                              :: L m ClassNeuronsB
        os2 = gs' <\> ys                                                :: L ClassNeuronsB 3
        l2  = uncurry neuralLayer R.idPair (const 0.0) (const 0.0) os2  :: Network 1 ClassNeuronsB 3
    in  liftFb l1 (const $ konst 0.0) >>> bias >>> l2

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
            let network1 = makeRadialClass1KMeans xsl gen1
                (centers, errors) = evalState (fmap last $ replicateM epochs1 $ run xsl) network1

--            let network2 = evalState (makeRadialClass2Neural centers scale rate mom) (randomInit initRange gen2)
--                ((resLs, resTs), trained) = runState ((fmap unzip . replicateM epochs2) (trainEpoch ysl xsl >> zipM (predictInRows xsl) (predictInRows xst))) network2
--
            let network2 = makeRadialClass2Inv centers scale xsl ysl
                ((resLs, resTs), trained) = runState (zipM (predictInRows xsl) (predictInRows xst)) network2

--                resLsCls = fmap matrixToClasses resLs
--                resTsCls = fmap matrixToClasses resTs
                resLsCls = matrixToClasses resLs
                resTsCls = matrixToClasses resTs

--            print $ last resTs
--            print $ last resTsCls
--            print $ cst
--            print $ meanSquaredError ysl resLs

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

            void $ plot (PNG $ printf "radial-class-quantization-%f-%f-%f-%fto%f-%fto%f-%d-%d-%f.png" scale rate mom rateWinS rateWinE rateNbS rateNbE epochs1 epochs2 initRange) $
                Data2D [Title "Quantization error", Style Lines] [] $ zip [1..] errors

            void $ plot (PNG $ printf "radial-class-guesses-%f-%f-%f-%fto%f-%fto%f-%d-%d-%f.png" scale rate mom rateWinS rateWinE rateNbS rateNbE epochs1 epochs2 initRange)
                [ Data2D     [Title "Ones",    Style Points, Color Red  ]  [] ones
                , Data2D     [Title "Twos",    Style Points, Color Green]  [] twos
                , Data2D     [Title "Threes",  Style Points, Color Blue ]  [] thrs
                , Data2D     [Title "Centers", Style Points, Color Black]  [] (toPoints centers)
                ]

            void $ plot (PNG $ printf "radial-class-correct-%f-%f-%f-%fto%f-%fto%f-%d-%d-%f.png" scale rate mom rateWinS rateWinE rateNbS rateNbE epochs1 epochs2 initRange)
                [ Data2D     [Title "Correct Ones",  Style Points, Color Red  ]  [] wones
                , Data2D     [Title "Correct Twos",  Style Points, Color Green]  [] wtwos
                , Data2D     [Title "Correct Threes", Style Points, Color Blue ]  [] wthrs
                , Data2D     [Title "Centers",        Style Points, Color Black]  [] (toPoints centers)
                ]

--            print $ meanSquaredError ysl (last resLs)
            print $ meanSquaredError ysl resLs
            print $ meanSquaredError yst resTs
--            void $ plot (PNG $ printf "radial-class-mse-%f-%f-%f-%fto%f-%fto%f-%d-%d-%f.png" scale rate mom rateWinS rateWinE rateNbS rateNbE epochs1 epochs2 initRange)
--                [ Data2D [Title "Mean squared error - training set", Style Lines] [] $ zip [1..]  (fmap (meanSquaredError ysl) resLs)
--                , Data2D [Title "Mean squared error - test set",     Style Lines] [] $ zip [1..]  (fmap (meanSquaredError yst) resTs)
--                ]
--
            print $ precision csl resLsCls
            print $ precision cst resTsCls
--            void $ plot (PNG $ printf "radial-class-prec-%f-%f-%f-%fto%f-%fto%f-%d-%d-%f.png" scale rate mom rateWinS rateWinE rateNbS rateNbE epochs1 epochs2 initRange)
--                [ Data2D [Title "Precision - training set", Style Lines] [] $ zip [1..]  (fmap (precision csl) resLsCls)
--                , Data2D [Title "Precision - test set",     Style Lines] [] $ zip [1..]  (fmap (precision cst) resTsCls)
--                ]

--doRadialClass :: IO ()
--doRadialClass = do
--    (gen1:gen2:_) <- fmap splits getStdGen
--    ps1 <- readMatrix "test/classification_train.txt" :: IO (L 90 5)
--    pst <- readMatrix "test/classification_test.txt"  :: IO (L 93 5)
--
--    let (xsl, csl) = separateYs ps1 :: (L 90 4, L 90 1)
--        (xst, cst) = separateYs pst :: (L 93 4, L 93 1)
--        ysl = classesToMatrix csl :: L 90 3
--        yst = classesToMatrix cst :: L 93 3
--
--    let network1 = makeRadialClass1KMeans xsl gen1
--        (centers, errors) = evalState (fmap last $ replicateM 40 $ run xsl) network1
--
--    let aux :: Double -> Double -> Double -> Int -> Double -> IO (Double, Double, Double, Double, Double, Double, Double)
--        aux scale rate mom epochs2 initRange = do
--
--            let network2 = evalState (makeRadialClass2Neural centers scale rate mom) (randomInit initRange gen2)
--                ((resLs, resTs), trained) = runState ((fmap unzip . replicateM epochs2) (trainEpoch ysl xsl >> zipM (predictInRows xsl) (predictInRows xst))) network2
--
--                resLsCls = matrixToClasses $ last resLs
--                resTsCls = matrixToClasses $ last resTs
--
--            return (scale, rate, mom, meanSquaredError ysl $ last resLs, meanSquaredError yst $ last resTs, precision csl resLsCls, precision cst resTsCls)
--
--    let auxInv :: Double -> IO (Double, Double, Double, Double, Double)
--        auxInv scale = do
--
--            let network2inv = makeRadialClass2Inv centers scale xsl ysl
--                ((resLs, resTs), trained) = runState (zipM (predictInRows xsl) (predictInRows xst)) network2inv
--
--                resLsCls = matrixToClasses resLs
--                resTsCls = matrixToClasses resTs
--
--            return (scale, meanSquaredError ysl resLs, meanSquaredError yst resTs, precision csl resLsCls, precision cst resTsCls)
--
--    results <- sequence $ do
--        scale <- scales
--        rate  <- rates
--        mom   <- moms
--        return $ aux scale rate mom 200 0.5
--
--    resultsInv <- sequence $ do
--        scale <- scales
--        return $ auxInv scale
--
--    let noNaNs = filter (not . anyNaN) results
--
--    let noNaNsInv = filter (not . anyNaN5) resultsInv
--
--    let minMse = minimumBy leastTrain noNaNs
--
--    let minMseInv = minimumBy leastTrain5 noNaNsInv
--
--    putStrLn $ printf "&%.4f&" (last errors) ++ showTuple minMse ++ "&" ++ showTuple5 minMseInv
--
--    where
--        scales = [5.0, 2.0, 1.0, 0.1, 0.01]
--        rates = [0.1, 0.03, 0.01, 0.003, 0.001, 0.0003, 0.0001]
--        moms =  [0.9, 0.5, 0.1, 0.0]
--
--        leastTrain (_, _, _, msela, _, _, _) (_, _, _, mselb, _, _, _) = compare msela mselb
--        leastTrain5 (_, msela, _, _, _) (_, mselb, _, _, _) = compare msela mselb
--        showTuple (scale, rate, mom, msel, mset, prel, pret) = printf "%f&%f&%f&%.4f&%.4f&%.4f&%.4f" scale rate mom msel mset prel pret
--        showTuple5 (scale, msel, mset, prel, pret) = printf "%f&%.4f&%.4f&%.4f&%.4f" scale msel mset prel pret
--        anyNaN (a, b, c, d, e, f, g) = isNaN a || isNaN b || isNaN c || isNaN d || isNaN e || isNaN f || isNaN g
--        anyNaN5 (a, b, c, d, e) = isNaN a || isNaN b || isNaN c || isNaN d || isNaN e


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
    <*> option auto (short 'E' <> long "epochs2" <> value 250 <> showDefault
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
                     Double -> Double -> Double -> L ml 1 -> g ->
                     State [Double] (Network m 1 1)
makeRadialDescentApprox rate1 rate2 mom2 xsl gen = do
    let os1 = takeSample xsl gen
        rates1 = konst 0.5
    os2 <- takeMatrix
    let l1  = uncurry radialDescent gaussPair rate1 os1 rates1               :: Network m 1 5
        l2  = uncurry neuralLayer   R.idPair  (const rate2) (const mom2) os2 :: Network m 6 1
    return $ l1 >>> bias >>> l2

solveRadialDescentApprox :: Bool -> Double -> Double -> Double -> Int -> Double -> IO ()
solveRadialDescentApprox alt = if not alt
                         then aux "test/approximation_train_1.txt" (Proxy :: Proxy 81)
                         else aux "test/approximation_train_2.txt" (Proxy :: Proxy 15)
    where
        aux :: forall m . (KnownNat m) => String -> Proxy m -> Double -> Double -> Double -> Int -> Double -> IO ()
        aux path _proxy rate1 rate2 mom2 epochs initRange = do
            gen <- getStdGen
--            let gen = mkStdGen 50
            pst <- readMatrix "test/approximation_test.txt" :: IO (L 1000 2)
            psl <- readMatrix path :: IO (L m 2)

            let (xst, yst) = separateYs pst :: (L 1000 1, L 1000 1)
                (xsl, ysl) = separateYs psl :: (L m 1, L m 1)

            let network = evalState (makeRadialDescentApprox rate1 rate2 mom2 xsl gen) (randomInit initRange gen)
                ((resls, rests), trained) = runState ((fmap unzip . replicateM epochs) (trainEpoch ysl xsl >> zipM (predictInRows xsl) (predictInRows xst))) network

--            return (, meanSquaredError yst (last rests))
            print $ meanSquaredError ysl (last resls)
            void $ plot (PNG $ printf "radial-descent-approx-mse-%f-%f-%f-%d-%f.png" rate1 rate2 mom2 epochs initRange)
                [ Data2D [Title "Mean squared error - training set", Style Lines] [] $ zip [1..] (fmap (meanSquaredError ysl) resls)
                , Data2D [Title "Mean squared error - test set",     Style Lines] [] $ zip [1..] (fmap (meanSquaredError yst) rests)
                ]
            void $ plot (PNG $ printf "radial-descent-approx-result-%f-%f-%f-%d-%f.png" rate1 rate2 mom2 epochs initRange)
                [ Data2D     [Title "Training set", Style Points, Color Red  ] [] $ zip (toList1 xsl) (toList1 ysl)
                , Data2D     [Title "Test set",     Style Points, Color Black] [] $ zip (toList1 xst) (toList1 yst)
                , Function2D [Title "Prediction",   Style Lines , Color Green] [] $ predictFunc trained
                ]
            where
                predictFunc :: Network 1 1 1 -> Double -> Double
                predictFunc nn x = (mfirst . snd . propagate nn) (konst x :: L 1 1)

--doMinRadialDescentApprox :: IO ()
--doMinRadialDescentApprox = do
--    results <- sequence $ do
--        rate1 <- rates1
--        rate2 <- rates2
--        mom   <- moms
--        return $ aux rate1 rate2 mom
--
--    let noNaNs = filter (not . anyNaN) results
--
--    let minTrain = minimumBy leastTrain noNaNs
--
----    forM_ noNaNs showTuple
--    putStrLn $ showTuple minTrain
--    where
--        rates1 = [0.01, 0.005, 0.001, 0.0005, 0.0001]
--        rates2 = [1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001]
--        moms =  [0.9, 0.5, 0.1, 0.0]
--
--        aux :: Double -> Double -> Double -> IO (Double, Double, Double, Double, Double)
--        aux rate1 rate2 mom = do
--            (msel, mset) <- solveRadialDescentApprox False rate1 rate2 mom 250 0.5
--            return (rate1, rate2, mom, msel, mset)
--
--        leastTrain (_, _, _, msela, _) (_, _, _, mselb, _) = compare msela mselb
--
--        anyNaN (a, b, c, d, e) = isNaN a || isNaN b || isNaN c || isNaN d || isNaN e
--        showTuple (a, b, c, d, e) = printf "&%f&%f&%f&%.4f&%.4f\n" a b c d e

parseRadialDescentApprox :: Parser (IO ())
parseRadialDescentApprox = solveRadialDescentApprox
    <$> switch      (short 'a' <> long "alt"
                    <> help "Use an alternative data set in text/approximation_train_2.txt.")
    <*> option auto (short 'r' <> long "rate1" <> value 0.001 <> showDefault
                    <> help "Learning rate of the radial layer.")
    <*> option auto (short 'R' <> long "rate2" <> value 0.01 <> showDefault
                    <> help "Learning rate of the neurons in the output layer.")
    <*> option auto (short 'M' <> long "momentum2" <> value 0.0 <> showDefault
                    <> help "Momentum of the neurons in the output layer.")
    <*> option auto (short 'e' <> long "epochs" <> value 250 <> showDefault
                    <> help "Number of epochs of training the network will go through.")
    <*> option auto (short 'I' <> long "initRange" <> value 1.0 <> showDefault
                    <> help "The neurons will be initialized with values from (-I, I).")

makeRadialDescentClass :: forall m n ml g . (KnownNat m, KnownNat n, KnownNat ml, RandomGen g) =>
                      Double -> Double -> Double -> L ml n -> g ->
                      State [Double] (Network m n 3)
makeRadialDescentClass rate1 rate2 mom2 xsl gen = do
    let os1 = takeSample xsl gen
        rates1 = konst 0.5
    os2 <- takeMatrix
    let l1  = uncurry radialDescent gaussPair rate1 os1 rates1            :: Network m n 90
        l2  = uncurry neuralLayer R.idPair (const rate2) (const mom2) os2 :: Network m 91 3
    return $ l1 >>> bias >>> l2

solveRadialDescentClass :: [Bool] -> Double -> Double -> Double -> Int -> Double -> IO ()
solveRadialDescentClass chosen =
    case length $ filter id chosen of
        0 -> aux chosen (Proxy :: Proxy 0)
        1 -> aux chosen (Proxy :: Proxy 1)
        2 -> aux chosen (Proxy :: Proxy 2)
        3 -> aux chosen (Proxy :: Proxy 3)
        4 -> aux chosen (Proxy :: Proxy 4)
    where
        aux :: forall n . (KnownNat n) =>
               [Bool] -> Proxy n -> Double -> Double -> Double -> Int -> Double -> IO ()
        aux chosen _proxy rate1 rate2 mom2 epochs initRange = do
            (gen1:gen2:_) <- fmap splits getStdGen
            ps1 <- readMatrix "test/classification_train.txt" :: IO (L 90 5)
            pst <- readMatrix "test/classification_test.txt"  :: IO (L 93 5)

            let (xsl', cs1) = separateYs ps1 :: (L 90 4, L 90 1)
                xsl = dropChosen chosen xsl' :: L 90 n
                (xst', cst) = separateYs pst :: (L 93 4, L 93 1)
                xst = dropChosen chosen xst' :: L 93 n
                ys1 = classesToMatrix cs1 :: L 90 3
                yst = classesToMatrix cst :: L 93 3

            let network = evalState (makeRadialDescentClass rate1 rate2 mom2 xsl gen1) (randomInit initRange gen2)
                ((resLs, resTs), trained) = runState ((fmap unzip . replicateM epochs) (trainEpoch ys1 xsl >> zipM (predictInRows xsl) (predictInRows xst))) network
                resLsCls = fmap matrixToClasses resLs
                resTsCls = fmap matrixToClasses resTs

--            return (meanSquaredError ys1 $ last resLs, meanSquaredError yst $ last resTs, precision cs1 $ last resLsCls, precision cst $ last resTsCls)

--            print $ takeNth 10 resLs
--            print $ takeNth 10 resLsCls
            print $ meanSquaredError ys1 (last resLs)
            print $ precision cs1 (last resLsCls)
--            print $ last resTsCls
--            print $ cst
--
            let toPoints :: (KnownNat kot, KnownNat pies) => L kot pies -> [(Double, Double)]
                toPoints = fmap (\(i, row) -> (takePoint . (++[i]) . D.toList . unwrap) row) . zip [1 * x | x <- [1..]] . toRows

                takePoint :: [Double] -> (Double, Double)
                takePoint (a:b:_) = (a, b)
                takePoint [a] = (a, 0.0)
                ixses = toPoints xst

                filterBro :: (b -> Bool) -> [a] -> [b] -> [a]
                filterBro f (a:as) (b:bs) = if f b then a : filterBro f as bs else filterBro f as bs
                filterBro _ [] [] = []

                [ones, twos, thrs] = (\n -> filterBro (==n) ixses (toList1 $ last resTsCls)) <$> [1.0, 2.0, 3.0]
                [wones, wtwos, wthrs] = (\n -> filterBro (==n) ixses (toList1 cst)) <$> [1.0, 2.0, 3.0]

            void $ plot (PNG $ printf "radial-descent-class-guesses-%f-%f-%f-%d-%f.png" rate1 rate2 mom2 epochs initRange)
                [ Data2D     [Title "Ones", Style Points, Color Red  ]  [] ones
                , Data2D     [Title "Twos", Style Points, Color Green]  [] twos
                , Data2D     [Title "Threes", Style Points, Color Blue ]  [] thrs
                ]

            void $ plot (PNG $ printf "radial-descent-class-correct-%f-%f-%f-%d-%f.png" rate1 rate2 mom2 epochs initRange)
                [ Data2D     [Title "Correct Ones", Style Points, Color Red  ]  [] wones
                , Data2D     [Title "Correct Twos", Style Points, Color Green]  [] wtwos
                , Data2D     [Title "Correct Threes", Style Points, Color Blue ]  [] wthrs
                ]

            void $ plot (PNG $ printf "radial-descent-class-mse-%f-%f-%f-%d-%f.png" rate1 rate2 mom2 epochs initRange)
                [ Data2D [Title "Mean squared error - training set", Style Lines] [] $ zip [1..]  (fmap (meanSquaredError ys1) resLs)
                , Data2D [Title "Mean squared error - test set",     Style Lines] [] $ zip [1..]  (fmap (meanSquaredError yst) resTs)
                ]

            void $ plot (PNG $ printf "radial-descent-class-prec-%f-%f-%f-%d-%f.png" rate1 rate2 mom2 epochs initRange)
                [ Data2D [Title "Precision - training set", Style Lines] [] $ zip [1..]  (fmap (precision cs1) resLsCls)
                , Data2D [Title "Precision - test set",     Style Lines] [] $ zip [1..]  (fmap (precision cst) resTsCls)
                ]


--doRadialDescentClass :: IO ()
--doRadialDescentClass = do
--    results <- sequence $ do
--        rate1 <- rates1
--        rate2 <- rates2
--        mom2  <- moms2
--        return $ aux rate1 rate2 mom2
--
--    let noNaNs = filter (not . anyNaN) results
--
--    let minMse = minimumBy leastTrain noNaNs
--
--    putStrLn $ showTuple minMse
--    where
--        rates1 = [0.03, 0.01, 0.003, 0.001, 0.0003, 0.0001]
--        rates2 = [0.3, 0.1, 0.03, 0.01, 0.003, 0.001, 0.0003, 0.0001]
--        moms2  = [0.9, 0.5, 0.1, 0.0]
--
--        aux :: Double -> Double -> Double -> IO (Double, Double, Double, Double, Double, Double, Double)
--        aux rate1 rate2 mom2 = do
--            (msel, mset, precl, prect) <- solveRadialDescentClass (replicate 4 True) rate1 rate2 mom2 200 0.5
--            return (rate1, rate2, mom2, msel, mset, precl, prect)
--
--        leastTrain (_, _, _, msela, _, _, _) (_, _, _, mselb, _, _, _) = compare msela mselb
--        anyNaN (a, b, c, d, e, f, g) = isNaN a || isNaN b || isNaN c || isNaN d || isNaN e || isNaN f || isNaN g
--        showTuple (rate1, rate2, mom2, msel, mset, prel, pret) = printf "%f&%f&%f&%.4f&%.4f&%.4f&%.4f" rate1 rate2 mom2 msel mset prel pret

parseRadialDescentClass :: Parser (IO ())
parseRadialDescentClass = solveRadialDescentClass
    <$> (fmap parseFeatures . strOption)
                    (short 'f' <> long "features" <> value "1111" <> showDefault
                    <> help "A string of four 0s and 1s denoting which features to use.")
    <*> option auto (short 'r' <> long "rate1" <> value 0.001 <> showDefault
                    <> help "Learning rate of the neurons of the radial layer.")
    <*> option auto (short 'R' <> long "rate2" <> value 0.3 <> showDefault
                    <> help "Learning rate of the neurons of the output layer.")
    <*> option auto (short 'M' <> long "momentum2" <> value 0.0 <> showDefault
                    <> help "Momentum of the neurons of the output layer.")
    <*> option auto (short 'e' <> long "epochs" <> value 200 <> showDefault
                    <> help "Number of epochs of training the network will go through.")
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
