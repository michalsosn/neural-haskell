{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators #-}

module MainIAD1 where

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
import RandomInit
import Regression

makeLinear :: forall m . (KnownNat m) => Double -> Double -> Network m 2 1
makeLinear rate mom =
    let l1  = uncurry neuralLayer sigmoidPair (const rate) (const mom) (matrix [0.0, 1.0, 0.0]) :: Network m 3 1
    in  bias >>> l1

solveLinear :: Double -> Double -> Int -> IO ()
solveLinear rate mom points = do
    gen <- getStdGen

    let xs = matrix $ take points $ randomRs (-10.0, 10.0) gen               :: L 500 2
        ys = col $ vmap (label (>= -2.0)) $ xs #> (vector [1.0, 3.0] :: R 2) :: L 500 1

        network = makeLinear rate mom
        result = evalState (forM (toRowMs ys `zip` toRowMs xs) (\(y, x) -> trainRow y x >> predictInRows xs)) network

    void $ plot X11 $ Data2D [Title "Mean squared error", Style Lines] [] $ zip [1..] (fmap (meanSquaredError ys) result)
    showPoints "Labels" xs ys
--    forM_ (takeNth 10 ([1..] `zip` result)) (\(i, us) -> showPoints (show i) xs us >> getLine)
    animatePlots "linear.gif" 3 (fmap (\(i, us) -> plotPoints (show i) xs us) (takeNth 10 ([1..] `zip` result)))

    where
        showPoints :: (KnownNat m) => String -> L m 2 -> L m 1 -> IO ()
        showPoints n xs bs =
            let (ps, ns) = partitionWith (>= 0.5) bs xs
            in void $ plot X11 [ Data2D [Title $ n ++ " Positives", Style Points, Color Green] [] ps
                               , Data2D [Title "Negatives", Style Points, Color Red] [] ns
                               ]
        plotPoints :: (KnownNat m) => String -> L m 2 -> L m 1 -> [Graph2D Double Double]
        plotPoints n xs bs =
            let (ps, ns) = partitionWith (>= 0.5) bs xs
            in  [ Data2D [Title $ n ++ " Positives", Style Points, Color Green] [] ps
                , Data2D [Title "Negatives", Style Points, Color Red] [] ns
                ]

parseLinear :: Parser (IO ())
parseLinear = solveLinear
    <$> option auto (short 'r' <> long "rate" <> value 0.1 <> showDefault
                    <> help "Learning rate of the neural network.")
    <*> option auto (short 'm' <> long "momentum" <> value 0.0 <> showDefault
                    <> help "Momentum of the neural network.")
    <*> option auto (short 'p' <> long "points" <> value 1000 <> showDefault
                    <> help "Number of random points.")


makeTransformation :: forall m . (KnownNat m) =>
                      Double -> Double -> Double -> Double -> State [Double] (Network m 4 4)
makeTransformation rate1 mom1 rate2 mom2 = do
    os1 <- takeMatrix
    os2 <- takeMatrix
    let l1  = uncurry neuralLayer sigmoidPair (const rate1) (const mom1) os1 :: Network m 5 2
        l2  = uncurry neuralLayer sigmoidPair (const rate2) (const mom2) os2 :: Network m 3 4
    return $ bias >>> l1 >>> bias >>> l2

solveTransformation :: Double -> Double -> Double -> Double -> Int -> Double -> IO ()
solveTransformation rate1 mom1 rate2 mom2 epochs initRange = do
    gen <- getStdGen
--    let gen = mkStdGen 50
    xs <- readMatrix "test/transformation.txt" :: IO (L 4 4)

    let network = evalState (makeTransformation rate1 mom1 rate2 mom2 ) (randomInit initRange gen)
        result = evalState (replicateM epochs $ train xs (\r -> r - xs)) network

    void $ plot (PNG $ "transformation" ++ (show rate1) ++ "-" ++ show mom1 ++ ".png") $
        Data2D [Title "Mean squared error", Style Lines] [] $ zip [1..] (fmap (meanSquaredError xs) result)
--    print $ takeNth 100 result
--    print $ meanSquaredError xs $ last result
    print $ last result

--doTransformation :: Double -> Double -> IO Double
--doTransformation mom rate = do
--    gen <- getStdGen
--    xs <- readMatrix "test/transformation.txt" :: IO (L 4 4)
--
--    let network = evalState (makeTransformation rate mom rate mom) (randomInit 1.0 gen)
--        result = evalState (replicateM 1000 $ train xs (\r -> r - xs)) network
--
--    return $ meanSquaredError xs $ last result

parseTransformation :: Parser (IO ())
parseTransformation = solveTransformation
    <$> option auto (short 'r' <> long "rate1" <> value 0.3 <> showDefault
                    <> help "Learning rate of the neurons in the hidden layer.")
    <*> option auto (short 'm' <> long "momentum1" <> value 0.0 <> showDefault
                    <> help "Momentum of the neurons in the hidden layer.")
    <*> option auto (short 'R' <> long "rate2" <> value 0.3 <> showDefault
                    <> help "Learning rate of the neurons in the output layer.")
    <*> option auto (short 'M' <> long "momentum2" <> value 0.0 <> showDefault
                    <> help "Momentum of the neurons in the output layer.")
    <*> option auto (short 'e' <> long "epochs" <> value 10000 <> showDefault
                    <> help "Number of epochs of training the network will go through.")
    <*> option auto (short 'I' <> long "initRange" <> value 1.0 <> showDefault
                    <> help "The neurons will be initialized with values from (-I, I).")


makeApproximation :: forall m . (KnownNat m) =>
                     Double -> Double -> Double -> Double -> State [Double] (Network m 1 1)
makeApproximation rate1 mom1 rate2 mom2 = do
    os1 <- takeMatrix
    os2 <- takeMatrix
    let l1  = uncurry neuralLayer sigmoidPair (const rate1) (const mom1) os1 :: Network m 2 10
        l2  = uncurry neuralLayer idPair      (const rate2) (const mom2) os2 :: Network m 11 1
    return $ bias >>> l1 >>> bias >>> l2

solveApproximation :: Bool -> Double -> Double -> Double -> Double -> Int -> Double -> IO ()
solveApproximation alt = if not alt
                         then aux "test/approximation_train_1.txt" (Proxy :: Proxy 81)
                         else aux "test/approximation_train_2.txt" (Proxy :: Proxy 15)
    where
        aux :: forall m . (KnownNat m) => String -> Proxy m -> Double -> Double -> Double -> Double -> Int -> Double -> IO ()
        aux path _proxy rate1 mom1 rate2 mom2 epochs initRange = do
            gen <- getStdGen
            pst <- readMatrix "test/approximation_test.txt" :: IO (L 1000 2)
            psl <- readMatrix path :: IO (L m 2)

            let (xst, yst) = separateYs pst :: (L 1000 1, L 1000 1)
                (xsl, ysl) = separateYs psl :: (L m 1, L m 1)

            let network = evalState (makeApproximation rate1 mom1 rate2 mom2) (randomInit initRange gen)
                ((resls, rests), trained) = runState ((fmap unzip . replicateM epochs) (trainEpoch ysl xsl >> zipM (predictInRows xsl) (predictInRows xst))) network

            void $ plot (PNG $ "error" ++ show rate1 ++ "-" ++ show mom1 ++ ".png")
                [ Data2D [Title "Mean squared error - training set", Style Lines] [] $ zip [1..] (fmap (meanSquaredError ysl) resls)
                , Data2D [Title "Mean squared error - test set",     Style Lines] [] $ zip [1..] (fmap (meanSquaredError yst) rests)
                ]
            void $ plot (PNG $ "predict" ++ show rate1 ++ "-" ++ show mom1 ++ ".png")
                [ Data2D     [Title "Training set", Style Points, Color Red  ] [] $ zip (toList1 xsl) (toList1 ysl)
                , Data2D     [Title "Test set",     Style Points, Color Black] [] $ zip (toList1 xst) (toList1 yst)
                , Function2D [Title "Prediction",   Style Lines , Color Green] [] $ predictFunc trained
                ]
            where
                predictFunc :: Network 1 1 1 -> Double -> Double
                predictFunc nn x = (mfirst . snd . propagate nn) (konst x :: L 1 1)


--doApproximation :: Bool -> Double -> Double -> IO (Double, Double)
--doApproximation alt mom rate = if not alt
--                      then aux "test/approximation_train_1.txt" (Proxy :: Proxy 81) rate mom rate mom 250 1.0
--                      else aux "test/approximation_train_2.txt" (Proxy :: Proxy 15) rate mom rate mom 500 1.0
--    where
--        aux :: forall m . (KnownNat m) => String -> Proxy m -> Double -> Double -> Double -> Double -> Int -> Double -> IO (Double, Double)
--        aux path _proxy rate1 mom1 rate2 mom2 epochs initRange = do
--            gen <- getStdGen
--            pst <- readMatrix "test/approximation_test.txt" :: IO (L 1000 2)
--            psl <- readMatrix path :: IO (L m 2)
--
--            let (xst, yst) = separateYs pst :: (L 1000 1, L 1000 1)
--                (xsl, ysl) = separateYs psl :: (L m 1, L m 1)
--
--            let network = evalState (makeApproximation rate1 mom1 rate2 mom2) (randomInit initRange gen)
--                ((resls, rests), trained) = runState ((fmap unzip . replicateM epochs) (trainEpoch ysl xsl >> zipM (predictInRows xsl) (predictInRows xst))) network
--
--            return (meanSquaredError ysl (last resls), meanSquaredError yst (last rests))

--doMin :: IO ()
--doMin = do
--    results <- sequence $ do
--        mom  <- moms
--        rate <- rates
--        return $ aux mom rate
--
--    let noNaNs = filter (not . anyNaN) results
--
--    let minTrain = minimumBy leastTrain noNaNs
--        minTest = minimumBy leastTest noNaNs
--
--    putStrLn $ showTuple minTrain ++ showTuple minTest
--    where
--        rates = [1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001, 0.0003, 0.0001]
--        moms =  [2.0, 1.0, 0.9, 0.5, 0.1, 0.0]
--        aux :: Double -> Double -> IO (Double, Double, Double, Double)
--        aux mom rate = do
--            (msel, mset) <- doApproximation True mom rate
--            return (rate, mom, msel, mset)
--        leastTrain (_, _, msela, _) (_, _, mselb, _) = compare msela mselb
--        leastTest  (_, _, _, mseta) (_, _, _, msetb) = compare mseta msetb
--        anyNaN (a, b, c, d) = isNaN a || isNaN b || isNaN c || isNaN d
--        showTuple (a, b, c, d) = "&" ++ printf "%.4f" a ++ "&" ++ printf "%.4f" b ++ "&" ++ printf "%.4f" c ++ "&" ++ printf "%.4f" d

parseApproximation :: Parser (IO ())
parseApproximation = solveApproximation
    <$> switch      (short 'a' <> long "alt"
                    <> help "Use an alternative data set in text/approximation_train_2.txt.")
    <*> option auto (short 'r' <> long "rate1" <> value 0.1 <> showDefault
                    <> help "Learning rate of the neurons in the hidden layer.")
    <*> option auto (short 'm' <> long "momentum1" <> value 0.0 <> showDefault
                    <> help "Momentum of the neurons in the hidden layer.")
    <*> option auto (short 'R' <> long "rate2" <> value 0.01 <> showDefault
                    <> help "Learning rate of the neurons in the output layer.")
    <*> option auto (short 'M' <> long "momentum2" <> value 0.0 <> showDefault
                    <> help "Momentum of the neurons in the output layer.")
    <*> option auto (short 'e' <> long "epochs" <> value 100 <> showDefault
                    <> help "Number of epochs of training the network will go through.")
    <*> option auto (short 'I' <> long "initRange" <> value 1.0 <> showDefault
                    <> help "The neurons will be initialized with values from (-I, I).")


makeClassification :: forall m n nb . (KnownNat m, KnownNat n, KnownNat nb) =>
                      Proxy nb -> Double -> Double -> Double -> Double -> State [Double] (Network m n 3)
makeClassification _proxyb rate1 mom1 rate2 mom2 = do
    os1 <- takeMatrix
    os2 <- takeMatrix
    let l1  = uncurry neuralLayer sigmoidPair (const rate1) (const mom1) os1 :: Network m nb 10
        l2  = uncurry neuralLayer sigmoidPair (const rate2) (const mom2) os2 :: Network m 11 3
    return $ bias >>> l1 >>> bias >>> l2

solveClassification :: [Bool] -> Double -> Double -> Double -> Double -> Int -> Double -> IO ()
solveClassification chosen =
    case length $ filter id chosen of
        0 -> aux chosen (Proxy :: Proxy 0) (Proxy :: Proxy 1)
        1 -> aux chosen (Proxy :: Proxy 1) (Proxy :: Proxy 2)
        2 -> aux chosen (Proxy :: Proxy 2) (Proxy :: Proxy 3)
        3 -> aux chosen (Proxy :: Proxy 3) (Proxy :: Proxy 4)
        4 -> aux chosen (Proxy :: Proxy 4) (Proxy :: Proxy 5)
    where
        aux :: forall n nb . (KnownNat n, KnownNat nb) =>
               [Bool] -> Proxy n -> Proxy nb -> Double -> Double -> Double -> Double -> Int -> Double -> IO ()
        aux chosen _proxy proxyb rate1 mom1 rate2 mom2 epochs initRange = do
            gen <- getStdGen
            ps1 <- readMatrix "test/classification_train.txt" :: IO (L 90 5)
            pst <- readMatrix "test/classification_test.txt"  :: IO (L 93 5)

            let (xsl', cs1) = separateYs ps1 :: (L 90 4, L 90 1)
                xsl = dropChosen chosen xsl' :: L 90 n
                (xst', cst) = separateYs pst :: (L 93 4, L 93 1)
                xst = dropChosen chosen xst' :: L 93 n
                ys1 = classesToMatrix cs1 :: L 90 3
                yst = classesToMatrix cst :: L 93 3

            let network = evalState (makeClassification proxyb rate1 mom1 rate2 mom2) (randomInit initRange gen)
                ((resLs, resTs), trained) = runState ((fmap unzip . replicateM epochs) (trainEpoch ys1 xsl >> zipM (predictInRows xsl) (predictInRows xst))) network
                resLsCls = fmap matrixToClasses resLs
                resTsCls = fmap matrixToClasses resTs

--            print $ takeNth 10 resLs
--            print $ takeNth 10 resLsCls
            print $ last resTs
            print $ last resTsCls
            print $ cst

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

                showBools = concat . fmap (\b -> if b then "1" else "0")

            void $ plot (PNG $ "guesses-" ++ showBools chosen ++ ".png")
                [ Data2D     [Title "Ones", Style Points, Color Red  ]  [] ones
                , Data2D     [Title "Twos", Style Points, Color Green]  [] twos
                , Data2D     [Title "Threes", Style Points, Color Blue ]  [] thrs
                ]

            void $ plot (PNG $ "correct-" ++ showBools chosen ++ ".png")
                [ Data2D     [Title "Correct Ones", Style Points, Color Red  ]  [] wones
                , Data2D     [Title "Correct Twos", Style Points, Color Green]  [] wtwos
                , Data2D     [Title "Correct Threes", Style Points, Color Blue ]  [] wthrs
                ]

            void $ plot (PNG $ "mserror-" ++ showBools chosen ++ ".png")
                [ Data2D [Title "Mean squared error - training set", Style Lines] [] $ zip [1..]  (fmap (meanSquaredError ys1) resLs)
                , Data2D [Title "Mean squared error - test set",     Style Lines] [] $ zip [1..]  (fmap (meanSquaredError yst) resTs)
                ]

            void $ plot (PNG $ "precision-" ++ showBools chosen ++ ".png")
                [ Data2D [Title "Precision - training set", Style Lines] [] $ zip [1..]  (fmap (precision cs1) resLsCls)
                , Data2D [Title "Precision - test set",     Style Lines] [] $ zip [1..]  (fmap (precision cst) resTsCls)
                ]

--doClassification :: [Bool] -> Double -> Double -> IO (Double, Double)
--doClassification chosen rate mom =
--    case length $ filter id chosen of
--        0 -> aux chosen (Proxy :: Proxy 0) (Proxy :: Proxy 1) rate mom rate mom 200 1.0
--        1 -> aux chosen (Proxy :: Proxy 1) (Proxy :: Proxy 2) rate mom rate mom 200 1.0
--        2 -> aux chosen (Proxy :: Proxy 2) (Proxy :: Proxy 3) rate mom rate mom 200 1.0
--        3 -> aux chosen (Proxy :: Proxy 3) (Proxy :: Proxy 4) rate mom rate mom 200 1.0
--        4 -> aux chosen (Proxy :: Proxy 4) (Proxy :: Proxy 5) rate mom rate mom 200 1.0
--    where
--        aux :: forall n nb . (KnownNat n, KnownNat nb) =>
--               [Bool] -> Proxy n -> Proxy nb -> Double -> Double -> Double -> Double -> Int -> Double -> IO (Double, Double)
--        aux chosen _proxy proxyb rate1 mom1 rate2 mom2 epochs initRange = do
--            gen <- getStdGen
--            ps1 <- readMatrix "test/classification_train.txt" :: IO (L 90 5)
--            pst <- readMatrix "test/classification_test.txt"  :: IO (L 93 5)
--
--            let (xsl', cs1) = separateYs ps1 :: (L 90 4, L 90 1)
--                xsl = dropChosen chosen xsl' :: L 90 n
--                (xst', cst) = separateYs pst :: (L 93 4, L 93 1)
--                xst = dropChosen chosen xst' :: L 93 n
--                ys1 = classesToMatrix cs1 :: L 90 3
--                yst = classesToMatrix cst :: L 93 3
--
--            let network = evalState (makeClassification proxyb rate1 mom1 rate2 mom2) (randomInit initRange gen)
--                ((resLs, resTs), trained) = runState ((fmap unzip . replicateM epochs) (trainEpoch ys1 xsl >> zipM (predictInRows xsl) (predictInRows xst))) network
--                resLsCls = fmap matrixToClasses resLs
--                resTsCls = fmap matrixToClasses resTs
--
--            return (meanSquaredError yst (last resTs), precision cst (last resTsCls))
--
--doMin :: [Bool] -> IO ()
--doMin which = do
--    results <- sequence $ do
--        mom  <- moms
--        rate <- rates
--        return $ aux mom rate
--
--    let noNaNs = filter (not . anyNaN) results
--
--    let minMse = minimumBy leastTrain noNaNs
--        minPrec = maximumBy leastTest noNaNs
--
--    putStrLn $ showBools which ++ "&" ++ showTuple minMse ++ showTuple minPrec
--    where
--        rates = [0.1, 0.03, 0.01, 0.003, 0.001, 0.0003, 0.0001]
--        moms =  [1.0, 0.9, 0.5, 0.1, 0.0]
--        aux :: Double -> Double -> IO (Double, Double, Double, Double)
--        aux mom rate = do
--            (msel, mset) <- doClassification which mom rate
--            return (rate, mom, msel, mset)
--        leastTrain (_, _, msela, _) (_, _, mselb, _) = compare msela mselb
--        leastTest  (_, _, _, mseta) (_, _, _, msetb) = compare mseta msetb
--        anyNaN (a, b, c, d) = isNaN a || isNaN b || isNaN c || isNaN d
--        showTuple (a, b, c, d) = "&" ++ printf "%.4f" a ++ "&" ++ printf "%.4f" b ++ "&" ++ printf "%.4f" c ++ "&" ++ printf "%.4f" d
--        showBools = concat . fmap (\b -> if b then "1" else "0")

--main = mapM_ doMin $ replicateM 4 [False, True]

parseClassification :: Parser (IO ())
parseClassification = solveClassification
    <$> (fmap parseFeatures . strOption)
                    (short 'f' <> long "features" <> value "1111" <> showDefault
                    <> help "A string of four 0s and 1s denoting which features to use.")
    <*> option auto (short 'r' <> long "rate1" <> value 0.3 <> showDefault
                    <> help "Learning rate of the neurons in the hidden layer.")
    <*> option auto (short 'm' <> long "momentum1" <> value 0.0 <> showDefault
                    <> help "Momentum of the neurons in the hidden layer.")
    <*> option auto (short 'R' <> long "rate2" <> value 0.3 <> showDefault
                    <> help "Learning rate of the neurons in the output layer.")
    <*> option auto (short 'M' <> long "momentum2" <> value 0.0 <> showDefault
                    <> help "Momentum of the neurons in the output layer.")
    <*> option auto (short 'e' <> long "epochs" <> value 100 <> showDefault
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

