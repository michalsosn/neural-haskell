{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators, Arrows #-}

module MainIAD2 where

import Codec.Picture
import Codec.Picture.Png
import Control.Arrow
import Control.Monad hiding (msum)
import Control.Monad.State hiding (msum)
import qualified Data.ByteString as B
import qualified Data.List as L
import Data.Proxy
import Graphics.EasyPlot
import GHC.TypeLits
import qualified Numeric.LinearAlgebra.Data as D
import Numeric.LinearAlgebra.Static hiding ((<>))
import Options.Applicative
import System.Random as R
import Text.Printf

import Animation
import Clustering
import Feedback
import IOUtils
import LinearUtils
import Neural
import RandomInit

type NetSize = 100

makeKohonen :: (KnownNat m) =>
               Double -> Double -> Double -> Double -> Double -> L m 2 ->
               State [Double] (Circuit (L 1 2) ([Double], [[Graph2D Double Double]]))
makeKohonen rateWinS rateWinE rateNbS rateNbE iters xss = do
    os <- takeMatrix
    let (scale, descale) = scaleFeatures xss
--        rateWinFunc it = rateWin * (2 * iters - fromIntegral it) / (2 * iters)
--        rateNbFunc it = rateNb * (iters - fromIntegral it) / iters
        network = kohonen (rateFunc rateWinS rateWinE iters) (rateFunc rateNbS rateNbE iters) os :: Network' 1 NetSize 2
    return $ proc xs -> do
        sXs <- arr (rmap scale)   -< xs
        sOs <- network            -< sXs
        os  <- arr (rmap descale) -< sOs
        it  <- total -< 1
        error <- logger (quantizationError xss) -< os
        plots <- logger plotNeurons             -< (it, xs, os)
        returnA -< (error, plots)
    where
        plotNeurons :: (KnownNat m) => (Int, L 1 2, L m 2) -> [Graph2D Double Double]
        plotNeurons (it, xs, os) =
            [ Data2D [Title $ "Neurons " ++ show it, Style Points, Color Red  ] [] $ toPoints os
            , Data2D [Title $ "Input   " ++ show it, Style Points, Color Black] [] $ toPoints xs
            ]
--        plotNeurons :: (KnownNat m) => (Int, L 1 3, L m 3) -> [Graph3D Double Double Double]
--        plotNeurons (it, xs, os) =
--            [ Data3D [Title $ "Neurons " ++ show it, Style Points, Color Red  ] [] $ toPoints3D os
--            , Data3D [Title $ "Input   " ++ show it, Style Points, Color Black] [] $ toPoints3D xs
--            ]

solveKohonen :: Double -> Double -> Double -> Double -> Int -> Double -> Double -> IO ()
solveKohonen rateWinS rateWinE rateNbS rateNbE epochs size initRange = do
    (gen1:gen2:_) <- fmap splits getStdGen

--    let shape = evalState (takeCross (size / 4.0)) (randomInit size gen1)            :: L 500 2
--        shape = evalState (takeCircle 0.5 0.5 1.5) (randomInit size gen1)              :: L 500 2
    let shape = evalState (takeThreeCircles size) (randomInit size gen1)               :: L 500 2
--    let shape = evalState (takeWeird size) (randomInit size gen1)                      :: L 500 2
--        shape = evalState (takeCuboid [1.0, 2.0, 5.0]) (randomInit size gen1)          :: L 2000 3
--        shape = evalState (takeEuclidean [1.0, 1.0, 0.0] 3.0) (randomInit size gen1)   :: L 1000 3
--        shape = evalState (takeVeryWeird size) (randomInit size gen1)                  :: L 1000 3

    let iters = fromIntegral epochs * rowNum shape
        network = evalState (makeKohonen rateWinS rateWinE rateNbS rateNbE iters shape) (randomInit initRange gen2)
        (errors, nplots) = evalState (fmap (last . concat) $ replicateM epochs $ forM (toRowMs shape) run) network

    let dataPlot = Data2D [Title $ "Data", Style Points, Color Green] [] $ toPoints shape
--        dataPlot = Data3D [Title $ "Data", Style Points, Color Green] [] $ toPoints3D shape
        plots = fmap (dataPlot:) nplots

    printf "%.4f" $ last errors
    void $ plot (PNG $ "kohonen-error.png") $
                Data2D [Title "Quantization error", Style Lines] [] $ zip [1..] errors
    void $ plot (PNG $ "kohonen-final.png") $
                last plots
--    animatePlots "kohonen.gif" 3 plots

parseKohonen :: Parser (IO ())
parseKohonen = solveKohonen
    <$> option auto (short 'w' <> long "rateWinS" <> value 0.9 <> showDefault
                    <> help "Starting learning rate of the winner.")
    <*> option auto (short 'W' <> long "rateWinE" <> value 0.01 <> showDefault
                    <> help "Ending learning rate of the winner.")
    <*> option auto (short 'n' <> long "rateNbS" <> value 0.01 <> showDefault
                    <> help "Starting learning rate of the neighbourhood.")
    <*> option auto (short 'N' <> long "rateNbE" <> value 0.00001 <> showDefault
                    <> help "Ending learning rate of the neighbourhood.")
    <*> option auto (short 'e' <> long "epochs" <> value 2 <> showDefault
                    <> help "Number of epochs of training the network will go through.")
    <*> option auto (short 's' <> long "shapeSize" <> value 5.0 <> showDefault
                    <> help "The training data will be generated with values from (-s, s).")
    <*> option auto (short 'I' <> long "initRange" <> value 0.5 <> showDefault
                    <> help "The neurons will be initialized with values from (-I, I).")

makeNeuralGas :: (KnownNat m) =>
               Double -> Double -> Double -> Double -> Double -> L m 2 ->
               State [Double] (Circuit (L 1 2) ([Double], [[Graph2D Double Double]]))
makeNeuralGas rateWinS rateWinE rateNbS rateNbE iters xss = do
    os <- takeMatrix
    let (scale, descale) = scaleFeatures xss
        network = neuralGas (rateFunc rateWinS rateWinE iters) (rateFunc rateNbS rateNbE iters) os :: Network' 1 NetSize 2
    return $ proc xs -> do
        sXs <- arr (rmap scale)   -< xs
        sOs <- network            -< sXs
        os  <- arr (rmap descale) -< sOs
        it  <- total -< 1
        error <- logger (quantizationError xss) -< os
        plots <- logger plotNeurons             -< (it, xs, os)
        returnA -< (error, plots)
    where
        plotNeurons :: (KnownNat m) => (Int, L 1 2, L m 2) -> [Graph2D Double Double]
        plotNeurons (it, xs, os) =
            [ Data2D [Title $ "Neurons " ++ show it, Style Points, Color Red  ] [] $ toPoints os
            , Data2D [Title $ "Input   " ++ show it, Style Points, Color Black] [] $ toPoints xs
            ]

solveNeuralGas :: Double -> Double -> Double -> Double -> Int -> Double -> Double -> IO ()
solveNeuralGas rateWinS rateWinE rateNbS rateNbE epochs size initRange = do
    (gen1:gen2:_) <- fmap splits getStdGen

--    let shape = evalState (takeCross (size / 4.0)) (randomInit size gen1)            :: L 500 2
    let shape = evalState (takeThreeCircles size) (randomInit size gen1)               :: L 500 2
--    let shape = evalState (takeWeird size) (randomInit size gen1)                      :: L 500 2

    let iters = fromIntegral epochs * rowNum shape
        network = evalState (makeNeuralGas rateWinS rateWinE rateNbS rateNbE iters shape) (randomInit initRange gen2)
        (errors, nplots) = evalState (fmap (last . concat) $ replicateM epochs $ forM (toRowMs shape) run) network

    let dataPlot = Data2D [Title $ "Data", Style Points, Color Green] [] $ toPoints shape
        plots = fmap (dataPlot:) nplots

    printf "%.4f" $ last errors
    void $ plot (PNG $ "gas-error.png") $
                Data2D [Title "Quantization error", Style Lines] [] $ zip [1..] errors
    void $ plot (PNG $ "gas-final.png") $
                last plots
--    animatePlots "neuralGas.gif" 3 plots


parseNeuralGas = solveNeuralGas
    <$> option auto (short 'w' <> long "rateWinS" <> value 1.0 <> showDefault
                    <> help "Starting learning rate of the winner.")
    <*> option auto (short 'W' <> long "rateWinE" <> value 0.001 <> showDefault
                    <> help "Ending learning rate of the winner.")
    <*> option auto (short 'n' <> long "rateNbS" <> value 200.0 <> showDefault
                    <> help "Starting learning rate of the neighbourhood.")
    <*> option auto (short 'N' <> long "rateNbE" <> value 0.01 <> showDefault
                    <> help "Ending learning rate of the neighbourhood.")
    <*> option auto (short 'e' <> long "epochs" <> value 5 <> showDefault
                    <> help "Number of epochs of training the network will go through.")
    <*> option auto (short 's' <> long "shapeSize" <> value 5.0 <> showDefault
                    <> help "The training data will be generated with values from (-s, s).")
    <*> option auto (short 'I' <> long "initRange" <> value 0.5 <> showDefault
                    <> help "The neurons will be initialized with values from (-I, I).")

makeKMeans :: forall g m1 . (RandomGen g, KnownNat m1) =>
              L m1 2 -> g ->
              Circuit (L m1 2) ([Double], [[Graph2D Double Double]])
makeKMeans xss gen = proc xs -> do
    sXs <- arr (rmap scale)   -< xs
    sOs <- network            -< sXs
    os  <- arr (rmap descale) -< sOs
    it  <- total -< 1
    error <- logger (quantizationError xss) -< os
    plots <- logger plotNeurons -< (it, os)
    returnA -< (error, plots)
    where
        (scale, descale) = scaleFeatures xss
        ios = takeSample (rmap scale xss) gen
        network = kMeans ios :: Network' m1 NetSize 2

        plotNeurons :: (KnownNat m2) => (Int, L m2 2) -> [Graph2D Double Double]
        plotNeurons (it, os) =
            [ Data2D [Title $ "Neurons " ++ show it, Style Points, Color Red] [] $ toPoints os
            ]

solveKMeans :: Int -> Double -> IO ()
solveKMeans epochs size = do
    (gen1:gen2:_) <- fmap splits getStdGen

--    let shape = evalState (takeCross (size / 4.0)) (randomInit size gen1)            :: L 500 2
    let shape = evalState (takeThreeCircles size) (randomInit size gen1)               :: L 500 2
--    let shape = evalState (takeWeird size) (randomInit size gen1)                      :: L 500 2

    let network = makeKMeans shape gen2
        (errors, nplots) = evalState (fmap last $ replicateM epochs $ run shape) network

    let dataPlot = Data2D [Title $ "Data", Style Points, Color Green] [] $ toPoints shape
        plots = fmap (dataPlot:) nplots

    printf "%.4f" $ last errors
    void $ plot (PNG $ "kMeans-error.png") $
                Data2D [Title "Quantization error", Style Lines] [] $ zip [1..] errors
    void $ plot (PNG $ "kMeans-final.png") $
                last plots
--    void $ plot X11 $ Data2D [Title "Quantization error", Style Lines] [] $ zip [1..] errors
--    animatePlots "kMeans.gif" 10 plots

parseKMeans :: Parser (IO ())
parseKMeans = solveKMeans
    <$> option auto (short 'e' <> long "epochs" <> value 10 <> showDefault
                    <> help "Number of epochs of training the network will go through.")
    <*> option auto (short 's' <> long "shapeSize" <> value 5.0 <> showDefault
                    <> help "The training data will be generated with values from (-s, s).")


makeCompression :: forall m n . (KnownNat m, KnownNat n) =>
                   Double -> Double -> Double -> Double -> Double -> L m n ->
                   State [Double] (Circuit (L 1 n) (L m n, [Double], [Double]))
makeCompression rateWinS rateWinE rateNbS rateNbE iters xss = do
    ios <- takeMatrix

    let (scale, descale) = scaleFeatures xss
--        network = kohonen (rateFunc rateWinS rateWinE iters) (rateFunc rateNbS rateNbE iters) ios :: Network' 1 400 n
--        network = neuralGas (rateFunc rateWinS rateWinE iters) (rateFunc rateNbS rateNbE iters) ios :: Network' 1 400 n
        network = kMeans ios :: Network' 1 400 n

    return $ proc xs -> do
        sXs <- arr (rmap scale)   -< xs
        sOs <- network            -< sXs
        os  <- arr (rmap descale) -< sOs

        nearest <- arr (compressNearest xss)               -< os
        error   <- logger (quantizationError xss)          -< os
        psnr    <- logger (peakSignalToNoiseRatio 255 xss) -< nearest
        returnA -< (nearest, error, psnr)

makeCompression' :: forall g m1 n . (RandomGen g, KnownNat m1, KnownNat n) =>
                   L m1 n -> g ->
                   Circuit (L m1 n) (L m1 n, [Double], [Double])
makeCompression' xss gen = proc xs -> do
    sXs <- arr (rmap scale)   -< xs
    sOs <- network            -< sXs
    os  <- arr (rmap descale) -< sOs

    nearest <- arr (compressNearest xss)               -< os
    error   <- logger (quantizationError xss)          -< os
    psnr    <- logger (peakSignalToNoiseRatio 255 xss) -< nearest
    returnA -< (nearest, error, psnr)
    where
        (scale, descale) = scaleFeatures xss
        ios = takeSample (rmap scale xss) gen
        network = kMeans ios :: Network' m1 120 n

type Frame = [[PixelRGB8]]
--type Frame = [[Pixel8]]

solveCompression :: Double -> Double -> Double -> Double -> Int -> Double -> IO ()
solveCompression rateWinS rateWinE rateNbS rateNbE epochs initRange = do
    gen <- getStdGen

    file <- B.readFile "smieszny-piesek.png"
    let Right png = decodePng file
        ImageRGB8 img = png
--        ImageY8 img = png
        w = imageWidth img
        h = imageHeight img

        pixels = [[pixelAt img x y | x <- [0..w-1]] | y <- [0..h-1]]
        frames = splitFrames 4 4 pixels                                   :: [[Frame]]
--        features = fromRows . join . (fmap . fmap) frameToFeatures $ frames :: L 1600 27
        features = fromRows . join . (fmap . fmap) frameToFeatures $ frames :: L 900 48
--        features = fromRows . join . (fmap . fmap) frameToFeatures $ frames :: L 1600 9
--        features = fromRows . join . (fmap . fmap) frameToFeatures $ frames :: L 900 16

    let iters = fromIntegral epochs * rowNum features
        network = makeCompression' features gen
        (features', errors, psnrs) = evalState (fmap last $ replicateM epochs $ run features) network
--        network = evalState (makeCompression rateWinS rateWinE rateNbS rateNbE iters features) $ randomInit initRange gen
--        (features', errors, psnrs) = evalState (fmap (last . concat) $ replicateM epochs $ forM (toRowMs features) run) network

        -- 40 = sqrt 1600 czyli liczby cech, 30 = sqrt 900
        frames' = (fmap . fmap) featuresToFrame . grouped 30 . toRows $ features'
        pixels' = joinFrames frames'

    printf "%.4f\n" $ last errors
    printf "%.4f\n" $ last psnrs
    void $ plot (PNG $ "compression-error.png") $
                Data2D [Title "Quantization error", Style Lines] [] $ zip [1..] errors -- $ takeNth 25 errors
    void $ plot (PNG $ "compression-psnrs.png") $
                Data2D [Title "Peak signal to noise ratio", Style Lines] [] $ zip [1..] psnrs -- $ takeNth 25 psnrs

    let img' = snd $ generateFoldImage imageFromLists pixels' w h
    savePngImage "smieszna-kompresja.png" $ ImageRGB8 img'
--    savePngImage "smieszna-kompresja.png" $ ImageY8 img'

    where
        frameToFeatures :: (KnownNat n) => Frame -> R n
        frameToFeatures xss = vector $ do
            xs <- xss
            PixelRGB8 r g b <- xs
            fmap fromIntegral [r, g, b]
--            fmap fromIntegral xs

        featuresToFrame :: (KnownNat n) => R n -> Frame
        featuresToFrame ps = grouped (round . sqrt . fromIntegral $ size ps `div` 3) . fmap makePixel . grouped 3 . D.toList . unwrap $ ps
            where
                makePixel [r, g, b] = PixelRGB8 (round r) (round g) (round b)
--        featuresToFrame :: (KnownNat n) => R n -> Frame
--        featuresToFrame ps = grouped (round . sqrt . fromIntegral $ size ps) . fmap makePixel . grouped 1 . D.toList . unwrap $ ps
--            where
--                makePixel [v] = round v

        splitFrames :: Int -> Int -> [[a]] -> [[[[a]]]]
        splitFrames w h = fmap L.transpose . grouped h . fmap (grouped w)

        joinFrames :: [[[[a]]]] -> [[a]]
        joinFrames = fmap join . join . fmap L.transpose

        imageFromLists :: [[a]] -> Int -> Int -> ([[a]], a)
        imageFromLists ([]:xs) w h = imageFromLists xs w h
        imageFromLists ((hd:tl):xss) _ _ = ((tl:xss), hd)

parseCompression :: Parser (IO ())
parseCompression = solveCompression
    <$> option auto (short 'w' <> long "rateWinS" <> value 1.0 <> showDefault
                    <> help "Starting learning rate of the winner.")
    <*> option auto (short 'W' <> long "rateWinE" <> value 0.001 <> showDefault
                    <> help "Ending learning rate of the winner.")
    <*> option auto (short 'n' <> long "rateNbS" <> value 200.0 <> showDefault
                    <> help "Starting learning rate of the neighbourhood.")
    <*> option auto (short 'N' <> long "rateNbE" <> value 0.01 <> showDefault
                    <> help "Ending learning rate of the neighbourhood.")
    <*> option auto (short 'e' <> long "epochs" <> value 10 <> showDefault
                    <> help "Number of epochs of training the network will go through.")
    <*> option auto (short 'I' <> long "initRange" <> value 0.5 <> showDefault
                    <> help "The neurons will be initialized with values from (-I, I).")
