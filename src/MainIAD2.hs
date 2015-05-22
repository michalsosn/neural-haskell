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

makeKohonen :: (KnownNat m) =>
               Double -> Double -> L m 3 ->
               State [Double] (Circuit (L 1 3) ([Double], [[Graph3D Double Double Double]]))
makeKohonen rateWin rateNb xss = do
    os <- takeMatrix
    return $ proc xs -> do
        os <- (kohonen (const rateWin) (const rateNb) os :: Network' 1 50 3) -< xs
        it <- total -< 1
        error <- logger (clusteringError xss) -< os
        plots <- logger plotNeurons -< (it, xs, os)
        returnA -< (error, plots)
    where
        plotNeurons :: (KnownNat m) => (Int, L 1 3, L m 3) -> [Graph3D Double Double Double]
        plotNeurons (it, xs, os) =
            [ Data3D [Title $ "Neurons " ++ show it, Style Points, Color Red  ] [] $ toPoints3D os
            , Data3D [Title $ "Input   " ++ show it, Style Points, Color Black] [] $ toPoints3D xs
            ]

solveKohonen :: IO ()
solveKohonen = do
    (gen1:gen2:_) <- fmap splits getStdGen

    let size = 5.0
--        shape = evalState (takeCross (size / 4.0)) (randomInit size gen1)       :: L 500 2
--        shape = evalState (takeCircle 0.5 0.5 1.5) (randomInit size gen1)       :: L 500 2
--        shape = evalState (takeThreeCircles size) (randomInit size gen1)        :: L 500 2
--        shape = evalState (takeWeird size) (randomInit size gen1)                :: L 500 2
--        shape = evalState (takeCuboid [1.0, 2.0, 5.0]) (randomInit size gen1)                :: L 2000 3
--        shape = evalState (takeEuclidean [1.0, 1.0, 0.0] 3.0) (randomInit size gen1)   :: L 1000 3
        shape = evalState (takeVeryWeird size) (randomInit size gen1)                :: L 1000 3

    let network = evalState (makeKohonen 0.1 0.01 shape) (randomInit (size / 10.0) gen2)
        (errors, nplots) = evalState (fmap (last . concat) $ replicateM 1 $ forM (toRowMs shape) run) network

    let dataPlot = Data3D [Title $ "Data", Style Points, Color Green] [] $ toPoints3D shape
        plots = takeNth 1000 $ fmap (dataPlot:) nplots

    void $ plot X11 $ Data2D [Title "Clustering error", Style Lines] [] $ zip [1..] errors
    plotAnimation "kohonen.gif" 3 plots

parseKohonen :: Parser (IO ())
parseKohonen = pure solveKohonen

makeNeuralGas :: (KnownNat m) =>
               Double -> Double -> L m 2 ->
               State [Double] (Circuit (L 1 2) ([Double], [[Graph2D Double Double]]))
makeNeuralGas rateWin rateNb xss = do
    os <- takeMatrix
    return $ proc xs -> do
        os <- (neuralGas (const rateWin) (const rateNb) os :: Network' 1 50 2) -< xs
        it <- total -< 1
        error <- logger (clusteringError xss) -< os
        plots <- logger plotNeurons -< (it, xs, os)
        returnA -< (error, plots)
    where
        plotNeurons :: (KnownNat m) => (Int, L 1 2, L m 2) -> [Graph2D Double Double]
        plotNeurons (it, xs, os) =
            [ Data2D [Title $ "Neurons " ++ show it, Style Points, Color Red  ] [] $ toPoints os
            , Data2D [Title $ "Input   " ++ show it, Style Points, Color Black] [] $ toPoints xs
            ]

solveNeuralGas :: IO ()
solveNeuralGas = do
    (gen1:gen2:_) <- fmap splits getStdGen

    let size = 2.0
        shape = evalState (takeRect size (0.5 * size)) (randomInit size gen1)     :: L 1000 2

    let network = evalState (makeNeuralGas 0.5 0.5 shape) (randomInit (size / 10.0) gen2)
        (errors, nplots) = evalState (fmap (last . concat) $ replicateM 10 $ forM (toRowMs shape) run) network

    let dataPlot = Data2D [Title $ "Data", Style Points, Color Green] [] $ toPoints shape
        plots = takeNth 25 $ fmap (dataPlot:) nplots

    void $ plot X11 $ Data2D [Title "Clustering error", Style Lines] [] $ zip [1..] errors
    plotAnimation "neuralGas.gif" 3 plots

parseNeuralGas :: Parser (IO ())
parseNeuralGas = pure solveNeuralGas

makeKMeans :: forall g m1 . (RandomGen g, KnownNat m1) =>
              L m1 2 -> g ->
              Circuit (L m1 2) ([Double], [[Graph2D Double Double]])
makeKMeans xss gen = proc xs -> do
        os <- (kMeans ios :: Network' m1 10 2) -< xs
        it <- total -< 1
        error <- logger (clusteringError xss) -< os
        plots <- logger plotNeurons -< (it, os)
        returnA -< (error, plots)
    where
        ios = takeSample xss gen

        plotNeurons :: (KnownNat m2) => (Int, L m2 2) -> [Graph2D Double Double]
        plotNeurons (it, os) =
            [ Data2D [Title $ "Neurons " ++ show it, Style Points, Color Red] [] $ toPoints os
            ]

solveKMeans :: IO ()
solveKMeans = do
    (gen1:gen2:_) <- fmap splits getStdGen

    let size = 2.0
--        shape = evalState (takeTriangle size) (randomInit (size * sqrt 3) gen1) :: L 1000 2
--        shape = evalState (takeRect size (0.5 * size)) (randomInit size gen1)   :: L 1000 2
        shape = evalState (takeCross (size / 4.0)) (randomInit size gen1)       :: L 100 2

    let network = makeKMeans shape gen2
        (errors, nplots) = evalState (fmap last $ replicateM 10 $ run shape) network

    let dataPlot = Data2D [Title $ "Data", Style Points, Color Green] [] $ toPoints shape
        plots = takeNth 1 $ fmap (dataPlot:) nplots

    void $ plot X11 $ Data2D [Title "Clustering error", Style Lines] [] $ zip [1..] errors
    plotAnimation "kmeans.gif" 10 plots

parseKMeans :: Parser (IO ())
parseKMeans = pure solveKMeans


makeCompression :: forall m n . (KnownNat m, KnownNat n) =>
                   Double -> Double -> L m n ->
                   State [Double] (Circuit (L 1 n) ([L m n], [Double]))
makeCompression rateWin rateNb xss = do
    os <- takeMatrix
    return $ proc xs -> do
        os <- (kohonen (const rateWin) (const rateNb) os :: Network' 1 100 n) -< xs
        nearest <- logger (compressNearest xss) -< os
        error   <- logger (clusteringError xss) -< os
        returnA -< (nearest, error)

type Frame = [[PixelRGB8]]

solveCompression :: IO ()
solveCompression = do
    gen <- getStdGen

    file <- B.readFile "smieszny-piesek.png"
    let Right png = decodePng file
        ImageRGB8 img = png  -- ImageY8
        w = imageWidth img
        h = imageHeight img

        pixels = [[pixelAt img x y | x <- [0..w-1]] | y <- [0..h-1]]
        frames = splitFrames 3 3 pixels                                   :: [[Frame]]
        features = fromRows . join . (fmap . fmap) frameToFeatures $ frames :: L 1600 27

    let network = evalState (makeCompression 0.1 0.01 features) (fmap (+50.0) $ randomInit (10.0) gen)
        (features', error) = evalState (fmap (last . concat) $ replicateM 10 $ forM (toRowMs features) run) network

        frames' = (fmap . fmap) featuresToFrame . grouped 40 . toRows $ last features'
        pixels' = joinFrames frames'

    let img' = snd $ generateFoldImage imageFromLists pixels' w h
    savePngImage "smieszna-kompresja.png" $ ImageRGB8 img'

    where
        frameToFeatures :: (KnownNat n) => Frame -> R n
        frameToFeatures xss = vector $ do
            xs <- xss
            PixelRGB8 r g b <- xs
            fmap fromIntegral [r, g, b]

        featuresToFrame :: (KnownNat n) => R n -> Frame
        featuresToFrame ps = grouped (round . sqrt . fromIntegral $ size ps `div` 3) . fmap makePixel . grouped 3 . D.toList . unwrap $ ps
            where
                makePixel [r, g, b] = PixelRGB8 (round r) (round g) (round b)

        splitFrames :: Int -> Int -> [[a]] -> [[[[a]]]]
        splitFrames w h = fmap L.transpose . grouped h . fmap (grouped w)

        joinFrames :: [[[[a]]]] -> [[a]]
        joinFrames = fmap join . join . fmap L.transpose

        imageFromLists :: [[a]] -> Int -> Int -> ([[a]], a)
        imageFromLists ([]:xs) w h = imageFromLists xs w h
        imageFromLists ((hd:tl):xss) _ _ = ((tl:xss), hd)

parseCompression :: Parser (IO ())
parseCompression = pure solveCompression
