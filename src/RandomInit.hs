{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

module RandomInit where

import Control.Monad hiding (msum)
import Control.Monad.State hiding (msum)
import Data.Proxy
import GHC.TypeLits
import Numeric.LinearAlgebra.Static
import System.Random as R
import System.Random.Shuffle as RS

import LinearUtils

splits :: (RandomGen g) => g -> [g]
splits g = let (g1, g2) = R.split g in g1 : splits g2

randomInit :: (RandomGen g) => Double -> g -> [Double]
randomInit e g = randomRs (-e, e) g

takeMatrix :: forall n1 n2 . (KnownNat n1, KnownNat n2) => State [Double] (L n1 n2)
takeMatrix = do
    let h = fromIntegral . natVal $ (Proxy :: Proxy n1) :: Int
        w = fromIntegral . natVal $ (Proxy :: Proxy n2) :: Int
    took <- state $ splitAt (h * w)
    return $ matrix took

takeSample :: forall g m1 m2 n . (RandomGen g, KnownNat m1, KnownNat m2, KnownNat n) => L m1 n -> g -> L m2 n
takeSample xs g =
    let m1 = fromIntegral . natVal $ (Proxy :: Proxy m1) :: Int
        m2 = fromIntegral . natVal $ (Proxy :: Proxy m2) :: Int
        rs = toRows xs
    in  fromRows $ take m2 $ RS.shuffle' rs m1 g

cut :: [Double] -> [Double] -> [Double]
cut ws ys = cutAll [ws] ys

cutAll :: [[Double]] -> [Double] -> [Double]
cutAll _ [] = []
cutAll wss xs = if all (flip testVec hs) wss then cutAll wss rest else hs ++ cutAll wss rest
    where
        dim = length (head wss) - 1
        (hs, rest) = splitAt dim xs

        product :: [Double] -> [Double] -> Double
        product (x:xs) (y:ys) = x * y + product xs ys
        product [] [] = 0.0

        testVec :: [Double] -> [Double] -> Bool
        testVec (w:ws) hs = product ws hs > w

cutEuclidean' :: Int -> [Double] -> Double -> [Double] -> [Double]
cutEuclidean' _ _ _ [] = []
cutEuclidean' dim ctr r xs = if dist ctr hs < r * r
                             then hs ++ cutEuclidean' dim ctr r rest
                             else cutEuclidean' dim ctr r rest
    where
        (hs, rest) = splitAt dim xs

        dist :: [Double] -> [Double] -> Double
        dist (x:xs) (y:ys) = let d = x - y in d * d + dist xs ys
        dist [] _ = 0.0

cutEuclidean :: [Double] -> Double -> [Double] -> [Double]
cutEuclidean _ _ [] = []
cutEuclidean ctr r xs = cutEuclidean' (length ctr) ctr r xs

cutCuboid :: [Double] -> [Double] -> [Double]
cutCuboid ds = composeAll $ fmap cut $ zipWith (:) ds (eye dim)
    where
        dim = length ds
        eye n = [[if i == j then 1.0 else 0.0 | i <- [1..n]] | j <- [1..n]]

        composeAll :: [a -> a] -> a -> a
        composeAll [] = id
        composeAll (f:fs) = composeAll fs . f


union :: Int -> [[Double] -> [Double]] -> [Double] -> [Double]
union dim cuts xs = if all (null . ($hs)) cuts
                    then rest
                    else hs ++ rest
    where
        (hs, ts) = splitAt dim xs
        rest = union dim cuts ts

translate :: [Double] -> [Double] -> [Double]
translate ws = zipWith (+) (cycle ws)

scale :: [Double] -> [Double] -> [Double]
scale ws = zipWith (*) (cycle ws)

takeTriangle :: forall m . (KnownNat m) => Double -> State [Double] (L m 2)
takeTriangle size = do
    let m = fromIntegral . natVal $ (Proxy :: Proxy m) :: Int
    modify $ cut [size, 0.0, -1.0]
    modify $ cut [size, 0.866, 0.5]
    modify $ cut [size, -0.866, 0.5]
    took <- state $ splitAt (m * 2)
    return $ matrix took

takeRect :: forall m . (KnownNat m) => Double -> Double -> State [Double] (L m 2)
takeRect w h = do
    let m = fromIntegral . natVal $ (Proxy :: Proxy m) :: Int
    modify $ cut [w/2.0, 1.0, 0.0]
    modify $ cut [w/2.0, -1.0, 0.0]
    modify $ cut [h/2.0, 0.0, 1.0]
    modify $ cut [h/2.0, 0.0, -1.0]
    took <- state $ splitAt (m * 2)
    return $ matrix took

takeCross :: forall m . (KnownNat m) => Double -> State [Double] (L m 2)
takeCross full = do
    let m = fromIntegral . natVal $ (Proxy :: Proxy m) :: Int
        above = [full, 0.0, 1.0]
        below = [full, 0.0, -1.0]
        right = [full, 1.0, 0.0]
        left  = [full, -1.0, 0.0]
    modify $ cutAll [above, right]
    modify $ cutAll [above, left]
    modify $ cutAll [below, right]
    modify $ cutAll [below, left]
    took <- state $ splitAt (m * 2)
    return $ matrix took

takeCircle :: forall m . (KnownNat m) => Double -> Double -> Double -> State [Double] (L m 2)
takeCircle x y r = do
    let m = fromIntegral . natVal $ (Proxy :: Proxy m) :: Int
    modify $ cutEuclidean [x, y] r
    took <- state $ splitAt (m * 2)
    return $ matrix took

takeThreeCircles :: forall m . (KnownNat m) => Double -> State [Double] (L m 2)
takeThreeCircles s = do
    let m = fromIntegral . natVal $ (Proxy :: Proxy m) :: Int
    modify $ union 2 [ cutEuclidean [s * 0.5, s * 0.9] (s * 0.1)
                     , cutEuclidean [s * 0.5, s * 0.5] (s * 0.15)
                     , cutEuclidean [s * 0.2, s * 0.2] (s * 0.2)
                     ]
    modify $ translate $ repeat (-0.5 * s)
    took <- state $ splitAt (m * 2)
    return $ matrix took

takeWeird :: forall m . (KnownNat m) => Double -> State [Double] (L m 2)
takeWeird s = do
    let m = fromIntegral . natVal $ (Proxy :: Proxy m) :: Int
    modify $ union 2 [ cutEuclidean [0.25 * s, 0.20 * s] (s * 0.2)
                     , cutEuclidean [-0.25 * s, 0.20 * s] (s * 0.2)
                     , ( cut [s * 0.2, 1.0, 0.0]
                       . cut [s * 0.2, -1.0, 0.0]
                       . cut [s * 0.8, 0.0, 1.0]
                       . cut [0.0, 0.0, -1.0]
                       )
                     , cutEuclidean [0.0,  0.8 * s] (s * 0.1)
                     ]
    modify $ translate [0.0, -0.5 * s]
    took <- state $ splitAt (m * 2)
    return $ matrix took

takeEuclidean :: forall m d . (KnownNat m, KnownNat d) => [Double] -> Double -> State [Double] (L m d)
takeEuclidean ds r = do
    let m = fromIntegral . natVal $ (Proxy :: Proxy m) :: Int
        dim = fromIntegral . natVal $ (Proxy :: Proxy d) :: Int
    modify $ cutEuclidean ds r
    took <- state $ splitAt (m * dim)
    return $ matrix took

takeCuboid :: forall m d . (KnownNat m, KnownNat d) => [Double] -> State [Double] (L m d)
takeCuboid ds = do
    let m = fromIntegral . natVal $ (Proxy :: Proxy m) :: Int
        dim = fromIntegral . natVal $ (Proxy :: Proxy d) :: Int
    modify $ cutCuboid ds
    took <- state $ splitAt (m * dim)
    return $ matrix took

takeVeryWeird :: forall m . (KnownNat m) => Double -> State [Double] (L m 3)
takeVeryWeird s = do
    let m = fromIntegral . natVal $ (Proxy :: Proxy m) :: Int
    modify $ union 3 [ cutEuclidean' 3 [0.0, 0.0] (0.2 * s) . cutCuboid [0.4 * s, 0.4 * s, 0.8 * s]
                     , cutEuclidean [0.25 * s, 0.0, -0.25 * s] (s * 0.15)
                     , cutEuclidean [-0.25 * s, 0.0, -0.25 * s] (s * 0.15)
                     , cutEuclidean [0.0, 0.0, 0.4 * s] (s * 0.2)
                     ]
    took <- state $ splitAt (m * 3)
    return $ matrix took
