{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

module IOUtils where

import Control.Monad hiding (msum)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import qualified Data.List as L
import Data.Maybe
import GHC.TypeLits
import qualified Numeric.LinearAlgebra.Data as D
import Numeric.LinearAlgebra.Static

import LinearUtils

partitionWith :: (KnownNat m) => (Double -> Bool) -> L m 1 -> L m 2 -> ([(Double, Double)], [(Double, Double)])
partitionWith f ys xs = let ys' = concat $ toList ys
                            xs' = toPoints xs
                            (ps, ns) = L.partition (f . fst) (ys' `zip` xs')
                        in  (fmap snd ps, fmap snd ns)

takeNth :: Int -> [a] -> [a]
takeNth _ [] = []
takeNth n xs = head xs : (takeNth n $ drop n xs)

dropNth :: Int -> [a] -> [a]
dropNth _ [] = []
dropNth n xs = (take (n - 1) . drop 1) xs ++ (dropNth n $ drop n xs)

zipM :: (Monad m) => m a -> m b -> m (a, b)
zipM ma mb = do
     a <- ma
     b <- mb
     return $ (a, b)

zipM3 :: (Monad m) => m a -> m b -> m c -> m (a, b, c)
zipM3 ma mb mc = do
     a <- ma
     b <- mb
     c <- mc
     return $ (a, b, c)

grouped :: Int -> [a] -> [[a]]
grouped _ [] = []
grouped n xs = let (hd, tl) = splitAt n xs in hd : grouped n tl

readMatrix :: (KnownNat m, KnownNat n) => String -> IO (L m n)
readMatrix path = do
    lns <- fmap (fmap U.toString . U.lines) $ B.readFile path
    return $ matrix $ concatMap (fmap read . words) lns

separateYs :: forall m n1 n2 . (KnownNat m, KnownNat n1, KnownNat n2) => L m n1 -> (L m n2, L m 1)
separateYs xs =
    let (h, w) = size xs
        xs' = blockAt 1.0 0 0 (unwrap xs) :: L m n2
        ys = (fromJust . create . D.fromRows . fmap (D.fromList . (:[]) . last . D.toList . unwrap) . toRows) xs
    in (xs', ys)


