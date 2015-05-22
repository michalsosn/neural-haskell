module Feedback where

import Control.Arrow
import qualified Control.Category as C
import Control.Monad.State hiding (msum)

newtype Circuit a b = Circuit { runCircuit :: a -> (Circuit a b, b) }

instance C.Category Circuit where
    id = Circuit $ \a -> (C.id, a)
    (.) = dot
        where
            (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
                let (cir1', b) = cir1 a
                    (cir2', c) = cir2 b
                in  (cir2' `dot` cir1', c)

instance Arrow Circuit where
    arr f = Circuit $ \a -> (arr f, f a)
    first (Circuit cir) = Circuit $ \(a, b) ->
        let (cir', a') = cir a
        in  (first cir', (a', b))

instance ArrowChoice Circuit where
    left orig@(Circuit cir) = Circuit $ \ebd -> case ebd of
        Left b -> let (cir', c) = cir b
                  in  (left cir', Left c)
        Right d -> (left orig, Right d)

accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \a ->
    let (b, acc') = f a acc
    in  (accum acc' f, b)

accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\a b -> let b' = a `f` b in (b', b'))

total :: Num a => Circuit a a
total = accum' 0 (+)

logger :: (a -> b) -> Circuit a [b]
logger f = accum' [] (\a bs -> bs ++ [f a])

run :: a -> State (Circuit a b) b
run xs = do
    (cir', ys) <- gets (`runCircuit` xs)
    put cir'
    return ys

dryRun :: a -> State (Circuit a b) b
dryRun xs = do
    (_, ys) <- gets (`runCircuit` xs)
    return ys


newtype Feedback a b = Feedback { propagate :: a -> (b -> (Feedback a b, a), b) }

instance C.Category Feedback where
    id = Feedback $ \a -> (\b -> (C.id, b), a)
    (.) = dot
        where
            (Feedback fwd2) `dot` (Feedback fwd1) = Feedback $ \a ->
                let (bwd1, b) = fwd1 a
                    (bwd2, c) = fwd2 b
                    bwd' c = let (fb2', b') = bwd2 c
                                 (fb1', a') = bwd1 b'
                             in  (fb2' `dot` fb1', a')
                in  (bwd', c)

close :: Feedback a b -> Circuit (a, b -> b) b
close fb@(Feedback fwd) = Circuit $ \(a, ts) ->
    let (bwd, b) = fwd a
        fb' = (fst . bwd) (ts b)
    in (close fb', b)

train :: a -> (b -> b) -> State (Feedback a b) b
train xs ts = do
    (bwd, ys) <- gets (`propagate` xs)
    put $ (fst . bwd) (ts ys)
    return ys

predict :: a -> State (Feedback a b) b
predict xs = do
    (_, ys) <- gets (`propagate` xs)
    return ys
