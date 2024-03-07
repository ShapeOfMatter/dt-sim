{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Random
  ( -- * Effect
    Random (..)

    -- * Actions
  , random
  , randomR

    -- * Interpretations
  , runRandom
  , runRandomIO

    -- * Helpers
  , attrit
  , bernoulli
  , distributed
  , manyOf
  , oneOf
  , randoms
  , randomRs
  , shuffle
  , weighted
  , weightedShuffle
  ) where

import           Control.Monad (filterM)
import           Data.List (genericReplicate, sortOn)
import           Data.List.NonEmpty as NonEmpty ((!!), NonEmpty((:|)))
import           GHC.Exts (fromList, IsList (Item), toList)
import           Numeric.Natural (Natural)
import           Polysemy
import           Polysemy.State
import qualified System.Random as R

------------------------------------------------------------------------------
-- | An effect capable of providing 'R.Random' values.
data Random m a where
  Random :: R.Uniform x => Random m x
  RandomR :: R.UniformRange x => (x, x) -> Random m x

makeSem_ ''Random

------------------------------------------------------------------------------
-- | Yield a value, randomly sampled from the uniform distribution over all values of the given type.
-- /e.g./ 'p <- random @Bool'
random :: forall x r.
          (R.Uniform x
          ,Member Random r) =>
          Sem r x

------------------------------------------------------------------------------
-- | Yield a value, randomly sampled from the uniform distribution over the given inclusive range.
-- /e.g./ 'p <- random @Int (-10, 10)'
randomR :: forall x r.
           (R.UniformRange x
           ,Member Random r) =>
           (x, x) -> Sem r x

------------------------------------------------------------------------------
-- | Run a 'Random' effect with an explicit 'R.RandomGen'.
runRandom
    :: forall q r a
     . R.RandomGen q
    => q
    -> Sem (Random ': r) a
    -> Sem r a
runRandom q = evalState q . reinterpret (\case
  Random -> do
    ~(a, q') <- gets @q R.uniform
    put q'
    pure a
  RandomR r -> do
    ~(a, q') <- gets @q $ R.uniformR r
    put q'
    pure a
                                       )
{-# INLINE runRandom #-}


------------------------------------------------------------------------------
-- | Run a 'Random' effect by using the 'IO' random generator.
runRandomIO :: Member (Embed IO) r => Sem (Random ': r) a -> Sem r a
runRandomIO m = do
  q <- embed @IO R.newStdGen
  runRandom q m
{-# INLINE runRandomIO #-}


------------------------------------------------------------------------------
-- | Pick (uniformly) randomly from a (finite) non-empty list.
oneOf :: forall a r.
         (Member Random r) =>
         NonEmpty a -> Sem r a
oneOf xs = do i <- randomR (0, length xs - 1)
              return $ xs NonEmpty.!! i

------------------------------------------------------------------------------
-- | Randomly choose 'n' items from 'xs', without replacement.
-- If 'n <= length xs', all of 'xs' (permuted) will be returned.
manyOf :: forall l r.
          (Member Random r,
           IsList l) =>
          Int -> l -> Sem r [Item l]
n `manyOf` xs = do take n <$> shuffle (toList xs)

------------------------------------------------------------------------------
-- | Form a subset of 'xs', where each element of 'xs' has a '1-p' chance of being included.
attrit :: forall l r.
          (Member Random r,
           IsList l) =>
          Double -> l -> Sem r [Item l]
p `attrit` xs = filterM (\_ -> bernoulli (1 - p)) $ toList xs

------------------------------------------------------------------------------
-- | A random permutation of 'xs'.
shuffle :: forall l r.
           (Member Random r,
            IsList l) =>
           l -> Sem r l
shuffle xs = do let lxs = toList xs
                ns <- randoms @Int $ length lxs
                return . fromList . fmap snd . sortOn fst $ ns `zip` lxs


------------------------------------------------------------------------------
-- | Pick randomly from a finite non-empty list, using weight annotations.
-- Behavior is undefined if all weights are zero.
weighted :: forall a r.
            (Member Random r) =>
            NonEmpty (Natural, a) -> Sem r a
weighted xs = case sum (fst <$> xs) of
                space | 0 < space -> consume xs <$> randomR (0, space - 1)
                      | otherwise -> error $ "Attempted to sample from a distribution who's weights can not be normalized: "
                                             ++ show ((() <$) <$> xs)

------------------------------------------------------------------------------
-- | Shuffle weighted values s.t. (e.g.) `Pr[ result_0 == v_i ] = w_i / Σ[w]` (and so on through the list).
-- Cribbed from http://utopia.duth.gr/~pefraimi/research/data/2007EncOfAlg.pdf ;
-- it'd be great to have the final paper on hand to be sure I'm using it correctly.
weightedShuffle :: forall w a r.
                   (Member Random r,
                    Floating w, Ord w, R.UniformRange w) =>
                   [(w, a)] -> Sem r [a]
weightedShuffle pairs = fmap snd . sortOn fst <$> sequence [ do u <- randomR(0, 1)
                                                                return(u ** (1 / w), v)
                                                             | (w, v) <- pairs
                                                             ]

------------------------------------------------------------------------------
-- | Pick randomly from a non-empty possibly-infinite list, using normalized weight annotations.
-- The requirement that all weights be 0-1 (inclusive) and that they sum to 1 is not checked!
distributed :: forall a w r.
               (RealFloat w
               ,R.UniformRange w
               ,Member Random r) =>
               NonEmpty (w, a) -> Sem r a
distributed xs = consume xs <$> randomR (0, 1)


consume :: (Num w, Ord w) => NonEmpty (w, a) -> w -> a
consume ((_, x) :| []) _ = x
consume ((weight, x) :| (x' : xs)) threshold | threshold < weight = x
                                             | otherwise          = consume (x' :| xs) (threshold - weight)

------------------------------------------------------------------------------
-- | Generate n random values.
randoms :: forall a i r.
          (Integral i,
           R.Uniform a,
           Member Random r) =>
          i -> Sem r [a]
randoms n = sequence . genericReplicate n $ random

------------------------------------------------------------------------------
-- | Generate n random values in a range.
randomRs :: forall a i r.
           (Integral i,
            R.UniformRange a,
            Member Random r) =>
           i -> (a, a) -> Sem r [a]
randomRs n r = sequence . genericReplicate n $ randomR r

------------------------------------------------------------------------------
-- | Yields 'True' with probability 'p'
bernoulli :: forall r.
             (Member Random r) =>
             Double -> Sem r Bool
bernoulli p = (< p) <$> randomR (0, 1)


