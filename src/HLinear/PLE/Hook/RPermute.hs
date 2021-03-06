{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  #-}

module HLinear.PLE.Hook.RPermute
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.DeepSeq ( NFData(..) )
import qualified Data.Vector as V
import Data.Vector ( Vector(..) )
import Data.Permute ( Permute )
import qualified Data.Permute as P
import Math.Structure

import HLinear.Matrix
import HLinear.PLE.Hook.PLMatrix


instance NFData Permute where
  rnf p = seq p ()

-- right permutations, i.e. action on vectors with indices ... 3 2 1
-- instead of 1 2 3 ...
newtype RPermute = RPermute Permute
  deriving ( Show, NFData )

instance Eq RPermute where
  p == p' = compare p p' == EQ

instance Ord RPermute where
  compare rp@(RPermute p) rp'@(RPermute p')
    | V.null cmps = EQ
    | otherwise   = V.head cmps
    where
      np = P.size p
      np' = P.size p'
      maxnp = max np np'
      cmps = V.dropWhile (==EQ) $ V.zipWith compare cp cp'
      cp = toVectorSize maxnp rp :: Vector Int
      cp' = toVectorSize maxnp rp' :: Vector Int

rpermute :: Int -> RPermute
rpermute n = RPermute $ P.permute n

fromTransposition :: Int -> (Int,Int) -> RPermute
fromTransposition n (a,b) =
  RPermute $ P.swapsPermute n [(n-1 - a, n-1 - b)]

toPermute :: RPermute -> Permute
toPermute (RPermute p) =
  P.swapsPermute n [(n-a,n-b) | (a,b) <- P.swaps p]
    where
    n = P.size p

size :: RPermute -> Int
size (RPermute p) = P.size p

at :: RPermute -> Int -> Int -> Int
at (RPermute p) n ix = n-1 - P.at p (n-1 - ix)


-- todo: check for Permute.at 
toMatrix :: Ring a => RPermute -> Matrix a
toMatrix p = Matrix np np $
                V.generate npZ $ \ix ->
                V.generate npZ $ \jx ->
                  if at p npZ jx == ix then one else zero
  where
    npZ = size p
    np = fromIntegral npZ 

toVector :: Num a => RPermute -> Vector a
toVector (RPermute p) = V.map (fromIntegral . P.at p) ( V.enumFromStepN (pred np) (-1) np )
  where
  np = P.size p 

toVectorSize :: Num a => Int -> RPermute -> Vector a
toVectorSize maxnp rp@(RPermute p) =
  V.map fromIntegral $ V.enumFromStepN (maxnp-1) (-1) (maxnp - np)
  V.++
  toVector rp
    where
    np = P.size p 

-- product structure

instance MultiplicativeMagma RPermute where
  rp@(RPermute p) * rp'@(RPermute p') =
    RPermute $ P.swapsPermute n $ P.swaps p ++ P.swaps p'
      where
      n = max (size rp) (size rp')

instance MultiplicativeSemigroup RPermute

instance MultiplicativeMonoid RPermute where
  one = rpermute 0

instance DecidableOne RPermute where
  isOne rp@(RPermute p) =  V.all (\ix -> p `P.at` ix == ix)
                                 (V.enumFromTo 0 (size rp-1))
                                         
instance MultiplicativeGroup RPermute where
  recip (RPermute p) = RPermute $ P.inverse p

-- action on vectors

newtype RPVector a = RPVector {fromRPVector :: Vector a}

-- this is partially defined (i.e. for internal use only)
instance
  MultiplicativeSemigroupLeftAction RPermute (PLVector a)
  where
  rp@(RPermute p) *. (PLVector v) = PLVector $ V.backpermute v vp'
    where
      nv = V.length v
      np = size rp
      pi = P.inverse p
      vp = V.generate np $ \ix -> nv-1 - (pi `P.at` (np-1 - ix))
      vp' = case compare nv np of
              EQ -> vp
              GT -> V.enumFromN 0 (nv-np) V.++ vp
              LT -> error "RPermute *. lVector: permutation to large"

instance MultiplicativeLeftAction RPermute (PLVector a)


instance MultiplicativeSemigroupLeftAction RPermute (PLMatrix a) where
  p *. PLMatrix (Matrix nrs ncs rs) = PLMatrix $
    Matrix nrs ncs $ fromPLVector $ p *. PLVector rs

instance MultiplicativeLeftAction RPermute (PLMatrix a)

