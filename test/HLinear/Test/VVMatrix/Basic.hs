module HLinear.Test.VVMatrix.Basic
where

import Prelude hiding ( (+) )
import Control.Applicative ( (<$>), (<*>), (<|>) )
import Data.Maybe
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ()
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty

import HLinear.Test.Utils

import HLinear.VVMatrix


properties :: TestTree
properties = testGroup "Basic properties"
  [ testProperty "zeroMatrix' ! ix ! jx == 0" $
      \ix jx nrs ncs->
        0 ==
        (zeroMatrix' (Just $ ix+1+nrs) (Just $ jx+1+ncs) :: VVMatrix Int)
        ! fromIntegral ix V.! fromIntegral jx

  , testProperty "zeroMatrix' !? ix == Nothing" $
      \ix -> isNothing
             ( (zeroMatrix' Nothing Nothing :: VVMatrix Int) !? ix )
  , testProperty "zeroMatrix' !? ix == Nothing" $
      \ix -> isNothing
             ( (zeroMatrix' (Just ix) Nothing :: VVMatrix Int)
               !? fromIntegral ix )

  , testPropertyVVMatrix "m == m" $ \m -> (m :: VVMatrix Int) == m
  , testPropertyVVMatrix "m == forceVV m" $
      \m -> (m :: VVMatrix Int) == fromMaybe m (forceVVMay m)

  , testPropertyVVMatrix "m == forceSize m" $
      \m nrs ncs -> (m :: VVMatrix Int) ==
                    ( let nrs' = fromMaybe nrs $ nmbRows m
                          ncs' = fromMaybe ncs $ nmbCols m
                      in forceSize nrs' ncs' m )

  , testPropertyVVMatrix "transpose . transpose = id" $
      \m -> (m :: VVMatrix Int) == transpose (transpose m)
  , testPropertyVVMatrix "transpose . forceVV . transpose == id" $
      \m -> forceVVMay (m :: VVMatrix Int) ==
            ( transpose <$> forceVVMay (transpose m) )
  ]