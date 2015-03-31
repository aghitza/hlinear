{-# LANGUAGE
    DataKinds
  #-}

module Main
where

import Control.Monad.Logic
import Control.Monad.Reader
import qualified Data.Vector.Fusion.Stream as VS
import Data.Functor.Identity


import Prelude hiding ( (+) )
import qualified Control.DeepSeq as DS
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector ( Vector )
import Math.Structure
import Test.SmallCheck.Series
import Test.Tasty
import qualified Test.Tasty.Runners as TR
import qualified Test.Tasty.SmallCheck as SC

import HLinear.VVMatrix

-- main :: IO ()
-- main = do
--   res <- fromJust $ TR.tryIngredients defaultIngredients mempty testPerf
--   DS.deepseq res $ return ()
-- 
-- testPerf = SC.testProperty "Performance" $ SC.changeDepth (const 5) $ \m m' ->
--            DS.deepseq ((m::SizedVVMatrix 2 2 Int) + (m'::SizedVVMatrix 2 2 Int)) True

main :: IO ()
main = case msadd of
         Left s -> print s
         Right _ -> print "OK"
   where
   msadd = (`V.mapM_` ms) $ \m ->
           (`V.mapM_` ms) $ \m' ->
           (`V.mapM_` ms) $ \m'' ->
             if m+(m'+m'') == (m+m')+m''
             then Right ()
             else Left $ "Error at " ++ show m ++ " " ++ show m' ++ " " ++ show m''
   ms = applicative 1 series :: Vector (SizedVVMatrix 2 2 Int)

-- main :: IO ()
-- main = msadd >> print $ "Length " ++ (show $ V.length ms)
--   where
--   msadd = (`V.mapM_` ms) $ \m -> (`V.mapM_` ms) $ \m' ->
--           DS.deepseq (m+m') (return ())
--   ms = applicative 7 series :: Vector (SizedVVMatrix 2 2 Int)
