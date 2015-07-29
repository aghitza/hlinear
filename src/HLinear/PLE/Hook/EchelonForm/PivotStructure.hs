{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module HLinear.PLE.Hook.EchelonForm.PivotStructure
where

import Control.DeepSeq ( NFData )
import Data.Sequence ( Seq )
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Math.Structure ( DecidableZero, isZero )

import HLinear.PLE.Hook.EchelonForm.Definition
import qualified HLinear.PLE.Hook.EchelonForm.Row as EFR


newtype PivotStructure = PivotStructure (Seq (Int,Int))
  deriving (Show, Eq, Ord, NFData)

pivotStructure
  :: DecidableZero a
  => EchelonForm a -> PivotStructure
pivotStructure (EchelonForm nrs ncs rs) = PivotStructure $ go S.empty rs 0 0
  where
    go s rs ix jx
      | V.null rs = s
      | otherwise = 
          case EFR.pivotIx' (V.head rs) ix of
            Nothing  -> s
            Just ix' -> go (s S.|> (ix',jx)) (V.tail rs) (succ ix') (succ jx)