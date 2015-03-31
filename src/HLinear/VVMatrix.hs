module HLinear.VVMatrix
  ( VVMatrix
  , SizedVVMatrix

  , fromSized
  , toSized

  , nmbRows
  , nmbCols

  , (!)
  , (!?)

  , transpose

  , toVectors
  , fromVectors
  , fromVectors'

  , toLists
  , fromLists
  , fromLists'

  , zeroMatrix
  , identityMatrix
  , diagonalMatrix

  , zeroMatrix'
  , oneMatrix'

  , forceVV
  , forceVVMay
  , forceSize
  , forceSizeMay
  )

where

import HLinear.VVMatrix.Algebra
import HLinear.VVMatrix.Basic
import HLinear.VVMatrix.Creation
import HLinear.VVMatrix.Definition

import HLinear.VVMatrix.NFData

import HLinear.VVMatrix.QuickCheck
import HLinear.VVMatrix.SmallCheck

