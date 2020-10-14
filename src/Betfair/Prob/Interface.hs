{-# LANGUAGE ForeignFunctionInterface #-}

module Betfair.Prob.Interface
  ( Odds
  , createProbabilitiesResult
  , freeProbabilitiesResult
  , calculateOdds
  ) where

import           Prelude (IO, Int, fromIntegral, map, zip)

import           Control.Monad (return)

import           Data.Function (($))
import           Data.Ratio ((%))

import           Foreign.C.Types (CULong)
import           Foreign.Ptr (Ptr)
import           Foreign.Marshal.Array (peekArray)

import           Betfair.Model.Game (Odds(Odds))

foreign import ccall "createProbabilitiesResult" createProbabilitiesResult :: Int -> IO (Ptr CULong)

foreign import ccall "freeProbabilitiesResult" freeProbabilitiesResult :: Ptr CULong -> IO ()

foreign import ccall "getLengthOfProbabilities" getLengthOfProbabilities :: Int -> Int

foreign import ccall "calculateProbabilities" calculateProbabilities :: Ptr CULong -> Ptr CULong -> Int -> Int -> IO ()

calculateOdds :: Ptr CULong -> Ptr CULong -> Int -> Int -> IO [Odds]
calculateOdds numeratorsResult denominatorsResult size numberLower =
  do calculateProbabilities numeratorsResult denominatorsResult size numberLower
     let lengthOfProbabilities = getLengthOfProbabilities size
     numeratorsList <- peekArray lengthOfProbabilities numeratorsResult
     denominatorsList <- peekArray lengthOfProbabilities denominatorsResult
     return $ createOddsList numeratorsList denominatorsList

     where createOddsList numeratorsList denominatorsList =
             map createOdds $ zip numeratorsList denominatorsList

           createOdds (numerator, denominator) =
             Odds (fromIntegral denominator % fromIntegral numerator)
