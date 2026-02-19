module Main where

import Test.Tasty
import Test.Tasty.HUnit

import CPUTests

import Machine
import Machine.CPU
import Machine.Run
import Init

import qualified Data.ByteString as BS
import System.Random
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State




main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Cpu tests" 
    $ cpuTestMemory
    ++ cpuTestLD_RR_nn 
    ++ cpuTestLD_R_n
    ++ cpuTestINC_R
