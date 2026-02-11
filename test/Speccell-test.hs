module Main where

import Test.Tasty
import Test.Tasty.HUnit

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
tests = testGroup "tests" $
        [ testCase "testing BC" $ mkTest01 0x01 getBC "BC"
        , testCase "testing DE" $ mkTest01 0x11 getDE "DE"
        ]

mkTest01 :: Byte -> (CPU -> Address) -> String -> Assertion
mkTest01 op getReg reg =  do
    ws <- replicateM 1024 randomIO
    let rom = concat [ op:p | p <- [[l,h] | w<- ws, let (h,l) = fromWord w]]
    machine <- ((initMachine $ BS.pack rom)::IO Machine)
    evalStateT (forM_ ws (doStep01 getReg reg)) machine

doStep01 :: (CPU -> Address) -> String -> Address -> StateT Machine IO ()
doStep01 getReg reg w = do
    m <- get
    (m', t) <- cpuStep m
    put m'
    lift $ assertBool ("LD "++reg++",nn tacts /=10") (t==10)
    lift $ w @?= getReg (mCPU m')
