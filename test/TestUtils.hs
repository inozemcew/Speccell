module TestUtils 
    ( mkRom
    , mkRomI
    , runMTest
    , runMTestI
    ) where

import Test.Tasty.HUnit

import Machine
import Machine.CPU
import Machine.Run
import Init

import qualified Data.ByteString as BS
import Control.Monad.Trans
import Control.Monad.Trans.State

mkRom :: Integral a => (a -> [Byte]) -> [[Byte]] -> [a] -> BS.ByteString
mkRom = mkRomI []
mkRomI :: Integral a => [Byte] -> (a -> [Byte]) -> [[Byte]] -> [a] -> BS.ByteString
mkRomI startup mkOperand opcodes values = BS.pack $ startup ++ concat [opcode ++ operand 
    | (opcode, operand) <- zip (cycle opcodes) (map mkOperand values) 
    ]

runMTest :: Integral a => BS.ByteString -> [a] -> ([(Machine, Int)] -> a -> IO [(Machine, Int)]) -> Assertion
runMTest = runMTestI 0

runMTestI :: Integral a => Int -> BS.ByteString -> [a] -> ([(Machine, Int)] -> a -> IO [(Machine, Int)]) -> Assertion
runMTestI n rom vs assertion = do
    machine <- initMachine rom
    machines' <- evalStateT run machine
    let machines = drop n machines'
    test machines vs
    where 
        run = do
            m <- get
            r@(m', t) <- cpuStep m
            put m'
            if cpuPC (mCPU m') < fromIntegral(BS.length rom)
               then (r:) <$> run
               else return [r]

        test [] [] = return ()
        test _  [] = assertFailure "Test sequence is longer than values list"
        test [] _  = assertFailure "Test sequence is shorter than values list"
        test ms (v:vs) = do 
            ms' <- assertion ms v
            test ms' vs
