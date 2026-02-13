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
tests = testGroup "tests" $ map (\(o, g, s) -> testCase ("testing " ++ s) $ mkTest01 o g s)
        [ (0x01, getBC, "BC")
        , (0x11, getDE, "DE")
        , (0x21, getHL, "HL")
        , (0x31, getSP, "SP")
        ]
    where
        mkTest01 :: Byte -> (CPU -> Address) -> String -> Assertion
        mkTest01 op getReg reg =  do
            ws <- replicateM 1024 randomIO
            let rom = mkRom mkCommand [[op]] ws
            runMTest rom ws assertion
            where
                mkCommand w = let (h,l) = fromWord w in [l, h]
                assertion ms w = do
                    let ((m, t), ms') = (head ms,tail ms)
                    assertBool ("LD "++reg++",nn tacts /=10") (t==10)
                    w @?= getReg (mCPU m)
                    return ms'

mkRom :: Integral a => (a -> [Byte]) -> [[Byte]] -> [a] -> BS.ByteString
mkRom mkOperand opcodes values = BS.pack $ concat [opcode ++ operand 
    | (opcode, operand) <- zip (cycle opcodes) (map mkOperand values) 
    ]

runMTest :: Integral a => BS.ByteString -> [a] -> ([(Machine, Int)] -> a -> IO [(Machine, Int)]) -> Assertion
runMTest rom vs assertion = do
    machine <- initMachine rom
    machines <- evalStateT run machine
    test machines vs
    where 
        run = replicateM (length vs) $ do
            m <- get
            (m', t) <- cpuStep m
            put m'
            return (m',t)
        test [] [] = return ()
        test _  [] = assertFailure "Test sequence is longer than values list"
        test [] _  = assertFailure "Test sequence is shorter than values list"
        test ms (v:vs) = do 
            ms' <- assertion ms v
            test ms' vs
