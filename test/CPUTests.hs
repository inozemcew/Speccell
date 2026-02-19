module CPUTests 
    ( cpuTestMemory
    , cpuTestLD_RR_nn
    , cpuTestLD_R_n
    , cpuTestINC_R
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import TestUtils

import Machine
import Machine.CPU
import Machine.CPUFlags
import Init(initMachine)

import System.Random
import Control.Monad
import Numeric
import qualified Data.ByteString as BS

cpuTestMemory :: [TestTree]
cpuTestMemory = [testCase "Memory" mkTestMemory]

mkTestMemory :: Assertion
mkTestMemory = do
    machine <- initMachine BS.empty
    bs <- take 65536 . randoms <$> newStdGen
    forM_ [0..0xffff] $ \i -> do
        let b = bs!! fromIntegral i
        writeMemory machine i b
    forM_ [0..0xffff] $ \i -> do
        b <- readMemory machine i
        let e = bs !!  fromIntegral i
            s = showHex i . showString ": " . showHex e . showString " - " . showHex b $""
        if i<0x4000 
           then
                assertEqual s 0 b 
           else
                assertEqual s e b  


cpuTestLD_RR_nn :: [TestTree]
cpuTestLD_RR_nn = map (\(o, g, s) -> testCase ("LD RR,nn" ++ s) $ mkTestLD_RR_nn o g s)
        [ (0x01, getBC, "BC")
        , (0x11, getDE, "DE")
        , (0x21, getHL, "HL")
        , (0x31, getSP, "SP")
        ] ++ 
        [ testCase "LD IX,nn" $ mkTestLD_XY_nn DDPrefix getIX "IX"
        , testCase "LD IY,nn" $ mkTestLD_XY_nn FDPrefix getIY "IY"
        ]

cpuTestLD_R_n :: [TestTree]
cpuTestLD_R_n = [testCase "LD R,n" $ mkTestLD_R_n o g s | (o, g, s) <-
        [ (0x06, cpuB, "B")
        , (0x0E, cpuC, "C")
        , (0x16, cpuD, "D")
        , (0x1E, cpuE, "E")
        , (0x26, cpuH, "H")
        , (0x2E, cpuL, "L")
        , (0x3E, cpuA, "A")
        ]
    ] ++
    [testCase "LD (HL),n " mkTestLD_HL_n]


cpuTestINC_R :: [TestTree]
cpuTestINC_R = [testCase "INC R" $ mkTestINC_r 0x04 cpuB "B"]

mkTestLD_RR_nn :: Byte -> (CPU -> Address) -> String -> Assertion
mkTestLD_RR_nn op getReg reg =  do
    ws <- replicateM 100 randomIO
    let rom = mkRom mkCommand [[op]] ws
    runMTest rom ws assertion
    where
        mkCommand w = let (h,l) = fromWord w in [l, h]
        assertion ms w = do
            let ((m, t), ms') = (head ms, tail ms)
            assertBool ("LD "++reg++",nn tacts /=10") (t==10)
            w @?= getReg (mCPU m)
            return ms'

mkTestLD_XY_nn :: OpcodePrefix -> (CPU -> Address) -> String -> Assertion
mkTestLD_XY_nn pfx getReg reg = do
    ws <- replicateM 100 randomIO
    let opfx = case pfx of
                DDPrefix -> 0xDD
                FDPrefix -> 0xFD
                _Other   -> 0x00
    let rom = mkRom mkCommand [[opfx, 0x21]] ws
    runMTest rom ws assertion
    where
        mkCommand w = let (h,l) = fromWord w in [l, h]
        assertion ms w = do
            let ((m, t), ms1) = (head ms, tail ms)
            assertBool "1.LD IX/Y,nn tacts /=4" (t==4)
            cpuOP (mCPU m) @=? pfx

            let ((m', t'), ms2) = (head ms1, tail ms1)
            assertBool "1.LD IX/Y,nn tacts /=10" (t'==10)
            w @?= getReg (mCPU m')
            return ms2


mkTestLD_R_n :: Byte -> (CPU -> Byte) -> String -> Assertion
mkTestLD_R_n op cpuReg reg = do
    bs <- replicateM 100 randomIO
    let rom = mkRom mkCommand [[op]] bs
    runMTest rom bs assertion
    where
        mkCommand b = [b]
        assertion ms b = do
            let ((m, t), ms') = (head ms, tail ms)
            assertBool ("LD "++reg++",nn tacts /=10") (t==10)
            b @?= cpuReg (mCPU m)
            return ms'

mkTestLD_HL_n :: Assertion
mkTestLD_HL_n = do
    bs <- replicateM 100 randomIO
    let rom = mkRomI [0x21,0x00,0x40] mkCommand [[0x23,0x36]] bs
    runMTestI 1 rom bs assertion
    where
        mkCommand b = [b]
        assertion ms b = do
            let (mt, ms') = splitAt 2 ms
            let [(m1,t1), (m2,t2)] = mt
            assertEqual "INC HL tacts /=6" 6 t1
            assertEqual "LD (HL),n tacts /=10" 10 t2
            let hl = getHL (mCPU m2)
            r <- readMemory m2 hl 
            print $ showString "PC="
                  . showHex (cpuPC $ mCPU m2) 
                  . showString ", HL=" . showHex hl
                  . showString ", (HL)=" . showHex r $ ""
            r @?= b
            return ms'


mkTestLD_IX_n :: Assertion
mkTestLD_IX_n = do
    bs <- replicateM 100 randomIO
    let rom = mkRomI [0xDD,0x21,0x00,0x41] mkCommand [[0xDD,0x23,0xDD,0x36]] bs
    runMTestI 2 rom bs assertion
    where
        mkCommand w = let (l,h) = fromWord w in [l,h]
        assertion ms w = do
            let (b, d) = fromWord w
            let (mt, ms') = splitAt 4 ms
            let [_, (m1,t1),_ , (m2,t2)] = mt
            assertEqual "INC IX tacts /=6" 6 t1
            assertEqual "LD (HL),n tacts /=10" 10 t2
            let ix = getIX (mCPU m2)+ fromIntegral d - if d>127 then 256 else 0
            r <- readMemory m2 ix 
            print $ showString "PC="
                  . showHex (cpuPC $ mCPU m2) 
                  . showString ", IX=" . showHex ix
                  . showString ", (IX)=" . showHex r $ ""
            r @?= b
            return ms'


mkTestINC_r :: Byte -> (CPU -> Byte) -> String -> Assertion
mkTestINC_r op cpuReg rName = do
    let rom = BS.pack $ replicate 300 op
    runMTest rom [1..300] assertion
    where
        assertion ms _ = do
            let ((m, t), ms') = (head ms, tail ms)
            let cpu = mCPU m
            --print $ showString "PC=" . showHex (cpuPC cpu) $""
            ----------------------
            print cpu
            opcode1 <- readMemory m (cpuPC cpu)
            opcode2 <- readMemory m (cpuPC cpu + 1)
            print $ showsByte opcode1 . showsByte opcode2 $ ""
            ----------------------

            assertEqual ("INC "++rName++" tacts /=4") 4 t 
            assertBool ("INC r Z flag error @"++rName++"==0") ((cpuReg cpu == 0) == isFlagZ cpu)
            assertBool "INC r S flag error @R<0" ((cpuReg cpu >= 0x80) == isFlagS cpu)

            return ms'

