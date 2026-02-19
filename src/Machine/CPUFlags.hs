module Machine.CPUFlags 
    ( updFlagsAdd
    , updFlagsInc
    , updFlagsSub
    , updFlagsDec
    , updFlagsAddW
    , isFlagZ
    , isFlagC
    , isFlagS

    ) where

import Prelude hiding (Word)
import Machine.CPU

import Data.Bits
import Control.Monad.State

cpuFlagS  :: Int
cpuFlagS  = 7
cpuFlagZ  :: Int
cpuFlagZ  = 6
cpuFlagH  :: Int
cpuFlagH  = 4
cpuFlagPV :: Int
cpuFlagPV = 2
cpuFlagN  :: Int
cpuFlagN  = 1
cpuFlagC  :: Int
cpuFlagC  = 0

isFlagZ :: CPU -> Bool
isFlagZ cpu = testBit (cpuF cpu) cpuFlagZ

isFlagC :: CPU -> Bool
isFlagC cpu = testBit (cpuF cpu) cpuFlagC

isFlagS :: CPU -> Bool
isFlagS cpu = testBit (cpuF cpu) cpuFlagS


updFlagsAdd :: CPU -> Byte -> Byte -> CPU
updFlagsAdd cpu oR nR = cpu{cpuF = f .|. c}
    where
        (f, c) = setFlagsAddInc oR nR

updFlagsInc :: CPU -> Byte -> Byte -> CPU
updFlagsInc cpu oR nR = cpu{cpuF = f .|. c}
    where
        (f, _) = setFlagsAddInc oR nR
        c = cpuF cpu .&. bit cpuFlagC

setFlagsAddInc :: Byte -> Byte -> (Byte, Byte)
setFlagsAddInc oR nR = (f, c)
    where 
        modBit b = modify' (`setBit` b)
        f = execState m zeroBits
        m = do
            when (nR > 127) $ modBit cpuFlagS
            when (nR == 0) $ modBit cpuFlagZ
            when (oR .&. 0x0F > nR .&. 0x0F) $ modBit cpuFlagH 
            when (oR `xor` 0x80 > nR `xor` 0x80) $ modBit cpuFlagPV
        c  = if oR > nR then bit cpuFlagC else zeroBits


updFlagsSub :: CPU -> Byte -> Byte -> CPU
updFlagsSub cpu oR nR = cpu{cpuF = f .|. c}
    where
        (f, c) = setFlagsSubDec oR nR

updFlagsDec :: CPU -> Byte -> Byte -> CPU
updFlagsDec cpu oR nR = cpu{cpuF = f .|. c}
    where
        (f, _) = setFlagsSubDec oR nR
        c = cpuF cpu .&. bit cpuFlagC

setFlagsSubDec :: Byte -> Byte -> (Byte, Byte)
setFlagsSubDec oR nR = (f, c)
    where 
        modBit b = modify' (`setBit` b)
        f = execState m $ bit cpuFlagN
        m = do
            when (nR > 127) $ modBit cpuFlagS
            when (nR == 0) $ modBit cpuFlagZ
            when (oR .&. 0x0F < nR .&. 0x0F) $ modBit cpuFlagH 
            when (oR `xor` 0x80 < nR `xor` 0x80) $ modBit cpuFlagPV
        c  = if oR < nR then bit cpuFlagC else zeroBits

updFlagsAddW :: CPU -> Word -> Word -> CPU
updFlagsAddW cpu r1 r2 = cpu{ cpuF = setFlagsAddW (cpuF cpu) r1 r2}

setFlagsAddW :: Byte -> Word -> Word -> Byte
setFlagsAddW f r1 r2 = if r2<r1 then setBit f cpuFlagC else clearBit f cpuFlagC
