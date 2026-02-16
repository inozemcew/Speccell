-- CPU.hs
module Machine.CPU
    ( Address
    , Word
    , Byte
    , CPU(..)
    , OpcodePrefix(..)
    , setAF
    , setBC
    , setDE
    , setHL
    , setSP
    , setIX
    , setIY
    , setHL_IX_IY
    , getBC
    , getDE
    , getHL
    , getSP
    , getIX
    , getIY
    , getHL_IX_IY
    , getIX_Off
    , getIY_Off
    , getReg8
    , setReg8
    , toWord
    , fromWord
    , plusOffset
    ) where

import Prelude hiding (Word)
import Data.Word(Word8, Word16)
import Data.Bits
import Numeric(showHex)

type Address = Word16
type Word    = Word16
type Byte    = Word8

data OpcodePrefix = NoPrefix | CBPrefix | DDPrefix | EDPrefix | FDPrefix
    deriving (Eq, Show)

data CPU = CPU
    { cpuPC :: !Address
    , cpuSP :: !Address
    , cpuA  :: !Byte
    , cpuF  :: !Byte
    , cpuB  :: !Byte
    , cpuC  :: !Byte
    , cpuD  :: !Byte
    , cpuE  :: !Byte
    , cpuH  :: !Byte
    , cpuL  :: !Byte
    , cpuA' :: !Byte
    , cpuF' :: !Byte
    , cpuB' :: !Byte
    , cpuC' :: !Byte
    , cpuD' :: !Byte
    , cpuE' :: !Byte
    , cpuH' :: !Byte
    , cpuL' :: !Byte
    , cpuXH :: !Byte
    , cpuXL :: !Byte
    , cpuYH :: !Byte
    , cpuYL :: !Byte
    , cpuI  :: !Byte
    , cpuR  :: !Byte
-- internal
    , cpuOP :: !OpcodePrefix
    , cpuOP2:: !OpcodePrefix
    }

instance Show CPU where
    showsPrec _ cpu =
        showsReg " PC=" (cpuPC cpu) .
        showsReg " SP=" (getSP cpu) .
        showsReg " BC=" (getBC cpu) .
        showsReg " DE=" (getDE cpu) .
        showsReg " HL=" (getHL cpu) .
        showsReg " AF=" (getAF cpu) .
        showsReg " IX=" (getIX cpu) .
        showsReg " IY=" (getIY cpu)
        where
            showsReg s w = showString s . showHex w




setBC :: CPU -> Word-> CPU
setBC m w = let (b,c) = fromWord w in  m{cpuB = b, cpuC = c }

getBC :: CPU -> Word
getBC cpu = toWord (cpuB cpu) (cpuC cpu)


setDE :: CPU -> Word -> CPU
setDE m w = let (d,e) = fromWord w in  m{cpuD = d, cpuE = e }

getDE :: CPU -> Word
getDE cpu = toWord (cpuD cpu) (cpuE cpu)


setHL :: CPU -> Word -> CPU
setHL m w = let (h,l) = fromWord w in  m{cpuH = h, cpuL = l }

getHL :: CPU -> Word
getHL cpu = toWord (cpuH cpu) (cpuL cpu)


setAF :: CPU -> Word -> CPU
setAF cpu w = let (a, f) = fromWord w in cpu{cpuA = a, cpuF =f}

getAF :: CPU -> Word
getAF cpu = toWord (cpuA cpu) (cpuF cpu)

setSP :: CPU -> Word -> CPU
setSP m w = m { cpuSP = w }

getSP :: CPU -> Word
getSP cpu = cpuSP cpu

setIX :: CPU -> Word -> CPU
setIX m w = let (h,l) = fromWord w in  m{cpuXH = h, cpuXL = l }

getIX :: CPU -> Word
getIX cpu = toWord (cpuXH cpu) (cpuXL cpu)


setIY :: CPU -> Word -> CPU
setIY m w = let (h,l) = fromWord w in  m{cpuYH = h, cpuYL = l }

getIY :: CPU -> Word
getIY cpu = toWord (cpuYH cpu) (cpuYL cpu)


setHL_IX_IY :: CPU -> Word -> CPU
setHL_IX_IY cpu  
    | op == DDPrefix = setIX cpu
    | op == FDPrefix = setIY cpu
    | otherwise      = setHL cpu 
    where
        op = cpuOP cpu

getHL_IX_IY :: CPU -> Word
getHL_IX_IY cpu
    | op == DDPrefix = getIX cpu
    | op == FDPrefix = getIY cpu
    | otherwise      = getHL cpu 
    where
        op = cpuOP cpu


getIX_Off :: CPU -> Byte -> Address
getIX_Off cpu o = plusOffset (getIX cpu) o

getIY_Off :: CPU -> Byte -> Address
getIY_Off cpu o = plusOffset (getIY cpu) o


getReg8 :: CPU -> OpcodePrefix -> Byte -> Byte
getReg8 cpu DDPrefix 4 = cpuXH cpu
getReg8 cpu DDPrefix 5 = cpuXL cpu
getReg8 cpu FDPrefix 4 = cpuYH cpu
getReg8 cpu FDPrefix 5 = cpuYL cpu
getReg8 cpu _        n = case n of
                              0 -> cpuB cpu
                              1 -> cpuC cpu
                              2 -> cpuD cpu
                              3 -> cpuE cpu
                              4 -> cpuH cpu
                              5 -> cpuL cpu
                              7 -> cpuA cpu
                              _ -> error "getReg8: invalid reg number >7"


setReg8 :: CPU -> OpcodePrefix -> Byte -> Byte -> CPU
setReg8 cpu DDPrefix 4 b = cpu{cpuXH = b}
setReg8 cpu DDPrefix 5 b = cpu{cpuXL = b}
setReg8 cpu FDPrefix 4 b = cpu{cpuYH = b}
setReg8 cpu FDPrefix 5 b = cpu{cpuYL = b}
setReg8 cpu _        n b = case n of
                                0 -> cpu{cpuB = b}
                                1 -> cpu{cpuC = b}
                                2 -> cpu{cpuD = b}
                                3 -> cpu{cpuE = b}
                                4 -> cpu{cpuH = b}
                                5 -> cpu{cpuL = b}
                                7 -> cpu{cpuA = b}
                                _ -> error "getReg8: invalid reg number >7"



{-# INLINE plusOffset #-}
plusOffset :: Address -> Byte -> Address
plusOffset a o = a + fromIntegral o - if o > 127 then 256 else 0


{-# INLINE toWord #-}
toWord :: Byte -> Byte -> Address
toWord h l = (fromIntegral h `shiftL` 8) .|. fromIntegral l

{-# INLINE fromWord #-}
fromWord :: Address -> (Byte, Byte)
fromWord w = (fromIntegral (w `shiftR` 8), fromIntegral (w .&. 0xFF))
