-- Machine.hs

module Machine
    ( CPU(..)
    , Machine(..)
    , Address
    , Byte
    , Memory
    , Ports
    , Output(..)
    , FrameBuffer
    , readMemory
    , writeMemory
    ) where

import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Storable.Mutable as SM
import Data.Word


type Address = Word16
type Byte    = Word8


type Memory       = M.IOVector Byte
type Ports        = M.IOVector Byte
type FrameBuffer  = SM.IOVector Word32

data CPU = CPU
    { cpuPC :: !Word16
    , cpuSP :: !Word16
    -- регистры потом
    } deriving (Show)


data Machine = NewMachine
    { mCPU        :: !CPU
    , mMemory     :: !Memory
    , mPorts      :: !Ports
    , mFrameCnt   :: !Int -- global frame count
    , mTStates    :: !Int -- tstates from frame start
--    , mScanline   :: !Int -- current raster line
    }

data Output = NewOutput
    { oFrameBuffer :: !FrameBuffer
--  , oAudioBuffer ::
    }


readMemory :: Machine -> Address -> IO Byte
readMemory m addr = M.read (mMemory m) (fromIntegral addr)


writeMemory :: Machine -> Address -> Byte -> IO ()
writeMemory m addr b
  | addr > 0x4000 = M.write (mMemory m) (fromIntegral addr) b
  | otherwise = pure ()


readPort :: Machine -> Address -> IO Byte
readPort m p = M.read (mPorts m) (fromIntegral p)

writePort :: Machine -> Address -> Byte -> IO ()
writePort m p v = M.write (mPorts m) (fromIntegral p) v

