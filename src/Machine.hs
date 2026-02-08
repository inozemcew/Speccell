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
    , readMemoryW
    , writeMemory
    , writeMemoryW
    ) where

import Machine.CPU
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Storable.Mutable as SM
import Data.Word
import Data.Bits


type Memory       = M.IOVector Byte
type Ports        = M.IOVector Byte
type FrameBuffer  = SM.IOVector Word32


data Machine = NewMachine
    { mCPU        :: !CPU
    , mMemory     :: !Memory
    , mPorts      :: !Ports
    , mFrameCnt   :: !Int -- global frame count
    , mTStates    :: !Int -- tstates from frame start
    }

data Output = NewOutput
    { oFrameBuffer :: !FrameBuffer
--  , oAudioBuffer ::
    }


readMemory :: Machine -> Address -> IO Byte
readMemory m addr = M.read (mMemory m) (fromIntegral addr)

readMemoryW :: Machine -> Address -> IO Address
readMemoryW m addr = do
    l <- readMemory m addr
    h <- readMemory m (addr+1)
    return $ toWord h l

writeMemory :: Machine -> Address -> Byte -> IO ()
writeMemory m addr b
  | addr > 0x4000 = M.write (mMemory m) (fromIntegral addr) b
  | otherwise = pure ()

writeMemoryW :: Machine -> Address -> Address -> IO ()
writeMemoryW m addr w = do
    let (h, l) = fromWord w
    writeMemory m addr l 
    writeMemory m (addr+1) h

readPort :: Machine -> Address -> IO Byte
readPort m p = M.read (mPorts m) (fromIntegral p)

writePort :: Machine -> Address -> Byte -> IO ()
writePort m p v = M.write (mPorts m) (fromIntegral p) v

