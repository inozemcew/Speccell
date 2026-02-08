module Init
    ( initMachine
    , initOutput
    , screenW
    , screenH
    , SDLContext(..)
    , initSDL
    ) where

import Machine
import Machine.CPU
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Storable.Mutable as SM
import qualified Data.ByteString as BS
import qualified SDL
import Data.IORef(IORef, newIORef)
import Data.Word
import Data.Text(pack)
import Control.Monad(unless)

memSize, portCount :: Int
memSize   = 65536
portCount = 256

initCPU :: CPU
initCPU = CPU
    { cpuPC = 0x0000
    , cpuSP = 0xffff
    , cpuA  = 0x00
    , cpuF  = 0x00
    , cpuB  = 0x00
    , cpuC  = 0x00
    , cpuD  = 0x00
    , cpuE  = 0x00
    , cpuH  = 0x00
    , cpuL  = 0x00
    , cpuA' = 0x00
    , cpuF' = 0x00
    , cpuB' = 0x00
    , cpuC' = 0x00
    , cpuD' = 0x00
    , cpuE' = 0x00
    , cpuH' = 0x00
    , cpuL' = 0x00
    , cpuXH = 0x00
    , cpuXL = 0x00
    , cpuYH = 0x00
    , cpuYL = 0x00
    , cpuI  = 0x00
    , cpuR  = 0x00
-- internal
    , cpuOP = NoPrefix
}

initMachine :: BS.ByteString -> BS.ByteString -> IO Machine
initMachine rom scr = do
    memory  <- VM.replicate memSize 0
    ports   <- VM.replicate portCount 0
    unless (BS.null rom) $ loadRom memory rom
    unless (BS.null scr) $ loadScr memory scr
    return NewMachine
        { mCPU          = initCPU
        , mMemory       = memory
        , mPorts        = ports
        , mFrameCnt     = 0
        , mTStates      = 0
        }


initOutput :: IO Output
initOutput = do
    fb <- SM.replicate (screenW * screenH * 4) 0xff000000
    return $ NewOutput
        { oFrameBuffer = fb
        }


loadRom :: Memory -> BS.ByteString -> IO ()
loadRom mem rom = do
    let n = BS.length rom `min` VM.length mem `min` 16384
    mapM_ (\i -> VM.write mem i (BS.index rom i)) [0..n-1]


loadScr :: Memory -> BS.ByteString -> IO ()
loadScr mem scr = do
    let n = BS.length scr `min` VM.length mem `min` 0x1B00
    mapM_ (\i -> VM.write mem (i+0x4000) (BS.index scr i)) [0..n-1]



screenW, screenH :: Int
screenW = 256
screenH = 192

-- SDLContext.hs
data SDLContext = SDLContext
    { sdlWindow   :: SDL.Window
    , sdlRenderer :: SDL.Renderer
    , sdlTexture  :: SDL.Texture
    , sdlLastTick :: IORef Word32
    }

initSDL :: Int -> IO SDLContext
initSDL scale = do
    SDL.initialize [SDL.InitVideo, SDL.InitAudio]
    let winW = screenW * scale
        winH = screenH * scale
    window <- SDL.createWindow (pack "8-bit Emulator")
        SDL.defaultWindow
            { SDL.windowInitialSize =
                SDL.V2 (fromIntegral winW) (fromIntegral winH)
            }
    renderer <- SDL.createRenderer window (-1)
        SDL.RendererConfig
            { SDL.rendererType = SDL.AcceleratedRenderer
            , SDL.rendererTargetTexture = False
            --, SDL.rendererPresentVSync = False
            }
    SDL.rendererScale renderer SDL.$=
        SDL.V2 (fromIntegral scale) (fromIntegral scale)
    texture <- SDL.createTexture
        renderer
        SDL.ARGB8888
        SDL.TextureAccessStreaming
        (SDL.V2 (fromIntegral screenW) (fromIntegral screenH))
    tick <- SDL.ticks
    tickRef <- newIORef tick
    pure SDLContext
        { sdlWindow   = window
        , sdlRenderer = renderer
        , sdlTexture  = texture
        , sdlLastTick = tickRef
        }


