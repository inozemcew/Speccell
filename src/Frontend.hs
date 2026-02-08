-- Frontend.hs
module Frontend
    ( runEmulator
    ) where

import Config(Config(..))
import Init
import Machine(Machine, Output(..), FrameBuffer)
import Input(inpHostCommands,HostCommand(..), pollInput, applyInput)
import Frame(emulateFrame)
import Audio(playAudio)
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable.Mutable as SM
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import qualified SDL as SDL
import SDL.Raw.Event(setRelativeMouseMode)
import Data.IORef(readIORef, writeIORef)
import Data.Word
import Control.Monad(when, foldM, replicateM_)
import Control.Monad.Trans
import Control.Monad.Trans.State(evalStateT, get, put)


data FrontendState = FrontendState
  { feSDL       :: SDLContext
  , feMouseGrab :: Bool
  }


runEmulator :: Config -> IO ()
runEmulator cfg = do
    rom <- rdFile (cfgRomPath cfg)
    scr <- rdFile (cfgScrPath cfg)
    m0 <- initMachine rom scr
    print "Machine initialized"
    o0 <- initOutput
    print "Output initialized"

    sdl <- initSDL (cfgScale cfg)
    print "SDL initialized"
    loop (FrontendState sdl False) m0 o0
    where 
        rdFile f = if null f then return BS.empty else BS.readFile f


loop :: FrontendState -> Machine -> Output -> IO ()
loop st m o = do
  input <- pollInput

  mState <- handleHostCommands st (inpHostCommands input)

  case mState of
    Nothing ->
      shutdown (feSDL st)

    Just st' -> do
        let m1 = applyInput input m
        m2 <- emulateFrame m1 o

        let fb = oFrameBuffer o
        drawFrame (feSDL st') fb
        playAudio m2
        delayTo50Hz (feSDL st')

        loop st' m2 o

handleHostCommands :: FrontendState -> [HostCommand] -> IO (Maybe FrontendState)
handleHostCommands fest cmds =
    foldM apply (Just fest) cmds
    where
        apply :: Maybe FrontendState -> HostCommand -> IO (Maybe FrontendState)
        apply Nothing _ = pure Nothing

        apply (Just _) HostQuit =
            pure Nothing

        apply (Just st) HostToggleMouseGrab = do
            let oldGrab = not (feMouseGrab st)
            result <- setRelativeMouseMode $ not oldGrab  -- SDL.Raw.Event
            let newGrab = if result == 0 then oldGrab else feMouseGrab st
            pure $ Just st { feMouseGrab = newGrab }


shutdown :: SDLContext -> IO ()
shutdown ctx = do
    SDL.destroyTexture  (sdlTexture ctx)
    SDL.destroyRenderer (sdlRenderer ctx)
    SDL.destroyWindow   (sdlWindow ctx)
    SDL.quit



drawFrame :: SDLContext -> FrameBuffer -> IO ()
drawFrame ctx fb = do
    (rawPtr, pitch) <- SDL.lockTexture (sdlTexture ctx) Nothing

    let dst :: Ptr Word32
        dst = castPtr rawPtr

        rowBytes = screenW * 4
        pitchB   = fromIntegral pitch

    liftIO $ SM.unsafeWith fb $ \src -> evalStateT (replicateM_ screenH $ copyRow src dst pitchB rowBytes) 0

    SDL.unlockTexture (sdlTexture ctx)
    SDL.clear (sdlRenderer ctx)
    SDL.copy (sdlRenderer ctx) (sdlTexture ctx) Nothing Nothing
    SDL.present (sdlRenderer ctx)
        where
            copyRow src dst pitchB rowBytes = do
                y <- get
                liftIO $ copyBytes (dst `plusPtr` (y * pitchB)) (castPtr src `plusPtr` (y*rowBytes)) rowBytes
                put (y+1)


delayTo50Hz :: SDLContext -> IO ()
delayTo50Hz ctx = do
    let frameMs = 20  -- 1000 / 50
    now <- SDL.ticks
    prev <- readIORef (sdlLastTick ctx)
    let elapsed = now - prev

    when (elapsed < frameMs) $
        SDL.delay (frameMs - elapsed)

    now' <- SDL.ticks
    writeIORef (sdlLastTick ctx) now'
