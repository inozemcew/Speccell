-- Frame.hs
module Frame
    ( emulateFrame
    ) where

import Machine
    ( Machine(..)
    , Output(..)
    )
import Machine.Run(cpuStep)
import Video

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

tStatesPerLine :: Int
tStatesPerLine = 224

linesPerFrame :: Int
linesPerFrame = 312

tStatesPerFrame :: Int
tStatesPerFrame = tStatesPerLine * linesPerFrame   -- PAL

emulateFrame :: Machine -> Output -> IO Machine
emulateFrame machine out = do
  (m,_,_) <- liftIO $ execStateT (replicateM_ linesPerFrame $ emulateScanline out) (machine, 0, 0)
  return $ m
      { mFrameCnt = mFrameCnt m +1
      }


emulateScanline :: Output -> StateT (Machine, Int, Int) IO ()
emulateScanline out = do
  (m0, t0, l0) <- get
  (m, t, l) <- go m0 t0 l0
  put (m, t, l)
  renderScanline l0 m0 out
    where
      go m t l = if  t >= tStatesPerLine * (l+1)
                then do
                    return $ (m, t, l + 1)
                else do
                    (m', dt) <- cpuStep m
                    go m' (t+dt) l
