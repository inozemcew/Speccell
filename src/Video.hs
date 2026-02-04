-- Video.hs
module Video
    ( renderScanline
    ) where

import Machine
import Init(screenW, screenH)
import Data.Bits
import qualified Data.Vector.Storable.Mutable as SM
import Control.Monad
import Control.Monad.IO.Class

renderScanline
    :: (MonadIO m) => Int
    -> Machine
    -> Output
    -> m ()           -- RGBA пиксели
renderScanline y m o = do
  -- пока заглушка
    let f = mFrameCnt m `mod` 256
        p = 0xFF000000 .|. (f `shiftL` 16) .|. (y `shiftL` 8)
        base = y * screenW
        fb = oFrameBuffer o

    forM_ [0..screenW-1] $ \x -> do
        let pixel = fromIntegral $ p .|. x
        when (y<screenH) $ liftIO $ SM.write fb (base + x) pixel
