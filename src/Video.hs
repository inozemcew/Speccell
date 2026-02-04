-- Video.hs
module Video
    ( renderScanline
    ) where

import Machine
import Init(screenW, screenH)
import Data.Word
import Data.Bits
import qualified Data.Vector.Storable.Mutable as SM
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.IO.Class

renderScanline
    :: (MonadIO m) => Int
    -> Machine
    -> Output
    -> m ()           -- RGBA пиксели
renderScanline y machine o = do
  -- пока заглушка
    let scrnBase = 0x4000
        attrBase = scrnBase + 0x1800

        y8 = y .&. 7
        yBlock = (y .&. 0x38) `shiftL` 2
        yThird = y `shiftR` 6

        rowAddr = scrnBase + (y8 `shiftL` 8) + yBlock  + (yThird `shiftL` 11)

        base = y * screenW
        fb = oFrameBuffer o
        memory = mMemory machine
        attrAddr = attrBase + yBlock + (yThird `shiftL` 8)

    forM_ [0..(screenW `shiftR` 3)-1] $ \xByte -> do

        let flashPhase = mFrameCnt machine .&. 32 == 0

        byte <- liftIO $ M.read memory (rowAddr + xByte)
        attr <- liftIO $ M.read memory (attrAddr + xByte)

        let ink    = fromIntegral (attr .&. 7)
            paper  = fromIntegral ((attr `shiftR` 3) .&. 7)
            bright = testBit attr 6
            flash  = testBit attr 7

            (i,p)  = applyFlash flash flashPhase (ink, paper)
            colInk   = palette bright i
            colPaper = palette bright p

        forM_ [0..7] $ \pBit -> do
            let pixel = if testBit byte (7 - pBit)
                then colInk
                else colPaper

            when (y<screenH) $ liftIO $ SM.write fb (base + xByte * 8 + pBit) pixel


applyFlash :: Bool -> Bool -> (Int, Int) -> (Int, Int)
applyFlash flash flashPhase (ink, paper)
  | flash && flashPhase = (paper, ink)
  | otherwise           = (ink, paper)


palette :: Bool -> Int -> Word32
palette bright c = if bright then paletteBright V.! c else paletteDark V.! c

paletteDark :: V.Vector Word32
paletteDark = V.map (0xC0C0C0 .&.) paletteBright

paletteBright :: V.Vector Word32
paletteBright = V.fromList $ map (0xFF000000 .|.) [0x000000, 0x0000FF, 0xFF0000, 0xFF00FF, 0x00FF00, 0x00FFFF, 0xFFFF00, 0xFFFFFF]
