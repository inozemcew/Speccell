-- Input.hs
module Input
    ( Input(..)
    , pollInput
    , applyInput
    , HostCommand(..)
    ) where

import Machine
import Data.Word
import SDL

-- Input.hs
data HostCommand
    = HostQuit
    | HostToggleMouseGrab
    deriving (Eq, Show)

data MouseButtons = MouseButtons
    { mouseLeft   :: Bool
    , mouseMiddle :: Bool
    , mouseRight  :: Bool
    } deriving (Eq, Show)

data Input = Input
    { inpKeyMask       :: Word8
    , inpMousePos      :: V2 Int
    , inpMouseButtons  :: MouseButtons
    , inpHostCommands  :: [HostCommand]
    } deriving (Eq, Show)


emptyInput :: Input
emptyInput = Input
    { inpKeyMask       = 0
    , inpMousePos      = V2 0 0
    , inpMouseButtons  = MouseButtons False False False
    , inpHostCommands  = []
    }


pollInput :: IO Input
pollInput = do
  events <- SDL.pollEvents
  pure $ foldl handleEvent emptyInput events



applyInput :: Input -> Machine -> Machine
applyInput i m =
  -- запись в порты
  m

handleEvent :: Input -> SDL.Event -> Input
handleEvent inp ev =
  case SDL.eventPayload ev of
    -- Закрытие окна
    SDL.QuitEvent ->
      inp { inpHostCommands = HostQuit : inpHostCommands inp }
    -- Ctrl + F12 → выход
    SDL.KeyboardEvent kbd
      | SDL.keyboardEventKeyMotion kbd == SDL.Pressed
      , let ks = SDL.keyboardEventKeysym kbd
      , SDL.keysymKeycode ks == SDL.KeycodeF12
      , SDL.keyModifierLeftCtrl (SDL.keysymModifier ks)
      -> inp { inpHostCommands = HostQuit : inpHostCommands inp }
    -- F11 → захват / освобождение мыши
    SDL.KeyboardEvent kbd
      | SDL.keyboardEventKeyMotion kbd == SDL.Pressed
      , SDL.keysymKeycode (SDL.keyboardEventKeysym kbd) == SDL.KeycodeF11
      ->
        inp { inpHostCommands = HostToggleMouseGrab : inpHostCommands inp }
    -- Движение мыши
    SDL.MouseMotionEvent motion ->
      let SDL.P (V2 x y) = SDL.mouseMotionEventPos motion
      in inp { inpMousePos = V2 (fromIntegral x) (fromIntegral y) }

    -- Кнопки мыши
    SDL.MouseButtonEvent btn
      | SDL.mouseButtonEventMotion btn == SDL.Pressed ->
          inp { inpMouseButtons =
                  setButton True btn (inpMouseButtons inp) }

      | SDL.mouseButtonEventMotion btn == SDL.Released ->
          inp { inpMouseButtons =
                  setButton False btn (inpMouseButtons inp) }

    _ ->
      inp


setButton
    :: Bool
    -> SDL.MouseButtonEventData
    -> MouseButtons
    -> MouseButtons
setButton state btn mb =
    case SDL.mouseButtonEventButton btn of
        SDL.ButtonLeft   -> mb { mouseLeft   = state }
        SDL.ButtonMiddle -> mb { mouseMiddle = state }
        SDL.ButtonRight  -> mb { mouseRight  = state }
        _                -> mb

