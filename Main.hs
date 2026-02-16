module Main(main) where

-- Main.hs

import Config
import Frontend

main :: IO ()
main = do
  cfg <- getConfig
  runEmulator cfg
