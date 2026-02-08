-- Config.hs
module Config
  ( Config(..)
  , getConfig
  ) where

import Options.Applicative

data Config = Config
  { cfgRomPath   :: !FilePath
  , cfgScrPath   :: !FilePath
  , cfgScale     :: !Int
  , cfgNoSound   :: !Bool
  } deriving Show


configParser :: Parser Config
configParser = Config
    <$> strOption
        ( long  "rom"
        <>short 'r'
        <>metavar "FILE"
        <>value ""
        <>help "ROM image" 
        )
    <*> strOption
        ( long "screen"
        <>short 's'
        <>metavar "SCREEN"
        <>value ""
        <>help "Screen image"
        )
  <*> option auto
        ( long "scale"
        <>value 2
        <>help "Video scale factor" )
  <*> switch
        ( long "nosound"
       <> help "Disable sound" )

getConfig :: IO Config
getConfig = execParser $ info (configParser <**> helper) fullDesc
