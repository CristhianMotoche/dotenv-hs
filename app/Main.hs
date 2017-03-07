{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

#if MIN_VERSION_optparse_applicative(0,13,0)
import Data.Monoid ((<>))
#endif

import Options.Applicative

import Control.Monad (void)
import Control.Monad.IO.Class(MonadIO(..))

import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import System.Process (system)
import System.Exit (exitWith)

import Configuration.Dotenv
import Configuration.Dotenv.Types

data Options = Options
  { program           :: String -- ^ Program to run with the load env variables
  , dotenvFile        :: String -- ^ Path for the .env file
  , dotenvExampleFile :: String -- ^ Path for the .env.example file
  , override          :: Bool   -- ^ Override current environment variables
  } deriving (Show)

buildConfig :: FilePath -> FilePath -> Config
buildConfig dotenvPath dotenvExamplePath =
  Config
    { configExamplePath = dotenvExamplePath
    , configOverride    = False
    , configPath        = dotenvPath
    }

main :: IO ()
main = execParser opts >>= dotEnv
  where
    opts = info (helper <*> config)
      ( fullDesc
     <> progDesc "Runs PROGRAM after loading options from FILE"
     <> header "dotenv - loads options from dotenv files" )

config :: Parser Options
config =
  Options
     <$> argument str (metavar "PROGRAM")

     <*> strOption
          ( long "env"
          <> value ".env"
          <> metavar "\"DOTENV FILE\""
          <> help "File with the env variables" )

     <*> strOption
          ( long "env-example"
          <> value ".env.example"
          <> metavar "\"DOTENV EXAMPLE\" FILE"
          <> help "File with all the necesary env variables" )

     <*> switch
           ( long "overload"
           <> short 'o'
           <> help "Specify this flag to override existing variables" )

dotEnv :: MonadIO m => Options -> m ()
dotEnv Options{..} = liftIO $ do
  current <- getCurrentDirectory

  let defaultConfig = buildConfig (current </> dotenvFile) (current </> dotenvExampleFile)

  void $ loadFile (defaultConfig { configOverride = override })
  code <- system program
  exitWith code

