-- |
-- Module      :  Configuration.Dotenv
-- Copyright   :  © 2015–2017 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides the ability to load a dotenv file with an specific configuration.

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Configuration.Dotenv
  ( loadFile
  ) where

import Control.Monad (liftM)
import Control.Monad.IO.Class

#if MIN_VERSION_base(4,7,0)
import System.Environment (getEnv, setEnv, lookupEnv)
#else
import System.Environment.Compat (getEnv, setEnv, lookupEnv)
#endif

import Configuration.Dotenv.File
import Configuration.Dotenv.Types

-- | @loadFile@ parses the given dotenv file and checks if the environment are empty or not.
loadFile :: MonadIO m => Config -> m [Variable]
loadFile Config{..} = do
  keys      <- map fst `liftM` parseFile configExamplePath
  maybeVars <- parseMaybeFile configPath
  setupEnvVars configOverride keys maybeVars

setupEnvVars :: MonadIO m => Bool -> [String] -> Maybe [Variable] -> m [Variable]
setupEnvVars override keys maybeVars =
  case maybeVars of
    Nothing   -> mapM getEnvVariable keys
    Just vars -> setVariables override keys vars

getEnvVariable :: MonadIO m => String -> m Variable
getEnvVariable key =
  liftM ((,) key) (liftIO (getEnv key))

setVariables :: MonadIO m => Bool -> [String] -> [Variable] -> m [Variable]
setVariables override keys vars =
  mapM (getVariable vars) keys >>= mapM (setVariable override)

getVariable :: MonadIO m => [Variable] -> String -> m Variable
getVariable vars key =
  case lookup key vars of
    Nothing  -> getEnvVariable key
    Just var -> return (key, var)

setVariable :: MonadIO m => Bool -> Variable -> m Variable
setVariable override var@(key, value) =
  if override then
    liftIO $ setEnv key value >> return var
  else do
    res <- liftIO $ lookupEnv key
    case res of
      Nothing   -> liftIO $ setEnv key value >> return var
      Just val' -> return (key, val')
