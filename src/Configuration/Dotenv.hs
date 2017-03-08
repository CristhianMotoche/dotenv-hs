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
  , onMissingFile
  ) where

import Control.Monad (liftM)
import Control.Monad.Catch (MonadCatch, catchIf)
import Control.Monad.IO.Class
import System.IO.Error (isDoesNotExistError)

#if MIN_VERSION_base(4,7,0)
import System.Environment (getEnv, setEnv, lookupEnv)
#else
import System.Environment.Compat (getEnv, setEnv, lookupEnv)
#endif

import Configuration.Dotenv.File
import Configuration.Dotenv.Types

-- | @loadFile@ parses the given dotenv file and checks if the environment are empty or not.
loadFile :: MonadIO m => Config -> m [NameValuePair]
loadFile Config{..} = do
  keys      <- map fst `liftM` parseFile configExamplePath
  maybeVars <- parseMaybeFile configPath
  setupEnvVars configOverride keys maybeVars

setupEnvVars :: MonadIO m => Bool -> [String] -> Maybe [NameValuePair] -> m [NameValuePair]
setupEnvVars override keys maybeVars =
  case maybeVars of
    Nothing   -> mapM getEnvVariable keys
    Just vars -> setVariables override keys vars

getEnvVariable :: MonadIO m => String -> m NameValuePair
getEnvVariable key =
  liftM ((,) key) (liftIO (getEnv key))

setVariables :: MonadIO m => Bool -> [String] -> [NameValuePair] -> m [NameValuePair]
setVariables override keys vars =
  mapM (getVariable vars) keys >>= mapM (setVariable override)

getVariable :: MonadIO m => [NameValuePair] -> String -> m NameValuePair
getVariable vars key =
  case lookup key vars of
    Nothing  -> getEnvVariable key
    Just var -> return (key, var)

setVariable :: MonadIO m => Bool -> NameValuePair -> m NameValuePair
setVariable override var@(key, value) =
  if override then
    liftIO $ setEnv key value >> return var
  else do
    res <- liftIO $ lookupEnv key
    case res of
      Nothing   -> liftIO $ setEnv key value >> return var
      Just val' -> return (key, val')


-- | The helper allows to avoid exceptions in the case of missing files and
-- perform some action instead.
--
-- @since 0.3.1.0

onMissingFile :: MonadCatch m
  => m a -- ^ Action to perform that may fail because of missing file
  -> m a -- ^ Action to perform if file is indeed missing
  -> m a
onMissingFile f h = catchIf isDoesNotExistError f (const h)
