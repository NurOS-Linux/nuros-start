{-# LANGUAGE OverloadedStrings #-}

-- NurOS Ruzen42 2025
module Lib
    ( installNurOS 
    , InstallOptions(..)
    , InstallResult(..)
    ) where

import Storage.OSLoader
import Control.Monad.Logger
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS

data InstallOptions = InstallOptions 
  { osLoaderOpts :: OSLoaderOptions 
  } deriving (Show)

data InstallResult 
  = InstallDryRun !ShortText
  | InstallSuccess !ShortText !ShortText
  | InstallFailure !ShortText
  deriving (Show, Eq)

installNurOS :: InstallOptions -> IO T.Text 
installNurOS opts = runStdoutLoggingT $ do
  result <- installationPipeline opts
  return $ resultToText result

resultToText :: InstallResult -> T.Text
resultToText (InstallDryRun msg) = "Dry run: " <> TS.toText msg
resultToText (InstallSuccess title msg) = "Success: " <> TS.toText title <> " - " <> TS.toText msg
resultToText (InstallFailure err) = "Failed: " <> TS.toText err

installationPipeline :: (MonadIO m, MonadLogger m) => InstallOptions -> m InstallResult
installationPipeline opts = do
  logInfoN "Starting NurOS installation..." 
  
  result <- runPipeline
    [ ("OSLoader", installOSLoaderStep (osLoaderOpts opts))
    , ("Network", installNetworkStep)
    , ("Storage", installStorageStep)
    ]
  
  case result of
    Just err -> do
      logErrorN $ "Installation failed at: " <> err
      return $ InstallFailure (TS.fromString $ T.unpack err)
    Nothing -> do
      logInfoN "NurOS installation finished successfully"
      return $ InstallSuccess "NurOS" "Installation completed"
  where
    runPipeline :: (MonadIO m, MonadLogger m) => [(String, m (Maybe T.Text))] -> m (Maybe T.Text)
    runPipeline [] = return Nothing
    runPipeline ((name, step):rest) = do
      logInfoN $ "Running step: " <> T.pack name
      stepResult <- step
      case stepResult of
        Nothing -> runPipeline rest
        err -> return err

installOSLoaderStep :: (MonadIO m, MonadLogger m) => OSLoaderOptions -> m (Maybe T.Text)
installOSLoaderStep loaderOpts = do
  result <- installOSLoader loaderOpts
  case result of
    OLSuccess cmd args -> do
      logInfoN $ "OS Loader configured: " <> T.pack (TS.unpack cmd)
      logDebugN $ "Arguments: " <> T.pack (TS.unpack args)
      return Nothing
    OLFailure err -> do
      logErrorN $ "OS Loader installation failed: " <> T.pack (TS.unpack err)
      return $ Just $ T.pack (TS.unpack err)
    OLDryRun msg -> do
      logInfoN $ "OS Loader dry run: " <> T.pack (TS.unpack msg)
      return Nothing

installNetworkStep :: (MonadIO m, MonadLogger m) => m (Maybe T.Text)
installNetworkStep = do
  logInfoN "Configuring network..."
  -- TODO: installNetwork with options
  return Nothing

installStorageStep :: (MonadIO m, MonadLogger m) => m (Maybe T.Text)
installStorageStep = do
  logInfoN "Configuring storage..."
  -- TODO: installStorage with options
  return Nothing
