{-# LANGUAGE OverloadedStrings #-}

-- NurOS Ruzen42 2025
module Storage.OSLoader
  ( SystemType(..)
  , OSLoaderOptions(..)
  , GrubOptions(..)
  , SystemdBootOptions(..)
  , OLResult(..)
  , installOSLoader
  , makeConfig
  ) where

import System.Process (callProcess)
import qualified Data.Text.Short as TS
import qualified Data.Text as T
import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception (catch, SomeException)

data SystemType = SystemdBootType | GrubType
  deriving (Show, Eq)

data OSLoaderOptions 
  = SystemdBoot SystemdBootOptions 
  | Grub GrubOptions
  deriving (Show)

data SystemdBootOptions = SystemdBootOptions 
  { efiDirectoryS :: !TS.ShortText 
  } deriving (Show)

data GrubOptions = GrubOptions
  { configFile :: !TS.ShortText
  , efiDirectoryG :: !TS.ShortText  
  } deriving (Show)

data OLResult 
  = OLDryRun !TS.ShortText
  | OLSuccess !TS.ShortText !TS.ShortText
  | OLFailure !TS.ShortText
  deriving (Show, Eq)

installOSLoader :: (MonadIO m, MonadLogger m) => OSLoaderOptions -> m OLResult
installOSLoader (SystemdBoot opts) = installSystemdBoot opts
installOSLoader (Grub opts) = installGrub opts

installSystemdBoot :: (MonadIO m, MonadLogger m) => SystemdBootOptions -> m OLResult 
installSystemdBoot opts = do
  let espPath = TS.unpack $ efiDirectoryS opts
  let args = ["install", "--esp-path=" ++ espPath]
  
  logInfoN $ "Starting systemd-boot installation to: " <> T.pack espPath
  logDebugN $ "bootctl arguments: " <> T.pack (unwords args)
  
  result <- runCommand "bootctl" args
  
  case result of
    OLSuccess cmd cmdArgs -> do
      logInfoN $ "systemd-boot successfully installed to: " <> T.pack espPath
      logDebugN $ "Command executed: " <> T.pack (TS.unpack cmd) <> " " <> T.pack (TS.unpack cmdArgs)
      return result
    OLFailure errMsg -> do
      logErrorN $ "systemd-boot installation failed: " <> TS.toText errMsg
      return result
    _ -> return result

installGrub :: (MonadIO m, MonadLogger m) => GrubOptions -> m OLResult 
installGrub opts = do
  let efiDir = TS.unpack $ efiDirectoryG opts
  let cfgPath = TS.unpack $ configFile opts
  let grubArgs = 
        [ "--target=x86_64-efi"
        , "--efi-directory=" ++ efiDir
        , "--bootloader-id=GRUB"
        ]
  
  logInfoN $ "Starting GRUB installation with EFI directory: " <> T.pack efiDir
  logDebugN $ "grub-install arguments: " <> T.pack (unwords grubArgs)
  
  grubResult <- runCommand "grub-install" grubArgs
  
  case grubResult of
    OLSuccess _ _ -> do
      logInfoN "GRUB bootloader installed successfully"
      logInfoN $ "Generating GRUB configuration at: " <> T.pack cfgPath
      makeConfig (configFile opts)
    OLFailure errMsg -> do
      logErrorN $ "GRUB installation failed: " <> TS.toText errMsg
      return grubResult

makeConfig :: (MonadIO m, MonadLogger m) => TS.ShortText -> m OLResult
makeConfig path = do
  let cfgPath = TS.unpack path
  logInfoN "Running grub-mkconfig to generate configuration"
  logDebugN $ "Output path: " <> T.pack cfgPath
  
  result <- runCommand "grub-mkconfig" ["-o", cfgPath]
  
  case result of
    OLSuccess _ _ -> do
      logInfoN $ "GRUB configuration successfully generated at: " <> T.pack cfgPath
    OLFailure errMsg -> do
      logErrorN $ "GRUB configuration generation failed: " <> TS.toText errMsg
  
  return result

runCommand :: (MonadIO m, MonadLogger m) => String -> [String] -> m OLResult
runCommand cmd args = liftIO $
  catch (do
    return $ OLSuccess (TS.fromString cmd) (TS.fromString $ unwords args)
  ) handleError
  where
    handleError :: SomeException -> IO OLResult
    handleError ex = do
      let errMsg = "Error running " ++ cmd ++ ": " ++ show ex
      return $ OLFailure (TS.fromString errMsg)

