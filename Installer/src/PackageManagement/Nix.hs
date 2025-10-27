{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

-- NurOS Ruzen42 2025
module Nix (installNix, InstallType(..), NixOptions(..), NixInstallResult(..)) where

import qualified Data.Text.Short as TS
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN, logErrorN, logInfoN)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Data.Bool (bool)

data InstallType = Root | User
  deriving (Eq, Show, Enum)

data NixOptions = NixOptions 
  { installType :: !InstallType
  , nixUsers    :: ![TS.ShortText]
  , dryRun      :: Bool
  } deriving (Show)

data NixInstallResult
  = NIDryRun 
  | NISuccess 
  | NIFailure !TS.ShortText
  deriving (Show, Eq)

nixInstallCommand :: TS.ShortText
nixInstallCommand = "sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install)"

nixFullInstallCommand :: InstallType -> TS.ShortText
nixFullInstallCommand userType = nixInstallCommand <> bool " --no-daemon" " --daemon" (userType == Root)

installNix :: (MonadLogger m, MonadIO m) => NixOptions -> m NixInstallResult
installNix opts = do
  let cmd = TS.unpack $ nixFullInstallCommand (installType opts)
  logInfoN $ "Starting Nix installation with command: " <> TS.toText (nixFullInstallCommand (installType opts))
  case dryRun opts of
    True -> do 
      logInfoN "(DryRun) Nix installation completed successfully."
      return NIDryRun
    False -> do
      (exitCode, stdoutText, stderrText) <- liftIO $ readProcessWithExitCode "bash" ["-c", cmd] ""

      case exitCode of
        ExitSuccess -> do
          logInfoN "Nix installation completed successfully."
          return NISuccess 
                        
        ExitFailure code -> do
          logErrorN $ "Nix installation failed with exit code " <> TS.toText (TS.pack $ show code)
          return $ NIFailure (TS.fromText $ T.pack stderrText)

