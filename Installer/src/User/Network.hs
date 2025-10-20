{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

-- NurOS Ruzen42 2025
module User.Network
  ( DeviceType(..)
  , NetworkDevice(..)
  , WifiOptions(..)
  , NetworkOptions(..)
  , ConnectResult(..)
  , networkConnect
  , buildWifiCmd 
  ) where

import qualified Data.Text as T
import qualified Data.Text.Short as TS
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

data DeviceType = Wireless | Wired
  deriving (Show, Eq)

data NetworkDevice = NetworkDevice 
  { ndName :: !TS.ShortText
  , ndType :: !DeviceType
  } deriving (Show, Eq)

data WifiOptions = WifiOptions 
  { wfBSSID    :: !TS.ShortText
  , wfSSID     :: !TS.ShortText
  , wfPassword :: !(Maybe TS.ShortText)
  } deriving (Show, Eq)

data NetworkOptions = NetworkOptions
  { noDevice   :: !NetworkDevice
  , noWifi     :: !(Maybe WifiOptions)
  , networkDryRun   :: !Bool
  } deriving (Show, Eq)

data ConnectResult
  = CRDryRun !TS.ShortText
  | CRSuccess !TS.ShortText !TS.ShortText
  | CRFailure !TS.ShortText
  deriving (Show, Eq)

networkConnect
  :: (MonadLogger m, MonadIO m)
  => NetworkOptions -> m ConnectResult
networkConnect opts = do
  let dev = noDevice opts
  case ndType dev of
    Wired -> do
      let cmd = buildWiredCmd dev
      if networkDryRun opts
        then dryRun cmd
        else runAndLog cmd

    Wireless ->
      case noWifi opts of
        Nothing -> do
          let msg = "Wi-Fi options missing for device: " <> ndName dev
          logDebugN (T.pack (show (CRFailure msg)))
          pure $ CRFailure msg
        Just wf -> do
          let cmd = buildWifiCmd dev wf
          if networkDryRun opts
            then dryRun cmd
            else runAndLog cmd

runAndLog :: (MonadLogger m, MonadIO m) => TS.ShortText -> m ConnectResult
runAndLog cmd = do
  logDebugN $ "Executing: " <> TS.toText cmd
  res <- liftIO $ executeCmd cmd
  case res of
    CRSuccess out _ -> logDebugN $ "Connection successful: " <> TS.toText out
    CRFailure err   -> logDebugN $ "Connection failed: " <> TS.toText err
    _               -> pure ()
  pure res

dryRun :: MonadLogger m => TS.ShortText -> m ConnectResult
dryRun cmd = do
  logDebugN $ "Dry-run: " <> TS.toText cmd
  pure $ CRDryRun cmd

executeCmd :: TS.ShortText -> IO ConnectResult
executeCmd cmd = do
  (ec, out, err) <- readProcessWithExitCode "bash" ["-c", TS.unpack cmd] ""
  case ec of
    ExitSuccess   -> pure $ CRSuccess (TS.fromText (T.pack out)) (TS.fromText (T.pack err))
    ExitFailure _ -> pure $ CRFailure (TS.fromText (T.pack err))

buildWiredCmd :: NetworkDevice -> TS.ShortText
buildWiredCmd dev =
  "bash -c 'nmcli device connect " <> ndName dev <> "'"

buildWifiCmd :: NetworkDevice -> WifiOptions -> TS.ShortText
buildWifiCmd dev wf =
  let base = "bash -c 'nmcli device wifi connect " <> wfSSID wf <>
             " ifname " <> ndName dev
      pwdPart = maybe "" (\p -> " password " <> p) (wfPassword wf)
      end = "'"
  in base <> pwdPart <> end
