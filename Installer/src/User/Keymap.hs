{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

-- NurOS Ruzen42 2025
module User.Keymap (Keymap(..), KeymapOptions(..), KMResult(..), applyKeymap) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import qualified Data.Text.Short as TS
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

data Keymap = Keymap
  { kmLang    :: !TS.ShortText
  , kmVariant :: !TS.ShortText
  } deriving (Show, Eq)

data KeymapOptions = KeymapOptions
  { koKeymap  :: !Keymap
  , koForTty  :: !Bool
  , koDryRun  :: !Bool
  } deriving (Show, Eq)

data KMResult
  = KMDRYRun !TS.ShortText
  | KMSuccess !TS.ShortText !TS.ShortText
  | KMFailure !TS.ShortText
  deriving (Show, Eq)

applyKeymap :: (MonadLogger m, MonadIO m) => KeymapOptions -> m KMResult
applyKeymap opts = do
  let Keymap{kmLang, kmVariant} = koKeymap opts
      dryRun = koDryRun opts
      forTty = koForTty opts
      langTxt = TS.toText kmLang
      varTxt  = TS.toText kmVariant

      cmd :: String
      cmd = if forTty
            then "loadkeys " <> T.unpack langTxt
            else "setxkbmap " <> T.unpack langTxt <> " " <> T.unpack varTxt

  if dryRun
    then do
      logDebugN $ "Dry-run: " <> T.pack cmd
      pure $ KMDRYRun (TS.fromText (T.pack cmd))
    else do
      logInfoN $ "Applying keymap: " <> T.pack cmd
      (exit, _out, err) <- liftIO $ readProcessWithExitCode "bash" ["-c", cmd] ""
      case exit of
        ExitSuccess -> do
          logInfoN "Keymap applied successfully"
          pure $ KMSuccess kmLang kmVariant
        ExitFailure _ -> do
          logErrorN $ "Failed to apply keymap: " <> T.pack err
          pure $ KMFailure (TS.fromText (T.pack err))
