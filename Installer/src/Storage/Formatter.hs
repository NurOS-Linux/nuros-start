{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- NurOS Ruzen42 2025
module Storage.Formatter
  ( FS(..)
  , FormatOptions(..)
  , FormatResult(..)
  , safeFormat
  , buildMkfsCommand 
  , shellEscape
  ) where

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import qualified Data.Text.Short as TS
import qualified Data.Text as T
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN)

data FS = Ext4 | Btrfs | F2fs | Xfs | Fat32
  deriving (Show, Eq)

data FormatOptions = FormatOptions
  { dryRun       :: !Bool
  , extraArgs    :: ![TS.ShortText]
  , requireToken :: !TS.ShortText
  } deriving (Show, Eq)

data FormatResult
  = FRDryRun !TS.ShortText
  | FRSuccess !TS.ShortText !TS.ShortText
  | FRFailure !TS.ShortText
  deriving (Show, Eq)

safeFormat
  :: (MonadLogger m, MonadIO m)
  => TS.ShortText -> FS -> FormatOptions -> m FormatResult
safeFormat device fs opts = do
  existsOk <- liftIO $ isBlockDevice device
  if not existsOk
    then do
      let msg = "Device not found: " <> device
      logDebugN $ T.pack (show (FRFailure msg))
      pure $ FRFailure msg
    else do
      mounted <- liftIO $ findMounts device
      if not (null mounted)
        then do
          let msg = "Device is mounted: " <> TS.fromText (T.pack (unlines mounted))
          logDebugN $ T.pack (show (FRFailure msg))
          pure $ FRFailure msg
        else if requireToken opts /= device
          then do
            let msg = "RequireToken is missing: " <> device
            logDebugN $ T.pack (show (FRFailure msg))
            pure $ FRFailure msg
          else do
            let cmd = buildMkfsCommand device fs (extraArgs opts)
            if dryRun opts
              then do
                logDebugN "Dry run mode enabled"
                pure $ FRDryRun cmd
              else do
                logDebugN $ "Executing: " <> TS.toText cmd
                liftIO $ executeCmd cmd

isBlockDevice :: TS.ShortText -> IO Bool
isBlockDevice dev = do
  (ec, _, _) <- readProcessWithExitCode "bash" ["-c", "test -b " ++ TS.unpack dev] ""
  pure (ec == ExitSuccess)

findMounts :: TS.ShortText -> IO [String]
findMounts dev = do
  content <- readFile "/proc/mounts"
  let ls = lines content
  pure $ mapMaybe (extractMount dev) ls

extractMount :: TS.ShortText -> String -> Maybe String
extractMount dev line =
  if TS.unpack dev `isInfixOf` line
     then case words line of
            (mnt:_) -> Just mnt
            _       -> Nothing
     else Nothing

buildMkfsCommand :: TS.ShortText -> FS -> [TS.ShortText] -> TS.ShortText
buildMkfsCommand dev fs extras =
  let base = case fs of
        Ext4  -> "mkfs.ext4 -F"
        Btrfs -> "mkfs.btrfs -f"
        F2fs  -> "mkfs.f2fs -f"
        Xfs   -> "mkfs.xfs -f"
        Fat32 -> "mkfs.fat -F32"
      extrasPart = TS.intercalate " " extras
  in TS.fromText $ T.pack $
       unwords ["bash", "-c", shellEscape (unwords [TS.unpack base, TS.unpack extrasPart, TS.unpack dev])]

executeCmd :: TS.ShortText -> IO FormatResult
executeCmd bashCmd = do
  (ec, out, err) <- readProcessWithExitCode "bash" ["-c", TS.unpack bashCmd] ""
  case ec of
    ExitSuccess   -> pure $ FRSuccess (TS.fromText (T.pack out)) (TS.fromText (T.pack err))
    ExitFailure _ -> pure $ FRFailure (TS.fromText (T.pack err))

shellEscape :: String -> String
shellEscape s = "'" ++ concatMap escapeChar s ++ "'"
  where
    escapeChar '\'' = "'\"'\"'"
    escapeChar c    = [c]
