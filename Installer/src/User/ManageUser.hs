{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

-- NurOS Ruzen42 2025
module User.ManageUser
  ( User(..)
  , URResult(..)
  , createUser
  ) where

import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import qualified Data.Text.Short as ST
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

data User = User
  { username :: !ST.ShortText
  , password :: !ST.ShortText
  , uid      :: !Int
  , gid      :: !Int
  , comment  :: !ST.ShortText
  , homeDir  :: !ST.ShortText
  , shell    :: !ST.ShortText
  } deriving (Show, Eq, Generic)

data URResult
  = URSuccess !ST.ShortText
  | URFailure !ST.ShortText
  | URDryRun  !ST.ShortText
  deriving (Show, Eq)

createUser :: (MonadLogger m, MonadIO m) => Bool -> User -> m URResult
createUser dryRun User{username, uid, gid, comment, homeDir, shell} = do
  let uname  = ST.toText username
      uidStr = T.pack (show uid)
      gidStr = T.pack (show gid)
      gecos  = ST.toText comment
      home   = ST.toText homeDir
      sh     = ST.toText shell

      cmdArgs =
        [ "-u", uidStr
        , "-g", gidStr
        , "-c", gecos
        , "-d", home
        , "-s", sh
        , uname
        ]

      fullCmd = "useradd " <> T.unwords cmdArgs

  if dryRun
    then do
      logDebugN $ "Dry-run: " <> fullCmd
      pure $ URDryRun (ST.fromText fullCmd)
    else do
      logInfoN $ "Creating user: " <> uname
      (ec, _out, err) <- liftIO $ readProcessWithExitCode "useradd" (map T.unpack cmdArgs) ""
      case ec of
        ExitSuccess -> do
          logInfoN $ "User created successfully: " <> uname
          pure $ URSuccess username
        ExitFailure _ -> do
          logErrorN $ "Failed to create user: " <> uname <> " (" <> T.pack err <> ")"
          pure $ URFailure (ST.fromText (T.pack err))