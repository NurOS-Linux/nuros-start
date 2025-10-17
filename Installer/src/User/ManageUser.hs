-- NurOS Ruzen42 2025
{-# LANGUAGE DeriveGeneric #-}

module User.ManageUser where

import GHC.Generics (Generic)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import System.Process (callProcess)

data User = User
  { username :: ShortText 
  , password :: ShortText  
  , uid      :: Int         
  , gid      :: Int      
  , comment  :: ShortText 
  , homeDir  :: ShortText  
  , shell    :: ShortText   
  } deriving (Show, Eq, Generic)

createUser :: User -> IO ()
createUser user = do
  let uname = ST.unpack (username user)
      uidStr = show (uid user)
      gidStr = show (gid user)
      gecos  = ST.unpack (comment user)
      home   = ST.unpack (homeDir user)
      sh     = ST.unpack (shell user)
  callProcess "useradd"
    [ "-u", uidStr
    , "-g", gidStr
    , "-c", gecos
    , "-d", home
    , "-s", sh
    , uname
    ]
