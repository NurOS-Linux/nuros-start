-- NurOS Ruzen42 2025
module PackageManagement.InstallSets where

import System.Process (callProcess)

data Package = Package
  { name :: String
  } deriving (Show, Eq)

data Set = Set
  { packages :: [Package]
  } deriving (Show, Eq)

installPackage :: Package -> Text -> IO ()
installPackage pkg destDir = do
  putStrLn $ "Installing " ++ name pkg ++ " to " ++ TS.unpack destDir
  callProcess "tulpar" ["-i", pkg]
