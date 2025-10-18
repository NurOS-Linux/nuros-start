-- NurOS Ruzen42 2025
module PackageManagement.InstallSets where

data Package = Package
  { name :: String
  } deriving (Show, Eq)

data Set = Set 
  { packages :: [Package] 
  } deriving (Show, Eq)
