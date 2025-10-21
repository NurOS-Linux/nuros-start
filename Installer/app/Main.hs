{-# LANGUAGE OverloadedStrings #-}

-- NurOS Ruzen42 2025
module Main (main) where

import Options.Applicative
import Lib
import Storage.OSLoader 
import qualified Data.Text.Short as TS

data Options = Options
  { optVerbose :: Bool
  , optDryRun  :: Bool
  , optJsonFile :: FilePath
  } deriving (Show)

installOpts :: InstallOptions 
installOpts = InstallOptions 
  { osLoaderOpts = SystemdBoot 
      SystemdBootOptions 
        { efiDirectoryS = TS.fromString "/boot/efi/" }
  }

main :: IO ()
main = do
  opts <- execParser optsInfo
  putStrLn "Test arg:"
  print opts
  putStrLn ""
  if optDryRun opts
    then putStrLn "Dry Mode on"
    else putStrLn "Dry mode off"
  if optVerbose opts
    then putStrLn "verbose on"
    else return ()
  putStrLn $ optJsonFile opts
  result <- installNurOS installOpts
  putStrLn $ show result

optionsParser :: Parser Options
optionsParser = Options
  <$> switch
        ( long "verbose"
       <> short 'v'
       <> help "Verbose output" )
  <*> switch
        ( long "dry-run"
       <> help "Execution without real work (for test)" )
  <*> argument str
        ( metavar "FILE.json"
       <> help "NurOS install configuration file (*.json)"
       <> completer (bashCompleter "file")
        )

optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "A Haskell program that reads a JSON file and bootstrapping NurOS onto your device."
 <> header "nuros-install – utility for install NurOS from *.json config" )
