-- NurOS Ruzen42 2025
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
  { optVerbose :: Bool
  , optDryRun  :: Bool
  , optJsonFile :: FilePath
  } deriving (Show)

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
  putStrLn optJsonFile opts

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
 <> progDesc "Install nuros via json"
 <> header "nuros-install — utility for install NurOS from *.json config" )

