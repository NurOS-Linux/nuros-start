-- NurOS Ruzen42 2025
module Storage.Grub
  ( SystemType(..)
  , GrubOptions(..)
  , installGrub
  , makeConfig
  ) where

import System.Process (callProcess)
import qualified Data.Text.Short as TS

data SystemType = X64 | X32
  deriving (Show, Enum, Eq)

data GrubOptions = GrubOptions
  { bitness :: SystemType
  , configFile :: TS.ShortText
  , efiDirectory :: Maybe TS.ShortText  
  , disk :: Maybe TS.ShortText         
  } deriving (Show)

installGrub :: GrubOptions -> IO ()
installGrub opts = do
  let target = case bitness opts of
        X64 -> "x86_64-efi"
        X32 -> "i386-pc"

      otherFlags = case bitness opts of
        X64 ->
          case efiDirectory opts of
            Just dir ->
              [ "--efi-directory=" ++ TS.unpack dir
              , "--bootloader-id=GRUB"
              ]
            Nothing ->
              error "GrubOptions: 'efiDirectory' is required for 64-bit installation"
        X32 ->
          case disk opts of
            Just d  -> [TS.unpack d]
            Nothing -> error "GrubOptions: 'disk' is required for 32-bit installation"

      args = ["--target=" ++ target] ++ otherFlags
      cfgPath = TS.unpack (configFile opts)

  putStrLn $ "Running grub-install with: " ++ unwords args
  callProcess "grub-install" args

  putStrLn $ "Generating grub config at: " ++ cfgPath
  makeConfig (configFile opts)

makeConfig :: TS.ShortText -> IO ()
makeConfig path = do
  let cfgPath = TS.unpack path
  callProcess "grub-mkconfig" ["-o", cfgPath]
  putStrLn $ "Running grub-mkconfig in: " ++ cfgPath
