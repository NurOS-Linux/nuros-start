{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- NurOS Ruzen42 2025
module Storage.Filesystem
  ( parseProcPartitions
  , Disk(..)
  , Partition(..)
  , FSResult(..)
  ) where

import Data.Char (isDigit)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Data.List (groupBy, sortOn)
import GHC.Generics (Generic)
import Control.Monad.Logger (MonadLogger, logDebugN, logErrorN)

data FSResult a
  = FSSuccess a        -- ^ Успешный результат
  | FSError ShortText  -- ^ Ошибка в виде текста
  deriving (Show, Eq, Generic)

data Partition = Partition
  { deviceName :: !ShortText
  , number     :: !Int
  } deriving (Show, Eq, Generic)

data Disk = Disk
  { diskName   :: !ShortText
  , partitions :: !(V.Vector Partition)
  } deriving (Show, Eq, Generic)

parseProcPartitions :: MonadLogger m => ShortText -> m (FSResult [Disk])
parseProcPartitions input = do
  let ls = drop 1 $ T.lines (TS.toText input)
  logDebugN $ "Parsing " <> T.pack (show (length ls)) <> " lines from /proc/partitions"

  let entries = mapMaybe parseLine ls
  logDebugN $ "Parsed " <> T.pack (show (length entries)) <> " partition entries"

  if null entries
    then do
      logErrorN "No valid partitions found in /proc/partitions"
      return $ FSError "No valid partitions found"
    else do
      let grouped = groupBy sameDisk $ sortOn fst entries
      logDebugN $ "Grouped into " <> T.pack (show (length grouped)) <> " disks"

      let disks =
            [ Disk (TS.fromText name) (V.fromList parts)
            | (name, parts) <- map collect grouped
            ]

      let diskNames = T.intercalate ", " (map (TS.toText . diskName) disks)
      logDebugN $ "Final parsed disks: [" <> diskNames <> "]"

      return $ FSSuccess disks

parseLine :: T.Text -> Maybe (T.Text, Partition)
parseLine line =
  case T.words line of
    (_major : _minor : _blocks : nameTxt : _) ->
      let name = T.strip nameTxt
      in if T.all isDigit name
         then Nothing
         else
           let (disk, num) = splitName name
           in Just (disk, Partition (TS.fromText name) num)
    _ -> Nothing

splitName :: T.Text -> (T.Text, Int)
splitName t =
  let (prefix, suffix) = spanEnd isDigit t
  in case TR.decimal suffix of
       Right (n, _) -> (prefix, n)
       Left _       -> (t, 0)

spanEnd :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
spanEnd p txt =
  let rev = T.reverse txt
      (suf, pre) = T.span p rev
  in (T.reverse pre, T.reverse suf)

sameDisk :: (T.Text, Partition) -> (T.Text, Partition) -> Bool
sameDisk (d1, _) (d2, _) = d1 == d2

collect :: [(T.Text, Partition)] -> (T.Text, [Partition])
collect xs@((d, _) : _) = (d, map snd xs)
collect [] = error "collect: empty list"

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []