-- NurOS Ruzen42 2025
{-# LANGUAGE DeriveGeneric #-}

module Storage.Filesystem where

import Data.Char (isDigit)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Data.List (groupBy, sortOn)
import GHC.Generics (Generic)

-- === Types ===
data Partition = Partition
  { deviceName :: !ShortText
  , number     :: !Int
  } deriving (Show, Eq, Generic)

data Disk = Disk
  { diskName   :: !ShortText
  , partitions :: !(V.Vector Partition)
  } deriving (Show, Eq, Generic)

-- === Main func ===

parseProcPartitions :: TS.ShortText -> [Disk]
parseProcPartitions input =
  let
    ls = drop 1 $ TS.lines input  
    entries = mapMaybe parseLine ls
    grouped = groupBy sameDisk $ sortOn fst entries
  in
    [ Disk name (V.fromList parts)
    | (name, parts) <- map collect grouped
    ]

-- === Helping funcs ===

parseLine :: ShortText -> Maybe (ShortText, Partition)
parseLine line =
  case TS.words line of
    (_major : _minor : _blocks : nameTxt : _) ->
      let name = TS.strip nameTxt
      in if TS.all isDigit name
         then Nothing
         else
           let (disk, num) = splitName name
           in Just (disk, Partition name num)
    _ -> Nothing

splitName :: ShortText -> (ShortText, Int)
splitName t =
  let (prefix, suffix) = TS.spanEnd isDigit t
  in case TR.decimal (TS.toText suffix) of
       Right (n, _) -> (prefix, n)
       Left _       -> (t, 0)

sameDisk :: (ShortText, Partition) -> (ShortText, Partition) -> Bool
sameDisk (d1, _) (d2, _) = d1 == d2

collect :: [(ShortText, Partition)] -> (ShortText, [Partition])
collect xs@((d, _) : _) = (d, map snd xs)
collect [] = error "collect: empty list"

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []
