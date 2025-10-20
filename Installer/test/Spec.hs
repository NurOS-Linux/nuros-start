{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- NurOS Ruzen42 2025
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.Short as TS
import qualified Data.Vector as V
import Control.Monad.Logger (runStdoutLoggingT, LoggingT, MonadLogger, logInfoN)
import Control.Monad.IO.Class (liftIO)

import Storage.Filesystem
import Storage.Grub
import Storage.Formatter
import User.ManageUser
import User.Keymap
import User.Network

instance Arbitrary SystemType where
  arbitrary = elements [X64, X32]

runWithLogging :: LoggingT IO a -> IO a
runWithLogging = runStdoutLoggingT

--------------------------------------------------------------------------------
-- Main test suite
--------------------------------------------------------------------------------
main :: IO ()
main = hspec . around_ (runWithLogging . const (pure ())) $ do
  describe "Storage.Filesystem" $ do
    describe "parseProcPartitions" $ do
      it "parses empty input as error" $ runWithLogging $ do
        logInfoN "Testing empty partitions input"
        let input = "major minor  #blocks  name\n"
        result <- parseProcPartitions (TS.fromText input)
        liftIO $ result `shouldBe` FSError ("No valid partitions found" :: TS.ShortText)

      it "parses valid partitions correctly" $ runWithLogging $ do
        logInfoN "Testing valid partition parsing"
        let input = "major minor  #blocks  name\n\
                    \   8        0  488386584 sda\n\
                    \   8        1   10485760 sda1\n\
                    \   8        2  477898752 sda2\n\
                    \   8       16  976762584 sdb\n\
                    \   8       17  976760576 sdb1"
        result <- parseProcPartitions (TS.fromText input)
        liftIO $ case result of
          FSSuccess disks -> do
            length disks `shouldBe` 2
            let sda = head disks
            diskName sda `shouldBe` "sda"
            V.length (partitions sda) `shouldBe` 2
            let sdb = disks !! 1
            diskName sdb `shouldBe` "sdb"
            V.length (partitions sdb) `shouldBe` 1
          _ -> expectationFailure "Expected FSSuccess"

      it "ignores lines with numeric-only names" $ runWithLogging $ do
        logInfoN "Testing numeric-only line skip"
        let input = "major minor  #blocks  name\n\
                    \   1        0       100 1\n\
                    \   8        0  488386584 sda"
        result <- parseProcPartitions (TS.fromText input)
        liftIO $ case result of
          FSSuccess disks -> do
            length disks `shouldBe` 1
            diskName (head disks) `shouldBe` "sda"
          _ -> expectationFailure "Expected FSSuccess"

    describe "splitName" $ do
      it "splits names with numbers correctly" $ do
        splitName "sda1" `shouldBe` ("sda", 1)
        splitName "nvme0n1p2" `shouldBe` ("nvme0n1p", 2)
        splitName "vda" `shouldBe` ("vda", 0)

  describe "Storage.Grub" $ do
    describe "installGrub" $ do
      it "validates 64-bit options require efiDirectory" $ runWithLogging $ do
        logInfoN "Testing installGrub 64-bit EFI validation"
        let opts = GrubOptions 
              { bitness = X64
              , configFile = "/boot/grub/grub.cfg"
              , efiDirectory = Nothing
              , disk = Just "/dev/sda"
              }
        liftIO $ installGrub opts `shouldThrow` anyException

      it "validates 32-bit options require disk" $ runWithLogging $ do
        logInfoN "Testing installGrub 32-bit disk validation"
        let opts = GrubOptions 
              { bitness = X32
              , configFile = "/boot/grub/grub.cfg"
              , efiDirectory = Just "/boot/efi"
              , disk = Nothing
              }
        liftIO $ installGrub opts `shouldThrow` anyException

    describe "GrubOptions" $ do
      prop "SystemType has correct string representation" $
        \(s :: SystemType) ->
          case s of
            X64 -> show s == "X64"
            X32 -> show s == "X32"

  describe "Storage.Formatter" $ do
    describe "buildMkfsCommand" $ do
      it "builds correct ext4 command" $ runWithLogging $ do
        logInfoN "Testing mkfs.ext4 command builder"
        let cmd = buildMkfsCommand "/dev/sda1" Ext4 []
        liftIO $ do
          TS.unpack cmd `shouldContain` "mkfs.ext4 -F"
          TS.unpack cmd `shouldContain` "/dev/sda1"

      it "builds correct btrfs command" $ runWithLogging $ do
        logInfoN "Testing mkfs.btrfs command builder"
        let cmd = buildMkfsCommand "/dev/sdb1" Btrfs ["-L", "DATA"]
        liftIO $ do
          TS.unpack cmd `shouldContain` "mkfs.btrfs -f"
          TS.unpack cmd `shouldContain` "-L DATA"

      it "builds correct fat32 command" $ runWithLogging $ do
        logInfoN "Testing mkfs.fat command builder"
        let cmd = buildMkfsCommand "/dev/sdc1" Fat32 []
        liftIO $ do
          TS.unpack cmd `shouldContain` "mkfs.fat -F32"

    describe "shellEscape" $ do
      it "escapes single quotes correctly" $ do
        shellEscape "test'file" `shouldBe` "'test'\"'\"'file'"

  describe "User.ManageUser" $ do
    let testUser = User
          { username = "testuser"
          , password = "password123"
          , uid = 1001
          , gid = 1001
          , comment = "Test User"
          , homeDir = "/home/testuser"
          , shell = "/bin/bash"
          }

    describe "createUser" $ do
      it "returns dry run command correctly" $ runWithLogging $ do
        logInfoN "Testing createUser dry run"
        result <- createUser True testUser
        liftIO $ case result of
          URDryRun cmd ->
            TS.unpack cmd `shouldContain` "useradd"
          _ -> expectationFailure "Expected URDryRun"

      it "constructs correct useradd command" $ runWithLogging $ do
        logInfoN "Testing createUser command construction"
        result <- createUser True testUser
        liftIO $ case result of
          URDryRun cmd -> do
            let cmdStr = TS.unpack cmd
            cmdStr `shouldContain` "-u 1001"
            cmdStr `shouldContain` "-g 1001"
            cmdStr `shouldContain` "-c Test User"
            cmdStr `shouldContain` "-d /home/testuser"
            cmdStr `shouldContain` "-s /bin/bash"
            cmdStr `shouldContain` "testuser"
          _ -> expectationFailure "Expected URDryRun"

  describe "User.Keymap" $ do
    let testKeymap = Keymap "us" "altgr-intl"
    let testOptions = KeymapOptions testKeymap False False

    describe "applyKeymap" $ do
      it "returns dry run command for TTY" $ runWithLogging $ do
        logInfoN "Testing applyKeymap for TTY"
        let opts = testOptions { koDryRun = True, koForTty = True }
        result <- applyKeymap opts
        liftIO $ case result of
          KMDRYRun cmd ->
            TS.unpack cmd `shouldContain` "loadkeys us"
          _ -> expectationFailure "Expected KMDRYRun"

      it "returns dry run command for X11" $ runWithLogging $ do
        logInfoN "Testing applyKeymap for X11"
        let opts = testOptions { koDryRun = True, koForTty = False }
        result <- applyKeymap opts
        liftIO $ case result of
          KMDRYRun cmd ->
            TS.unpack cmd `shouldContain` "setxkbmap us altgr-intl"
          _ -> expectationFailure "Expected KMDRYRun"

  describe "User.Network" $ do
    let wiredDevice = NetworkDevice "eth0" Wired
    let wirelessDevice = NetworkDevice "wlan0" Wireless
    let wifiOpts = WifiOptions "00:11:22:33:44:55" "MyWiFi" (Just "password123")

    describe "networkConnect" $ do
      it "handles wired connection dry run" $ runWithLogging $ do
        logInfoN "Testing networkConnect wired dry run"
        let opts = NetworkOptions wiredDevice Nothing True
        result <- networkConnect opts
        liftIO $ case result of
          CRDryRun cmd ->
            TS.unpack cmd `shouldContain` "nmcli device connect eth0"
          _ -> expectationFailure "Expected CRDryRun"

      it "handles wireless connection dry run" $ runWithLogging $ do
        logInfoN "Testing networkConnect wireless dry run"
        let opts = NetworkOptions wirelessDevice (Just wifiOpts) True
        result <- networkConnect opts
        liftIO $ case result of
          CRDryRun cmd -> do
            let cmdStr = TS.unpack cmd
            cmdStr `shouldContain` "nmcli device wifi connect MyWiFi"
            cmdStr `shouldContain` "ifname wlan0"
            cmdStr `shouldContain` "password password123"
          _ -> expectationFailure "Expected CRDryRun"

      it "requires wifi options for wireless devices" $ runWithLogging $ do
        logInfoN "Testing networkConnect missing Wi-Fi options"
        let opts = NetworkOptions wirelessDevice Nothing True
        result <- networkConnect opts
        liftIO $ case result of
          CRFailure msg ->
            TS.unpack msg `shouldContain` "Wi-Fi options missing"
          _ -> expectationFailure "Expected CRFailure"

    describe "buildWifiCmd" $ do
      it "builds command without password when not provided" $ runWithLogging $ do
        logInfoN "Testing buildWifiCmd without password"
        let wifiNoPwd = wifiOpts { wfPassword = Nothing }
        let cmd = buildWifiCmd wirelessDevice wifiNoPwd
        let cmdStr = TS.unpack cmd
        liftIO $ do
          cmdStr `shouldContain` "nmcli device wifi connect MyWiFi"
          cmdStr `shouldNotContain` "password"

  describe "Type classes and instances" $ do
    it "FSResult has Show instance" $ do
      show $ (FSSuccess ("test" :: TS.ShortText)) `shouldBe` "FSSuccess \"test\""
      show $ (FSError ("error" :: TS.ShortText))  `shouldBe` "FSError \"error\""

    it "Partition has Eq instance" $ do
      let p1 = Partition "sda1" 1
      let p2 = Partition "sda1" 1
      let p3 = Partition "sda2" 2
      p1 `shouldBe` p2
      p1 `shouldNotBe` p3

    it "Disk has Show instance" $ do
      let disk = Disk "sda" V.empty
      show disk `shouldContain` "sda"     
