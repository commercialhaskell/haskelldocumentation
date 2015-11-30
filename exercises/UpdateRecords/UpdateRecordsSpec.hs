{-# LANGUAGE ScopedTypeVariables #-}

module UpdateRecords.UpdateRecordsSpec where

import qualified UpdateRecords     as UR

import qualified Test.QuickCheck as QC
import Test.Hspec

spec = do
    describe "UpdateRecords.backupDBWeekly" $ do
        it "updates the record" $ do
            UR.backupDBWeekly `shouldBe` correctBackupDBWeekly

correctBackupDBWeekly :: UR.ScheduledJob
correctBackupDBWeekly = UR.ScheduledJob {
    UR.user = "dbadmin",
    UR.priority = 8,
    UR.displayName = "Weekly Backup of Postgres DB",

    UR.command = "/usr/bin/backupdb",
    UR.cron = "@weekly",
    UR.threads = 1,
    UR.timeoutMinutes = 60
    }
