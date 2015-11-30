module UpdateRecords where

{-
    Sometimes you want to change some fields of a record, but keep the
    rest as it is:
-}
data Country = Country {
    name :: String,
    nickname :: String,
    population :: Integer
    } deriving (Show)

sweden :: Country
sweden = Country {name = "Sweden", nickname = "", population = 9593000}

{-
    Then you remember that Sweden is also called "Söta bror", and you
    start copying the entire record, but with nickName altered.
-} 

sweetBrother :: Country
sweetBrother = Country {
    name = (name sweden),
    nickname = "Söta bror", 
    population = (population sweden)
    }

{-
    Luckily Haskell has some sweet syntactic sugar for this:
-}
sweeterBrother :: Country
sweeterBrother = sweden {nickname = "Söta bror"}



{-
    Exercise:
    Define backupDBWeekly as a copy of backupDB, but with a few 
    altered fields: 
        cron = "@weekly"
        user = "dbadmin"
        priority = 8
        displayName = "Weekly Backup of Postgres DB"
-}
data ScheduledJob = ScheduledJob {
    displayName :: String,
    command :: String,
    user :: String,
    cron :: String,
    threads :: Int,
    priority :: Int,
    timeoutMinutes :: Int
    } deriving (Show, Eq)

backupDB :: ScheduledJob
backupDB = ScheduledJob {
    displayName = "Nightly Backup of Postgres DB",
    command = "/usr/bin/backupdb",
    user = "admin",
    cron = "@daily",
    threads = 1,
    priority = 10,
    timeoutMinutes = 60
    }

backupDBWeekly :: ScheduledJob
backupDBWeekly = undefined













