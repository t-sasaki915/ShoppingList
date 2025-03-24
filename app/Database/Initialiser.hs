module Database.Initialiser (initialiseDatabase) where

import qualified Data.ByteString        as BS
import qualified Database.Resource      as Res
import           Database.SQLite.Simple (Connection, open)
import           System.Directory       (doesFileExist)
import           Text.Printf            (printf)

writeDefaultDatabaseFile :: FilePath -> IO ()
writeDefaultDatabaseFile = flip BS.writeFile Res.defaultDatabaseFile

initialiseDatabase :: FilePath -> IO Connection
initialiseDatabase filePath =
    doesFileExist filePath >>= \case
        True -> do
            putStrLn $ printf "Acquired '%s' as the database." filePath
            open filePath
        False -> do
            putStrLn $ printf "Could not find '%s'. Creating..." filePath
            writeDefaultDatabaseFile filePath
            initialiseDatabase filePath
