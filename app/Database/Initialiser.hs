module Database.Initialiser (initialiseDatabase) where

import qualified Data.ByteString        as BS
import           Data.String.Here       (i)
import           Database.SQLite.Simple (Connection, open)
import           System.Directory       (doesFileExist)

import qualified Database.Resource      as Res

writeDefaultDatabaseFile :: FilePath -> IO ()
writeDefaultDatabaseFile = flip BS.writeFile Res.defaultDatabaseFile

initialiseDatabase :: FilePath -> IO Connection
initialiseDatabase filePath =
    doesFileExist filePath >>= \case
        True -> do
            putStrLn [i|Acquired '${filePath}' as the database.|]
            open filePath

        False -> do
            putStrLn [i|Could not find '${filePath}'. Creating...|]
            writeDefaultDatabaseFile filePath
            initialiseDatabase filePath
