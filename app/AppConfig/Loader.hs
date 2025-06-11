module AppConfig.Loader (loadConfig) where

import qualified Data.ByteString    as BS
import           Data.String.Here   (i)
import           Data.Yaml          (decodeEither', prettyPrintParseException)
import           System.Directory   (doesFileExist)

import           AppConfig          (AppConfig)
import qualified AppConfig.Resource as Res

writeDefaultConfigFile :: FilePath -> IO ()
writeDefaultConfigFile = flip BS.writeFile Res.defaultConfigFile

readConfigFile :: FilePath -> IO BS.ByteString
readConfigFile filePath =
    doesFileExist filePath >>= \case
        True -> do
            putStrLn [i|Acquired '${filePath}' as the configuration.|]
            BS.readFile filePath

        False -> do
            putStrLn [i|Could not find '${filePath}'. Creating...|]
            writeDefaultConfigFile filePath
            readConfigFile filePath

loadConfig :: FilePath -> IO AppConfig
loadConfig filePath = do
    rawConfig <- readConfigFile filePath
    case decodeEither' rawConfig of
        Right conf -> return conf
        Left err   -> error (prettyPrintParseException err)
