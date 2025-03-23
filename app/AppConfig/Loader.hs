module AppConfig.Loader (loadConfig) where

import           AppConfig          (AppConfig)
import           AppConfig.Resource (defaultConfigFile)
import qualified Data.ByteString    as BS
import           Data.Yaml          (decodeEither', prettyPrintParseException)
import           System.Directory   (doesFileExist)
import           Text.Printf        (printf)

writeDefaultConfigFile :: FilePath -> IO ()
writeDefaultConfigFile = flip BS.writeFile defaultConfigFile

readConfigFile :: FilePath -> IO BS.ByteString
readConfigFile filePath =
    doesFileExist filePath >>= \case
        True -> do
            putStrLn $ printf "Acquired '%s' as the configuration." filePath
            BS.readFile filePath
        False -> do
            putStrLn $ printf "Could not find '%s'. Using default configurations." filePath
            writeDefaultConfigFile filePath
            return defaultConfigFile

loadConfig :: FilePath -> IO AppConfig
loadConfig filePath = do
    rawConfig <- readConfigFile filePath
    case decodeEither' rawConfig of
        Right conf -> return conf
        Left err   -> error (prettyPrintParseException err)
