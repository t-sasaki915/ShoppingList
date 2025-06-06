module AppConfig.Loader (loadConfig) where

import           AppConfig          (AppConfig)
import qualified AppConfig.Resource as Res
import qualified Data.ByteString    as BS
import           Data.Yaml          (decodeEither', prettyPrintParseException)
import           System.Directory   (doesFileExist)
import           Text.Printf        (printf)

writeDefaultConfigFile :: FilePath -> IO ()
writeDefaultConfigFile = flip BS.writeFile Res.defaultConfigFile

readConfigFile :: FilePath -> IO BS.ByteString
readConfigFile filePath =
    doesFileExist filePath >>= \case
        True -> do
            putStrLn $ printf "Acquired '%s' as the configuration." filePath
            BS.readFile filePath

        False -> do
            putStrLn $ printf "Could not find '%s'. Creating..." filePath
            writeDefaultConfigFile filePath
            readConfigFile filePath

loadConfig :: FilePath -> IO AppConfig
loadConfig filePath = do
    rawConfig <- readConfigFile filePath
    case decodeEither' rawConfig of
        Right conf -> return conf
        Left err   -> error (prettyPrintParseException err)
