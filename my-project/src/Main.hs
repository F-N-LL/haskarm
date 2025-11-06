module Main where

import System.Process (readProcess)
import Data.Aeson (decode, Value(..), Object)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Aeson.Key (fromString)
import Control.Monad (when)
import Control (sendAngle)
import Control.Concurrent (threadDelay)
import Data.Scientific (toRealFloat)
import Control.Exception (try, SomeException)
import Data.Text (Text, unpack)

parseAngle :: String -> Maybe (Maybe Int)
parseAngle str =
    case decode (BLC.pack str) :: Maybe Value of
        Just (Object o) ->
            case KM.lookup (fromString "angle") o of
                Just (Number n) -> Just (Just (round (toRealFloat n :: Double)))
                Just Null -> Just Nothing
                _ -> Nothing
        _ -> Nothing

data Config = Config
    { esp32Ip :: String
    , pythonPath :: String
    } deriving Show

parseConfig :: Value -> Maybe Config
parseConfig (Object o) = do
    String ip <- KM.lookup (fromString "esp32_ip") o
    String path <- KM.lookup (fromString "python_path") o
    return $ Config (unpack ip) (unpack path)
parseConfig _ = Nothing

loadConfig :: IO (Maybe Config)
loadConfig = do
    content <- BL.readFile "config.json"
    case decode content of
        Just val -> return $ parseConfig val
        Nothing -> return Nothing

main :: IO ()
main = do
    configMaybe <- loadConfig
    case configMaybe of
        Nothing -> putStrLn "Error: config.json not found. Copy config.example.json to config.json and configure."
        Just config -> loop Nothing config
  where
    scriptPath = "vision/vision.py"
    
    loop prevAngle config = do
        result <- try (readProcess (pythonPath config) [scriptPath] "") :: IO (Either SomeException String)
        case result of
            Left err -> do
                putStrLn $ "Error Python: " ++ show err
                threadDelay 500000
                loop prevAngle config
            Right output ->
                case parseAngle output of
                    Just (Just a) -> do
                        let prev = maybe (-1) id prevAngle
                        when (abs (a - prev) > 5) $ do
                            sendAngle (esp32Ip config) a
                            putStrLn $ "Servo moved to: " ++ show a ++ "°"
                        threadDelay 200000
                        loop (Just a) config
                    _ -> loop prevAngle config
