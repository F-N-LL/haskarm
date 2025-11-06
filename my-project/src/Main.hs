module Main where

import System.Process (readProcess)
import Data.Aeson (decode, Value(..), Object)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad (when)
import Control (sendAngle)
import Control.Concurrent (threadDelay)

parseAngle :: String -> Maybe (Maybe Int)
parseAngle str =
    case decode (BL.pack str) :: Maybe Value of
        Just (Object o) ->
            case KM.lookup "angle" o of
                Just (Number n) -> Just (Just (round n))
                Just Null -> Just Nothing
                _ -> Nothing
        _ -> Nothing

main :: IO ()
main = loop Nothing
    where
        ip = "esp32cam.local"
        loop prevAngle = do
            output <- readProcess "python3" ["vision/vision.py"] ""
            case parseAngle output of
                Just (Just a) -> do
                    let prev = maybe (-1) id prevAngle
                    when (abs (a - prev) > 2) $ sendAngle ip a
                    threadDelay 200000
                    loop (Just a)
                _ -> loop prevAngle

