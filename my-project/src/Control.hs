module Control (sendAngle) where

import Network.HTTP.Simple (httpNoBody, parseRequest_)

sendAngle :: String -> Int -> IO ()
sendAngle ip angle = do
    let url = "http://" ++ ip ++ "/servo?angle=" ++ show angle
    _ <- httpNoBody (parseRequest_ url)
    pure ()
