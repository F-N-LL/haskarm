module Control (sendAngle) where

import Network.HTTP.Simple (httpNoBody, parseRequest_)
import Control.Exception (try, SomeException)

sendAngle :: String -> Int -> IO ()
sendAngle ip angle = do
    let url = "http://" ++ ip ++ "/servo?angle=" ++ show angle
    _ <- try (httpNoBody (parseRequest_ url)) :: IO (Either SomeException ())
    pure ()
