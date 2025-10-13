module Hydra.Lightning.Scenario (
  main,
)
where

import Network.WebSockets (receiveDataMessage, runClient)

main :: IO ()
main = do
  runClient "127.0.0.1" 4001 "/?history=no" $ \aliceConn -> do
    runClient "127.0.0.1" 4002 "/?history=no" $ \bobConn -> do
      runClient "127.0.0.1" 4003 "/?history=no" $ \carolConn1 -> do
        runClient "127.0.0.1" 4004 "/?history=no" $ \carolConn2 -> do
          receiveDataMessage aliceConn >>= print
          receiveDataMessage bobConn >>= print
          receiveDataMessage carolConn1 >>= print
          receiveDataMessage carolConn2 >>= print
