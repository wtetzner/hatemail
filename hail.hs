module Main where
import Network.Socket
import Network.TLS
import Network.TLS.Extra
import Crypto.Random
import System.IO

port = "993"
hostname = "imap.googlemail.com"

-- connectIMAPsocket :: String -> Int -> Bool -> IO Stream
-- connectIMAPsocket hostname port useSSL = undefined

-- createSSLClient hostname port = client TLSParams { pConnectVersion = SSL3
--                                                  , pAllowedVersions = [SSL3]}

generator :: IO SystemRandom
generator = newGenIO

connectClient :: String -> String -> IO (TLSCtx Handle)
connectClient hostname port = do
  g <- generator
  connectionClient hostname port
                   TLSParams { pConnectVersion = SSL3
                             , pAllowedVersions = [SSL3]}
                   g

main = do
  addrinfos <-  getAddrInfo Nothing (Just hostname) (Just port)
  let imapaddr = head addrinfos
  sock <- socket (addrFamily imapaddr) Stream defaultProtocol
  setSocketOption sock KeepAlive 1
  connect sock (addrAddress imapaddr)
  --clientThing <- connectClient hostname port
  putStrLn $ show imapaddr

