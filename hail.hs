module Main where
import Network.Socket
import Network.TLS
import Network.TLS.Extra
import Crypto.Random
import System.IO

port = "993"
hostname = "imap.gmail.com"

connectClient :: String -> String -> IO (TLSCtx Handle)
connectClient hostname port = do
  g <- newGenIO :: IO SystemRandom
  connectionClient hostname port defaultParams { pCiphers = ciphersuite_all } g

main = do
  imapclient <- connectClient hostname port
  line <- recvData imapclient
  return line

