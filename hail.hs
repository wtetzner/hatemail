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
  putStr "Connecting..."
  hFlush stdout
  imapclient <- connectClient hostname port
  success <- handshake imapclient
  putStrLn "done."
  putStr "Receiving line..."
  hFlush stdout
  line <- recvData imapclient
  putStrLn "done."
  return line
