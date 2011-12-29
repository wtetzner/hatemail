module Main where
import Network.Socket
import Network.TLS
import Network.TLS.Extra
import Crypto.Random
import System.IO

port = "993"
hostname = "imap.gmail.com"

generator :: IO SystemRandom
generator = newGenIO

connectClient :: String -> String -> IO (TLSCtx Handle)
connectClient hostname port = do
  g <- generator
  connectionClient hostname port
                   TLSParams { pConnectVersion = SSL3
                             , pAllowedVersions = [SSL3]
                             , pCiphers = ciphersuite_all
                             , pCompressions = []
                             , pWantClientCert = False
                             , pUseSecureRenegotiation = True
                             , pUseSession = True
                             }
                   g

main = do
  imapclient <- connectClient hostname port
  line <- recvData imapclient
  return line

