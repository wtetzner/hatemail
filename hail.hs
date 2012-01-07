{--
This file is part of hatemail.

hatemail is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

hailmail is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with hatemail.  If not, see <http://www.gnu.org/licenses/>.
--}

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
