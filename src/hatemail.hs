{--
Copyright (C) 2011,2012 Walter Tetzner

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
import Connection
import Network.TLS
import Network.TLS.Extra
import Crypto.Random
import System.IO
import qualified Data.ByteString.Lazy as BZ
import qualified Data.ByteString.Internal as BS
import System(getArgs)

main = do
  args <- getArgs
  doimap $ parseArgs args

parseArgs :: [String] -> IMAPConnect
parseArgs (hostname:port:username:password:args) = 
    IMAPConnect hostname port username password

doimap :: IMAPConnect -> IO ()
doimap imapconn = do
  putStr "Connecting..."
  hFlush stdout
  imapclient <- imapConnect imapconn
  putStrLn "done."
  putStr "Receiving line..."
  hFlush stdout
  line <- recvData imapclient
  putStrLn "done."
  BZ.putStrLn line
  -- putStr "Sending login..."
  -- hFlush stdout
  -- imapLogin imapclient username password
  -- putStrLn "sent."
  hFlush stdout
  line2 <- recvData imapclient
  BZ.putStrLn line2
