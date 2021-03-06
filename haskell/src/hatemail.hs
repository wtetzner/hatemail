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
import Parse
import Network.TLS
import Network.TLS.Extra
import Crypto.Random
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BZ
import qualified Data.ByteString.Char8 as BS
import System.Environment(getArgs)
import Control.Concurrent

main = do
  args <- getArgs
  doimap $ parseArgs args

parseArgs :: [String] -> ConnectionInfo
parseArgs (hostname:port:username:password:args) = 
    Auth True hostname port username password

runCommand conn command = do
  putStrLn command
  writeText conn $ BZ.pack $ command ++ "\r\n"
  line <- keepReadingText conn
  -- line <- readText conn
  -- line2 <- readTextNoBlock conn
  -- line3 <- readTextNoBlock conn
  BZ.putStrLn line
  hFlush stdout

doimap :: ConnectionInfo -> IO ()
doimap imapconn = do
  putStr "Connecting..."
  hFlush stdout
  imapclient <- imapConnect imapconn
  putStrLn "done."
  putStr "Receiving line..."
  hFlush stdout
  line <- readText imapclient
  putStrLn "done."
  BZ.putStrLn line
  hFlush stdout
  line2 <- readText imapclient
  BZ.putStrLn line2
  hFlush stdout
  runCommand imapclient "a002 select INBOX"
  runCommand imapclient "a003 LIST \"\" *"
  runCommand imapclient "a004 NOOP"
  -- runCommand imapclient "a005 STORE 1:1 +FLAGS (\\Deleted)"
  -- runCommand imapclient "a006 EXPUNGE"
  runCommand imapclient "a007 SEARCH TEXT \"Privacy\""
  runCommand imapclient "a008 FETCH 1 full"
  disconnectClient imapclient
