{-- Copyright (C) 2011,2012 Walter Tetzner

This file is part of hatemail.

hatemail is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

hailmail is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with hatemail.  If not, see <http://www.gnu.org/licenses/>.  --}

module Connection where
import Network
import Network.TLS
import Network.TLS.Extra
import Crypto.Random
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BZ
import qualified Data.ByteString.Char8 as BS
import Control.Monad.State
import Text.Printf

type Hostname = String
type Port = String
type Username = String
type Password = String
type SSLConnection = (TLSCtx Handle)
data ConnectionInfo = Auth Bool Hostname Port Username Password
                    | NoAuth Bool Hostname Port

data IMAPConnection = SSLConn (TLSCtx Handle)
                    | Conn Handle

connectClient :: Bool -> String -> String -> IO IMAPConnection
connectClient True hostname port = do
  g <- newGenIO :: IO SystemRandom
  client <- connectionClient hostname port defaultParams { pCiphers = ciphersuite_all } g
  success <- handshake client
  return $ SSLConn client
connectClient False hostname port = do
  h <- connectTo hostname $ PortNumber $ fromIntegral $ (read port::Int)
  return $ Conn h

readText :: IMAPConnection -> IO BZ.ByteString
readText (SSLConn ctx) = do
  str <- recvData ctx
  return $ BZ.fromChunks [str]
readText (Conn h) = do
  BZ.hGet h 4096

readTextNoBlock :: IMAPConnection -> IO BZ.ByteString
readTextNoBlock (SSLConn ctx) = do
  let conn = ctxConnection ctx
  ready <- hReady conn
  if ready
    then do str <- recvData ctx
            return $ BZ.fromChunks [str]
    else return BZ.empty
readTextNoBlock (Conn h) = do
  BZ.hGetNonBlocking h 4096

keepReadingText :: IMAPConnection -> IO BZ.ByteString
keepReadingText conn = do
  text <- readText conn
  moreText <- readLazy
  return $ BZ.append text moreText
      where readLazy = do
              text <- readTextNoBlock conn
              if BZ.null text then return text else do
                                                 moreText <- readLazy
                                                 return $ BZ.append text moreText

writeText :: IMAPConnection -> BZ.ByteString -> IO ()
writeText (SSLConn ctx) text = sendData ctx text
writeText (Conn h) text = BZ.hPut h text

imapLogin :: IMAPConnection -> String -> String -> IO ()
imapLogin conn username password = do
  writeText conn $ BZ.pack $ printf "a001 login %s %s\r\n" username password

disconnectClient :: IMAPConnection -> IO ()
disconnectClient (SSLConn client) = do
  bye client
  let handle = ctxConnection client
  hClose handle
disconnectClient (Conn client) = do
  hClose client

imapConnect :: ConnectionInfo -> IO IMAPConnection
imapConnect (Auth isSSL hostname port username password) = do
    imapclient <- connectClient isSSL hostname port
    imapLogin imapclient username password
    return imapclient
imapConnect (NoAuth isSSL hostname port) = connectClient isSSL hostname port
