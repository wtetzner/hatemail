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

{-# LANGUAGE OverloadedStrings #-}

module Parse where
import Command
import qualified Data.Attoparsec.ByteString.Lazy as AZ
--import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as BZ
import qualified Data.ByteString.Char8 as BS
import Data.Char (ord, chr)
import Data.Attoparsec.Combinator
--import Data.ByteString.Lazy.Char8
--import Data.ByteString.Char8

isSP c = c == ' '
sp = C8.satisfy isSP

isCTL c = ((ord c) >= 0x00 && (ord c) <= 0x1F) || c == (chr 0x7F)

isListWildcard c = c == '%' || c == '*'
listWildcard = C8.satisfy isListWildcard

isDQUOTE c = c == '"'
dquote = C8.satisfy isDQUOTE

isQuotedSpecial c = c == '\\' || (isDQUOTE c)
quotedSpecial = C8.satisfy isQuotedSpecial

isRespSpecial c = c == ']'

isAtomSpecial c = c == '(' || c == ')' || c == '{' || (isSP c)
                  || (isCTL c) || (isListWildcard c) || (isQuotedSpecial c)
                  || (isRespSpecial c)

isAtomChar c = (not $ isAtomSpecial c) && ((ord c) >= 0x01) && ((ord c) <= 0x7F)

isAStringChar c = (isAtomChar c) || (isRespSpecial c)
astringChar = C8.satisfy isAStringChar

atom = do text <- C8.takeWhile1 isAtomChar
          return $ IAtom $ BS.unpack text

isTextChar c = c /= '\r' && c /= '\n'

textChar = C8.satisfy isTextChar

isQChar c = (isTextChar c) && (not $ isQuotedSpecial c)

qchar = C8.satisfy isQChar
quotedEscaped = do C8.string "\\"
                   chr <- quotedSpecial
                   return chr

quotedChar = choice [qchar, quotedEscaped]

quotedText = do dquote
                chars <- many1 quotedChar
                dquote
                return chars
quotedEmpty = do dquote
                 dquote
                 return ""
quoted = choice [quotedText, quotedEmpty]

number = do num <- many1 C8.digit
            return $ INumber (read num :: Int)

crlf = do C8.string "\r\n"
literal = do C8.string "{"
             (INumber num) <- number
             C8.string "}"
             crlf
             text <- C8.take num
             return $ BS.unpack text

string = do text <- choice [literal, quoted]
            return $ IString text

nil = do C8.stringCI "NIL"
         return NIL

nstring = choice [nil, string]

command = choice [nstring, atom]

isTagChar c = (isAStringChar c) && (c /= '+')

tagged = do tag <- many1 $ C8.satisfy isTagChar
            sp
            cmd <- command
            return $ Tagged tag cmd

untagged = do C8.string "*"
              sp
              cmd <- command
              return $ Untagged cmd

imapCommand = do cmd <- choice [tagged, untagged]
                 crlf
                 return cmd
