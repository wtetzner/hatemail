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
import Control.Applicative (many)
import qualified Data.Attoparsec.ByteString.Lazy as AZ
import qualified Data.Attoparsec.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as BZ
import qualified Data.ByteString.Char8 as BS
import Data.Char (ord, chr)
import Data.Attoparsec.Combinator

str s = do text <- C8.stringCI s
           return $ BS.unpack text

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

astring = choice [astr, atm]
    where astr = do text <- many1 astringChar
                    return text
          atm = do a <- atom
                   return a

atom = do text <- C8.takeWhile1 isAtomChar
          return $ BS.unpack text

isTextChar c = c /= '\r' && c /= '\n'
textChar = C8.satisfy isTextChar

text = many1 textChar

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


isDigitNZ c = (C8.isDigit c) && (c /= '0')
digitNZ = C8.satisfy isDigitNZ

number = do num <- many1 C8.digit
            return (read num :: Int)
nzNumber = do first <- digitNZ
              rest <- many1 C8.digit
              return (read (first : rest) :: Int)

flagExtension = do bslash <- C8.char '\\'
                   a <- atom
                   return $  bslash : a
flagKeyword = do a <- atom
                 return a
flag = do text <- choice [ str "\\Answered"
                        , str "\\Flagged"
                        , str "\\Deleted"
                        , str "\\Seen"
                        , str "\\Draft"
                        , flagKeyword
                        , flagExtension]
          return text
    where str s = do text <- C8.stringCI s
                     return $ BS.unpack s
flagFetch = choice [flag, recent]
  where recent = do text <- C8.string "\\Recent"
                    return "\\Recent"

flagPerm = choice [flag, ast]
    where ast = do text <- C8.string "\\*"
                   return $ BS.unpack text

authType = do text <- atom
              return text

capability = choice [auth, at]
    where auth = do C8.stringCI "AUTH="
                    atype <- authType
                    return atype
          at = do text <- atom
                  return text
capabilityData = do C8.stringCI "CAPABILITY"
                    caps <- sepBy1 capability sp
                    return $ Capability caps
respTextCode = do choice [ alert, badcharset, capabilityData
                         , parse, permFlags, readOnly, readWrite
                         , tryCreate, uidnext, uidValidity
                         , unseen]
    where alert = do str "ALERT"
                     return Alert
          parse = do str "PARSE"
                     return Parse
          readOnly = do str "READONLY"
                        return ReadOnly
          readWrite = do str "READWRITE"
                         return ReadWrite
          tryCreate = do str "TRYCREATE"
                         return TryCreate
          uidnext = do str "UIDNEXT"
                       sp
                       num <- nzNumber
                       return $ UIDNext num
          uidValidity = do str "UIDVALIDITY"
                           sp
                           num <- nzNumber
                           return $ UIDValidity num
          unseen = do str "UNSEEN"
                      sp
                      num <- nzNumber
                      return $ Unseen num
          atm = do text <- atom
                   thing <- option Nothing rest
                   return $ GenRespCode text thing
              where rest = do sp
                              txt <- text
                              return $ Just txt
          permFlags = do str "PERMANENTFLAGS"
                         sp
                         C8.char '('
                         flags <- sepBy flagPerm sp
                         C8.char ')'
                         return $ PermanentFlags flags
          badcharset = choice [bcslst, bcs]
          bcs = do str "BADCHARSET"
                   return $ BadCharset []
          bcslst = do str "BADCHARSET"
                      sp
                      C8.char '('
                      strs <- sepBy1 astring sp
                      C8.char ')'
                      return $ BadCharset strs
          str s = do text <- C8.stringCI s
                     return $ BS.unpack text

respText = do code <- option Nothing textCode
              txt <- ptext
              return $ (code, txt)
  where textCode = do C8.char '['
                      code <- respTextCode
                      C8.char ']'
                      sp
                      return $ Just code
        ptext = do txt <- text
                   return txt

responseState = do state <- choice [ok, no, bad]
                   return state
    where ok = do str "OK"
                  return Ok
          no = do str "NO"
                  return No
          bad = do str "BAD"
                   return Bad
respCondState = do state <- responseState
                   sp
                   (code, text) <- respText
                   return $ ResponseCond state code text

responseTagged = do t <- tag
                    sp
                    cond <- respCondState
                    crlf
                    return $ Tagged t cond

respCondBye = do state <- respState
                 sp
                 (code, text) <- respText
                 return $ ResponseCond state code text
    where respState = do state <- str "BYE"
                         return Bye

responseFatal = do C8.char '*'
                   sp
                   cond <- respCondBye
                   crlf
                   return $ Untagged cond

responseDone = choice [responseTagged, responseFatal]

continueRequest = do C8.char '+'
                     sp
                     resp <- choice [rtext, base64]
                     return resp
    where rtext = do (code, text) <- respText
                     return $ ResponseText code text

crlf = do C8.string "\r\n"
literal = do C8.string "{"
             num <- number
             C8.string "}"
             crlf
             text <- C8.take num
             return $ BS.unpack text

string = do text <- choice [literal, quoted]
            return text

nil = do C8.stringCI "NIL"
         return Nothing

nstring = choice [nil, mstr]
    where mstr = do text <- string
                    return $ Just text

isBase64Char c = (C8.isAlpha_ascii c) || (C8.isDigit c) || (c == '+')
                 || (c == '/')
base64Char = C8.satisfy isBase64Char

base64Terminal2 = do one <- base64Char
                     two <- base64Char
                     end <- C8.string "=="
                     return $ [one, two] ++ (BS.unpack end)
base64Terminal3 = do one <- base64Char
                     two <- base64Char
                     three <- base64Char
                     end <- C8.string "="
                     return $ [one, two, three] ++ (BS.unpack end)
base64Terminal = choice [base64Terminal2, base64Terminal3]

base64Chunk = do one <- base64Char
                 two <- base64Char
                 three <- base64Char
                 four <- base64Char
                 return $ [one, two, three, four]
base64 = do chunks <- many base64Chunk
            term <- option [] base64Terminal
            return $ Base64 $ (concat chunks) ++ term

isTagChar c = (isAStringChar c) && (c /= '+')

tag = many1 $ C8.satisfy isTagChar

serverResponse = do cmd <- choice [responseTagged]
                    return cmd
