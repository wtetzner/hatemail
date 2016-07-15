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
import Response
import Control.Applicative (many)
import qualified Data.Attoparsec.ByteString.Lazy as AZ
import qualified Data.Attoparsec.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as BZ
import qualified Data.ByteString.Char8 as BS
import Data.Char (ord, chr)
import Data.Attoparsec.Combinator
import Data.String.Utils

-- Case-insensitive string match, returning s
str s = do text <- C8.stringCI s
           return $ BS.unpack s

-- Case-insensitive string match, return t
strVal s t = do C8.stringCI s
                return t

isSP c = c == ' '
sp = C8.char ' '

-- Test for control characters
isCTL c = ((ord c) >= 0x00 && (ord c) <= 0x1F) || c == (chr 0x7F)

isListWildcard c = c == '%' || c == '*'
listWildcard = C8.satisfy isListWildcard

isDQUOTE c = c == '"'
dquote = C8.char '"'

isQuotedSpecial c = c == '\\' || (isDQUOTE c)
quotedSpecial = C8.satisfy isQuotedSpecial

isRespSpecial c = c == ']'

isAtomSpecial c = c == '(' || c == ')' || c == '{' || (isSP c)
                  || (isCTL c) || (isListWildcard c)
                  || (isQuotedSpecial c) || (isRespSpecial c)

isAtomChar c = (not $ isAtomSpecial c) && ((ord c) >= 0x01)
               && ((ord c) <= 0x7F)

isAStringChar c = (isAtomChar c) || (isRespSpecial c)
astringChar = C8.satisfy isAStringChar

astring = choice [astr, string]
    where astr = do text <- many1 astringChar
                    return text

atom = do text <- C8.takeWhile1 isAtomChar
          return $ BS.unpack text

isChar c = ((ord c) >= 0x01) && ((ord c) <= 0x7F)
char = C8.satisfy isChar

isTextChar c = c /= '\r' && c /= '\n' && (isChar c)
textChar = C8.satisfy isTextChar

text = many1 textChar

isQChar c = (isTextChar c) && (not $ isQuotedSpecial c)
qchar = C8.satisfy isQChar

quotedEscaped = do C8.char '\\'
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
            return (read num :: Integer)
nzNumber = do first <- digitNZ
              rest <- many C8.digit
              return (read (first : rest) :: Integer)

flagExtension = do bslash <- C8.char '\\'
                   a <- atom
                   return $  bslash : a
flagKeyword = do a <- atom
                 return a
flag = do text <- choice [str "\\Answered",
                         str "\\Flagged",
                         str "\\Deleted",
                         str "\\Seen",
                         str "\\Draft",
                         flagKeyword,
                         flagExtension]
          return text
flagFetch = choice [flag, recent] AZ.<?> "flag-fetch"
  where recent = do text <- C8.string "\\Recent"
                    return "\\Recent"

flagPerm = choice [flag, ast]
    where ast = do text <- C8.string "\\*"
                   return $ BS.unpack text

authType = do text <- atom
              return text

-- TODO: Figure out what to do with AUTH=
capability = choice [auth, at]
    where auth = do C8.stringCI "AUTH="
                    atype <- authType
                    return atype
          at = do text <- atom
                  return text
capabilityData = do C8.stringCI "CAPABILITY"
                    sp
                    caps <- sepBy1 atom sp
                    return $ Capability caps
respTextCode = do choice [alert, badcharset, capabilityData,
                          parse, permFlags, readOnly, readWrite,
                          tryCreate, uidnext, uidValidity,
                          unseen]
    where alert = do str "ALERT"
                     return Alert
          parse = do str "PARSE"
                     return Parse
          readOnly = do str "READ-ONLY"
                        return ReadOnly
          readWrite = do str "READ-WRITE"
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
                   return $ Response state code text

respCondBye = do state <- respState
                 sp
                 (code, text) <- respText
                 return $ Response state code text
    where respState = do state <- str "BYE"
                         return Bye

responseFatal = do C8.char '*'
                   sp
                   cond <- respCondBye
                   crlf
                   return $ Untagged cond

crlf = do C8.string "\r\n"
literal = do C8.string "{"
             num <- number
             C8.string "}"
             crlf
             text <- C8.take $ fromIntegral num
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

flagList = do C8.char '('
              flags <- sepBy1 flag sp
              C8.char ')'
              return flags

flags = do str "FLAGS"
           sp
           flgs <- flagList
           return $ Flags flgs

mailbox = choice [inbox, astring]
    where inbox = str "INBOX"

mbxListOflag = choice [str "\\Noinferiors", flagExtension]
mbxListSflag = choice [str "\\Noselect",
                       str "\\Marked",
                       str "\\Unmarked"]
mbxListFlags = sepBy1 mflag sp
    where mflag = choice [mbxListSflag, mbxListOflag]

mailboxList = do C8.char '('
                 mflags <- option [] mbxListFlags
                 C8.char ')'
                 sp
                 qchr <- choice [chr, nil]
                 sp
                 mbx <- mailbox
                 return $ MailboxList mflags qchr mbx
    where chr = do dquote
                   c <- quotedChar
                   dquote
                   return $ Just [c]

statusAtt = choice [str "MESSAGES",
                    str "RECENT",
                    str "UIDNEXT",
                    str "UIDVALIDITY",
                    str "UNSEEN"]

statusAttList = sepBy1 sattPair sp
    where sattPair = do stat <- statusAtt
                        sp
                        num <- number
                        return (stat, num)

mailboxData = choice [flags, list, lsub, search, status,
                      exists, recent]
  where flags = do str "FLAGS"
                   sp
                   flgs <- flagList
                   return $ Flags flgs
        list = do str "LIST"
                  sp
                  mblist <- mailboxList
                  return $ List mblist
        lsub = do str "LSUB"
                  sp
                  mblist <- mailboxList
                  return $ LSub mblist
        search = do str "SEARCH"
                    ns <- option [] nums
                    return $ Search ns
        nums = do sp
                  ns <- (sepBy1 nzNumber sp)
                  return ns
        status = do str "STATUS"
                    sp
                    mbx <- mailbox
                    sp
                    C8.char '('
                    lst <- option [] statusAttList
                    C8.char ')'
                    return $ Status mbx lst
        exists = do num <- number
                    sp
                    str "EXISTS"
                    return $ Exists num
        recent = do num <- number
                    sp
                    str "RECENT"
                    return $ Recent num

addrName = nstring
addrAdl = nstring
addrHost = nstring
addrMailbox = nstring
address = do C8.char '('
             name <- addrName
             sp
             adl <- addrAdl
             sp
             mailbox <- addrMailbox
             sp
             host <- addrHost
             C8.char ')'
             return $ Address name adl mailbox host

addrs = do C8.char '('
           ads <- many1 address
           C8.char ')'
           return $ Just ads
naddrs = choice [addrs, nil]

envelope = do C8.char '('
              date <- nstring
              sp
              subject <- nstring
              sp
              from <- naddrs
              sp
              sender <- naddrs
              sp
              replyTo <- naddrs
              sp
              to <- naddrs
              sp
              cc <- naddrs
              sp
              bcc <- naddrs
              sp
              inReplyTo <- naddrs
              sp
              messageID <- nstring
              C8.char ')'
              return $ Envelope { envDate = date,
                                  envSubject = subject,
                                  envFrom = from,
                                  envSender = sender,
                                  envReplyTo = replyTo,
                                  envTo = to,
                                  envCC = cc,
                                  envBCC = bcc,
                                  envInReplyTo = inReplyTo,
                                  envMessageID = messageID }

bodyExtension = choice [extStr, extNum, extList]
    where extList = do C8.char '('
                       exts <- sepBy1 bodyExtension sp
                       C8.char ')'
                       return $ BodyExtension exts
          extStr = do s <- nstring
                      return $ BodyExtStr s
          extNum = do num <- number
                      return $ BodyExtNum num

mediaBasic = do mediaType <- choice [known, custom]
                sp
                subType <- string
                return $ MediaBasic mediaType subType
  where known = do C8.char '"'
                   val <- choice [strVal "APPLICATION" Application,
                                 strVal "AUDIO" Audio,
                                 strVal "IMAGE" Image,
                                 strVal "MESSAGE" Message,
                                 strVal "VIDEO" Video]
                   C8.char '"'
                   return val
        custom = do text <- string
                    return $ MediaType text

bodyFieldPrm = choice [mult, nil]
    where param = do left <- string
                     sp
                     right <- string
                     return $ (left, right)
          mult = do C8.char '('
                    items <- sepBy1 param sp
                    C8.char ')'
                    return $ Just items

bodyFldEnc = choice [known, custom]
    where val = choice [strVal "7BIT" SevenBitEnc,
                        strVal "8BIT" EightBitEnc,
                        strVal "BINARY" BinaryEnc,
                        strVal "BASE64" Base64Enc,
                        strVal "QUOTED-PRINTABLE" QuotedPrintableEnc]
          known = do C8.char '"'
                     v <- val
                     C8.char '"'
                     return v
          custom = do text <- string
                      return $ BodyFieldEnc text

bodyFields = do param <- bodyFieldPrm
                sp
                id <- nstring
                sp
                desc <- nstring
                sp
                enc <- bodyFldEnc
                sp
                oct <- number
                return $ BodyFields { bodyFieldParam = param,
                                      bodyFieldID = id,
                                      bodyFieldDesc = desc,
                                      bodyFieldEnc = enc,
                                      bodyFieldOctets = oct }

bodyTypeBasic = do media <- mediaBasic
                   sp
                   flds <- bodyFields
                   return $ BodyTypeBasic media flds

bodyTypeMsg = do str "\"MESSAGE\""
                 sp
                 str "\"RFC822\""
                 sp
                 flds <- bodyFields
                 sp
                 env <- envelope
                 sp
                 bdy <- body
                 sp
                 lines <- number
                 return $ BodyTypeMsg flds env bdy lines

bodyTypeText = do str "\"TEXT\""
                  sp
                  sub <- string
                  sp
                  flds <- bodyFields
                  sp
                  lines <- number
                  return $ BodyTypeText sub flds lines

bodyExtMD5 = do text <- nstring
                return $ BodyExt text

bodyExtDsp a = do x <- a
                  sp
                  extDsp <- choice [dsp, nil]
                  return $ BodyExtDsp x extDsp
    where dsp = do C8.char '('
                   text <- string
                   sp
                   params <- bodyFieldPrm
                   C8.char ')'
                   return $ Just (text, params)

bodyExtLang a = do (BodyExtDsp x params) <- bodyExtDsp a
                   sp
                   lang <- bodyFldLang
                   return $ BodyExtLang x params lang
    where bodyFldLang = choice [text, lst]
          text = do txt <- nstring
                    return $ BodyFieldLang txt
          lst = do C8.char '('
                   strs <- sepBy1 string sp
                   C8.char ')'
                   return $ BodyFieldLangs strs

bodyExtLoc a = do (BodyExtLang x params lang) <- bodyExtLang a
                  sp
                  loc <- nstring
                  bdy <- option [] exts
                  return $ BodyExtLoc x params lang bdy
    where exts = do sp
                    lst <- sepBy1 bodyExtension sp
                    return lst

bodyExt1Part = choice [bodyExtLoc nstring,
                       bodyExtLang nstring,
                       bodyExtDsp nstring,
                       bodyExtMD5]

bodyExtMPart = choice [bodyExtLoc bodyFieldPrm,
                       bodyExtLang bodyFieldPrm,
                       bodyExtDsp bodyFieldPrm,
                       prm]
    where prm = do params <- bodyFieldPrm
                   return $ BodyExt params

bodyType1Part = do btype <- choice [bodyTypeText,
                                   bodyTypeMsg,
                                   bodyTypeBasic]
                   ex <- option Nothing ext
                   return $ BodyType1Part btype ex
    where ext = do sp
                   ex <- bodyExt1Part
                   return $ Just ex

bodyTypeMPart = do bdy <- many1 body
                   sp
                   msub <- string
                   ex <- option Nothing ext
                   return $ BodyTypeMPart bdy msub ex
    where ext = do sp
                   ex <- bodyExtMPart
                   return $ Just ex

body = do C8.char '('
          bdy <- choice [bodyType1Part, bodyTypeMPart]
          C8.char ')'
          return bdy

headerList = do C8.char '('
                names <- sepBy1 astring sp
                C8.char ')'
                return names

sectionMsgText = choice [strVal "HEADER" Header,
                         fields,
                         fieldsNot,
                         strVal "TEXT" Text]
  where fields = do str "HEADER.FIELDS"
                    lst <- headerList
                    return $ HeaderFields lst
        fieldsNot = do str "HEADER.FIELDS.NOT"
                       lst <- headerList
                       return $ HeaderFieldsNot lst

sectionPart = do nums <- sepBy1 nzNumber (C8.char '.')
                 return nums

sectionText = choice [msgText, strVal "MIME" MIME]
  where msgText = do txt <- sectionMsgText
                     return $ SectionText txt

sectionSpec = do choice [msgText, part]
  where msgText = do txt <- sectionMsgText
                     return $ SectionMsgText txt
        part = do spart <- sectionPart
                  text <- option Nothing txt
                  return $ SectionPartText spart text
        txt = do C8.char '.'
                 stext <- sectionText
                 return $ Just stext

section = do C8.char '['
             spec <- option Nothing secSpec
             C8.char ']'
             return spec
    where secSpec = do spec <- sectionSpec
                       return $ Just spec

msgAttDynamic = do str "FLAGS"
                   sp
                   C8.char '('
                   flgs <- sepBy flagFetch sp
                   C8.char ')'
                   return $ AttFlags flgs

twoDig = do dig1 <- C8.digit
            dig2 <- C8.digit
            return (read [dig1, dig2] :: Integer)

fourDig = do dig1 <- C8.digit
             dig2 <- C8.digit
             dig3 <- C8.digit
             dig4 <- C8.digit
             return (read [dig1, dig2, dig3, dig4] :: Integer)

time = do hour <- twoDig
          C8.char ':'
          min <- twoDig
          C8.char ':'
          sec <- twoDig
          return $ Time hour min sec

timeZone = do sign <- choice [C8.char '+', C8.char '-']
              hours <- twoDig
              mins <- twoDig
              let hourMins = hours * 60
              let time = hourMins + mins
              return $ if sign == '-' then time * (-1) else time

dateTime = do C8.char '"'
              day <- dateDay
              C8.char '-'
              month <- choice [strVal "Jan" Jan,
                              strVal "Feb" Feb,
                              strVal "Mar" Mar,
                              strVal "Apr" Apr,
                              strVal "May" May,
                              strVal "Jun" Jun,
                              strVal "Jul" Jul,
                              strVal "Aug" Aug,
                              strVal "Sep" Sep,
                              strVal "Oct" Oct,
                              strVal "Nov" Nov,
                              strVal "Dec" Dec]
              C8.char '-'
              year <- fourDig
              sp
              tm <- time
              sp
              zn <- timeZone
              C8.char '"'
              return $ DateTime day month year tm zn
    where dateDay1 = do sp
                        dig <- C8.digit
                        return (read [dig] :: Integer)
          dateDay = choice [dateDay1, twoDig]

msgAttStatic = choice [env, date, head, text, size, bstruct,
                       sect, id]
    where env = do str "ENVELOPE"
                   sp
                   envl <- envelope
                   return $ AttEnvelope envl
          date = do str "INTERNALDATE"
                    sp
                    dt <- dateTime
                    return $ AttInternalDate dt
          head = do str "RFC822.HEADER"
                    sp
                    txt <- nstring
                    return $ RFC822Header txt
          text = do str "RFC822.TEXT"
                    sp
                    txt <- nstring
                    return $ RFC822Text txt
          size = do str "RFC822.SIZE"
                    sp
                    num <- number
                    return $ RFC822Size num
          bstruct = do choice [str "BODYSTRUCTURE", str "BODY"]
                       sp
                       bdy <- body
                       return $ BodyStructure bdy
          sect = do str "BODY"
                    sec <- section
                    num <- option Nothing numb
                    sp
                    text <- nstring
                    return $ Body sec num text
          id = do str "UID"
                  sp
                  uid <- nzNumber
                  return $ UID uid
          numb = do C8.char '<'
                    num <- number
                    C8.char '>'
                    return $ Just num

msgAtt = do C8.char '('
            atts <- sepBy1 (choice [msgAttDynamic, msgAttStatic]) sp
            C8.char ')'
            return atts

messageData = do num <- nzNumber
                 sp
                 mdata <- choice [exp num, fetch num]
                 return mdata
    where exp num = do str "EXPUNGE"
                       return $ Expunge num
          fetch num = do str "FETCH"
                         sp
                         msg <- msgAtt
                         return $ Fetch num msg

responseData = do C8.char '*'
                  sp
                  resp <- choice [respCondState,
                                 respCondBye,
                                 mboxData,
                                 msgData,
                                 capData]
                  crlf
                  return $ Untagged resp
    where mboxData = do mdata <- mailboxData
                        return $ MailboxData mdata
          msgData = do msg <- messageData
                       return $ MessageData msg
          capData = do (Capability strs) <- capabilityData
                       return $ CapabilityData strs

responseTagged = do t <- tag
                    sp
                    cond <- respCondState
                    crlf
                    return $ Tagged t cond

responseDone = choice [responseTagged, responseFatal]

continueRequest = do C8.char '+'
                     sp
                     resp <- choice [rtext, base64]
                     crlf
                     return $ Continue resp
    where rtext = do (code, text) <- respText
                     return $ ResponseText code text

serverResponse = choice [responseTagged C8.<?> "Response Tagged",
                         responseData C8.<?> "Response Data",
                         responseDone C8.<?> "Response Done",
                         continueRequest C8.<?> "Continue Request"]

parseFile filename = do text <- readFile filename
                        return $ AZ.parse serverResponse $ BZ.pack $
                                   (replace "\n" "\r\n" text) -- ++ "\r\n"
