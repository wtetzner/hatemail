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

module Response where

type Flag = String
type NString = Maybe String
type NList a = Maybe [a]
type NAddrList = Maybe [Address]

data RespTextCode = Alert
                  | BadCharset [String]
                  | Parse
                  | Capability [String]
                  | PermanentFlags [Flag]
                  | ReadOnly
                  | ReadWrite
                  | TryCreate
                  | UIDNext Integer
                  | UIDValidity Integer
                  | Unseen Integer
                  | GenRespCode String NString
                    deriving (Eq, Ord, Show)

data State = Ok | No | Bad | Bye | PreAuth
             deriving (Eq, Ord, Show)

type Mailbox = String
type StatusAtt = String

data MailboxList = MailboxList [Flag] NString Mailbox
                   deriving (Eq, Ord, Show)

data MBoxData = Flags [Flag]
              | List MailboxList
              | LSub MailboxList
              | Search [Integer]
              | Status Mailbox [(StatusAtt, Integer)]
              | Exists Integer
              | Recent Integer
                deriving (Eq, Ord, Show)

type AddrName = NString
type AddrAdl = NString
type AddrHost = NString
type AddrMailbox = NString

data Address = Address AddrName AddrAdl AddrMailbox AddrHost
               deriving (Eq, Ord, Show)

data Envelope = Envelope { envDate      :: NString,
                           envSubject   :: NString,
                           envFrom      :: NList Address,
                           envSender    :: NList Address,
                           envReplyTo   :: NList Address,
                           envTo        :: NList Address,
                           envCC        :: NList Address,
                           envBCC       :: NList Address,
                           envInReplyTo :: NList Address,
                           envMessageID :: NString }
                deriving (Eq, Ord, Show)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug
           | Sep | Oct | Nov | Dec
             deriving (Eq, Ord, Show)

type Hours = Integer
type Minutes = Integer
type Seconds = Integer
data Time = Time Hours Minutes Seconds
            deriving (Eq, Ord, Show)

type TimeZone = Integer

type Day = Integer
type Year = Integer
data DateTime = DateTime Day Month Year Time TimeZone
                deriving (Eq, Ord, Show)

data BodyExtension = BodyExtStr NString
                   | BodyExtNum Integer
                   | BodyExtension [BodyExtension]
                     deriving (Eq, Ord, Show)

data MediaType = Application
               | Audio
               | Image
               | Message
               | Video
               | MediaType String
                 deriving (Eq, Ord, Show)
type MediaSubtype = String

data MediaBasic = MediaBasic MediaType MediaSubtype
                  deriving (Eq, Ord, Show)

data BodyFieldEncoding = SevenBitEnc
                       | EightBitEnc
                       | BinaryEnc
                       | Base64Enc
                       | QuotedPrintableEnc
                       | BodyFieldEnc String
                         deriving (Eq, Ord, Show)

data BodyFields = BodyFields { bodyFieldParam  :: BodyFieldParams,
                               bodyFieldID     :: NString,
                               bodyFieldDesc   :: NString,
                               bodyFieldEnc    :: BodyFieldEncoding,
                               bodyFieldOctets :: Integer }
                  deriving (Eq, Ord, Show)

type BodyFieldLines = Integer

data BodyType = BodyTypeBasic MediaBasic BodyFields
              | BodyTypeMsg BodyFields Envelope Body BodyFieldLines
              | BodyTypeText MediaSubtype BodyFields BodyFieldLines
                deriving (Eq, Ord, Show)

type BodyFieldParams = NList (String, String)

type BodyFieldDsp = Maybe (String, BodyFieldParams)

data BodyFieldLang = BodyFieldLang NString
                   | BodyFieldLangs [String]
                     deriving (Eq, Ord, Show)

type BodyFieldLoc = NString

data BodyExt a = BodyExt a
               | BodyExtDsp a BodyFieldDsp
               | BodyExtLang a BodyFieldDsp BodyFieldLang
               | BodyExtLoc a BodyFieldDsp BodyFieldLang
                 [BodyExtension]
                 deriving (Eq, Ord, Show)

type BodyFieldMD5 = NString

type BodyExt1Part = BodyExt BodyFieldMD5
type BodyExtMPart = BodyExt BodyFieldParams

data Body = BodyType1Part BodyType (Maybe BodyExt1Part)
          | BodyTypeMPart [Body] MediaSubtype (Maybe BodyExtMPart)
            deriving (Eq, Ord, Show)

type HeaderFieldName = String
type HeaderList = [HeaderFieldName]

data SectionMsgText = Header
                    | HeaderFields HeaderList
                    | HeaderFieldsNot HeaderList
                    | Text
                      deriving (Eq, Ord, Show)

data SectionText = SectionText SectionMsgText
                 | MIME
                   deriving (Eq, Ord, Show)

type SectionPart = [Integer]

data SectionSpec = SectionMsgText SectionMsgText
                 | SectionPartText SectionPart (Maybe SectionText)
                   deriving (Eq, Ord, Show)

type Section = Maybe SectionSpec

type UniqueID = Integer

data MsgAtt = AttFlags [Flag]
            | AttEnvelope Envelope
            | AttInternalDate DateTime
            | RFC822Text NString
            | RFC822Header NString
            | RFC822 NString
            | RFC822Size Integer
            | BodyStructure Body
            | Body Section (Maybe Integer) NString
            | UID UniqueID
              deriving (Eq, Ord, Show)

data MessageData = Expunge Integer
                 | Fetch Integer [MsgAtt]
                   deriving (Eq, Ord, Show)

type Tag = String

data ContinueResponse = Base64 String
                      | ResponseText (Maybe RespTextCode) String
                        deriving (Eq, Ord, Show)

data Response = Response State (Maybe RespTextCode) String
              | MailboxData MBoxData
              | MessageData MessageData
              | CapabilityData [String]
                deriving (Eq, Ord, Show)

data IMAPResponse = Tagged Tag Response
                  | Untagged Response
                  | Continue ContinueResponse
                    deriving (Eq, Ord, Show)
