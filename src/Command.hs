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

module Command where

type Flag = String

data RespTextCode = Alert
                  | BadCharset [String]
                  | Parse
                  | Capability [String]
                  | PermanentFlags [Flag]
                  | ReadOnly
                  | ReadWrite
                  | TryCreate
                  | UIDNext Int
                  | UIDValidity Int
                  | Unseen Int
                  | GenRespCode String (Maybe String)
                    deriving (Eq, Ord, Show)

data State = Ok | No | Bad | Bye | PreAuth
             deriving (Eq, Ord, Show)

type Mailbox = String
type StatusAtt = String

data MailboxList = MailboxList [Flag] (Maybe String) Mailbox
                   deriving (Eq, Ord, Show)

data MailboxData = Flags [Flag]
                 | List MailboxList
                 | LSub MailboxList
                 | Search [Int]
                 | Status Mailbox [(StatusAtt, Int)]
                 | Exists Int
                 | Recent Int
                   deriving (Eq, Ord, Show)

type Tag = String

data ContinueResponse = Base64 String
                      | ResponseText (Maybe RespTextCode) String
                        deriving (Eq, Ord, Show)

data Response = ResponseCond State (Maybe RespTextCode) String
                deriving (Eq, Ord, Show)

data IMAPResponse = Tagged Tag Response
                  | Untagged Response
                  | Continue ContinueResponse
                    deriving (Eq, Ord, Show)
