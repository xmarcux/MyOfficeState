{-|
Module      : Common.Data
Description : Common data structures
Copyright   : (C) Marcus Pedersén, 2014

License     : GPLv3
Maintainer  : marcux@marcux.org
Stability   : Stable
Portability : Portable
Version     : v1.0.0

This module contains common data structures
shared by both server and client program.
-}

module Common.Data where

{-|
  Represent data structure for a user in the system.
-}
data User = User { nickname   :: String
                 , username   :: String
                 , profession :: String
                 , location   :: String
                 , gender     :: Gender
                 } deriving (Show, Eq, Read)

{-|
  Data structure represent the gender and is 
  used in the User data structure.
-}
data Gender = Male | Female deriving (Show, Eq, Read)


{-| 
  Data structure Status represent the different
  status that a user can be in.
-}
data Status = Offline
            | NotInTheOffice
            | Available
            | OnThePhone
            | VideoConference
            | InAMeeting
            | Busy
            | DoNotDisurb
            deriving (Eq, Show, Read)

{-|
  Makes a list of users from a
  list of strings.
  Strings must be formated the same
  way you get a string with show User.
-}
mkUserListFromStrList :: [String] -> [User]
mkUserListFromStrList = map read

