{-|
Module      : Parse of network streams
Description : Parse of streamed strings server/client
Copyright   : (C) Marcus Pederśen, 2014

License     : GPLv3
Maintainer  : marcux@marcux.org
Stability   : Stable
Portability : Portable
Version     : v1.0.0

This module parse String streams sent between server (moses)
and client (mos). Streams are built up with simple xml format.
-}

module Common.Inet.Parse where

import Data.List
import Common.Data

{-|
  Converts a User to an xml String.
  The xml format is simplyfied and looks like:
  <user>
      <nickname>nick</nickname>
      <username>user</username>
      <profession>engineer</profession>
      <location>New York</location>
      <gender>Male</gender>
  </user>

  The returned string is only a single line string
  without newlines and spaces. 
  All tags continues after each other in one long string.
-}
userToXml :: User -> String
userToXml u = "<user><nickname>" ++
              nickname u ++ 
              "</nickname><username>" ++
              username u ++
              "</username><profession>" ++
              profession u ++
              "</profession><location>" ++
              location u ++
              "</location><gender>" ++
              (show $ gender u) ++
              "</gender></user>"


{-|
  Converts an xml string to a User.
  If xml string has wrong format Nothing is returned.
-}
xmlToMaybeUser :: String -> Maybe User
xmlToMaybeUser xml 
    | ("<user>" `isPrefixOf` xml) &&
      ("</user>" `isSuffixOf` xml) &&
      ("<nickname>" `isInfixOf` xml) &&
      ("</nickname>" `isInfixOf` xml) &&
      ("<username>" `isInfixOf` xml) &&
      ("</username>" `isInfixOf` xml) &&
      ("<profession>" `isInfixOf` xml) &&
      ("</profession>" `isInfixOf` xml) &&
      ("<location>" `isInfixOf` xml) &&
      ("</location>" `isInfixOf` xml) &&
      ("<gender>" `isInfixOf` xml) &&
      ("</gender>" `isInfixOf` xml) = 
          Just (User (strBetweenTags "<nickname>" xml "</nickname>")
                     (strBetweenTags "<username>" xml "</username>")
                     (strBetweenTags "<profession>" xml "</profession>")
                     (strBetweenTags "<location>" xml "</location>")
                     (read $ strBetweenTags "<gender>" xml "</gender>"))
    | otherwise = Nothing


{-|
  Returns the string between starting tag 
  and ending tag. If tag does not exist 
  or string is formated wrong the empty
  String is returned.
-}
strBetweenTags :: String -> String -> String -> String
strBetweenTags (f:fstTag) (s:searchStr) endTag = 
    if (f:fstTag) `isPrefixOf` (s:searchStr)
      then strBetweenTags [] (drop (length (f:fstTag)) (s:searchStr)) endTag
      else strBetweenTags (f:fstTag) searchStr endTag

strBetweenTags [] (s:searchStr) endTag =
    if endTag `isPrefixOf` (s:searchStr)
      then []
      else s:(strBetweenTags [] searchStr endTag)

strBetweenTags (f:fstTag) [] endTag = []

strBetweenTags [] [] _ = []
