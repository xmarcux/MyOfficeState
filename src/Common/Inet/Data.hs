{-|
Module      : Common.Inet.Data
Description : Data structures related to communication
Copyright   : (C) Marcus Pedersén, 2014

License     : GPLv3
Maintainer  : marcux@marcux.org
Stability   : Stable
Portability : Portable
Version     : v1.0.0

Module contains data structures related to communication
and is shared between server and client.
-}

module Common.Inet.Data where

{-|
  Data structure representing errors in
  communication between server and client.
-}
data InetError 
  -- | NoError is returned when request is processed correctly.
  = NoError

  -- | UserParseError is returned when a bad formated user xml is parsed.
  | UserParseError
  
  -- | UserDoesNotExistError is returned when a request for a user that does not exist is sent.
  | UserDoesNotExistError

  -- | ServiceError is returned when client askes for unknown service.
  | ServiceError
  deriving(Eq, Show, Read)
