{-|
Module      : Start of program
Description : Server side program
Copyright   : (C) Marcus Pedersén, 2014

License     : GPLv3
Maintainer  : marcux@marcux.org
Stability   : Exprimental
Portability : Portable
Version     : v0.0.1-alpha

This program is the server side of MyOfficeState.
Keeps track of the states for users and report
state to client program.
-}

module Main where

import Network.Socket
import Data.List
import Common.Inet.Data
import Common.Inet.Parse

{-|
  Main function for application moses.
  Execution of program starts here.
-}
main :: IO ()
main = do 
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 11211 iNADDR_ANY)
    listen sock 2
    connectLoop sock

{-|
  Function accepts a connection
  and handles the connection.
-}
connectLoop :: Socket -> IO ()
connectLoop sock = do
    conn <- accept sock
    readWriteConn conn
    connectLoop sock

{-|
  Takes care of reading and writing
  to a connected client.
-}
readWriteConn :: (Socket, SockAddr) -> IO ()
readWriteConn (sock, iaddr) = do
    putStrLn $ "Client connected to server: " ++ (show iaddr)
    recvmsg <- recv sock 4096
    putStrLn "Message recieved from client:"
    putStrLn . show $ parseNetService recvmsg
    send sock "Connected to moses!\n"
    close sock


{-|
  Parse string from client for supported services.
  If parsing fails ServiceError is returned,
  otherwise parse will continue.
  Supported services:

  newUser      - Client registers a new user at server. User string is supplied.
  updateUser   - Client wants to update its user information. User string is supplied.
  getUser      - Returns the requested user string. User nickname should be supplied as well.
  status       - Returns status for all registered users to client.
  updateStatus - Client updates its status. Status is supplied.

-}
parseNetService :: String -> InetError
parseNetService iStr 
    | "newUser "      `isPrefixOf` iStr = NoError
    | "updateUser "   `isPrefixOf` iStr = NoError
    | "getUser "      `isPrefixOf` iStr = NoError
    | "updateStatus " `isPrefixOf` iStr = NoError
    | "status" == iStr = NoError
    | otherwise = ServiceError

    