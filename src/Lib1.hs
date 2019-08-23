module Lib1
    ( someFunc
    , server_send
    , server
    , twitch_connect
    , echo_server
    )
where

import qualified Control.Exception             as E
import qualified Data.ByteString.Char8         as CHAR
import           Network.Simple.TCP


someFunc :: IO ()
someFunc = putStrLn $ "Hello"

-- tst :: IO ()
-- tst = withSocketsDo $ do
--     addr <- resolve "www.freenode.net" "6697"
--     E.bracket (open addr) close talk
--   where
--     resolve host port = do
--         let hints = defaultHints { addrSocketType = Stream }
--         addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
--         return addr
--     open addr = do
--         sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--         connect sock $ addrAddress addr
--         return sock
--     talk sock = do
--         sendAll sock (C.pack "PASS d)

--         msg <- recv sock 1024
--         putStr "Received: "
--         C.putStrLn msg



--PASS d
-- NICK amathematicalway

-- send_credentials :: (Socket, SockAddr) -> IO ()
-- send_credentials connectionInfo@(connectionSocket, remoteAddr) = do
--     send connectionSocket (CHAR.pack "PASS d)
--     send connectionSocket (CHAR.pack "NICK amathematicalway")

twitch_recv :: (Socket, SockAddr) -> IO ()
twitch_recv connectionInfo@(connectionSocket, remoteAddr) = do
    a <- recv connectionSocket 10000
    case a of
        Just a -> do
            putStrLn $ (CHAR.unpack a)
        Nothing -> twitch_recv connectionInfo

twitch_connect :: IO ()
twitch_connect =
    connect "irc.chat.twitch.tv" "6697"
        $ \connectionInfo@(connectionSocket, remoteAddr) -> do
              putStrLn $ "Connection established to " ++ show remoteAddr
              tst <- CHAR.getLine
              send connectionSocket tst
              putStrLn "Sent Line"
              --send connectionSocket (CHAR.pack $ "PASS d++"\n")
              --send connectionSocket (CHAR.pack $ "NICK amathematicalway"++"\n")
              a <- recv connectionSocket 1
              case a of
                  Just a -> do
                      putStrLn $ (CHAR.unpack a)
                  Nothing -> putStrLn $ "Nothing recieved."
        --send connectionSocket (CHAR.pack $ "JOIN #amathematicalway"++"\n")
        --send connectionSocket (CHAR.pack $ "PRIVMSG #amathematicalway :hello guys!"++"\n")


-- client_recv :: (Socket, SockAddr) -> IO ()
-- client_recv connectionInfo@(connectionSocket, remoteAddr) = do
--     a <- recv connectionSocket 100
--     case a of
--         Just a -> do
--             putStrLn $ (toString a)
--             client_recv connectionInfo
--         Nothing -> client_recv connectionInfo

-- client = connect "127.0.0.1" "8000" $ client_recv

server_send :: (Socket, SockAddr) -> IO ()
server_send connectionInfo@(connectionSocket, remoteAddr) = do
    message <- getLine
    send connectionSocket (CHAR.pack $ message ++ "\n")
    send connectionSocket (CHAR.pack $ message ++ "\n")

server :: IO a
server = serve (Host "127.0.0.1") "8000" $ server_send

echo_server_recv :: (Socket, SockAddr) -> IO ()
echo_server_recv connectionInfo@(connectionSocket, remoteAddr) = do
    message <- recv connectionSocket 1000
    case message of
        Just a -> do
            putStr $ (CHAR.unpack a)
            send connectionSocket a
            echo_server_recv connectionInfo
        Nothing -> echo_server_recv connectionInfo

echo_server :: IO a
echo_server = serve (Host "127.0.0.1") "8000" $ echo_server_recv
