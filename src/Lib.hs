module Lib
    ( main
    , Bot
    )
where

-------------- ### Change to Your Bot ###-----------------
import           CountingBot
----------------------------------------------------------

import           System.IO
import qualified Network.Socket                as N
import qualified Secret                        as SECRET
import           Data.List
import           Text.Read               hiding ( get )
import           Control.Monad.State.Lazy
import           Control.Monad.IO.Class
import           Control.Exception
import           IRCTypes

instance Show IRC_Command where
    show PING     = "PING"
    show PRIVMSG { get_user = u, get_message = m } = u ++ ": " ++ m
    show (NICK s) = "NICK " ++ s
    show (JOIN s) = "JOIN " ++ s
    show (PASS _) = "PASS <hidden>"
    show PONG     = "PONG"

instance Show IRC_Event where
    show (COMMAND c) = "< " ++ show c
    show (OTHER   m) = "< OTHER: " ++ m

main :: IO ((), Bot)
main = bracket connect disconnect loop
  where
    disconnect = hClose . botSocket
    loop       = runStateT join_server

connect :: IO Bot
connect = notify $ do
    h <- connectTo server port
    return (Bot h initial_state)
  where
    notify = bracket_
        (putStrLn ("Connecting to " ++ server ++ " ...") >> hFlush stdout)
        (putStrLn "done.")

join_server :: Net ()
join_server = do
    issue_command (PASS SECRET.oauth_key)
    issue_command (NICK "BOT")
    issue_command (JOIN channel)
    issue_command (PRIVMSG "BOT" greeting)
    listen

process_event :: String -> IRC_Event
process_event (':' : xs)
    | isInfixOf "PRIVMSG" xs = COMMAND PRIVMSG
        { get_user    = takeWhile (\x -> x /= '!') xs
        , get_message = tail $ grabAfter xs ' ' 3
        }
    | otherwise = OTHER xs
process_event str@('P' : xs) = case "ING" == (take 3 xs) of
    True  -> COMMAND PING
    False -> OTHER str

grabAfter :: String -> Char -> Int -> String
grabAfter str _ 0 = str
grabAfter (x : xs) char cntr | x == char = grabAfter xs char (cntr - 1)
                             | otherwise = grabAfter xs char cntr
grabAfter [] _ _ = ""

listen :: Net ()
listen = forever $ do
    state <- get
    line  <- liftIO $ hGetLine (botSocket state)
    let evnt = process_event line
    liftIO $ print evnt
    case evnt of
        COMMAND (PRIVMSG user message) ->
            input_handler (\x -> issue_command (PRIVMSG "BOT" x)) user message
        COMMAND PING -> issue_command PONG
        _            -> return ()
  where
    forever :: Net () -> Net ()
    forever a = do
        a
        forever a

issue_command :: IRC_Command -> Net ()
issue_command cmd = do
    state <- get
    let h = (botSocket state)
    liftIO $ putStrLn $ "> " ++ show cmd
    case cmd of
        PONG   -> write h "PONG\r\n"
        PING   -> write h "PING\r\n"
        NICK x -> write h ("NICK " ++ x ++ "\r\n")
        PASS x -> write h ("PASS " ++ x ++ "\r\n")
        JOIN x -> write h ("JOIN " ++ x ++ "\r\n")
        PRIVMSG _ message ->
            write h ("PRIVMSG " ++ channel ++ " :" ++ message ++ "\r\n")
    where write h str = liftIO $ hPutStr h str

connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
    addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
    sock     <- N.socket (N.addrFamily addr)
                         (N.addrSocketType addr)
                         (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode
