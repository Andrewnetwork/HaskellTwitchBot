module Lib
    ( main
    , grabAfter
    , process_event
    )
where

-- Modified from: https://wiki.haskell.org/Roll_your_own_IRC_bot
import           System.IO
import qualified Network.Socket                as N
import qualified Secret                        as SECRET
import           Data.List
import           Text.Read
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
import           Control.Exception
import qualified Config                        as C

data IRC_Event  =  COMMAND IRC_Command | OTHER String
data IRC_Command = PING | PONG | PASS String | NICK String | JOIN String | PRIVMSG {get_user::String,  get_message::String}
data Bot = Bot { botSocket :: Handle, channel :: String}

type Net = ReaderT Bot IO

instance Show IRC_Command where
    show PING     = "> PING"
    show PRIVMSG { get_user = u, get_message = m } = u ++ ": " ++ m
    show (NICK s) = "> NICK " ++ s
    show (JOIN s) = "> JOIN " ++ s
    show (PASS _) = "> PASS <hidden>"

instance Show IRC_Event where
    show (COMMAND c) = show c
    show (OTHER   m) = "OTHER: " ++ m

target_number = 34 :: Integer

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . botSocket
    loop       = runReaderT join_server

connect :: IO Bot
connect = notify $ do
    h <- connectTo C.server C.port
    return (Bot h C.channel)
  where
    notify = bracket_
        (putStrLn ("Connecting to " ++ C.server ++ " ...") >> hFlush stdout)
        (putStrLn "done.")

join_server :: Net ()
join_server = do
    issue_command (PASS SECRET.oauth_key)
    issue_command (NICK "amathematicalway_bot")
    issue_command (JOIN C.channel)
    issue_command
        (PRIVMSG "amathematicalway_bot" "I have a number, please guess it!")
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


make_guess :: String -> Maybe Integer -> String
make_guess user (Just guess)
    | guess == target_number = user ++ " guessed correctly!"
    | otherwise              = user ++ " guessed incorrectly!"
make_guess user Nothing = user ++ " doesn't follow instructions!"

handle_guess :: String -> String -> Net ()
handle_guess user message = do
    h <- asks botSocket
    issue_command $ PRIVMSG "amathematicalway_bot" (make_guess user guess)
    where guess = readMaybe message :: Maybe Integer


-- Process each line from the server
listen :: Net ()
listen = forever $ do
    h    <- asks botSocket
    line <- liftIO $ hGetLine h
    let evnt = process_event line
    liftIO $ print evnt
    case evnt of
        COMMAND (PRIVMSG user message) -> handle_guess user message
        COMMAND PING                   -> issue_command PONG
        _                              -> return ()
  where
    forever :: Net () -> Net ()
    forever a = do
        a
        forever a


-- Send a message to a handle
issue_command :: IRC_Command -> Net ()
issue_command cmd = do
    h <- asks botSocket
    liftIO $ print cmd
    case cmd of
        PONG   -> write h "PONG\r\n"
        PING   -> write h "PING\r\n"
        NICK x -> write h ("NICK " ++ x ++ "\r\n")
        PASS x -> write h ("PASS " ++ x ++ "\r\n")
        JOIN x -> write h ("JOIN " ++ x ++ "\r\n")
        PRIVMSG _ message ->
            write h ("PRIVMSG " ++ C.channel ++ " :" ++ message ++ "\r\n")
    where write h str = liftIO $ hPutStr h str


-- Connect to a server given its name and port number
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
    addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
    sock     <- N.socket (N.addrFamily addr)
                         (N.addrSocketType addr)
                         (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode
