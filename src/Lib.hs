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
import Data.List
import Text.Read

data IRC_Event = PING | PRIVMSG {user::String, msg::String} | OTHER String

instance Show IRC_Event where 
    show PING = "PING! You must PONG back!"
    show PRIVMSG{user=u,msg=m} = u++": "++m
    show (OTHER m) = "OTHER: "++m

-- Configuration options
myServer = "irc.chat.twitch.tv" :: String
myPort = 6667 :: N.PortNumber

target_number = 34 :: Integer

main :: IO ()
main = do
    h <- connectTo myServer myPort
    hSetBuffering stdout NoBuffering
    write h "PASS"    SECRET.oauth_key
    write h "NICK"    "amathematicalway_bot"
    write h "JOIN"    "#amathematicalway"
    write h "PRIVMSG" "#amathematicalway :I have a number, please guess it!"
    listen h

process_event :: String -> IRC_Event
process_event (':':xs) 
    | isInfixOf "PRIVMSG" xs = PRIVMSG {user=takeWhile (\x->x/='!') xs,msg=tail $ grabAfter xs ' ' 3}
    | otherwise = OTHER xs
process_event str@('P':xs) = case "ING" == (take 3 xs) of 
                            True -> PING
                            False -> OTHER str

grabAfter :: String -> Char -> Int -> String
grabAfter str _ 0 = str
grabAfter (x:xs) char cntr
    | x == char = grabAfter xs char (cntr-1)
    | otherwise = grabAfter xs char cntr
grabAfter [] _ _ = ""


make_guess :: Handle -> String -> Integer -> IO ()
make_guess h user guess 
    | guess==target_number = write h "PRIVMSG" ("#amathematicalway :"++user++" guessed correctly!")
    | otherwise = write h "PRIVMSG" ("#amathematicalway :"++user++" guessed incorrectly!")

handle_guess :: Handle -> IRC_Event -> IO ()
handle_guess h PRIVMSG{user=u,msg=m} = case guess of 
    Just v -> make_guess h u v 
    Nothing ->  write h "PRIVMSG" ("#amathematicalway :"++u++" doesn't follow instructions!")
    where guess = readMaybe m :: Maybe Integer


-- Process each line from the server
listen :: Handle -> IO ()
listen h = forever $ do
    line <- hGetLine h
    let evnt = process_event line  
    putStrLn $ show evnt
    case evnt of 
        PRIVMSG _ _ -> handle_guess h evnt 
        PING -> write h "PONG" ""
        _ -> return ()
    
  where
    forever :: IO () -> IO ()
    forever a = do
        a
        forever a


-- Send a message to a handle
write :: Handle -> String -> String -> IO ()
write h cmd args = do
    let msg = cmd ++ " " ++ args ++ "\r\n"
    hPutStr h msg          -- Send message on the wire
    if cmd == "PASS"
        then putStr ("> " ++ cmd ++ " <hidden>" ++ "\r\n")
        else putStr ("> " ++ msg)   -- Show sent message on the command line

-- Connect to a server given its name and port number
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
    addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
    sock     <- N.socket (N.addrFamily addr)
                         (N.addrSocketType addr)
                         (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode
