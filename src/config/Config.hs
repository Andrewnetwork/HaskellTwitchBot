module Config
    ( server
    , port
    , channel
    , input_handler
    )
where

import qualified Network.Socket                as N
import qualified SumBot                        as SB
import           IRCTypes

-- Edit these values as needed. --
server = "irc.chat.twitch.tv" :: String
port = 6667 :: N.PortNumber
channel = "#amathematicalway" :: String
-- Add your chatbot. 
input_handler :: String -> String -> String
input_handler = SB.input_handler
