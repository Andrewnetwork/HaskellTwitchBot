module Config
    ( server
    , port
    , channel
    , input_handler
    , initial_state
    , NG.Net (..)
    , NG.Bot (..)
    , greeting
    )
where

import qualified Network.Socket                as N
import qualified NumberGuesser                 as NG
import qualified CountingBot as CB 

-- Edit these values as needed. --
server = "irc.chat.twitch.tv" :: String
port = 6667 :: N.PortNumber
channel = "#amathematicalway" :: String
-- Add your chatbot. 
input_handler = NG.input_handler
initial_state = NG.initial_state
greeting = NG.greeting
