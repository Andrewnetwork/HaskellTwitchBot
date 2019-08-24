module TemplateBot
    ( input_handler
    , initial_state
    , Net
    , Bot(..)
    , BotState
    , greeting
    , server
    , port
    , channel
    )
where

import           IRCTypes
import           Text.Read               hiding ( get )
import           Control.Monad.State.Lazy
import           System.IO
import qualified Network.Socket                as N

-- Settings 
server = "irc.chat.twitch.tv" :: String
port = 6667 :: N.PortNumber
channel = "#amathematicalway" :: String
-- Bot Data Structures 
type BotState = ()
data Bot = Bot { botSocket :: Handle, botState :: BotState }
type Net = StateT Bot IO
-- Bot Constants 
greeting = "I am a basic bot."
initial_state = ()
-- Bot Functions
input_handler :: (String -> Net ()) -> String -> String -> Net ()
input_handler writer user message = writer $ "Hello "++user++", I am a basic bot."
