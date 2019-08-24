module CountingBot
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

import           Control.Monad.State.Lazy
import           System.IO
import qualified Network.Socket                as N

-- Bot State
type BotState = Integer
initial_state = 0
-- Settings 
server = "irc.chat.twitch.tv" :: String
port = 6667 :: N.PortNumber
channel = "#amathematicalway" :: String
-- Bot Greeting 
greeting = "We will now count."
-- Bot Functions 
input_handler :: (String -> Net ()) -> String -> String -> Net ()
input_handler writer user message = do
    Bot soc state <- get
    writer $ show state
    put (Bot soc (state + 1))

--- ### Bot Data Structures: Don't Touch ### ---
data Bot = Bot { botSocket :: Handle, botState :: BotState }
type Net = StateT Bot IO