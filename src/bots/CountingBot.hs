module CountingBot(
    input_handler
    , initial_state
    , Net
    , Bot (..)
    , BotState
    , greeting
)
where

import           Control.Monad.State.Lazy
import           System.IO

-- Bot Data Structures 
type BotState = Integer
data Bot = Bot { botSocket :: Handle, botState :: BotState }
type Net = StateT Bot IO
-- Bot Constants 
initial_state = 0 
greeting = "We will now count."
-- Bot Functions 
input_handler :: (String->Net ())->String->String->Net ()
input_handler writer user msg = do 
    Bot soc state <- get 
    writer $ show state
    put (Bot soc (state+1))