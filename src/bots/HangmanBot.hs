module HangmanBot
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
import           System.Exit

-- Settings 
server = "irc.chat.twitch.tv" :: String
port = 6667 :: N.PortNumber
channel = "#amathematicalway" :: String
-- Bot State
data BotState = BotState String
initial_state = BotState "" :: BotState
target_string = "Whereof one cannot speak thereof one must remain silent!"
-- Bot Greeting
greeting = display_guess_str $ fill_guessed_char target_string ""
-- Bot Functions
display_guess_str :: String -> String
display_guess_str guessStr =
    guessStr
        >>= (\x -> case x of
                ' ' -> " [-] "
                '_' -> "__ "
                _   -> [' ',x,' ']
            )

fill_guessed_char :: String -> String -> String
fill_guessed_char target guessedChars =
    target_string
        >>= (\x -> case x of
                ' ' -> " "
                _   -> if elem x guessedChars then [x] else "_"
            )

input_handler :: (String -> Net ()) -> String -> String -> Net ()
input_handler writer user message = do
    Bot soc (BotState guessed) <- get

    let new_guessed = head message : guessed
    let guessed_str = fill_guessed_char target_string new_guessed

    writer $ display_guess_str guessed_str
    when (guessed_str == target_string) $ do
        writer $ user ++ " won! Game over!"
        liftIO exitSuccess

    put (Bot soc (BotState new_guessed))

--- ### Bot Data Structures: Don't Touch ### ---
data Bot = Bot { botSocket :: Handle, botState :: BotState }
type Net = StateT Bot IO
