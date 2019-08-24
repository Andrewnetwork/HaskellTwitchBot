module NumberGuesser
    ( input_handler
    , initial_state
    , Net
    , Bot(..)
    , BotState
    , greeting
    )
where

import           IRCTypes
import           Text.Read               hiding ( get )
import           Control.Monad.State.Lazy
import           System.IO

-- Bot Data Structures 
type BotState = Bool
data Bot = Bot { botSocket :: Handle, botState :: BotState }
type Net = StateT Bot IO
-- Bot Constants 
initial_state = True
greeting = "I have a number. Try and guess it."
target_number = 22 :: Integer
-- Bot Functions
eval_guess :: Maybe Integer -> Maybe Bool
eval_guess (Just guess) = Just (guess == target_number)
eval_guess Nothing      = Nothing

input_handler :: (String -> Net ()) -> String -> String -> Net ()
input_handler writer user message = do
    Bot soc state <- get
    case state of
        True -> case eval_guess guess of
            Just True -> do
                writer $ user ++ " guessed correctly!"
                put (Bot soc False)
            Just False -> writer $ user ++ " guessed incorrectly!"
            Nothing    -> writer $ user ++ " doesn't follow instructions!"
        False -> writer "Someone has already guessed my number!"
    where guess = readMaybe message :: Maybe Integer
