module SumBot
    ( make_guess
    , input_handler
    )
where

import           IRCTypes
import           Text.Read
import           Control.Monad.Trans.Reader

target_number = 34 :: Integer

make_guess :: String -> Maybe Integer -> String
make_guess user (Just guess)
    | guess == target_number = user ++ " guessed correctly!"
    | otherwise              = user ++ " guessed incorrectly!"
make_guess user Nothing = user ++ " doesn't follow instructions!"

input_handler :: String -> String -> String
input_handler user message = 
    make_guess user guess
    where guess = readMaybe message :: Maybe Integer
