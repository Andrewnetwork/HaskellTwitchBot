module IRCTypes
    ( IRC_Event (..)
    , IRC_Command (..)
    )
where

import           System.IO
import           Control.Monad.State.Lazy

data IRC_Event  =  COMMAND IRC_Command | OTHER String
data IRC_Command = PING | PONG | PASS String | NICK String | JOIN String | PRIVMSG {get_user::String,  get_message::String}