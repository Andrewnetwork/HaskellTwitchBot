module IRCTypes
    ( IRC_Event (..)
    , IRC_Command (..)
    , Bot (..)
    , Net (..)
    )
where

import           Control.Monad.Trans.Reader
import           System.IO
import           Control.Monad.Trans.Reader

data IRC_Event  =  COMMAND IRC_Command | OTHER String
data IRC_Command = PING | PONG | PASS String | NICK String | JOIN String | PRIVMSG {get_user::String,  get_message::String}
data Bot = Bot { botSocket :: Handle}

type Net = ReaderT Bot IO
