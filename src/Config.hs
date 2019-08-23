module Config
    ( server
    , port
    , channel
    )
where

import qualified Network.Socket                as N

server = "irc.chat.twitch.tv" :: String
port = 6667 :: N.PortNumber
channel = "#swarmcollective" :: String
