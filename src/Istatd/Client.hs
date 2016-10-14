{-# LANGUAGE RecordWildCards #-}
module Istatd.Client
( IstatdConfig (..)
, connect
, send
)
where

import            Control.Monad.IO.Class        ( MonadIO
                                                , liftIO
                                                )
import            Istatd.Types                  ( IstatdDatum (..)
                                                , toPacket
                                                )

import qualified  Network.BSD                   as NetBsd
import qualified  Network.Socket                as Net
import qualified  Network.Socket.ByteString     as NetBS

data IstatdConfig = IstatdConfig
  { hostName :: !Net.HostName
  , hostPort :: !Net.PortNumber
  }

data Connection = Connection
  { socket :: !Net.Socket
  , addr   :: !Net.SockAddr
  }

connect :: (MonadIO m)
        => IstatdConfig
        -> m Connection
connect IstatdConfig {..} = liftIO $ do
    hostEntry <- NetBsd.getHostByName hostName
    let addr = Net.SockAddrInet hostPort (NetBsd.hostAddress hostEntry)
    socket <- Net.socket Net.AF_INET Net.Datagram 0
    return $ Connection socket addr

send :: (MonadIO m)
     => Connection
     -> [IstatdDatum]
     -> m ()
send (Connection {..}) ps = liftIO $ NetBS.sendManyTo socket (map toPacket ps) addr
