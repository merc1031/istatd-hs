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
import            Istatd.Types                  ( IstatdDatum (..) )

import qualified  Data.ByteString               as BS
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
     -> (IstatdDatum -> m BS.ByteString)
     -> m ()
send (Connection {..}) ps encoder = do
  ps' <- mapM encoder ps
  liftIO $ NetBS.sendManyTo socket ps' addr
