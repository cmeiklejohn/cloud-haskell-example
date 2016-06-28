{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- Allows automatic derivation of e.g. Monad
{-# LANGUAGE DeriveGeneric              #-} -- Allows Generic, for auto-generation of serialization code
{-# LANGUAGE TemplateHaskell            #-} -- Allows automatic creation of Lenses for ServerStateodule Main where

module Main where

import Data.Binary (Binary) -- Objects have to be binary to send over the network
import GHC.Generics (Generic) -- For auto-derivation of serialization
import Data.Typeable (Typeable) -- For safe serialization

import Control.Monad.RWS.Strict (
  RWS, MonadReader, MonadWriter, MonadState,
  ask, tell, get, execRWS, liftIO)

import Control.Distributed.Process.Node (initRemoteTable, runProcess, newLocalNode)
import Control.Distributed.Process (Process, ProcessId,
  send, say, expect, getSelfPid, spawnLocal, match, receiveWait)

import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Control.Monad (replicateM, forever)
import Control.Concurrent (threadDelay)
import Control.Lens (makeLenses, (+=), (%%=))
import System.Random (StdGen, Random, randomR, newStdGen)

-- Message payload contains either a bing or a bong.
data BingBong = Bing | Bong
                deriving (Show, Generic, Typeable)

-- Messages have a sender and a recipient and a message.
data Message = Message {senderOf :: ProcessId,
                        recipientOf :: ProcessId,
                        msg :: BingBong}
                        deriving (Show, Generic, Typeable)

-- Each process has a periodic tick message.
data Tick = Tick deriving (Show, Generic, Typeable)

-- Automatically generate serialization code for these types.
instance Binary BingBong
instance Binary Message
instance Binary Tick

-- ServerState record, with automatic setters/getters via Lens.
data ServerState = ServerState {
  _bingCount :: Int,
  _bongCount :: Int,
  _randomGen :: StdGen
} deriving Show

makeLenses ''ServerState

-- ServerConfig.
data ServerConfig = ServerConfig {
  myId  :: ProcessId,
  peers :: [ProcessId]
} deriving (Show)

-- ServerAction: custom Monad.
newtype ServerAction a = ServerAction {
  runAction :: RWS ServerConfig [Message] ServerState a
} deriving (Functor,
            Applicative,
            Monad,
            MonadState  ServerState,
            MonadWriter [Message],
            MonadReader ServerConfig)

-- -- Server logic.

tickHandler :: Tick -> ServerAction ()
tickHandler Tick = do
  ServerConfig myPid peers <- ask
  random <- randomWithin (0, length peers - 1)
  let peer = peers !! random
  sendBingBongTo peer Bing

msgHandler :: Message -> ServerAction ()
msgHandler (Message sender recipient Bing) = do
  bingCount += 1
  sendBingBongTo sender Bong
msgHandler (Message sender recipient Bong) = do
  bongCount += 1

sendBingBongTo :: ProcessId -> BingBong -> ServerAction ()
sendBingBongTo recipient bingbong = do
  ServerConfig myId _ <- ask
  tell [Message myId recipient bingbong]

randomWithin :: Random r => (r,r) -> ServerAction r
randomWithin bounds = randomGen %%= randomR bounds

runServer :: ServerConfig -> ServerState -> Process ()
runServer config state = do
  let run handler msg = return $ execRWS (runAction $ handler msg) config state
  (state', outputMessages) <- receiveWait [
    match $ run msgHandler,
    match $ run tickHandler]
  say $ "Current state: " ++ show state'
  mapM (\msg -> send (recipientOf msg) msg) outputMessages
  runServer config state'

spawnServer :: Process ProcessId
spawnServer = spawnLocal $ do
  myPid <- getSelfPid
  otherPids <- expect
  spawnLocal $ forever $ do
    liftIO $ threadDelay (10^6)
    send myPid Tick
  randomGen <- liftIO newStdGen
  runServer (ServerConfig myPid otherPids) (ServerState 0 0 randomGen)

spawnServers :: Int -> Process ()
spawnServers count = do
  pids <- replicateM count spawnServer
  mapM_ (`send` pids) pids

main :: IO String
main = do
  Right transport <- createTransport "localhost" "0" defaultTCPParameters
  backendNode <- newLocalNode transport initRemoteTable
  runProcess backendNode (spawnServers 2)
  putStrLn "Push enter to exit"
  getLine
