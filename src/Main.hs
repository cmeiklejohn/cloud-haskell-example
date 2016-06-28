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

import Control.Distributed.Process (ProcessId)

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

-- Server logic.

tickHandler :: Tick -> ServerAction ()
tickHandler Tick = do
  ServerConfig myPidPeers peers <- ask
  random <- randomWithin (0, length peers - 1)
  let peer = peers !! random
  sendBingBongTo peer Bing

msgHandler :: Message -> ServerAction ()
msghandler (Message sender recipient Bing) = do
  bingCount += 1
  sendBingBongTo sender Bong
msgHandler (Message sender recipient Bong) = bongCount += 1

sendBingBongTo :: ProcessId -> BingBong -> ServerAction ()
sendBingBongTo recipient bingbong = do
  ServerConfig myId _ <- ask
  tell [Message myId recipient bingbong]

randomWithin :: Random r => (r,r) -> ServerAction r
randomWithin bounds = randomGen %%= randomR bounds

main :: IO ()
main = putStrLn "Hello World!"
