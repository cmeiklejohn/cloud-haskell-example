{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- Allows automatic derivation of e.g. Monad
{-# LANGUAGE DeriveGeneric              #-} -- Allows Generic, for auto-generation of serialization code
{-# LANGUAGE TemplateHaskell            #-} -- Allows automatic creation of Lenses for ServerStateodule Main where

import Data.Binary (Binary) -- Objects have to be binary to send over the network
import GHC.Generics (Generic) -- For auto-derivation of serialization
import Data.Typeable (Typeable) -- For safe serialization

import Control.Distributed.Process (ProcessId)

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

main :: IO ()
main = do
  putStrLn "Hello World!"
