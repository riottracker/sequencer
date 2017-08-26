{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Sound.Sequencer where

import Linear.V
import GHC.TypeLits
import Data.Type.Ordinal

import Codec.Tracker.Common

data Channel = Channel Int

type Track nc = [V nc Note]

data Sequencer nc np ni = Sequencer { channels :: V nc Channel
                                    , patterns :: V np (Track nc)
                                    , order    :: [Ordinal np]
                                    } 
                 
