{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterSeriesInfoStruct@.
module ObjC.Matter.MTRChannelClusterSeriesInfoStruct
  ( MTRChannelClusterSeriesInfoStruct
  , IsMTRChannelClusterSeriesInfoStruct(..)
  , season
  , setSeason
  , episode
  , setEpisode
  , seasonSelector
  , setSeasonSelector
  , episodeSelector
  , setEpisodeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- season@
season :: IsMTRChannelClusterSeriesInfoStruct mtrChannelClusterSeriesInfoStruct => mtrChannelClusterSeriesInfoStruct -> IO (Id NSString)
season mtrChannelClusterSeriesInfoStruct  =
    sendMsg mtrChannelClusterSeriesInfoStruct (mkSelector "season") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSeason:@
setSeason :: (IsMTRChannelClusterSeriesInfoStruct mtrChannelClusterSeriesInfoStruct, IsNSString value) => mtrChannelClusterSeriesInfoStruct -> value -> IO ()
setSeason mtrChannelClusterSeriesInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterSeriesInfoStruct (mkSelector "setSeason:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- episode@
episode :: IsMTRChannelClusterSeriesInfoStruct mtrChannelClusterSeriesInfoStruct => mtrChannelClusterSeriesInfoStruct -> IO (Id NSString)
episode mtrChannelClusterSeriesInfoStruct  =
    sendMsg mtrChannelClusterSeriesInfoStruct (mkSelector "episode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEpisode:@
setEpisode :: (IsMTRChannelClusterSeriesInfoStruct mtrChannelClusterSeriesInfoStruct, IsNSString value) => mtrChannelClusterSeriesInfoStruct -> value -> IO ()
setEpisode mtrChannelClusterSeriesInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterSeriesInfoStruct (mkSelector "setEpisode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @season@
seasonSelector :: Selector
seasonSelector = mkSelector "season"

-- | @Selector@ for @setSeason:@
setSeasonSelector :: Selector
setSeasonSelector = mkSelector "setSeason:"

-- | @Selector@ for @episode@
episodeSelector :: Selector
episodeSelector = mkSelector "episode"

-- | @Selector@ for @setEpisode:@
setEpisodeSelector :: Selector
setEpisodeSelector = mkSelector "setEpisode:"

