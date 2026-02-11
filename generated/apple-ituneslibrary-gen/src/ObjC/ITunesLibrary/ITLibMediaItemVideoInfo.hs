{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The ITLibMediaItemVideoInfo class encapsulates the video information of a video media item.
--
-- Generated bindings for @ITLibMediaItemVideoInfo@.
module ObjC.ITunesLibrary.ITLibMediaItemVideoInfo
  ( ITLibMediaItemVideoInfo
  , IsITLibMediaItemVideoInfo(..)
  , series
  , sortSeries
  , season
  , episode
  , episodeOrder
  , hd
  , videoWidth
  , videoHeight
  , seriesSelector
  , sortSeriesSelector
  , seasonSelector
  , episodeSelector
  , episodeOrderSelector
  , hdSelector
  , videoWidthSelector
  , videoHeightSelector


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

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The name of TV series the video is associated with (implies track is a TV show).
--
-- ObjC selector: @- series@
series :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO (Id NSString)
series itLibMediaItemVideoInfo  =
    sendMsg itLibMediaItemVideoInfo (mkSelector "series") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the TV series the video is associated with that should be used for when sorting (implies the track is a TV show).
--
-- ObjC selector: @- sortSeries@
sortSeries :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO (Id NSString)
sortSeries itLibMediaItemVideoInfo  =
    sendMsg itLibMediaItemVideoInfo (mkSelector "sortSeries") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of TV season the video is associated with (implies the track is a TV show).
--
-- ObjC selector: @- season@
season :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO CULong
season itLibMediaItemVideoInfo  =
    sendMsg itLibMediaItemVideoInfo (mkSelector "season") retCULong []

-- | The TV episode the video is associated with (implies the track is a TV show).
--
-- ObjC selector: @- episode@
episode :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO (Id NSString)
episode itLibMediaItemVideoInfo  =
    sendMsg itLibMediaItemVideoInfo (mkSelector "episode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The TV episode order the video is associated with (implies the track is a TV show).
--
-- ObjC selector: @- episodeOrder@
episodeOrder :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO CLong
episodeOrder itLibMediaItemVideoInfo  =
    sendMsg itLibMediaItemVideoInfo (mkSelector "episodeOrder") retCLong []

-- | Whether the video is high definition.
--
-- ObjC selector: @- hd@
hd :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO Bool
hd itLibMediaItemVideoInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibMediaItemVideoInfo (mkSelector "hd") retCULong []

-- | The width of the video.
--
-- ObjC selector: @- videoWidth@
videoWidth :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO CULong
videoWidth itLibMediaItemVideoInfo  =
    sendMsg itLibMediaItemVideoInfo (mkSelector "videoWidth") retCULong []

-- | The height of the video.
--
-- ObjC selector: @- videoHeight@
videoHeight :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO CULong
videoHeight itLibMediaItemVideoInfo  =
    sendMsg itLibMediaItemVideoInfo (mkSelector "videoHeight") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @series@
seriesSelector :: Selector
seriesSelector = mkSelector "series"

-- | @Selector@ for @sortSeries@
sortSeriesSelector :: Selector
sortSeriesSelector = mkSelector "sortSeries"

-- | @Selector@ for @season@
seasonSelector :: Selector
seasonSelector = mkSelector "season"

-- | @Selector@ for @episode@
episodeSelector :: Selector
episodeSelector = mkSelector "episode"

-- | @Selector@ for @episodeOrder@
episodeOrderSelector :: Selector
episodeOrderSelector = mkSelector "episodeOrder"

-- | @Selector@ for @hd@
hdSelector :: Selector
hdSelector = mkSelector "hd"

-- | @Selector@ for @videoWidth@
videoWidthSelector :: Selector
videoWidthSelector = mkSelector "videoWidth"

-- | @Selector@ for @videoHeight@
videoHeightSelector :: Selector
videoHeightSelector = mkSelector "videoHeight"

