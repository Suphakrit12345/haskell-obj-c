{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterStateChangedEvent@.
module ObjC.Matter.MTRMediaPlaybackClusterStateChangedEvent
  ( MTRMediaPlaybackClusterStateChangedEvent
  , IsMTRMediaPlaybackClusterStateChangedEvent(..)
  , currentState
  , setCurrentState
  , startTime
  , setStartTime
  , duration
  , setDuration
  , sampledPosition
  , setSampledPosition
  , playbackSpeed
  , setPlaybackSpeed
  , seekRangeEnd
  , setSeekRangeEnd
  , seekRangeStart
  , setSeekRangeStart
  , data_
  , setData
  , audioAdvanceUnmuted
  , setAudioAdvanceUnmuted
  , currentStateSelector
  , setCurrentStateSelector
  , startTimeSelector
  , setStartTimeSelector
  , durationSelector
  , setDurationSelector
  , sampledPositionSelector
  , setSampledPositionSelector
  , playbackSpeedSelector
  , setPlaybackSpeedSelector
  , seekRangeEndSelector
  , setSeekRangeEndSelector
  , seekRangeStartSelector
  , setSeekRangeStartSelector
  , dataSelector
  , setDataSelector
  , audioAdvanceUnmutedSelector
  , setAudioAdvanceUnmutedSelector


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

-- | @- currentState@
currentState :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
currentState mtrMediaPlaybackClusterStateChangedEvent  =
    sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "currentState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentState:@
setCurrentState :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setCurrentState mtrMediaPlaybackClusterStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "setCurrentState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startTime@
startTime :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
startTime mtrMediaPlaybackClusterStateChangedEvent  =
    sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "startTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartTime:@
setStartTime :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setStartTime mtrMediaPlaybackClusterStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "setStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- duration@
duration :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
duration mtrMediaPlaybackClusterStateChangedEvent  =
    sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDuration:@
setDuration :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setDuration mtrMediaPlaybackClusterStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "setDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sampledPosition@
sampledPosition :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id MTRMediaPlaybackClusterPlaybackPositionStruct)
sampledPosition mtrMediaPlaybackClusterStateChangedEvent  =
    sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "sampledPosition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSampledPosition:@
setSampledPosition :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsMTRMediaPlaybackClusterPlaybackPositionStruct value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setSampledPosition mtrMediaPlaybackClusterStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "setSampledPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- playbackSpeed@
playbackSpeed :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
playbackSpeed mtrMediaPlaybackClusterStateChangedEvent  =
    sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "playbackSpeed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaybackSpeed:@
setPlaybackSpeed :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setPlaybackSpeed mtrMediaPlaybackClusterStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "setPlaybackSpeed:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- seekRangeEnd@
seekRangeEnd :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
seekRangeEnd mtrMediaPlaybackClusterStateChangedEvent  =
    sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "seekRangeEnd") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSeekRangeEnd:@
setSeekRangeEnd :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setSeekRangeEnd mtrMediaPlaybackClusterStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "setSeekRangeEnd:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- seekRangeStart@
seekRangeStart :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
seekRangeStart mtrMediaPlaybackClusterStateChangedEvent  =
    sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "seekRangeStart") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSeekRangeStart:@
setSeekRangeStart :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setSeekRangeStart mtrMediaPlaybackClusterStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "setSeekRangeStart:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- data@
data_ :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSData)
data_ mtrMediaPlaybackClusterStateChangedEvent  =
    sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSData value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setData mtrMediaPlaybackClusterStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioAdvanceUnmuted@
audioAdvanceUnmuted :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
audioAdvanceUnmuted mtrMediaPlaybackClusterStateChangedEvent  =
    sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "audioAdvanceUnmuted") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioAdvanceUnmuted:@
setAudioAdvanceUnmuted :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setAudioAdvanceUnmuted mtrMediaPlaybackClusterStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterStateChangedEvent (mkSelector "setAudioAdvanceUnmuted:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentState@
currentStateSelector :: Selector
currentStateSelector = mkSelector "currentState"

-- | @Selector@ for @setCurrentState:@
setCurrentStateSelector :: Selector
setCurrentStateSelector = mkSelector "setCurrentState:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @sampledPosition@
sampledPositionSelector :: Selector
sampledPositionSelector = mkSelector "sampledPosition"

-- | @Selector@ for @setSampledPosition:@
setSampledPositionSelector :: Selector
setSampledPositionSelector = mkSelector "setSampledPosition:"

-- | @Selector@ for @playbackSpeed@
playbackSpeedSelector :: Selector
playbackSpeedSelector = mkSelector "playbackSpeed"

-- | @Selector@ for @setPlaybackSpeed:@
setPlaybackSpeedSelector :: Selector
setPlaybackSpeedSelector = mkSelector "setPlaybackSpeed:"

-- | @Selector@ for @seekRangeEnd@
seekRangeEndSelector :: Selector
seekRangeEndSelector = mkSelector "seekRangeEnd"

-- | @Selector@ for @setSeekRangeEnd:@
setSeekRangeEndSelector :: Selector
setSeekRangeEndSelector = mkSelector "setSeekRangeEnd:"

-- | @Selector@ for @seekRangeStart@
seekRangeStartSelector :: Selector
seekRangeStartSelector = mkSelector "seekRangeStart"

-- | @Selector@ for @setSeekRangeStart:@
setSeekRangeStartSelector :: Selector
setSeekRangeStartSelector = mkSelector "setSeekRangeStart:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @audioAdvanceUnmuted@
audioAdvanceUnmutedSelector :: Selector
audioAdvanceUnmutedSelector = mkSelector "audioAdvanceUnmuted"

-- | @Selector@ for @setAudioAdvanceUnmuted:@
setAudioAdvanceUnmutedSelector :: Selector
setAudioAdvanceUnmutedSelector = mkSelector "setAudioAdvanceUnmuted:"

