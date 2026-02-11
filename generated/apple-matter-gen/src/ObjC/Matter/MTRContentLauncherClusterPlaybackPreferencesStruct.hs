{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterPlaybackPreferencesStruct@.
module ObjC.Matter.MTRContentLauncherClusterPlaybackPreferencesStruct
  ( MTRContentLauncherClusterPlaybackPreferencesStruct
  , IsMTRContentLauncherClusterPlaybackPreferencesStruct(..)
  , playbackPosition
  , setPlaybackPosition
  , textTrack
  , setTextTrack
  , audioTracks
  , setAudioTracks
  , playbackPositionSelector
  , setPlaybackPositionSelector
  , textTrackSelector
  , setTextTrackSelector
  , audioTracksSelector
  , setAudioTracksSelector


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

-- | @- playbackPosition@
playbackPosition :: IsMTRContentLauncherClusterPlaybackPreferencesStruct mtrContentLauncherClusterPlaybackPreferencesStruct => mtrContentLauncherClusterPlaybackPreferencesStruct -> IO (Id NSNumber)
playbackPosition mtrContentLauncherClusterPlaybackPreferencesStruct  =
    sendMsg mtrContentLauncherClusterPlaybackPreferencesStruct (mkSelector "playbackPosition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaybackPosition:@
setPlaybackPosition :: (IsMTRContentLauncherClusterPlaybackPreferencesStruct mtrContentLauncherClusterPlaybackPreferencesStruct, IsNSNumber value) => mtrContentLauncherClusterPlaybackPreferencesStruct -> value -> IO ()
setPlaybackPosition mtrContentLauncherClusterPlaybackPreferencesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterPlaybackPreferencesStruct (mkSelector "setPlaybackPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textTrack@
textTrack :: IsMTRContentLauncherClusterPlaybackPreferencesStruct mtrContentLauncherClusterPlaybackPreferencesStruct => mtrContentLauncherClusterPlaybackPreferencesStruct -> IO (Id MTRContentLauncherClusterTrackPreferenceStruct)
textTrack mtrContentLauncherClusterPlaybackPreferencesStruct  =
    sendMsg mtrContentLauncherClusterPlaybackPreferencesStruct (mkSelector "textTrack") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextTrack:@
setTextTrack :: (IsMTRContentLauncherClusterPlaybackPreferencesStruct mtrContentLauncherClusterPlaybackPreferencesStruct, IsMTRContentLauncherClusterTrackPreferenceStruct value) => mtrContentLauncherClusterPlaybackPreferencesStruct -> value -> IO ()
setTextTrack mtrContentLauncherClusterPlaybackPreferencesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterPlaybackPreferencesStruct (mkSelector "setTextTrack:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioTracks@
audioTracks :: IsMTRContentLauncherClusterPlaybackPreferencesStruct mtrContentLauncherClusterPlaybackPreferencesStruct => mtrContentLauncherClusterPlaybackPreferencesStruct -> IO (Id NSArray)
audioTracks mtrContentLauncherClusterPlaybackPreferencesStruct  =
    sendMsg mtrContentLauncherClusterPlaybackPreferencesStruct (mkSelector "audioTracks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioTracks:@
setAudioTracks :: (IsMTRContentLauncherClusterPlaybackPreferencesStruct mtrContentLauncherClusterPlaybackPreferencesStruct, IsNSArray value) => mtrContentLauncherClusterPlaybackPreferencesStruct -> value -> IO ()
setAudioTracks mtrContentLauncherClusterPlaybackPreferencesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterPlaybackPreferencesStruct (mkSelector "setAudioTracks:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playbackPosition@
playbackPositionSelector :: Selector
playbackPositionSelector = mkSelector "playbackPosition"

-- | @Selector@ for @setPlaybackPosition:@
setPlaybackPositionSelector :: Selector
setPlaybackPositionSelector = mkSelector "setPlaybackPosition:"

-- | @Selector@ for @textTrack@
textTrackSelector :: Selector
textTrackSelector = mkSelector "textTrack"

-- | @Selector@ for @setTextTrack:@
setTextTrackSelector :: Selector
setTextTrackSelector = mkSelector "setTextTrack:"

-- | @Selector@ for @audioTracks@
audioTracksSelector :: Selector
audioTracksSelector = mkSelector "audioTracks"

-- | @Selector@ for @setAudioTracks:@
setAudioTracksSelector :: Selector
setAudioTracksSelector = mkSelector "setAudioTracks:"

