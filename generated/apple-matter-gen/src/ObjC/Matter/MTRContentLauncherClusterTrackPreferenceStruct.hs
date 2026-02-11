{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterTrackPreferenceStruct@.
module ObjC.Matter.MTRContentLauncherClusterTrackPreferenceStruct
  ( MTRContentLauncherClusterTrackPreferenceStruct
  , IsMTRContentLauncherClusterTrackPreferenceStruct(..)
  , languageCode
  , setLanguageCode
  , characteristics
  , setCharacteristics
  , audioOutputIndex
  , setAudioOutputIndex
  , languageCodeSelector
  , setLanguageCodeSelector
  , characteristicsSelector
  , setCharacteristicsSelector
  , audioOutputIndexSelector
  , setAudioOutputIndexSelector


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

-- | @- languageCode@
languageCode :: IsMTRContentLauncherClusterTrackPreferenceStruct mtrContentLauncherClusterTrackPreferenceStruct => mtrContentLauncherClusterTrackPreferenceStruct -> IO (Id NSString)
languageCode mtrContentLauncherClusterTrackPreferenceStruct  =
    sendMsg mtrContentLauncherClusterTrackPreferenceStruct (mkSelector "languageCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLanguageCode:@
setLanguageCode :: (IsMTRContentLauncherClusterTrackPreferenceStruct mtrContentLauncherClusterTrackPreferenceStruct, IsNSString value) => mtrContentLauncherClusterTrackPreferenceStruct -> value -> IO ()
setLanguageCode mtrContentLauncherClusterTrackPreferenceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterTrackPreferenceStruct (mkSelector "setLanguageCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- characteristics@
characteristics :: IsMTRContentLauncherClusterTrackPreferenceStruct mtrContentLauncherClusterTrackPreferenceStruct => mtrContentLauncherClusterTrackPreferenceStruct -> IO (Id NSArray)
characteristics mtrContentLauncherClusterTrackPreferenceStruct  =
    sendMsg mtrContentLauncherClusterTrackPreferenceStruct (mkSelector "characteristics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCharacteristics:@
setCharacteristics :: (IsMTRContentLauncherClusterTrackPreferenceStruct mtrContentLauncherClusterTrackPreferenceStruct, IsNSArray value) => mtrContentLauncherClusterTrackPreferenceStruct -> value -> IO ()
setCharacteristics mtrContentLauncherClusterTrackPreferenceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterTrackPreferenceStruct (mkSelector "setCharacteristics:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioOutputIndex@
audioOutputIndex :: IsMTRContentLauncherClusterTrackPreferenceStruct mtrContentLauncherClusterTrackPreferenceStruct => mtrContentLauncherClusterTrackPreferenceStruct -> IO (Id NSNumber)
audioOutputIndex mtrContentLauncherClusterTrackPreferenceStruct  =
    sendMsg mtrContentLauncherClusterTrackPreferenceStruct (mkSelector "audioOutputIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioOutputIndex:@
setAudioOutputIndex :: (IsMTRContentLauncherClusterTrackPreferenceStruct mtrContentLauncherClusterTrackPreferenceStruct, IsNSNumber value) => mtrContentLauncherClusterTrackPreferenceStruct -> value -> IO ()
setAudioOutputIndex mtrContentLauncherClusterTrackPreferenceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterTrackPreferenceStruct (mkSelector "setAudioOutputIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @languageCode@
languageCodeSelector :: Selector
languageCodeSelector = mkSelector "languageCode"

-- | @Selector@ for @setLanguageCode:@
setLanguageCodeSelector :: Selector
setLanguageCodeSelector = mkSelector "setLanguageCode:"

-- | @Selector@ for @characteristics@
characteristicsSelector :: Selector
characteristicsSelector = mkSelector "characteristics"

-- | @Selector@ for @setCharacteristics:@
setCharacteristicsSelector :: Selector
setCharacteristicsSelector = mkSelector "setCharacteristics:"

-- | @Selector@ for @audioOutputIndex@
audioOutputIndexSelector :: Selector
audioOutputIndexSelector = mkSelector "audioOutputIndex"

-- | @Selector@ for @setAudioOutputIndex:@
setAudioOutputIndexSelector :: Selector
setAudioOutputIndexSelector = mkSelector "setAudioOutputIndex:"

