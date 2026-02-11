{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterTrackAttributesStruct@.
module ObjC.Matter.MTRMediaPlaybackClusterTrackAttributesStruct
  ( MTRMediaPlaybackClusterTrackAttributesStruct
  , IsMTRMediaPlaybackClusterTrackAttributesStruct(..)
  , languageCode
  , setLanguageCode
  , displayName
  , setDisplayName
  , languageCodeSelector
  , setLanguageCodeSelector
  , displayNameSelector
  , setDisplayNameSelector


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
languageCode :: IsMTRMediaPlaybackClusterTrackAttributesStruct mtrMediaPlaybackClusterTrackAttributesStruct => mtrMediaPlaybackClusterTrackAttributesStruct -> IO (Id NSString)
languageCode mtrMediaPlaybackClusterTrackAttributesStruct  =
    sendMsg mtrMediaPlaybackClusterTrackAttributesStruct (mkSelector "languageCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLanguageCode:@
setLanguageCode :: (IsMTRMediaPlaybackClusterTrackAttributesStruct mtrMediaPlaybackClusterTrackAttributesStruct, IsNSString value) => mtrMediaPlaybackClusterTrackAttributesStruct -> value -> IO ()
setLanguageCode mtrMediaPlaybackClusterTrackAttributesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterTrackAttributesStruct (mkSelector "setLanguageCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- displayName@
displayName :: IsMTRMediaPlaybackClusterTrackAttributesStruct mtrMediaPlaybackClusterTrackAttributesStruct => mtrMediaPlaybackClusterTrackAttributesStruct -> IO (Id NSString)
displayName mtrMediaPlaybackClusterTrackAttributesStruct  =
    sendMsg mtrMediaPlaybackClusterTrackAttributesStruct (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDisplayName:@
setDisplayName :: (IsMTRMediaPlaybackClusterTrackAttributesStruct mtrMediaPlaybackClusterTrackAttributesStruct, IsNSString value) => mtrMediaPlaybackClusterTrackAttributesStruct -> value -> IO ()
setDisplayName mtrMediaPlaybackClusterTrackAttributesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterTrackAttributesStruct (mkSelector "setDisplayName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @languageCode@
languageCodeSelector :: Selector
languageCodeSelector = mkSelector "languageCode"

-- | @Selector@ for @setLanguageCode:@
setLanguageCodeSelector :: Selector
setLanguageCodeSelector = mkSelector "setLanguageCode:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @setDisplayName:@
setDisplayNameSelector :: Selector
setDisplayNameSelector = mkSelector "setDisplayName:"

