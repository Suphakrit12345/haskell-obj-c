{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterTrackStruct@.
module ObjC.Matter.MTRMediaPlaybackClusterTrackStruct
  ( MTRMediaPlaybackClusterTrackStruct
  , IsMTRMediaPlaybackClusterTrackStruct(..)
  , id_
  , setId
  , trackAttributes
  , setTrackAttributes
  , idSelector
  , setIdSelector
  , trackAttributesSelector
  , setTrackAttributesSelector


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

-- | @- id@
id_ :: IsMTRMediaPlaybackClusterTrackStruct mtrMediaPlaybackClusterTrackStruct => mtrMediaPlaybackClusterTrackStruct -> IO (Id NSString)
id_ mtrMediaPlaybackClusterTrackStruct  =
    sendMsg mtrMediaPlaybackClusterTrackStruct (mkSelector "id") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setId:@
setId :: (IsMTRMediaPlaybackClusterTrackStruct mtrMediaPlaybackClusterTrackStruct, IsNSString value) => mtrMediaPlaybackClusterTrackStruct -> value -> IO ()
setId mtrMediaPlaybackClusterTrackStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterTrackStruct (mkSelector "setId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- trackAttributes@
trackAttributes :: IsMTRMediaPlaybackClusterTrackStruct mtrMediaPlaybackClusterTrackStruct => mtrMediaPlaybackClusterTrackStruct -> IO (Id MTRMediaPlaybackClusterTrackAttributesStruct)
trackAttributes mtrMediaPlaybackClusterTrackStruct  =
    sendMsg mtrMediaPlaybackClusterTrackStruct (mkSelector "trackAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTrackAttributes:@
setTrackAttributes :: (IsMTRMediaPlaybackClusterTrackStruct mtrMediaPlaybackClusterTrackStruct, IsMTRMediaPlaybackClusterTrackAttributesStruct value) => mtrMediaPlaybackClusterTrackStruct -> value -> IO ()
setTrackAttributes mtrMediaPlaybackClusterTrackStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterTrackStruct (mkSelector "setTrackAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @id@
idSelector :: Selector
idSelector = mkSelector "id"

-- | @Selector@ for @setId:@
setIdSelector :: Selector
setIdSelector = mkSelector "setId:"

-- | @Selector@ for @trackAttributes@
trackAttributesSelector :: Selector
trackAttributesSelector = mkSelector "trackAttributes"

-- | @Selector@ for @setTrackAttributes:@
setTrackAttributesSelector :: Selector
setTrackAttributesSelector = mkSelector "setTrackAttributes:"

