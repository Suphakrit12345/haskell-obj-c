{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterPlaybackPositionStruct@.
module ObjC.Matter.MTRMediaPlaybackClusterPlaybackPositionStruct
  ( MTRMediaPlaybackClusterPlaybackPositionStruct
  , IsMTRMediaPlaybackClusterPlaybackPositionStruct(..)
  , updatedAt
  , setUpdatedAt
  , position
  , setPosition
  , updatedAtSelector
  , setUpdatedAtSelector
  , positionSelector
  , setPositionSelector


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

-- | @- updatedAt@
updatedAt :: IsMTRMediaPlaybackClusterPlaybackPositionStruct mtrMediaPlaybackClusterPlaybackPositionStruct => mtrMediaPlaybackClusterPlaybackPositionStruct -> IO (Id NSNumber)
updatedAt mtrMediaPlaybackClusterPlaybackPositionStruct  =
    sendMsg mtrMediaPlaybackClusterPlaybackPositionStruct (mkSelector "updatedAt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUpdatedAt:@
setUpdatedAt :: (IsMTRMediaPlaybackClusterPlaybackPositionStruct mtrMediaPlaybackClusterPlaybackPositionStruct, IsNSNumber value) => mtrMediaPlaybackClusterPlaybackPositionStruct -> value -> IO ()
setUpdatedAt mtrMediaPlaybackClusterPlaybackPositionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterPlaybackPositionStruct (mkSelector "setUpdatedAt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- position@
position :: IsMTRMediaPlaybackClusterPlaybackPositionStruct mtrMediaPlaybackClusterPlaybackPositionStruct => mtrMediaPlaybackClusterPlaybackPositionStruct -> IO (Id NSNumber)
position mtrMediaPlaybackClusterPlaybackPositionStruct  =
    sendMsg mtrMediaPlaybackClusterPlaybackPositionStruct (mkSelector "position") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPosition:@
setPosition :: (IsMTRMediaPlaybackClusterPlaybackPositionStruct mtrMediaPlaybackClusterPlaybackPositionStruct, IsNSNumber value) => mtrMediaPlaybackClusterPlaybackPositionStruct -> value -> IO ()
setPosition mtrMediaPlaybackClusterPlaybackPositionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterPlaybackPositionStruct (mkSelector "setPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updatedAt@
updatedAtSelector :: Selector
updatedAtSelector = mkSelector "updatedAt"

-- | @Selector@ for @setUpdatedAt:@
setUpdatedAtSelector :: Selector
setUpdatedAtSelector = mkSelector "setUpdatedAt:"

-- | @Selector@ for @position@
positionSelector :: Selector
positionSelector = mkSelector "position"

-- | @Selector@ for @setPosition:@
setPositionSelector :: Selector
setPositionSelector = mkSelector "setPosition:"

