{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterPlaybackPosition@.
module ObjC.Matter.MTRMediaPlaybackClusterPlaybackPosition
  ( MTRMediaPlaybackClusterPlaybackPosition
  , IsMTRMediaPlaybackClusterPlaybackPosition(..)
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
updatedAt :: IsMTRMediaPlaybackClusterPlaybackPosition mtrMediaPlaybackClusterPlaybackPosition => mtrMediaPlaybackClusterPlaybackPosition -> IO (Id NSNumber)
updatedAt mtrMediaPlaybackClusterPlaybackPosition  =
    sendMsg mtrMediaPlaybackClusterPlaybackPosition (mkSelector "updatedAt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUpdatedAt:@
setUpdatedAt :: (IsMTRMediaPlaybackClusterPlaybackPosition mtrMediaPlaybackClusterPlaybackPosition, IsNSNumber value) => mtrMediaPlaybackClusterPlaybackPosition -> value -> IO ()
setUpdatedAt mtrMediaPlaybackClusterPlaybackPosition  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterPlaybackPosition (mkSelector "setUpdatedAt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- position@
position :: IsMTRMediaPlaybackClusterPlaybackPosition mtrMediaPlaybackClusterPlaybackPosition => mtrMediaPlaybackClusterPlaybackPosition -> IO (Id NSNumber)
position mtrMediaPlaybackClusterPlaybackPosition  =
    sendMsg mtrMediaPlaybackClusterPlaybackPosition (mkSelector "position") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPosition:@
setPosition :: (IsMTRMediaPlaybackClusterPlaybackPosition mtrMediaPlaybackClusterPlaybackPosition, IsNSNumber value) => mtrMediaPlaybackClusterPlaybackPosition -> value -> IO ()
setPosition mtrMediaPlaybackClusterPlaybackPosition  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterPlaybackPosition (mkSelector "setPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

