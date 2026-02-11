{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterDSTStatusEvent@.
module ObjC.Matter.MTRTimeSynchronizationClusterDSTStatusEvent
  ( MTRTimeSynchronizationClusterDSTStatusEvent
  , IsMTRTimeSynchronizationClusterDSTStatusEvent(..)
  , dstOffsetActive
  , setDstOffsetActive
  , dstOffsetActiveSelector
  , setDstOffsetActiveSelector


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

-- | @- dstOffsetActive@
dstOffsetActive :: IsMTRTimeSynchronizationClusterDSTStatusEvent mtrTimeSynchronizationClusterDSTStatusEvent => mtrTimeSynchronizationClusterDSTStatusEvent -> IO (Id NSNumber)
dstOffsetActive mtrTimeSynchronizationClusterDSTStatusEvent  =
    sendMsg mtrTimeSynchronizationClusterDSTStatusEvent (mkSelector "dstOffsetActive") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDstOffsetActive:@
setDstOffsetActive :: (IsMTRTimeSynchronizationClusterDSTStatusEvent mtrTimeSynchronizationClusterDSTStatusEvent, IsNSNumber value) => mtrTimeSynchronizationClusterDSTStatusEvent -> value -> IO ()
setDstOffsetActive mtrTimeSynchronizationClusterDSTStatusEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterDSTStatusEvent (mkSelector "setDstOffsetActive:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dstOffsetActive@
dstOffsetActiveSelector :: Selector
dstOffsetActiveSelector = mkSelector "dstOffsetActive"

-- | @Selector@ for @setDstOffsetActive:@
setDstOffsetActiveSelector :: Selector
setDstOffsetActiveSelector = mkSelector "setDstOffsetActive:"

