{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterPushTransportEndEvent@.
module ObjC.Matter.MTRPushAVStreamTransportClusterPushTransportEndEvent
  ( MTRPushAVStreamTransportClusterPushTransportEndEvent
  , IsMTRPushAVStreamTransportClusterPushTransportEndEvent(..)
  , connectionID
  , setConnectionID
  , connectionIDSelector
  , setConnectionIDSelector


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

-- | @- connectionID@
connectionID :: IsMTRPushAVStreamTransportClusterPushTransportEndEvent mtrPushAVStreamTransportClusterPushTransportEndEvent => mtrPushAVStreamTransportClusterPushTransportEndEvent -> IO (Id NSNumber)
connectionID mtrPushAVStreamTransportClusterPushTransportEndEvent  =
    sendMsg mtrPushAVStreamTransportClusterPushTransportEndEvent (mkSelector "connectionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConnectionID:@
setConnectionID :: (IsMTRPushAVStreamTransportClusterPushTransportEndEvent mtrPushAVStreamTransportClusterPushTransportEndEvent, IsNSNumber value) => mtrPushAVStreamTransportClusterPushTransportEndEvent -> value -> IO ()
setConnectionID mtrPushAVStreamTransportClusterPushTransportEndEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterPushTransportEndEvent (mkSelector "setConnectionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionID@
connectionIDSelector :: Selector
connectionIDSelector = mkSelector "connectionID"

-- | @Selector@ for @setConnectionID:@
setConnectionIDSelector :: Selector
setConnectionIDSelector = mkSelector "setConnectionID:"

