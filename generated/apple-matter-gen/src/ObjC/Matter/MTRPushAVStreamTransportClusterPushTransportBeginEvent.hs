{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterPushTransportBeginEvent@.
module ObjC.Matter.MTRPushAVStreamTransportClusterPushTransportBeginEvent
  ( MTRPushAVStreamTransportClusterPushTransportBeginEvent
  , IsMTRPushAVStreamTransportClusterPushTransportBeginEvent(..)
  , connectionID
  , setConnectionID
  , triggerType
  , setTriggerType
  , activationReason
  , setActivationReason
  , connectionIDSelector
  , setConnectionIDSelector
  , triggerTypeSelector
  , setTriggerTypeSelector
  , activationReasonSelector
  , setActivationReasonSelector


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
connectionID :: IsMTRPushAVStreamTransportClusterPushTransportBeginEvent mtrPushAVStreamTransportClusterPushTransportBeginEvent => mtrPushAVStreamTransportClusterPushTransportBeginEvent -> IO (Id NSNumber)
connectionID mtrPushAVStreamTransportClusterPushTransportBeginEvent  =
    sendMsg mtrPushAVStreamTransportClusterPushTransportBeginEvent (mkSelector "connectionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConnectionID:@
setConnectionID :: (IsMTRPushAVStreamTransportClusterPushTransportBeginEvent mtrPushAVStreamTransportClusterPushTransportBeginEvent, IsNSNumber value) => mtrPushAVStreamTransportClusterPushTransportBeginEvent -> value -> IO ()
setConnectionID mtrPushAVStreamTransportClusterPushTransportBeginEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterPushTransportBeginEvent (mkSelector "setConnectionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- triggerType@
triggerType :: IsMTRPushAVStreamTransportClusterPushTransportBeginEvent mtrPushAVStreamTransportClusterPushTransportBeginEvent => mtrPushAVStreamTransportClusterPushTransportBeginEvent -> IO (Id NSNumber)
triggerType mtrPushAVStreamTransportClusterPushTransportBeginEvent  =
    sendMsg mtrPushAVStreamTransportClusterPushTransportBeginEvent (mkSelector "triggerType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTriggerType:@
setTriggerType :: (IsMTRPushAVStreamTransportClusterPushTransportBeginEvent mtrPushAVStreamTransportClusterPushTransportBeginEvent, IsNSNumber value) => mtrPushAVStreamTransportClusterPushTransportBeginEvent -> value -> IO ()
setTriggerType mtrPushAVStreamTransportClusterPushTransportBeginEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterPushTransportBeginEvent (mkSelector "setTriggerType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- activationReason@
activationReason :: IsMTRPushAVStreamTransportClusterPushTransportBeginEvent mtrPushAVStreamTransportClusterPushTransportBeginEvent => mtrPushAVStreamTransportClusterPushTransportBeginEvent -> IO (Id NSNumber)
activationReason mtrPushAVStreamTransportClusterPushTransportBeginEvent  =
    sendMsg mtrPushAVStreamTransportClusterPushTransportBeginEvent (mkSelector "activationReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActivationReason:@
setActivationReason :: (IsMTRPushAVStreamTransportClusterPushTransportBeginEvent mtrPushAVStreamTransportClusterPushTransportBeginEvent, IsNSNumber value) => mtrPushAVStreamTransportClusterPushTransportBeginEvent -> value -> IO ()
setActivationReason mtrPushAVStreamTransportClusterPushTransportBeginEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterPushTransportBeginEvent (mkSelector "setActivationReason:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionID@
connectionIDSelector :: Selector
connectionIDSelector = mkSelector "connectionID"

-- | @Selector@ for @setConnectionID:@
setConnectionIDSelector :: Selector
setConnectionIDSelector = mkSelector "setConnectionID:"

-- | @Selector@ for @triggerType@
triggerTypeSelector :: Selector
triggerTypeSelector = mkSelector "triggerType"

-- | @Selector@ for @setTriggerType:@
setTriggerTypeSelector :: Selector
setTriggerTypeSelector = mkSelector "setTriggerType:"

-- | @Selector@ for @activationReason@
activationReasonSelector :: Selector
activationReasonSelector = mkSelector "activationReason"

-- | @Selector@ for @setActivationReason:@
setActivationReasonSelector :: Selector
setActivationReasonSelector = mkSelector "setActivationReason:"

