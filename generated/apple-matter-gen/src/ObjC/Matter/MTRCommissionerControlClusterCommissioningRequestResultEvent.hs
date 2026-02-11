{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommissionerControlClusterCommissioningRequestResultEvent@.
module ObjC.Matter.MTRCommissionerControlClusterCommissioningRequestResultEvent
  ( MTRCommissionerControlClusterCommissioningRequestResultEvent
  , IsMTRCommissionerControlClusterCommissioningRequestResultEvent(..)
  , requestID
  , setRequestID
  , clientNodeID
  , setClientNodeID
  , statusCode
  , setStatusCode
  , fabricIndex
  , setFabricIndex
  , requestIDSelector
  , setRequestIDSelector
  , clientNodeIDSelector
  , setClientNodeIDSelector
  , statusCodeSelector
  , setStatusCodeSelector
  , fabricIndexSelector
  , setFabricIndexSelector


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

-- | @- requestID@
requestID :: IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent => mtrCommissionerControlClusterCommissioningRequestResultEvent -> IO (Id NSNumber)
requestID mtrCommissionerControlClusterCommissioningRequestResultEvent  =
    sendMsg mtrCommissionerControlClusterCommissioningRequestResultEvent (mkSelector "requestID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequestID:@
setRequestID :: (IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent, IsNSNumber value) => mtrCommissionerControlClusterCommissioningRequestResultEvent -> value -> IO ()
setRequestID mtrCommissionerControlClusterCommissioningRequestResultEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterCommissioningRequestResultEvent (mkSelector "setRequestID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- clientNodeID@
clientNodeID :: IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent => mtrCommissionerControlClusterCommissioningRequestResultEvent -> IO (Id NSNumber)
clientNodeID mtrCommissionerControlClusterCommissioningRequestResultEvent  =
    sendMsg mtrCommissionerControlClusterCommissioningRequestResultEvent (mkSelector "clientNodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClientNodeID:@
setClientNodeID :: (IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent, IsNSNumber value) => mtrCommissionerControlClusterCommissioningRequestResultEvent -> value -> IO ()
setClientNodeID mtrCommissionerControlClusterCommissioningRequestResultEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterCommissioningRequestResultEvent (mkSelector "setClientNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- statusCode@
statusCode :: IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent => mtrCommissionerControlClusterCommissioningRequestResultEvent -> IO (Id NSNumber)
statusCode mtrCommissionerControlClusterCommissioningRequestResultEvent  =
    sendMsg mtrCommissionerControlClusterCommissioningRequestResultEvent (mkSelector "statusCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusCode:@
setStatusCode :: (IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent, IsNSNumber value) => mtrCommissionerControlClusterCommissioningRequestResultEvent -> value -> IO ()
setStatusCode mtrCommissionerControlClusterCommissioningRequestResultEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterCommissioningRequestResultEvent (mkSelector "setStatusCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent => mtrCommissionerControlClusterCommissioningRequestResultEvent -> IO (Id NSNumber)
fabricIndex mtrCommissionerControlClusterCommissioningRequestResultEvent  =
    sendMsg mtrCommissionerControlClusterCommissioningRequestResultEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent, IsNSNumber value) => mtrCommissionerControlClusterCommissioningRequestResultEvent -> value -> IO ()
setFabricIndex mtrCommissionerControlClusterCommissioningRequestResultEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterCommissioningRequestResultEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestID@
requestIDSelector :: Selector
requestIDSelector = mkSelector "requestID"

-- | @Selector@ for @setRequestID:@
setRequestIDSelector :: Selector
setRequestIDSelector = mkSelector "setRequestID:"

-- | @Selector@ for @clientNodeID@
clientNodeIDSelector :: Selector
clientNodeIDSelector = mkSelector "clientNodeID"

-- | @Selector@ for @setClientNodeID:@
setClientNodeIDSelector :: Selector
setClientNodeIDSelector = mkSelector "setClientNodeID:"

-- | @Selector@ for @statusCode@
statusCodeSelector :: Selector
statusCodeSelector = mkSelector "statusCode"

-- | @Selector@ for @setStatusCode:@
setStatusCodeSelector :: Selector
setStatusCodeSelector = mkSelector "setStatusCode:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

