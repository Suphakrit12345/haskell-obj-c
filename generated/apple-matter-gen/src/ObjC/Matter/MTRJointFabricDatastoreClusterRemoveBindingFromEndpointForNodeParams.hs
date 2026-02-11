{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams
  ( MTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams
  , IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams(..)
  , listID
  , setListID
  , endpointID
  , setEndpointID
  , nodeID
  , setNodeID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , listIDSelector
  , setListIDSelector
  , endpointIDSelector
  , setEndpointIDSelector
  , nodeIDSelector
  , setNodeIDSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector


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

-- | @- listID@
listID :: IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams => mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams -> IO (Id NSNumber)
listID mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams (mkSelector "listID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setListID:@
setListID :: (IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams -> value -> IO ()
setListID mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams (mkSelector "setListID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpointID@
endpointID :: IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams => mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams -> IO (Id NSNumber)
endpointID mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams (mkSelector "endpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams -> value -> IO ()
setEndpointID mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams (mkSelector "setEndpointID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nodeID@
nodeID :: IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams => mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams => mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams => mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @listID@
listIDSelector :: Selector
listIDSelector = mkSelector "listID"

-- | @Selector@ for @setListID:@
setListIDSelector :: Selector
setListIDSelector = mkSelector "setListID:"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector
setEndpointIDSelector = mkSelector "setEndpointID:"

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

