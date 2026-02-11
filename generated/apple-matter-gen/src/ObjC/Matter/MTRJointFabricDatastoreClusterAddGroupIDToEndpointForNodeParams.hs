{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams
  ( MTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams
  , IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams(..)
  , nodeID
  , setNodeID
  , endpointID
  , setEndpointID
  , groupID
  , setGroupID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , nodeIDSelector
  , setNodeIDSelector
  , endpointIDSelector
  , setEndpointIDSelector
  , groupIDSelector
  , setGroupIDSelector
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

-- | @- nodeID@
nodeID :: IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpointID@
endpointID :: IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> IO (Id NSNumber)
endpointID mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams (mkSelector "endpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> value -> IO ()
setEndpointID mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams (mkSelector "setEndpointID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupID@
groupID :: IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> IO (Id NSNumber)
groupID mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> value -> IO ()
setGroupID mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector
setEndpointIDSelector = mkSelector "setEndpointID:"

-- | @Selector@ for @groupID@
groupIDSelector :: Selector
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector
setGroupIDSelector = mkSelector "setGroupID:"

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

