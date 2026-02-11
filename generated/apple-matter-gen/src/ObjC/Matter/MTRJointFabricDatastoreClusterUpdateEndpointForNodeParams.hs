{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterUpdateEndpointForNodeParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterUpdateEndpointForNodeParams
  ( MTRJointFabricDatastoreClusterUpdateEndpointForNodeParams
  , IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams(..)
  , endpointID
  , setEndpointID
  , nodeID
  , setNodeID
  , friendlyName
  , setFriendlyName
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , endpointIDSelector
  , setEndpointIDSelector
  , nodeIDSelector
  , setNodeIDSelector
  , friendlyNameSelector
  , setFriendlyNameSelector
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

-- | @- endpointID@
endpointID :: IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> IO (Id NSNumber)
endpointID mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams (mkSelector "endpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> value -> IO ()
setEndpointID mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams (mkSelector "setEndpointID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nodeID@
nodeID :: IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams (mkSelector "friendlyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams, IsNSString value) => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams (mkSelector "setFriendlyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @friendlyName@
friendlyNameSelector :: Selector
friendlyNameSelector = mkSelector "friendlyName"

-- | @Selector@ for @setFriendlyName:@
setFriendlyNameSelector :: Selector
setFriendlyNameSelector = mkSelector "setFriendlyName:"

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

