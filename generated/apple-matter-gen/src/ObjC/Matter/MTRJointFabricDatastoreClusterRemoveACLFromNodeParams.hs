{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterRemoveACLFromNodeParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterRemoveACLFromNodeParams
  ( MTRJointFabricDatastoreClusterRemoveACLFromNodeParams
  , IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams(..)
  , listID
  , setListID
  , nodeID
  , setNodeID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , listIDSelector
  , setListIDSelector
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
listID :: IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> IO (Id NSNumber)
listID mtrJointFabricDatastoreClusterRemoveACLFromNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterRemoveACLFromNodeParams (mkSelector "listID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setListID:@
setListID :: (IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> value -> IO ()
setListID mtrJointFabricDatastoreClusterRemoveACLFromNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterRemoveACLFromNodeParams (mkSelector "setListID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nodeID@
nodeID :: IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterRemoveACLFromNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterRemoveACLFromNodeParams (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterRemoveACLFromNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterRemoveACLFromNodeParams (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterRemoveACLFromNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterRemoveACLFromNodeParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterRemoveACLFromNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterRemoveACLFromNodeParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterRemoveACLFromNodeParams  =
    sendMsg mtrJointFabricDatastoreClusterRemoveACLFromNodeParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterRemoveACLFromNodeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterRemoveACLFromNodeParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @listID@
listIDSelector :: Selector
listIDSelector = mkSelector "listID"

-- | @Selector@ for @setListID:@
setListIDSelector :: Selector
setListIDSelector = mkSelector "setListID:"

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

