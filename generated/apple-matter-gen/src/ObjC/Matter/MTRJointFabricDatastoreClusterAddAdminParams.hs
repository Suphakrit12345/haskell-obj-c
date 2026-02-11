{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterAddAdminParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterAddAdminParams
  ( MTRJointFabricDatastoreClusterAddAdminParams
  , IsMTRJointFabricDatastoreClusterAddAdminParams(..)
  , nodeID
  , setNodeID
  , friendlyName
  , setFriendlyName
  , vendorID
  , setVendorID
  , icac
  , setIcac
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , nodeIDSelector
  , setNodeIDSelector
  , friendlyNameSelector
  , setFriendlyNameSelector
  , vendorIDSelector
  , setVendorIDSelector
  , icacSelector
  , setIcacSelector
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
nodeID :: IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams => mtrJointFabricDatastoreClusterAddAdminParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterAddAdminParams  =
    sendMsg mtrJointFabricDatastoreClusterAddAdminParams (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddAdminParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterAddAdminParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddAdminParams (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams => mtrJointFabricDatastoreClusterAddAdminParams -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterAddAdminParams  =
    sendMsg mtrJointFabricDatastoreClusterAddAdminParams (mkSelector "friendlyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams, IsNSString value) => mtrJointFabricDatastoreClusterAddAdminParams -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterAddAdminParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddAdminParams (mkSelector "setFriendlyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vendorID@
vendorID :: IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams => mtrJointFabricDatastoreClusterAddAdminParams -> IO (Id NSNumber)
vendorID mtrJointFabricDatastoreClusterAddAdminParams  =
    sendMsg mtrJointFabricDatastoreClusterAddAdminParams (mkSelector "vendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVendorID:@
setVendorID :: (IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddAdminParams -> value -> IO ()
setVendorID mtrJointFabricDatastoreClusterAddAdminParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddAdminParams (mkSelector "setVendorID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- icac@
icac :: IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams => mtrJointFabricDatastoreClusterAddAdminParams -> IO (Id NSData)
icac mtrJointFabricDatastoreClusterAddAdminParams  =
    sendMsg mtrJointFabricDatastoreClusterAddAdminParams (mkSelector "icac") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIcac:@
setIcac :: (IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams, IsNSData value) => mtrJointFabricDatastoreClusterAddAdminParams -> value -> IO ()
setIcac mtrJointFabricDatastoreClusterAddAdminParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddAdminParams (mkSelector "setIcac:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams => mtrJointFabricDatastoreClusterAddAdminParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddAdminParams  =
    sendMsg mtrJointFabricDatastoreClusterAddAdminParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddAdminParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddAdminParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddAdminParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams => mtrJointFabricDatastoreClusterAddAdminParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterAddAdminParams  =
    sendMsg mtrJointFabricDatastoreClusterAddAdminParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddAdminParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterAddAdminParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddAdminParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @icac@
icacSelector :: Selector
icacSelector = mkSelector "icac"

-- | @Selector@ for @setIcac:@
setIcacSelector :: Selector
setIcacSelector = mkSelector "setIcac:"

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

