{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterAddGroupParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterAddGroupParams
  ( MTRJointFabricDatastoreClusterAddGroupParams
  , IsMTRJointFabricDatastoreClusterAddGroupParams(..)
  , groupID
  , setGroupID
  , friendlyName
  , setFriendlyName
  , groupKeySetID
  , setGroupKeySetID
  , groupCAT
  , setGroupCAT
  , groupCATVersion
  , setGroupCATVersion
  , groupPermission
  , setGroupPermission
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupIDSelector
  , setGroupIDSelector
  , friendlyNameSelector
  , setFriendlyNameSelector
  , groupKeySetIDSelector
  , setGroupKeySetIDSelector
  , groupCATSelector
  , setGroupCATSelector
  , groupCATVersionSelector
  , setGroupCATVersionSelector
  , groupPermissionSelector
  , setGroupPermissionSelector
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

-- | @- groupID@
groupID :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
groupID mtrJointFabricDatastoreClusterAddGroupParams  =
    sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setGroupID mtrJointFabricDatastoreClusterAddGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterAddGroupParams  =
    sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "friendlyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSString value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterAddGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "setFriendlyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupKeySetID@
groupKeySetID :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
groupKeySetID mtrJointFabricDatastoreClusterAddGroupParams  =
    sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "groupKeySetID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupKeySetID:@
setGroupKeySetID :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setGroupKeySetID mtrJointFabricDatastoreClusterAddGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "setGroupKeySetID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupCAT@
groupCAT :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
groupCAT mtrJointFabricDatastoreClusterAddGroupParams  =
    sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "groupCAT") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupCAT:@
setGroupCAT :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setGroupCAT mtrJointFabricDatastoreClusterAddGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "setGroupCAT:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupCATVersion@
groupCATVersion :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
groupCATVersion mtrJointFabricDatastoreClusterAddGroupParams  =
    sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "groupCATVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupCATVersion:@
setGroupCATVersion :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setGroupCATVersion mtrJointFabricDatastoreClusterAddGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "setGroupCATVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupPermission@
groupPermission :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
groupPermission mtrJointFabricDatastoreClusterAddGroupParams  =
    sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "groupPermission") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupPermission:@
setGroupPermission :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setGroupPermission mtrJointFabricDatastoreClusterAddGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "setGroupPermission:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddGroupParams  =
    sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterAddGroupParams  =
    sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterAddGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddGroupParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupID@
groupIDSelector :: Selector
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @friendlyName@
friendlyNameSelector :: Selector
friendlyNameSelector = mkSelector "friendlyName"

-- | @Selector@ for @setFriendlyName:@
setFriendlyNameSelector :: Selector
setFriendlyNameSelector = mkSelector "setFriendlyName:"

-- | @Selector@ for @groupKeySetID@
groupKeySetIDSelector :: Selector
groupKeySetIDSelector = mkSelector "groupKeySetID"

-- | @Selector@ for @setGroupKeySetID:@
setGroupKeySetIDSelector :: Selector
setGroupKeySetIDSelector = mkSelector "setGroupKeySetID:"

-- | @Selector@ for @groupCAT@
groupCATSelector :: Selector
groupCATSelector = mkSelector "groupCAT"

-- | @Selector@ for @setGroupCAT:@
setGroupCATSelector :: Selector
setGroupCATSelector = mkSelector "setGroupCAT:"

-- | @Selector@ for @groupCATVersion@
groupCATVersionSelector :: Selector
groupCATVersionSelector = mkSelector "groupCATVersion"

-- | @Selector@ for @setGroupCATVersion:@
setGroupCATVersionSelector :: Selector
setGroupCATVersionSelector = mkSelector "setGroupCATVersion:"

-- | @Selector@ for @groupPermission@
groupPermissionSelector :: Selector
groupPermissionSelector = mkSelector "groupPermission"

-- | @Selector@ for @setGroupPermission:@
setGroupPermissionSelector :: Selector
setGroupPermissionSelector = mkSelector "setGroupPermission:"

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

