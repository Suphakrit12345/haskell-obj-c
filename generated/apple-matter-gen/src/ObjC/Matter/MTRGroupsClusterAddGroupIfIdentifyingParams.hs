{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupsClusterAddGroupIfIdentifyingParams@.
module ObjC.Matter.MTRGroupsClusterAddGroupIfIdentifyingParams
  ( MTRGroupsClusterAddGroupIfIdentifyingParams
  , IsMTRGroupsClusterAddGroupIfIdentifyingParams(..)
  , groupID
  , setGroupID
  , groupName
  , setGroupName
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupId
  , setGroupId
  , groupIDSelector
  , setGroupIDSelector
  , groupNameSelector
  , setGroupNameSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , groupIdSelector
  , setGroupIdSelector


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
groupID :: IsMTRGroupsClusterAddGroupIfIdentifyingParams mtrGroupsClusterAddGroupIfIdentifyingParams => mtrGroupsClusterAddGroupIfIdentifyingParams -> IO (Id NSNumber)
groupID mtrGroupsClusterAddGroupIfIdentifyingParams  =
    sendMsg mtrGroupsClusterAddGroupIfIdentifyingParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupsClusterAddGroupIfIdentifyingParams mtrGroupsClusterAddGroupIfIdentifyingParams, IsNSNumber value) => mtrGroupsClusterAddGroupIfIdentifyingParams -> value -> IO ()
setGroupID mtrGroupsClusterAddGroupIfIdentifyingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupIfIdentifyingParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupName@
groupName :: IsMTRGroupsClusterAddGroupIfIdentifyingParams mtrGroupsClusterAddGroupIfIdentifyingParams => mtrGroupsClusterAddGroupIfIdentifyingParams -> IO (Id NSString)
groupName mtrGroupsClusterAddGroupIfIdentifyingParams  =
    sendMsg mtrGroupsClusterAddGroupIfIdentifyingParams (mkSelector "groupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupName:@
setGroupName :: (IsMTRGroupsClusterAddGroupIfIdentifyingParams mtrGroupsClusterAddGroupIfIdentifyingParams, IsNSString value) => mtrGroupsClusterAddGroupIfIdentifyingParams -> value -> IO ()
setGroupName mtrGroupsClusterAddGroupIfIdentifyingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupIfIdentifyingParams (mkSelector "setGroupName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupsClusterAddGroupIfIdentifyingParams mtrGroupsClusterAddGroupIfIdentifyingParams => mtrGroupsClusterAddGroupIfIdentifyingParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupsClusterAddGroupIfIdentifyingParams  =
    sendMsg mtrGroupsClusterAddGroupIfIdentifyingParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupsClusterAddGroupIfIdentifyingParams mtrGroupsClusterAddGroupIfIdentifyingParams, IsNSNumber value) => mtrGroupsClusterAddGroupIfIdentifyingParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupsClusterAddGroupIfIdentifyingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupIfIdentifyingParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGroupsClusterAddGroupIfIdentifyingParams mtrGroupsClusterAddGroupIfIdentifyingParams => mtrGroupsClusterAddGroupIfIdentifyingParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGroupsClusterAddGroupIfIdentifyingParams  =
    sendMsg mtrGroupsClusterAddGroupIfIdentifyingParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGroupsClusterAddGroupIfIdentifyingParams mtrGroupsClusterAddGroupIfIdentifyingParams, IsNSNumber value) => mtrGroupsClusterAddGroupIfIdentifyingParams -> value -> IO ()
setServerSideProcessingTimeout mtrGroupsClusterAddGroupIfIdentifyingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupIfIdentifyingParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupId@
groupId :: IsMTRGroupsClusterAddGroupIfIdentifyingParams mtrGroupsClusterAddGroupIfIdentifyingParams => mtrGroupsClusterAddGroupIfIdentifyingParams -> IO (Id NSNumber)
groupId mtrGroupsClusterAddGroupIfIdentifyingParams  =
    sendMsg mtrGroupsClusterAddGroupIfIdentifyingParams (mkSelector "groupId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupId:@
setGroupId :: (IsMTRGroupsClusterAddGroupIfIdentifyingParams mtrGroupsClusterAddGroupIfIdentifyingParams, IsNSNumber value) => mtrGroupsClusterAddGroupIfIdentifyingParams -> value -> IO ()
setGroupId mtrGroupsClusterAddGroupIfIdentifyingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupIfIdentifyingParams (mkSelector "setGroupId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupID@
groupIDSelector :: Selector
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector
groupNameSelector = mkSelector "groupName"

-- | @Selector@ for @setGroupName:@
setGroupNameSelector :: Selector
setGroupNameSelector = mkSelector "setGroupName:"

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

-- | @Selector@ for @groupId@
groupIdSelector :: Selector
groupIdSelector = mkSelector "groupId"

-- | @Selector@ for @setGroupId:@
setGroupIdSelector :: Selector
setGroupIdSelector = mkSelector "setGroupId:"

