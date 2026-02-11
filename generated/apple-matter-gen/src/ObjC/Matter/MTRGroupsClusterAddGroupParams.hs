{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupsClusterAddGroupParams@.
module ObjC.Matter.MTRGroupsClusterAddGroupParams
  ( MTRGroupsClusterAddGroupParams
  , IsMTRGroupsClusterAddGroupParams(..)
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
groupID :: IsMTRGroupsClusterAddGroupParams mtrGroupsClusterAddGroupParams => mtrGroupsClusterAddGroupParams -> IO (Id NSNumber)
groupID mtrGroupsClusterAddGroupParams  =
    sendMsg mtrGroupsClusterAddGroupParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupsClusterAddGroupParams mtrGroupsClusterAddGroupParams, IsNSNumber value) => mtrGroupsClusterAddGroupParams -> value -> IO ()
setGroupID mtrGroupsClusterAddGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupName@
groupName :: IsMTRGroupsClusterAddGroupParams mtrGroupsClusterAddGroupParams => mtrGroupsClusterAddGroupParams -> IO (Id NSString)
groupName mtrGroupsClusterAddGroupParams  =
    sendMsg mtrGroupsClusterAddGroupParams (mkSelector "groupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupName:@
setGroupName :: (IsMTRGroupsClusterAddGroupParams mtrGroupsClusterAddGroupParams, IsNSString value) => mtrGroupsClusterAddGroupParams -> value -> IO ()
setGroupName mtrGroupsClusterAddGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupParams (mkSelector "setGroupName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupsClusterAddGroupParams mtrGroupsClusterAddGroupParams => mtrGroupsClusterAddGroupParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupsClusterAddGroupParams  =
    sendMsg mtrGroupsClusterAddGroupParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupsClusterAddGroupParams mtrGroupsClusterAddGroupParams, IsNSNumber value) => mtrGroupsClusterAddGroupParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupsClusterAddGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGroupsClusterAddGroupParams mtrGroupsClusterAddGroupParams => mtrGroupsClusterAddGroupParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGroupsClusterAddGroupParams  =
    sendMsg mtrGroupsClusterAddGroupParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGroupsClusterAddGroupParams mtrGroupsClusterAddGroupParams, IsNSNumber value) => mtrGroupsClusterAddGroupParams -> value -> IO ()
setServerSideProcessingTimeout mtrGroupsClusterAddGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupId@
groupId :: IsMTRGroupsClusterAddGroupParams mtrGroupsClusterAddGroupParams => mtrGroupsClusterAddGroupParams -> IO (Id NSNumber)
groupId mtrGroupsClusterAddGroupParams  =
    sendMsg mtrGroupsClusterAddGroupParams (mkSelector "groupId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupId:@
setGroupId :: (IsMTRGroupsClusterAddGroupParams mtrGroupsClusterAddGroupParams, IsNSNumber value) => mtrGroupsClusterAddGroupParams -> value -> IO ()
setGroupId mtrGroupsClusterAddGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupParams (mkSelector "setGroupId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

