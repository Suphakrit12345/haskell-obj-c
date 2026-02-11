{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupsClusterViewGroupParams@.
module ObjC.Matter.MTRGroupsClusterViewGroupParams
  ( MTRGroupsClusterViewGroupParams
  , IsMTRGroupsClusterViewGroupParams(..)
  , groupID
  , setGroupID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupId
  , setGroupId
  , groupIDSelector
  , setGroupIDSelector
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
groupID :: IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams => mtrGroupsClusterViewGroupParams -> IO (Id NSNumber)
groupID mtrGroupsClusterViewGroupParams  =
    sendMsg mtrGroupsClusterViewGroupParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams, IsNSNumber value) => mtrGroupsClusterViewGroupParams -> value -> IO ()
setGroupID mtrGroupsClusterViewGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterViewGroupParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams => mtrGroupsClusterViewGroupParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupsClusterViewGroupParams  =
    sendMsg mtrGroupsClusterViewGroupParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams, IsNSNumber value) => mtrGroupsClusterViewGroupParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupsClusterViewGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterViewGroupParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams => mtrGroupsClusterViewGroupParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGroupsClusterViewGroupParams  =
    sendMsg mtrGroupsClusterViewGroupParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams, IsNSNumber value) => mtrGroupsClusterViewGroupParams -> value -> IO ()
setServerSideProcessingTimeout mtrGroupsClusterViewGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterViewGroupParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupId@
groupId :: IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams => mtrGroupsClusterViewGroupParams -> IO (Id NSNumber)
groupId mtrGroupsClusterViewGroupParams  =
    sendMsg mtrGroupsClusterViewGroupParams (mkSelector "groupId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupId:@
setGroupId :: (IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams, IsNSNumber value) => mtrGroupsClusterViewGroupParams -> value -> IO ()
setGroupId mtrGroupsClusterViewGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterViewGroupParams (mkSelector "setGroupId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @groupId@
groupIdSelector :: Selector
groupIdSelector = mkSelector "groupId"

-- | @Selector@ for @setGroupId:@
setGroupIdSelector :: Selector
setGroupIdSelector = mkSelector "setGroupId:"

