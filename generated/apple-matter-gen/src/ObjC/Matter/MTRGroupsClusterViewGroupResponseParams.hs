{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupsClusterViewGroupResponseParams@.
module ObjC.Matter.MTRGroupsClusterViewGroupResponseParams
  ( MTRGroupsClusterViewGroupResponseParams
  , IsMTRGroupsClusterViewGroupResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , groupID
  , setGroupID
  , groupName
  , setGroupName
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , groupId
  , setGroupId
  , initWithResponseValue_errorSelector
  , statusSelector
  , setStatusSelector
  , groupIDSelector
  , setGroupIDSelector
  , groupNameSelector
  , setGroupNameSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
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

-- | Initialize an MTRGroupsClusterViewGroupResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGroupsClusterViewGroupResponseParams -> responseValue -> error_ -> IO (Id MTRGroupsClusterViewGroupResponseParams)
initWithResponseValue_error mtrGroupsClusterViewGroupResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrGroupsClusterViewGroupResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams => mtrGroupsClusterViewGroupResponseParams -> IO (Id NSNumber)
status mtrGroupsClusterViewGroupResponseParams  =
    sendMsg mtrGroupsClusterViewGroupResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams, IsNSNumber value) => mtrGroupsClusterViewGroupResponseParams -> value -> IO ()
setStatus mtrGroupsClusterViewGroupResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterViewGroupResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupID@
groupID :: IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams => mtrGroupsClusterViewGroupResponseParams -> IO (Id NSNumber)
groupID mtrGroupsClusterViewGroupResponseParams  =
    sendMsg mtrGroupsClusterViewGroupResponseParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams, IsNSNumber value) => mtrGroupsClusterViewGroupResponseParams -> value -> IO ()
setGroupID mtrGroupsClusterViewGroupResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterViewGroupResponseParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupName@
groupName :: IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams => mtrGroupsClusterViewGroupResponseParams -> IO (Id NSString)
groupName mtrGroupsClusterViewGroupResponseParams  =
    sendMsg mtrGroupsClusterViewGroupResponseParams (mkSelector "groupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupName:@
setGroupName :: (IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams, IsNSString value) => mtrGroupsClusterViewGroupResponseParams -> value -> IO ()
setGroupName mtrGroupsClusterViewGroupResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterViewGroupResponseParams (mkSelector "setGroupName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams => mtrGroupsClusterViewGroupResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupsClusterViewGroupResponseParams  =
    sendMsg mtrGroupsClusterViewGroupResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams, IsNSNumber value) => mtrGroupsClusterViewGroupResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupsClusterViewGroupResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterViewGroupResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupId@
groupId :: IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams => mtrGroupsClusterViewGroupResponseParams -> IO (Id NSNumber)
groupId mtrGroupsClusterViewGroupResponseParams  =
    sendMsg mtrGroupsClusterViewGroupResponseParams (mkSelector "groupId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupId:@
setGroupId :: (IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams, IsNSNumber value) => mtrGroupsClusterViewGroupResponseParams -> value -> IO ()
setGroupId mtrGroupsClusterViewGroupResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterViewGroupResponseParams (mkSelector "setGroupId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

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

-- | @Selector@ for @groupId@
groupIdSelector :: Selector
groupIdSelector = mkSelector "groupId"

-- | @Selector@ for @setGroupId:@
setGroupIdSelector :: Selector
setGroupIdSelector = mkSelector "setGroupId:"

