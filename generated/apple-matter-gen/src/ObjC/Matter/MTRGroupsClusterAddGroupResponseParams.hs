{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupsClusterAddGroupResponseParams@.
module ObjC.Matter.MTRGroupsClusterAddGroupResponseParams
  ( MTRGroupsClusterAddGroupResponseParams
  , IsMTRGroupsClusterAddGroupResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , groupID
  , setGroupID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , groupId
  , setGroupId
  , initWithResponseValue_errorSelector
  , statusSelector
  , setStatusSelector
  , groupIDSelector
  , setGroupIDSelector
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

-- | Initialize an MTRGroupsClusterAddGroupResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGroupsClusterAddGroupResponseParams mtrGroupsClusterAddGroupResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGroupsClusterAddGroupResponseParams -> responseValue -> error_ -> IO (Id MTRGroupsClusterAddGroupResponseParams)
initWithResponseValue_error mtrGroupsClusterAddGroupResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrGroupsClusterAddGroupResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRGroupsClusterAddGroupResponseParams mtrGroupsClusterAddGroupResponseParams => mtrGroupsClusterAddGroupResponseParams -> IO (Id NSNumber)
status mtrGroupsClusterAddGroupResponseParams  =
    sendMsg mtrGroupsClusterAddGroupResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRGroupsClusterAddGroupResponseParams mtrGroupsClusterAddGroupResponseParams, IsNSNumber value) => mtrGroupsClusterAddGroupResponseParams -> value -> IO ()
setStatus mtrGroupsClusterAddGroupResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupID@
groupID :: IsMTRGroupsClusterAddGroupResponseParams mtrGroupsClusterAddGroupResponseParams => mtrGroupsClusterAddGroupResponseParams -> IO (Id NSNumber)
groupID mtrGroupsClusterAddGroupResponseParams  =
    sendMsg mtrGroupsClusterAddGroupResponseParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupsClusterAddGroupResponseParams mtrGroupsClusterAddGroupResponseParams, IsNSNumber value) => mtrGroupsClusterAddGroupResponseParams -> value -> IO ()
setGroupID mtrGroupsClusterAddGroupResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupResponseParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupsClusterAddGroupResponseParams mtrGroupsClusterAddGroupResponseParams => mtrGroupsClusterAddGroupResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupsClusterAddGroupResponseParams  =
    sendMsg mtrGroupsClusterAddGroupResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupsClusterAddGroupResponseParams mtrGroupsClusterAddGroupResponseParams, IsNSNumber value) => mtrGroupsClusterAddGroupResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupsClusterAddGroupResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupId@
groupId :: IsMTRGroupsClusterAddGroupResponseParams mtrGroupsClusterAddGroupResponseParams => mtrGroupsClusterAddGroupResponseParams -> IO (Id NSNumber)
groupId mtrGroupsClusterAddGroupResponseParams  =
    sendMsg mtrGroupsClusterAddGroupResponseParams (mkSelector "groupId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupId:@
setGroupId :: (IsMTRGroupsClusterAddGroupResponseParams mtrGroupsClusterAddGroupResponseParams, IsNSNumber value) => mtrGroupsClusterAddGroupResponseParams -> value -> IO ()
setGroupId mtrGroupsClusterAddGroupResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterAddGroupResponseParams (mkSelector "setGroupId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

