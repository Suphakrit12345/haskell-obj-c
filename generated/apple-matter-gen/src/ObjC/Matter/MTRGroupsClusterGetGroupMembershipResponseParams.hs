{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupsClusterGetGroupMembershipResponseParams@.
module ObjC.Matter.MTRGroupsClusterGetGroupMembershipResponseParams
  ( MTRGroupsClusterGetGroupMembershipResponseParams
  , IsMTRGroupsClusterGetGroupMembershipResponseParams(..)
  , initWithResponseValue_error
  , capacity
  , setCapacity
  , groupList
  , setGroupList
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , capacitySelector
  , setCapacitySelector
  , groupListSelector
  , setGroupListSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector


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

-- | Initialize an MTRGroupsClusterGetGroupMembershipResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGroupsClusterGetGroupMembershipResponseParams -> responseValue -> error_ -> IO (Id MTRGroupsClusterGetGroupMembershipResponseParams)
initWithResponseValue_error mtrGroupsClusterGetGroupMembershipResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrGroupsClusterGetGroupMembershipResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- capacity@
capacity :: IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams => mtrGroupsClusterGetGroupMembershipResponseParams -> IO (Id NSNumber)
capacity mtrGroupsClusterGetGroupMembershipResponseParams  =
    sendMsg mtrGroupsClusterGetGroupMembershipResponseParams (mkSelector "capacity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCapacity:@
setCapacity :: (IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams, IsNSNumber value) => mtrGroupsClusterGetGroupMembershipResponseParams -> value -> IO ()
setCapacity mtrGroupsClusterGetGroupMembershipResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterGetGroupMembershipResponseParams (mkSelector "setCapacity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupList@
groupList :: IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams => mtrGroupsClusterGetGroupMembershipResponseParams -> IO (Id NSArray)
groupList mtrGroupsClusterGetGroupMembershipResponseParams  =
    sendMsg mtrGroupsClusterGetGroupMembershipResponseParams (mkSelector "groupList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupList:@
setGroupList :: (IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams, IsNSArray value) => mtrGroupsClusterGetGroupMembershipResponseParams -> value -> IO ()
setGroupList mtrGroupsClusterGetGroupMembershipResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterGetGroupMembershipResponseParams (mkSelector "setGroupList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams => mtrGroupsClusterGetGroupMembershipResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupsClusterGetGroupMembershipResponseParams  =
    sendMsg mtrGroupsClusterGetGroupMembershipResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams, IsNSNumber value) => mtrGroupsClusterGetGroupMembershipResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupsClusterGetGroupMembershipResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupsClusterGetGroupMembershipResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @capacity@
capacitySelector :: Selector
capacitySelector = mkSelector "capacity"

-- | @Selector@ for @setCapacity:@
setCapacitySelector :: Selector
setCapacitySelector = mkSelector "setCapacity:"

-- | @Selector@ for @groupList@
groupListSelector :: Selector
groupListSelector = mkSelector "groupList"

-- | @Selector@ for @setGroupList:@
setGroupListSelector :: Selector
setGroupListSelector = mkSelector "setGroupList:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

