{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterSetUserParams@.
module ObjC.Matter.MTRDoorLockClusterSetUserParams
  ( MTRDoorLockClusterSetUserParams
  , IsMTRDoorLockClusterSetUserParams(..)
  , operationType
  , setOperationType
  , userIndex
  , setUserIndex
  , userName
  , setUserName
  , userUniqueID
  , setUserUniqueID
  , userStatus
  , setUserStatus
  , userType
  , setUserType
  , credentialRule
  , setCredentialRule
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , userUniqueId
  , setUserUniqueId
  , operationTypeSelector
  , setOperationTypeSelector
  , userIndexSelector
  , setUserIndexSelector
  , userNameSelector
  , setUserNameSelector
  , userUniqueIDSelector
  , setUserUniqueIDSelector
  , userStatusSelector
  , setUserStatusSelector
  , userTypeSelector
  , setUserTypeSelector
  , credentialRuleSelector
  , setCredentialRuleSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , userUniqueIdSelector
  , setUserUniqueIdSelector


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

-- | @- operationType@
operationType :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
operationType mtrDoorLockClusterSetUserParams  =
    sendMsg mtrDoorLockClusterSetUserParams (mkSelector "operationType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationType:@
setOperationType :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setOperationType mtrDoorLockClusterSetUserParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetUserParams (mkSelector "setOperationType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterSetUserParams  =
    sendMsg mtrDoorLockClusterSetUserParams (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setUserIndex mtrDoorLockClusterSetUserParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetUserParams (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userName@
userName :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSString)
userName mtrDoorLockClusterSetUserParams  =
    sendMsg mtrDoorLockClusterSetUserParams (mkSelector "userName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserName:@
setUserName :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSString value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setUserName mtrDoorLockClusterSetUserParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetUserParams (mkSelector "setUserName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userUniqueID@
userUniqueID :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
userUniqueID mtrDoorLockClusterSetUserParams  =
    sendMsg mtrDoorLockClusterSetUserParams (mkSelector "userUniqueID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserUniqueID:@
setUserUniqueID :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setUserUniqueID mtrDoorLockClusterSetUserParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetUserParams (mkSelector "setUserUniqueID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userStatus@
userStatus :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
userStatus mtrDoorLockClusterSetUserParams  =
    sendMsg mtrDoorLockClusterSetUserParams (mkSelector "userStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserStatus:@
setUserStatus :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setUserStatus mtrDoorLockClusterSetUserParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetUserParams (mkSelector "setUserStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userType@
userType :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
userType mtrDoorLockClusterSetUserParams  =
    sendMsg mtrDoorLockClusterSetUserParams (mkSelector "userType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserType:@
setUserType :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setUserType mtrDoorLockClusterSetUserParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetUserParams (mkSelector "setUserType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentialRule@
credentialRule :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
credentialRule mtrDoorLockClusterSetUserParams  =
    sendMsg mtrDoorLockClusterSetUserParams (mkSelector "credentialRule") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentialRule:@
setCredentialRule :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setCredentialRule mtrDoorLockClusterSetUserParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetUserParams (mkSelector "setCredentialRule:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterSetUserParams  =
    sendMsg mtrDoorLockClusterSetUserParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterSetUserParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetUserParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterSetUserParams  =
    sendMsg mtrDoorLockClusterSetUserParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterSetUserParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetUserParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userUniqueId@
userUniqueId :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
userUniqueId mtrDoorLockClusterSetUserParams  =
    sendMsg mtrDoorLockClusterSetUserParams (mkSelector "userUniqueId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserUniqueId:@
setUserUniqueId :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setUserUniqueId mtrDoorLockClusterSetUserParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetUserParams (mkSelector "setUserUniqueId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operationType@
operationTypeSelector :: Selector
operationTypeSelector = mkSelector "operationType"

-- | @Selector@ for @setOperationType:@
setOperationTypeSelector :: Selector
setOperationTypeSelector = mkSelector "setOperationType:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector
setUserIndexSelector = mkSelector "setUserIndex:"

-- | @Selector@ for @userName@
userNameSelector :: Selector
userNameSelector = mkSelector "userName"

-- | @Selector@ for @setUserName:@
setUserNameSelector :: Selector
setUserNameSelector = mkSelector "setUserName:"

-- | @Selector@ for @userUniqueID@
userUniqueIDSelector :: Selector
userUniqueIDSelector = mkSelector "userUniqueID"

-- | @Selector@ for @setUserUniqueID:@
setUserUniqueIDSelector :: Selector
setUserUniqueIDSelector = mkSelector "setUserUniqueID:"

-- | @Selector@ for @userStatus@
userStatusSelector :: Selector
userStatusSelector = mkSelector "userStatus"

-- | @Selector@ for @setUserStatus:@
setUserStatusSelector :: Selector
setUserStatusSelector = mkSelector "setUserStatus:"

-- | @Selector@ for @userType@
userTypeSelector :: Selector
userTypeSelector = mkSelector "userType"

-- | @Selector@ for @setUserType:@
setUserTypeSelector :: Selector
setUserTypeSelector = mkSelector "setUserType:"

-- | @Selector@ for @credentialRule@
credentialRuleSelector :: Selector
credentialRuleSelector = mkSelector "credentialRule"

-- | @Selector@ for @setCredentialRule:@
setCredentialRuleSelector :: Selector
setCredentialRuleSelector = mkSelector "setCredentialRule:"

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

-- | @Selector@ for @userUniqueId@
userUniqueIdSelector :: Selector
userUniqueIdSelector = mkSelector "userUniqueId"

-- | @Selector@ for @setUserUniqueId:@
setUserUniqueIdSelector :: Selector
setUserUniqueIdSelector = mkSelector "setUserUniqueId:"

