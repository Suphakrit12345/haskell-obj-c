{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterSetCredentialParams@.
module ObjC.Matter.MTRDoorLockClusterSetCredentialParams
  ( MTRDoorLockClusterSetCredentialParams
  , IsMTRDoorLockClusterSetCredentialParams(..)
  , operationType
  , setOperationType
  , credential
  , setCredential
  , credentialData
  , setCredentialData
  , userIndex
  , setUserIndex
  , userStatus
  , setUserStatus
  , userType
  , setUserType
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , operationTypeSelector
  , setOperationTypeSelector
  , credentialSelector
  , setCredentialSelector
  , credentialDataSelector
  , setCredentialDataSelector
  , userIndexSelector
  , setUserIndexSelector
  , userStatusSelector
  , setUserStatusSelector
  , userTypeSelector
  , setUserTypeSelector
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

-- | @- operationType@
operationType :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSNumber)
operationType mtrDoorLockClusterSetCredentialParams  =
    sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "operationType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationType:@
setOperationType :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setOperationType mtrDoorLockClusterSetCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "setOperationType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credential@
credential :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id MTRDoorLockClusterCredentialStruct)
credential mtrDoorLockClusterSetCredentialParams  =
    sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "credential") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredential:@
setCredential :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsMTRDoorLockClusterCredentialStruct value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setCredential mtrDoorLockClusterSetCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "setCredential:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentialData@
credentialData :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSData)
credentialData mtrDoorLockClusterSetCredentialParams  =
    sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "credentialData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentialData:@
setCredentialData :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSData value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setCredentialData mtrDoorLockClusterSetCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "setCredentialData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterSetCredentialParams  =
    sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setUserIndex mtrDoorLockClusterSetCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userStatus@
userStatus :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSNumber)
userStatus mtrDoorLockClusterSetCredentialParams  =
    sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "userStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserStatus:@
setUserStatus :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setUserStatus mtrDoorLockClusterSetCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "setUserStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userType@
userType :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSNumber)
userType mtrDoorLockClusterSetCredentialParams  =
    sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "userType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserType:@
setUserType :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setUserType mtrDoorLockClusterSetCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "setUserType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterSetCredentialParams  =
    sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterSetCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterSetCredentialParams  =
    sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterSetCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetCredentialParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operationType@
operationTypeSelector :: Selector
operationTypeSelector = mkSelector "operationType"

-- | @Selector@ for @setOperationType:@
setOperationTypeSelector :: Selector
setOperationTypeSelector = mkSelector "setOperationType:"

-- | @Selector@ for @credential@
credentialSelector :: Selector
credentialSelector = mkSelector "credential"

-- | @Selector@ for @setCredential:@
setCredentialSelector :: Selector
setCredentialSelector = mkSelector "setCredential:"

-- | @Selector@ for @credentialData@
credentialDataSelector :: Selector
credentialDataSelector = mkSelector "credentialData"

-- | @Selector@ for @setCredentialData:@
setCredentialDataSelector :: Selector
setCredentialDataSelector = mkSelector "setCredentialData:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector
setUserIndexSelector = mkSelector "setUserIndex:"

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

