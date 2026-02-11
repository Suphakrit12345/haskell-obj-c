{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterAppleSetAliroCredentialParams@.
module ObjC.Matter.MTRDoorLockClusterAppleSetAliroCredentialParams
  ( MTRDoorLockClusterAppleSetAliroCredentialParams
  , IsMTRDoorLockClusterAppleSetAliroCredentialParams(..)
  , operationType
  , setOperationType
  , credential
  , setCredential
  , credentialData
  , setCredentialData
  , userIndex
  , setUserIndex
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
operationType :: IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams => mtrDoorLockClusterAppleSetAliroCredentialParams -> IO (Id NSNumber)
operationType mtrDoorLockClusterAppleSetAliroCredentialParams  =
    sendMsg mtrDoorLockClusterAppleSetAliroCredentialParams (mkSelector "operationType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationType:@
setOperationType :: (IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams, IsNSNumber value) => mtrDoorLockClusterAppleSetAliroCredentialParams -> value -> IO ()
setOperationType mtrDoorLockClusterAppleSetAliroCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleSetAliroCredentialParams (mkSelector "setOperationType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credential@
credential :: IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams => mtrDoorLockClusterAppleSetAliroCredentialParams -> IO (Id MTRDoorLockClusterAppleAliroCredentialStruct)
credential mtrDoorLockClusterAppleSetAliroCredentialParams  =
    sendMsg mtrDoorLockClusterAppleSetAliroCredentialParams (mkSelector "credential") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredential:@
setCredential :: (IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams, IsMTRDoorLockClusterAppleAliroCredentialStruct value) => mtrDoorLockClusterAppleSetAliroCredentialParams -> value -> IO ()
setCredential mtrDoorLockClusterAppleSetAliroCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleSetAliroCredentialParams (mkSelector "setCredential:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentialData@
credentialData :: IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams => mtrDoorLockClusterAppleSetAliroCredentialParams -> IO (Id NSData)
credentialData mtrDoorLockClusterAppleSetAliroCredentialParams  =
    sendMsg mtrDoorLockClusterAppleSetAliroCredentialParams (mkSelector "credentialData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentialData:@
setCredentialData :: (IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams, IsNSData value) => mtrDoorLockClusterAppleSetAliroCredentialParams -> value -> IO ()
setCredentialData mtrDoorLockClusterAppleSetAliroCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleSetAliroCredentialParams (mkSelector "setCredentialData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams => mtrDoorLockClusterAppleSetAliroCredentialParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterAppleSetAliroCredentialParams  =
    sendMsg mtrDoorLockClusterAppleSetAliroCredentialParams (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams, IsNSNumber value) => mtrDoorLockClusterAppleSetAliroCredentialParams -> value -> IO ()
setUserIndex mtrDoorLockClusterAppleSetAliroCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleSetAliroCredentialParams (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams => mtrDoorLockClusterAppleSetAliroCredentialParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterAppleSetAliroCredentialParams  =
    sendMsg mtrDoorLockClusterAppleSetAliroCredentialParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams, IsNSNumber value) => mtrDoorLockClusterAppleSetAliroCredentialParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterAppleSetAliroCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleSetAliroCredentialParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams => mtrDoorLockClusterAppleSetAliroCredentialParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterAppleSetAliroCredentialParams  =
    sendMsg mtrDoorLockClusterAppleSetAliroCredentialParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams, IsNSNumber value) => mtrDoorLockClusterAppleSetAliroCredentialParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterAppleSetAliroCredentialParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleSetAliroCredentialParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

