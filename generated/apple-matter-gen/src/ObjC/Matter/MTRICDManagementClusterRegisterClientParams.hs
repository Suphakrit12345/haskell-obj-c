{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRICDManagementClusterRegisterClientParams@.
module ObjC.Matter.MTRICDManagementClusterRegisterClientParams
  ( MTRICDManagementClusterRegisterClientParams
  , IsMTRICDManagementClusterRegisterClientParams(..)
  , checkInNodeID
  , setCheckInNodeID
  , monitoredSubject
  , setMonitoredSubject
  , key
  , setKey
  , verificationKey
  , setVerificationKey
  , clientType
  , setClientType
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , checkInNodeIDSelector
  , setCheckInNodeIDSelector
  , monitoredSubjectSelector
  , setMonitoredSubjectSelector
  , keySelector
  , setKeySelector
  , verificationKeySelector
  , setVerificationKeySelector
  , clientTypeSelector
  , setClientTypeSelector
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

-- | @- checkInNodeID@
checkInNodeID :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSNumber)
checkInNodeID mtricdManagementClusterRegisterClientParams  =
    sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "checkInNodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCheckInNodeID:@
setCheckInNodeID :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSNumber value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setCheckInNodeID mtricdManagementClusterRegisterClientParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "setCheckInNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- monitoredSubject@
monitoredSubject :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSNumber)
monitoredSubject mtricdManagementClusterRegisterClientParams  =
    sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "monitoredSubject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMonitoredSubject:@
setMonitoredSubject :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSNumber value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setMonitoredSubject mtricdManagementClusterRegisterClientParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "setMonitoredSubject:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- key@
key :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSData)
key mtricdManagementClusterRegisterClientParams  =
    sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "key") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKey:@
setKey :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSData value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setKey mtricdManagementClusterRegisterClientParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "setKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- verificationKey@
verificationKey :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSData)
verificationKey mtricdManagementClusterRegisterClientParams  =
    sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "verificationKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVerificationKey:@
setVerificationKey :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSData value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setVerificationKey mtricdManagementClusterRegisterClientParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "setVerificationKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- clientType@
clientType :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSNumber)
clientType mtricdManagementClusterRegisterClientParams  =
    sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "clientType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClientType:@
setClientType :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSNumber value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setClientType mtricdManagementClusterRegisterClientParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "setClientType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtricdManagementClusterRegisterClientParams  =
    sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSNumber value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setTimedInvokeTimeoutMs mtricdManagementClusterRegisterClientParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtricdManagementClusterRegisterClientParams  =
    sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSNumber value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setServerSideProcessingTimeout mtricdManagementClusterRegisterClientParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtricdManagementClusterRegisterClientParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkInNodeID@
checkInNodeIDSelector :: Selector
checkInNodeIDSelector = mkSelector "checkInNodeID"

-- | @Selector@ for @setCheckInNodeID:@
setCheckInNodeIDSelector :: Selector
setCheckInNodeIDSelector = mkSelector "setCheckInNodeID:"

-- | @Selector@ for @monitoredSubject@
monitoredSubjectSelector :: Selector
monitoredSubjectSelector = mkSelector "monitoredSubject"

-- | @Selector@ for @setMonitoredSubject:@
setMonitoredSubjectSelector :: Selector
setMonitoredSubjectSelector = mkSelector "setMonitoredSubject:"

-- | @Selector@ for @key@
keySelector :: Selector
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @verificationKey@
verificationKeySelector :: Selector
verificationKeySelector = mkSelector "verificationKey"

-- | @Selector@ for @setVerificationKey:@
setVerificationKeySelector :: Selector
setVerificationKeySelector = mkSelector "setVerificationKey:"

-- | @Selector@ for @clientType@
clientTypeSelector :: Selector
clientTypeSelector = mkSelector "clientType"

-- | @Selector@ for @setClientType:@
setClientTypeSelector :: Selector
setClientTypeSelector = mkSelector "setClientType:"

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

