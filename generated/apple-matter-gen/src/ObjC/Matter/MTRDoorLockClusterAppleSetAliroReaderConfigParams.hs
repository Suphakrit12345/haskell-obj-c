{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterAppleSetAliroReaderConfigParams@.
module ObjC.Matter.MTRDoorLockClusterAppleSetAliroReaderConfigParams
  ( MTRDoorLockClusterAppleSetAliroReaderConfigParams
  , IsMTRDoorLockClusterAppleSetAliroReaderConfigParams(..)
  , signingKey
  , setSigningKey
  , verificationKey
  , setVerificationKey
  , groupIdentifier
  , setGroupIdentifier
  , groupResolvingKey
  , setGroupResolvingKey
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , signingKeySelector
  , setSigningKeySelector
  , verificationKeySelector
  , setVerificationKeySelector
  , groupIdentifierSelector
  , setGroupIdentifierSelector
  , groupResolvingKeySelector
  , setGroupResolvingKeySelector
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

-- | @- signingKey@
signingKey :: IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> IO (Id NSData)
signingKey mtrDoorLockClusterAppleSetAliroReaderConfigParams  =
    sendMsg mtrDoorLockClusterAppleSetAliroReaderConfigParams (mkSelector "signingKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSigningKey:@
setSigningKey :: (IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> value -> IO ()
setSigningKey mtrDoorLockClusterAppleSetAliroReaderConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleSetAliroReaderConfigParams (mkSelector "setSigningKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- verificationKey@
verificationKey :: IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> IO (Id NSData)
verificationKey mtrDoorLockClusterAppleSetAliroReaderConfigParams  =
    sendMsg mtrDoorLockClusterAppleSetAliroReaderConfigParams (mkSelector "verificationKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVerificationKey:@
setVerificationKey :: (IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> value -> IO ()
setVerificationKey mtrDoorLockClusterAppleSetAliroReaderConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleSetAliroReaderConfigParams (mkSelector "setVerificationKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupIdentifier@
groupIdentifier :: IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> IO (Id NSData)
groupIdentifier mtrDoorLockClusterAppleSetAliroReaderConfigParams  =
    sendMsg mtrDoorLockClusterAppleSetAliroReaderConfigParams (mkSelector "groupIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupIdentifier:@
setGroupIdentifier :: (IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> value -> IO ()
setGroupIdentifier mtrDoorLockClusterAppleSetAliroReaderConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleSetAliroReaderConfigParams (mkSelector "setGroupIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupResolvingKey@
groupResolvingKey :: IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> IO (Id NSData)
groupResolvingKey mtrDoorLockClusterAppleSetAliroReaderConfigParams  =
    sendMsg mtrDoorLockClusterAppleSetAliroReaderConfigParams (mkSelector "groupResolvingKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupResolvingKey:@
setGroupResolvingKey :: (IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> value -> IO ()
setGroupResolvingKey mtrDoorLockClusterAppleSetAliroReaderConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleSetAliroReaderConfigParams (mkSelector "setGroupResolvingKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterAppleSetAliroReaderConfigParams  =
    sendMsg mtrDoorLockClusterAppleSetAliroReaderConfigParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams, IsNSNumber value) => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterAppleSetAliroReaderConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleSetAliroReaderConfigParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterAppleSetAliroReaderConfigParams  =
    sendMsg mtrDoorLockClusterAppleSetAliroReaderConfigParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams, IsNSNumber value) => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterAppleSetAliroReaderConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleSetAliroReaderConfigParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @signingKey@
signingKeySelector :: Selector
signingKeySelector = mkSelector "signingKey"

-- | @Selector@ for @setSigningKey:@
setSigningKeySelector :: Selector
setSigningKeySelector = mkSelector "setSigningKey:"

-- | @Selector@ for @verificationKey@
verificationKeySelector :: Selector
verificationKeySelector = mkSelector "verificationKey"

-- | @Selector@ for @setVerificationKey:@
setVerificationKeySelector :: Selector
setVerificationKeySelector = mkSelector "setVerificationKey:"

-- | @Selector@ for @groupIdentifier@
groupIdentifierSelector :: Selector
groupIdentifierSelector = mkSelector "groupIdentifier"

-- | @Selector@ for @setGroupIdentifier:@
setGroupIdentifierSelector :: Selector
setGroupIdentifierSelector = mkSelector "setGroupIdentifier:"

-- | @Selector@ for @groupResolvingKey@
groupResolvingKeySelector :: Selector
groupResolvingKeySelector = mkSelector "groupResolvingKey"

-- | @Selector@ for @setGroupResolvingKey:@
setGroupResolvingKeySelector :: Selector
setGroupResolvingKeySelector = mkSelector "setGroupResolvingKey:"

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

