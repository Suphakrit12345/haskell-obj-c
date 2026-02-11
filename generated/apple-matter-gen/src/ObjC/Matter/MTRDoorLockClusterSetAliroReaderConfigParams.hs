{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterSetAliroReaderConfigParams@.
module ObjC.Matter.MTRDoorLockClusterSetAliroReaderConfigParams
  ( MTRDoorLockClusterSetAliroReaderConfigParams
  , IsMTRDoorLockClusterSetAliroReaderConfigParams(..)
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
signingKey :: IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams => mtrDoorLockClusterSetAliroReaderConfigParams -> IO (Id NSData)
signingKey mtrDoorLockClusterSetAliroReaderConfigParams  =
    sendMsg mtrDoorLockClusterSetAliroReaderConfigParams (mkSelector "signingKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSigningKey:@
setSigningKey :: (IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterSetAliroReaderConfigParams -> value -> IO ()
setSigningKey mtrDoorLockClusterSetAliroReaderConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetAliroReaderConfigParams (mkSelector "setSigningKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- verificationKey@
verificationKey :: IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams => mtrDoorLockClusterSetAliroReaderConfigParams -> IO (Id NSData)
verificationKey mtrDoorLockClusterSetAliroReaderConfigParams  =
    sendMsg mtrDoorLockClusterSetAliroReaderConfigParams (mkSelector "verificationKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVerificationKey:@
setVerificationKey :: (IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterSetAliroReaderConfigParams -> value -> IO ()
setVerificationKey mtrDoorLockClusterSetAliroReaderConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetAliroReaderConfigParams (mkSelector "setVerificationKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupIdentifier@
groupIdentifier :: IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams => mtrDoorLockClusterSetAliroReaderConfigParams -> IO (Id NSData)
groupIdentifier mtrDoorLockClusterSetAliroReaderConfigParams  =
    sendMsg mtrDoorLockClusterSetAliroReaderConfigParams (mkSelector "groupIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupIdentifier:@
setGroupIdentifier :: (IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterSetAliroReaderConfigParams -> value -> IO ()
setGroupIdentifier mtrDoorLockClusterSetAliroReaderConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetAliroReaderConfigParams (mkSelector "setGroupIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupResolvingKey@
groupResolvingKey :: IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams => mtrDoorLockClusterSetAliroReaderConfigParams -> IO (Id NSData)
groupResolvingKey mtrDoorLockClusterSetAliroReaderConfigParams  =
    sendMsg mtrDoorLockClusterSetAliroReaderConfigParams (mkSelector "groupResolvingKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupResolvingKey:@
setGroupResolvingKey :: (IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterSetAliroReaderConfigParams -> value -> IO ()
setGroupResolvingKey mtrDoorLockClusterSetAliroReaderConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetAliroReaderConfigParams (mkSelector "setGroupResolvingKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams => mtrDoorLockClusterSetAliroReaderConfigParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterSetAliroReaderConfigParams  =
    sendMsg mtrDoorLockClusterSetAliroReaderConfigParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams, IsNSNumber value) => mtrDoorLockClusterSetAliroReaderConfigParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterSetAliroReaderConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetAliroReaderConfigParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams => mtrDoorLockClusterSetAliroReaderConfigParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterSetAliroReaderConfigParams  =
    sendMsg mtrDoorLockClusterSetAliroReaderConfigParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams, IsNSNumber value) => mtrDoorLockClusterSetAliroReaderConfigParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterSetAliroReaderConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetAliroReaderConfigParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

