{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAdministratorCommissioningClusterOpenCommissioningWindowParams@.
module ObjC.Matter.MTRAdministratorCommissioningClusterOpenCommissioningWindowParams
  ( MTRAdministratorCommissioningClusterOpenCommissioningWindowParams
  , IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams(..)
  , commissioningTimeout
  , setCommissioningTimeout
  , pakePasscodeVerifier
  , setPakePasscodeVerifier
  , discriminator
  , setDiscriminator
  , iterations
  , setIterations
  , salt
  , setSalt
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , pakeVerifier
  , setPakeVerifier
  , commissioningTimeoutSelector
  , setCommissioningTimeoutSelector
  , pakePasscodeVerifierSelector
  , setPakePasscodeVerifierSelector
  , discriminatorSelector
  , setDiscriminatorSelector
  , iterationsSelector
  , setIterationsSelector
  , saltSelector
  , setSaltSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , pakeVerifierSelector
  , setPakeVerifierSelector


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

-- | @- commissioningTimeout@
commissioningTimeout :: IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> IO (Id NSNumber)
commissioningTimeout mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  =
    sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "commissioningTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCommissioningTimeout:@
setCommissioningTimeout :: (IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams, IsNSNumber value) => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> value -> IO ()
setCommissioningTimeout mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "setCommissioningTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pakePasscodeVerifier@
pakePasscodeVerifier :: IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> IO (Id NSData)
pakePasscodeVerifier mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  =
    sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "pakePasscodeVerifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPakePasscodeVerifier:@
setPakePasscodeVerifier :: (IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams, IsNSData value) => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> value -> IO ()
setPakePasscodeVerifier mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "setPakePasscodeVerifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- discriminator@
discriminator :: IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> IO (Id NSNumber)
discriminator mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  =
    sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "discriminator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDiscriminator:@
setDiscriminator :: (IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams, IsNSNumber value) => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> value -> IO ()
setDiscriminator mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "setDiscriminator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- iterations@
iterations :: IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> IO (Id NSNumber)
iterations mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  =
    sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "iterations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIterations:@
setIterations :: (IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams, IsNSNumber value) => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> value -> IO ()
setIterations mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "setIterations:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- salt@
salt :: IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> IO (Id NSData)
salt mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  =
    sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "salt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSalt:@
setSalt :: (IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams, IsNSData value) => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> value -> IO ()
setSalt mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "setSalt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  =
    sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams, IsNSNumber value) => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  =
    sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams, IsNSNumber value) => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> value -> IO ()
setServerSideProcessingTimeout mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pakeVerifier@
pakeVerifier :: IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> IO (Id NSData)
pakeVerifier mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  =
    sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "pakeVerifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPakeVerifier:@
setPakeVerifier :: (IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams mtrAdministratorCommissioningClusterOpenCommissioningWindowParams, IsNSData value) => mtrAdministratorCommissioningClusterOpenCommissioningWindowParams -> value -> IO ()
setPakeVerifier mtrAdministratorCommissioningClusterOpenCommissioningWindowParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAdministratorCommissioningClusterOpenCommissioningWindowParams (mkSelector "setPakeVerifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @commissioningTimeout@
commissioningTimeoutSelector :: Selector
commissioningTimeoutSelector = mkSelector "commissioningTimeout"

-- | @Selector@ for @setCommissioningTimeout:@
setCommissioningTimeoutSelector :: Selector
setCommissioningTimeoutSelector = mkSelector "setCommissioningTimeout:"

-- | @Selector@ for @pakePasscodeVerifier@
pakePasscodeVerifierSelector :: Selector
pakePasscodeVerifierSelector = mkSelector "pakePasscodeVerifier"

-- | @Selector@ for @setPakePasscodeVerifier:@
setPakePasscodeVerifierSelector :: Selector
setPakePasscodeVerifierSelector = mkSelector "setPakePasscodeVerifier:"

-- | @Selector@ for @discriminator@
discriminatorSelector :: Selector
discriminatorSelector = mkSelector "discriminator"

-- | @Selector@ for @setDiscriminator:@
setDiscriminatorSelector :: Selector
setDiscriminatorSelector = mkSelector "setDiscriminator:"

-- | @Selector@ for @iterations@
iterationsSelector :: Selector
iterationsSelector = mkSelector "iterations"

-- | @Selector@ for @setIterations:@
setIterationsSelector :: Selector
setIterationsSelector = mkSelector "setIterations:"

-- | @Selector@ for @salt@
saltSelector :: Selector
saltSelector = mkSelector "salt"

-- | @Selector@ for @setSalt:@
setSaltSelector :: Selector
setSaltSelector = mkSelector "setSalt:"

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

-- | @Selector@ for @pakeVerifier@
pakeVerifierSelector :: Selector
pakeVerifierSelector = mkSelector "pakeVerifier"

-- | @Selector@ for @setPakeVerifier:@
setPakeVerifierSelector :: Selector
setPakeVerifierSelector = mkSelector "setPakeVerifier:"

