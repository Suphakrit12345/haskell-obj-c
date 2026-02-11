{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRIdentifyClusterTriggerEffectParams@.
module ObjC.Matter.MTRIdentifyClusterTriggerEffectParams
  ( MTRIdentifyClusterTriggerEffectParams
  , IsMTRIdentifyClusterTriggerEffectParams(..)
  , effectIdentifier
  , setEffectIdentifier
  , effectVariant
  , setEffectVariant
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , effectIdentifierSelector
  , setEffectIdentifierSelector
  , effectVariantSelector
  , setEffectVariantSelector
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

-- | @- effectIdentifier@
effectIdentifier :: IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams => mtrIdentifyClusterTriggerEffectParams -> IO (Id NSNumber)
effectIdentifier mtrIdentifyClusterTriggerEffectParams  =
    sendMsg mtrIdentifyClusterTriggerEffectParams (mkSelector "effectIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEffectIdentifier:@
setEffectIdentifier :: (IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams, IsNSNumber value) => mtrIdentifyClusterTriggerEffectParams -> value -> IO ()
setEffectIdentifier mtrIdentifyClusterTriggerEffectParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrIdentifyClusterTriggerEffectParams (mkSelector "setEffectIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- effectVariant@
effectVariant :: IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams => mtrIdentifyClusterTriggerEffectParams -> IO (Id NSNumber)
effectVariant mtrIdentifyClusterTriggerEffectParams  =
    sendMsg mtrIdentifyClusterTriggerEffectParams (mkSelector "effectVariant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEffectVariant:@
setEffectVariant :: (IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams, IsNSNumber value) => mtrIdentifyClusterTriggerEffectParams -> value -> IO ()
setEffectVariant mtrIdentifyClusterTriggerEffectParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrIdentifyClusterTriggerEffectParams (mkSelector "setEffectVariant:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams => mtrIdentifyClusterTriggerEffectParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrIdentifyClusterTriggerEffectParams  =
    sendMsg mtrIdentifyClusterTriggerEffectParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams, IsNSNumber value) => mtrIdentifyClusterTriggerEffectParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrIdentifyClusterTriggerEffectParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrIdentifyClusterTriggerEffectParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams => mtrIdentifyClusterTriggerEffectParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrIdentifyClusterTriggerEffectParams  =
    sendMsg mtrIdentifyClusterTriggerEffectParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams, IsNSNumber value) => mtrIdentifyClusterTriggerEffectParams -> value -> IO ()
setServerSideProcessingTimeout mtrIdentifyClusterTriggerEffectParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrIdentifyClusterTriggerEffectParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effectIdentifier@
effectIdentifierSelector :: Selector
effectIdentifierSelector = mkSelector "effectIdentifier"

-- | @Selector@ for @setEffectIdentifier:@
setEffectIdentifierSelector :: Selector
setEffectIdentifierSelector = mkSelector "setEffectIdentifier:"

-- | @Selector@ for @effectVariant@
effectVariantSelector :: Selector
effectVariantSelector = mkSelector "effectVariant"

-- | @Selector@ for @setEffectVariant:@
setEffectVariantSelector :: Selector
setEffectVariantSelector = mkSelector "setEffectVariant:"

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

