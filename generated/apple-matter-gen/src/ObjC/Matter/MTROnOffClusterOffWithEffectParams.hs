{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROnOffClusterOffWithEffectParams@.
module ObjC.Matter.MTROnOffClusterOffWithEffectParams
  ( MTROnOffClusterOffWithEffectParams
  , IsMTROnOffClusterOffWithEffectParams(..)
  , effectIdentifier
  , setEffectIdentifier
  , effectVariant
  , setEffectVariant
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , effectId
  , setEffectId
  , effectIdentifierSelector
  , setEffectIdentifierSelector
  , effectVariantSelector
  , setEffectVariantSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , effectIdSelector
  , setEffectIdSelector


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
effectIdentifier :: IsMTROnOffClusterOffWithEffectParams mtrOnOffClusterOffWithEffectParams => mtrOnOffClusterOffWithEffectParams -> IO (Id NSNumber)
effectIdentifier mtrOnOffClusterOffWithEffectParams  =
    sendMsg mtrOnOffClusterOffWithEffectParams (mkSelector "effectIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEffectIdentifier:@
setEffectIdentifier :: (IsMTROnOffClusterOffWithEffectParams mtrOnOffClusterOffWithEffectParams, IsNSNumber value) => mtrOnOffClusterOffWithEffectParams -> value -> IO ()
setEffectIdentifier mtrOnOffClusterOffWithEffectParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOnOffClusterOffWithEffectParams (mkSelector "setEffectIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- effectVariant@
effectVariant :: IsMTROnOffClusterOffWithEffectParams mtrOnOffClusterOffWithEffectParams => mtrOnOffClusterOffWithEffectParams -> IO (Id NSNumber)
effectVariant mtrOnOffClusterOffWithEffectParams  =
    sendMsg mtrOnOffClusterOffWithEffectParams (mkSelector "effectVariant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEffectVariant:@
setEffectVariant :: (IsMTROnOffClusterOffWithEffectParams mtrOnOffClusterOffWithEffectParams, IsNSNumber value) => mtrOnOffClusterOffWithEffectParams -> value -> IO ()
setEffectVariant mtrOnOffClusterOffWithEffectParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOnOffClusterOffWithEffectParams (mkSelector "setEffectVariant:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROnOffClusterOffWithEffectParams mtrOnOffClusterOffWithEffectParams => mtrOnOffClusterOffWithEffectParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOnOffClusterOffWithEffectParams  =
    sendMsg mtrOnOffClusterOffWithEffectParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROnOffClusterOffWithEffectParams mtrOnOffClusterOffWithEffectParams, IsNSNumber value) => mtrOnOffClusterOffWithEffectParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOnOffClusterOffWithEffectParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOnOffClusterOffWithEffectParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROnOffClusterOffWithEffectParams mtrOnOffClusterOffWithEffectParams => mtrOnOffClusterOffWithEffectParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOnOffClusterOffWithEffectParams  =
    sendMsg mtrOnOffClusterOffWithEffectParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROnOffClusterOffWithEffectParams mtrOnOffClusterOffWithEffectParams, IsNSNumber value) => mtrOnOffClusterOffWithEffectParams -> value -> IO ()
setServerSideProcessingTimeout mtrOnOffClusterOffWithEffectParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOnOffClusterOffWithEffectParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- effectId@
effectId :: IsMTROnOffClusterOffWithEffectParams mtrOnOffClusterOffWithEffectParams => mtrOnOffClusterOffWithEffectParams -> IO (Id NSNumber)
effectId mtrOnOffClusterOffWithEffectParams  =
    sendMsg mtrOnOffClusterOffWithEffectParams (mkSelector "effectId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEffectId:@
setEffectId :: (IsMTROnOffClusterOffWithEffectParams mtrOnOffClusterOffWithEffectParams, IsNSNumber value) => mtrOnOffClusterOffWithEffectParams -> value -> IO ()
setEffectId mtrOnOffClusterOffWithEffectParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOnOffClusterOffWithEffectParams (mkSelector "setEffectId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @effectId@
effectIdSelector :: Selector
effectIdSelector = mkSelector "effectId"

-- | @Selector@ for @setEffectId:@
setEffectIdSelector :: Selector
setEffectIdSelector = mkSelector "setEffectId:"

