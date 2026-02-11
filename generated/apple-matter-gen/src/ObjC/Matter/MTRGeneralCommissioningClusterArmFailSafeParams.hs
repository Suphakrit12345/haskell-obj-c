{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralCommissioningClusterArmFailSafeParams@.
module ObjC.Matter.MTRGeneralCommissioningClusterArmFailSafeParams
  ( MTRGeneralCommissioningClusterArmFailSafeParams
  , IsMTRGeneralCommissioningClusterArmFailSafeParams(..)
  , expiryLengthSeconds
  , setExpiryLengthSeconds
  , breadcrumb
  , setBreadcrumb
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , expiryLengthSecondsSelector
  , setExpiryLengthSecondsSelector
  , breadcrumbSelector
  , setBreadcrumbSelector
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

-- | @- expiryLengthSeconds@
expiryLengthSeconds :: IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams => mtrGeneralCommissioningClusterArmFailSafeParams -> IO (Id NSNumber)
expiryLengthSeconds mtrGeneralCommissioningClusterArmFailSafeParams  =
    sendMsg mtrGeneralCommissioningClusterArmFailSafeParams (mkSelector "expiryLengthSeconds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExpiryLengthSeconds:@
setExpiryLengthSeconds :: (IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams, IsNSNumber value) => mtrGeneralCommissioningClusterArmFailSafeParams -> value -> IO ()
setExpiryLengthSeconds mtrGeneralCommissioningClusterArmFailSafeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralCommissioningClusterArmFailSafeParams (mkSelector "setExpiryLengthSeconds:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- breadcrumb@
breadcrumb :: IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams => mtrGeneralCommissioningClusterArmFailSafeParams -> IO (Id NSNumber)
breadcrumb mtrGeneralCommissioningClusterArmFailSafeParams  =
    sendMsg mtrGeneralCommissioningClusterArmFailSafeParams (mkSelector "breadcrumb") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams, IsNSNumber value) => mtrGeneralCommissioningClusterArmFailSafeParams -> value -> IO ()
setBreadcrumb mtrGeneralCommissioningClusterArmFailSafeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralCommissioningClusterArmFailSafeParams (mkSelector "setBreadcrumb:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams => mtrGeneralCommissioningClusterArmFailSafeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGeneralCommissioningClusterArmFailSafeParams  =
    sendMsg mtrGeneralCommissioningClusterArmFailSafeParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams, IsNSNumber value) => mtrGeneralCommissioningClusterArmFailSafeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGeneralCommissioningClusterArmFailSafeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralCommissioningClusterArmFailSafeParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams => mtrGeneralCommissioningClusterArmFailSafeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGeneralCommissioningClusterArmFailSafeParams  =
    sendMsg mtrGeneralCommissioningClusterArmFailSafeParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams, IsNSNumber value) => mtrGeneralCommissioningClusterArmFailSafeParams -> value -> IO ()
setServerSideProcessingTimeout mtrGeneralCommissioningClusterArmFailSafeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralCommissioningClusterArmFailSafeParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @expiryLengthSeconds@
expiryLengthSecondsSelector :: Selector
expiryLengthSecondsSelector = mkSelector "expiryLengthSeconds"

-- | @Selector@ for @setExpiryLengthSeconds:@
setExpiryLengthSecondsSelector :: Selector
setExpiryLengthSecondsSelector = mkSelector "setExpiryLengthSeconds:"

-- | @Selector@ for @breadcrumb@
breadcrumbSelector :: Selector
breadcrumbSelector = mkSelector "breadcrumb"

-- | @Selector@ for @setBreadcrumb:@
setBreadcrumbSelector :: Selector
setBreadcrumbSelector = mkSelector "setBreadcrumb:"

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

