{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterPowerAdjustRequestParams@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterPowerAdjustRequestParams
  ( MTRDeviceEnergyManagementClusterPowerAdjustRequestParams
  , IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams(..)
  , power
  , setPower
  , duration
  , setDuration
  , cause
  , setCause
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , powerSelector
  , setPowerSelector
  , durationSelector
  , setDurationSelector
  , causeSelector
  , setCauseSelector
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

-- | @- power@
power :: IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> IO (Id NSNumber)
power mtrDeviceEnergyManagementClusterPowerAdjustRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustRequestParams (mkSelector "power") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPower:@
setPower :: (IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> value -> IO ()
setPower mtrDeviceEnergyManagementClusterPowerAdjustRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustRequestParams (mkSelector "setPower:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- duration@
duration :: IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> IO (Id NSNumber)
duration mtrDeviceEnergyManagementClusterPowerAdjustRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustRequestParams (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDuration:@
setDuration :: (IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> value -> IO ()
setDuration mtrDeviceEnergyManagementClusterPowerAdjustRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustRequestParams (mkSelector "setDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cause@
cause :: IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> IO (Id NSNumber)
cause mtrDeviceEnergyManagementClusterPowerAdjustRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustRequestParams (mkSelector "cause") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCause:@
setCause :: (IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> value -> IO ()
setCause mtrDeviceEnergyManagementClusterPowerAdjustRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustRequestParams (mkSelector "setCause:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDeviceEnergyManagementClusterPowerAdjustRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDeviceEnergyManagementClusterPowerAdjustRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDeviceEnergyManagementClusterPowerAdjustRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrDeviceEnergyManagementClusterPowerAdjustRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @power@
powerSelector :: Selector
powerSelector = mkSelector "power"

-- | @Selector@ for @setPower:@
setPowerSelector :: Selector
setPowerSelector = mkSelector "setPower:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @cause@
causeSelector :: Selector
causeSelector = mkSelector "cause"

-- | @Selector@ for @setCause:@
setCauseSelector :: Selector
setCauseSelector = mkSelector "setCause:"

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

