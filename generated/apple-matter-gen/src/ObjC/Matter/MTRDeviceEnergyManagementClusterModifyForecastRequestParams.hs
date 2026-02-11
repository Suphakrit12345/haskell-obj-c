{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterModifyForecastRequestParams@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterModifyForecastRequestParams
  ( MTRDeviceEnergyManagementClusterModifyForecastRequestParams
  , IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams(..)
  , forecastID
  , setForecastID
  , slotAdjustments
  , setSlotAdjustments
  , cause
  , setCause
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , forecastIDSelector
  , setForecastIDSelector
  , slotAdjustmentsSelector
  , setSlotAdjustmentsSelector
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

-- | @- forecastID@
forecastID :: IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> IO (Id NSNumber)
forecastID mtrDeviceEnergyManagementClusterModifyForecastRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterModifyForecastRequestParams (mkSelector "forecastID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setForecastID:@
setForecastID :: (IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> value -> IO ()
setForecastID mtrDeviceEnergyManagementClusterModifyForecastRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterModifyForecastRequestParams (mkSelector "setForecastID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- slotAdjustments@
slotAdjustments :: IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> IO (Id NSArray)
slotAdjustments mtrDeviceEnergyManagementClusterModifyForecastRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterModifyForecastRequestParams (mkSelector "slotAdjustments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSlotAdjustments:@
setSlotAdjustments :: (IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams, IsNSArray value) => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> value -> IO ()
setSlotAdjustments mtrDeviceEnergyManagementClusterModifyForecastRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterModifyForecastRequestParams (mkSelector "setSlotAdjustments:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cause@
cause :: IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> IO (Id NSNumber)
cause mtrDeviceEnergyManagementClusterModifyForecastRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterModifyForecastRequestParams (mkSelector "cause") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCause:@
setCause :: (IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> value -> IO ()
setCause mtrDeviceEnergyManagementClusterModifyForecastRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterModifyForecastRequestParams (mkSelector "setCause:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDeviceEnergyManagementClusterModifyForecastRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterModifyForecastRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDeviceEnergyManagementClusterModifyForecastRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterModifyForecastRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDeviceEnergyManagementClusterModifyForecastRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterModifyForecastRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrDeviceEnergyManagementClusterModifyForecastRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterModifyForecastRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @forecastID@
forecastIDSelector :: Selector
forecastIDSelector = mkSelector "forecastID"

-- | @Selector@ for @setForecastID:@
setForecastIDSelector :: Selector
setForecastIDSelector = mkSelector "setForecastID:"

-- | @Selector@ for @slotAdjustments@
slotAdjustmentsSelector :: Selector
slotAdjustmentsSelector = mkSelector "slotAdjustments"

-- | @Selector@ for @setSlotAdjustments:@
setSlotAdjustmentsSelector :: Selector
setSlotAdjustmentsSelector = mkSelector "setSlotAdjustments:"

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

