{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterPauseRequestParams@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterPauseRequestParams
  ( MTRDeviceEnergyManagementClusterPauseRequestParams
  , IsMTRDeviceEnergyManagementClusterPauseRequestParams(..)
  , duration
  , setDuration
  , cause
  , setCause
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
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

-- | @- duration@
duration :: IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams => mtrDeviceEnergyManagementClusterPauseRequestParams -> IO (Id NSNumber)
duration mtrDeviceEnergyManagementClusterPauseRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterPauseRequestParams (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDuration:@
setDuration :: (IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPauseRequestParams -> value -> IO ()
setDuration mtrDeviceEnergyManagementClusterPauseRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPauseRequestParams (mkSelector "setDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cause@
cause :: IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams => mtrDeviceEnergyManagementClusterPauseRequestParams -> IO (Id NSNumber)
cause mtrDeviceEnergyManagementClusterPauseRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterPauseRequestParams (mkSelector "cause") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCause:@
setCause :: (IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPauseRequestParams -> value -> IO ()
setCause mtrDeviceEnergyManagementClusterPauseRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPauseRequestParams (mkSelector "setCause:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams => mtrDeviceEnergyManagementClusterPauseRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDeviceEnergyManagementClusterPauseRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterPauseRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPauseRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDeviceEnergyManagementClusterPauseRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPauseRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams => mtrDeviceEnergyManagementClusterPauseRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDeviceEnergyManagementClusterPauseRequestParams  =
    sendMsg mtrDeviceEnergyManagementClusterPauseRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPauseRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrDeviceEnergyManagementClusterPauseRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPauseRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

