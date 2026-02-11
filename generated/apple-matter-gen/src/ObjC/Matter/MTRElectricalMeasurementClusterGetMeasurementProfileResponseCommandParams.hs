{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams@.
module ObjC.Matter.MTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams
  ( MTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams
  , IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams(..)
  , initWithResponseValue_error
  , startTime
  , setStartTime
  , status
  , setStatus
  , profileIntervalPeriod
  , setProfileIntervalPeriod
  , numberOfIntervalsDelivered
  , setNumberOfIntervalsDelivered
  , attributeId
  , setAttributeId
  , intervals
  , setIntervals
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , startTimeSelector
  , setStartTimeSelector
  , statusSelector
  , setStatusSelector
  , profileIntervalPeriodSelector
  , setProfileIntervalPeriodSelector
  , numberOfIntervalsDeliveredSelector
  , setNumberOfIntervalsDeliveredSelector
  , attributeIdSelector
  , setAttributeIdSelector
  , intervalsSelector
  , setIntervalsSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector


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

-- | Initialize an MTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSDictionary responseValue, IsNSError error_) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> responseValue -> error_ -> IO (Id MTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams)
initWithResponseValue_error mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- startTime@
startTime :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSNumber)
startTime mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "startTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartTime:@
setStartTime :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setStartTime mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "setStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- status@
status :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSNumber)
status mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setStatus mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- profileIntervalPeriod@
profileIntervalPeriod :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSNumber)
profileIntervalPeriod mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "profileIntervalPeriod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProfileIntervalPeriod:@
setProfileIntervalPeriod :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setProfileIntervalPeriod mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "setProfileIntervalPeriod:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- numberOfIntervalsDelivered@
numberOfIntervalsDelivered :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSNumber)
numberOfIntervalsDelivered mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "numberOfIntervalsDelivered") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNumberOfIntervalsDelivered:@
setNumberOfIntervalsDelivered :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setNumberOfIntervalsDelivered mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "setNumberOfIntervalsDelivered:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributeId@
attributeId :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSNumber)
attributeId mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "attributeId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributeId:@
setAttributeId :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setAttributeId mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "setAttributeId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- intervals@
intervals :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSArray)
intervals mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "intervals") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIntervals:@
setIntervals :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSArray value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setIntervals mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "setIntervals:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @profileIntervalPeriod@
profileIntervalPeriodSelector :: Selector
profileIntervalPeriodSelector = mkSelector "profileIntervalPeriod"

-- | @Selector@ for @setProfileIntervalPeriod:@
setProfileIntervalPeriodSelector :: Selector
setProfileIntervalPeriodSelector = mkSelector "setProfileIntervalPeriod:"

-- | @Selector@ for @numberOfIntervalsDelivered@
numberOfIntervalsDeliveredSelector :: Selector
numberOfIntervalsDeliveredSelector = mkSelector "numberOfIntervalsDelivered"

-- | @Selector@ for @setNumberOfIntervalsDelivered:@
setNumberOfIntervalsDeliveredSelector :: Selector
setNumberOfIntervalsDeliveredSelector = mkSelector "setNumberOfIntervalsDelivered:"

-- | @Selector@ for @attributeId@
attributeIdSelector :: Selector
attributeIdSelector = mkSelector "attributeId"

-- | @Selector@ for @setAttributeId:@
setAttributeIdSelector :: Selector
setAttributeIdSelector = mkSelector "setAttributeId:"

-- | @Selector@ for @intervals@
intervalsSelector :: Selector
intervalsSelector = mkSelector "intervals"

-- | @Selector@ for @setIntervals:@
setIntervalsSelector :: Selector
setIntervalsSelector = mkSelector "setIntervals:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

