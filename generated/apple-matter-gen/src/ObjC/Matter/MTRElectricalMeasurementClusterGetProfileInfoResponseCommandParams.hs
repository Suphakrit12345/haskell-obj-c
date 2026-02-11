{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams@.
module ObjC.Matter.MTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams
  ( MTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams
  , IsMTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams(..)
  , initWithResponseValue_error
  , profileCount
  , setProfileCount
  , profileIntervalPeriod
  , setProfileIntervalPeriod
  , maxNumberOfIntervals
  , setMaxNumberOfIntervals
  , listOfAttributes
  , setListOfAttributes
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , profileCountSelector
  , setProfileCountSelector
  , profileIntervalPeriodSelector
  , setProfileIntervalPeriodSelector
  , maxNumberOfIntervalsSelector
  , setMaxNumberOfIntervalsSelector
  , listOfAttributesSelector
  , setListOfAttributesSelector
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

-- | Initialize an MTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams, IsNSDictionary responseValue, IsNSError error_) => mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams -> responseValue -> error_ -> IO (Id MTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams)
initWithResponseValue_error mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- profileCount@
profileCount :: IsMTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams => mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams -> IO (Id NSNumber)
profileCount mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams (mkSelector "profileCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProfileCount:@
setProfileCount :: (IsMTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams -> value -> IO ()
setProfileCount mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams (mkSelector "setProfileCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- profileIntervalPeriod@
profileIntervalPeriod :: IsMTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams => mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams -> IO (Id NSNumber)
profileIntervalPeriod mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams (mkSelector "profileIntervalPeriod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProfileIntervalPeriod:@
setProfileIntervalPeriod :: (IsMTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams -> value -> IO ()
setProfileIntervalPeriod mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams (mkSelector "setProfileIntervalPeriod:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxNumberOfIntervals@
maxNumberOfIntervals :: IsMTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams => mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams -> IO (Id NSNumber)
maxNumberOfIntervals mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams (mkSelector "maxNumberOfIntervals") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxNumberOfIntervals:@
setMaxNumberOfIntervals :: (IsMTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams -> value -> IO ()
setMaxNumberOfIntervals mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams (mkSelector "setMaxNumberOfIntervals:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- listOfAttributes@
listOfAttributes :: IsMTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams => mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams -> IO (Id NSArray)
listOfAttributes mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams (mkSelector "listOfAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setListOfAttributes:@
setListOfAttributes :: (IsMTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams, IsNSArray value) => mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams -> value -> IO ()
setListOfAttributes mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams (mkSelector "setListOfAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams => mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRElectricalMeasurementClusterGetProfileInfoResponseCommandParams mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetProfileInfoResponseCommandParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @profileCount@
profileCountSelector :: Selector
profileCountSelector = mkSelector "profileCount"

-- | @Selector@ for @setProfileCount:@
setProfileCountSelector :: Selector
setProfileCountSelector = mkSelector "setProfileCount:"

-- | @Selector@ for @profileIntervalPeriod@
profileIntervalPeriodSelector :: Selector
profileIntervalPeriodSelector = mkSelector "profileIntervalPeriod"

-- | @Selector@ for @setProfileIntervalPeriod:@
setProfileIntervalPeriodSelector :: Selector
setProfileIntervalPeriodSelector = mkSelector "setProfileIntervalPeriod:"

-- | @Selector@ for @maxNumberOfIntervals@
maxNumberOfIntervalsSelector :: Selector
maxNumberOfIntervalsSelector = mkSelector "maxNumberOfIntervals"

-- | @Selector@ for @setMaxNumberOfIntervals:@
setMaxNumberOfIntervalsSelector :: Selector
setMaxNumberOfIntervalsSelector = mkSelector "setMaxNumberOfIntervals:"

-- | @Selector@ for @listOfAttributes@
listOfAttributesSelector :: Selector
listOfAttributesSelector = mkSelector "listOfAttributes"

-- | @Selector@ for @setListOfAttributes:@
setListOfAttributesSelector :: Selector
setListOfAttributesSelector = mkSelector "setListOfAttributes:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

