{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalMeasurementClusterGetMeasurementProfileCommandParams@.
module ObjC.Matter.MTRElectricalMeasurementClusterGetMeasurementProfileCommandParams
  ( MTRElectricalMeasurementClusterGetMeasurementProfileCommandParams
  , IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams(..)
  , attributeId
  , setAttributeId
  , startTime
  , setStartTime
  , numberOfIntervals
  , setNumberOfIntervals
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , attributeIdSelector
  , setAttributeIdSelector
  , startTimeSelector
  , setStartTimeSelector
  , numberOfIntervalsSelector
  , setNumberOfIntervalsSelector
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

-- | @- attributeId@
attributeId :: IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> IO (Id NSNumber)
attributeId mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams (mkSelector "attributeId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributeId:@
setAttributeId :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> value -> IO ()
setAttributeId mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams (mkSelector "setAttributeId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startTime@
startTime :: IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> IO (Id NSNumber)
startTime mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams (mkSelector "startTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartTime:@
setStartTime :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> value -> IO ()
setStartTime mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams (mkSelector "setStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- numberOfIntervals@
numberOfIntervals :: IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> IO (Id NSNumber)
numberOfIntervals mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams (mkSelector "numberOfIntervals") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNumberOfIntervals:@
setNumberOfIntervals :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> value -> IO ()
setNumberOfIntervals mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams (mkSelector "setNumberOfIntervals:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams  =
    sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> value -> IO ()
setServerSideProcessingTimeout mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributeId@
attributeIdSelector :: Selector
attributeIdSelector = mkSelector "attributeId"

-- | @Selector@ for @setAttributeId:@
setAttributeIdSelector :: Selector
setAttributeIdSelector = mkSelector "setAttributeId:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @numberOfIntervals@
numberOfIntervalsSelector :: Selector
numberOfIntervalsSelector = mkSelector "numberOfIntervals"

-- | @Selector@ for @setNumberOfIntervals:@
setNumberOfIntervalsSelector :: Selector
setNumberOfIntervalsSelector = mkSelector "setNumberOfIntervals:"

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

