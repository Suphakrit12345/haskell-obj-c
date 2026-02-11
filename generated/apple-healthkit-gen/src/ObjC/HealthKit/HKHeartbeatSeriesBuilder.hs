{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKHeartbeatSeriesBuilder
--
-- An HKHeartbeatSeriesBuilder is used to generate an HKHeartbeatSeriesSample.
--
-- This class is intended for generating an HKHeartbeatSeriesSample which represents a series of                     heartbeats. If the discard method is called, collected data will be deleted.                     Calling finishSeriesWithcompletion: will stop and complete the series. If the builder is deleted,                     or the client goes away before calling the finish method, data will be lost.
--
-- Generated bindings for @HKHeartbeatSeriesBuilder@.
module ObjC.HealthKit.HKHeartbeatSeriesBuilder
  ( HKHeartbeatSeriesBuilder
  , IsHKHeartbeatSeriesBuilder(..)
  , initWithHealthStore_device_startDate
  , addHeartbeatWithTimeIntervalSinceSeriesStartDate_precededByGap_completion
  , addMetadata_completion
  , finishSeriesWithCompletion
  , maximumCount
  , initWithHealthStore_device_startDateSelector
  , addHeartbeatWithTimeIntervalSinceSeriesStartDate_precededByGap_completionSelector
  , addMetadata_completionSelector
  , finishSeriesWithCompletionSelector
  , maximumCountSelector


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

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithHealthStore:device:startDate:
--
-- The designated initializer to create an HKHeartbeatSeriesBuilder.
--
-- The HKHealthStore is retained during the life of the object for the saving of the series data and final                     return of the series sample.
--
-- @healthStore@ — Specifies the HKHealthStore object to use for building the series.
--
-- @device@ — The optional device represents the HKDevice from which the data is provided.
--
-- @startDate@ — The start date of the HKHeartbeatSeriesSample that will be generated.
--
-- ObjC selector: @- initWithHealthStore:device:startDate:@
initWithHealthStore_device_startDate :: (IsHKHeartbeatSeriesBuilder hkHeartbeatSeriesBuilder, IsHKHealthStore healthStore, IsHKDevice device, IsNSDate startDate) => hkHeartbeatSeriesBuilder -> healthStore -> device -> startDate -> IO (Id HKHeartbeatSeriesBuilder)
initWithHealthStore_device_startDate hkHeartbeatSeriesBuilder  healthStore device startDate =
  withObjCPtr healthStore $ \raw_healthStore ->
    withObjCPtr device $ \raw_device ->
      withObjCPtr startDate $ \raw_startDate ->
          sendMsg hkHeartbeatSeriesBuilder (mkSelector "initWithHealthStore:device:startDate:") (retPtr retVoid) [argPtr (castPtr raw_healthStore :: Ptr ()), argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_startDate :: Ptr ())] >>= ownedObject . castPtr

-- | addHeartbeatWithTimeIntervalSinceSeriesStartDate:precededByGap:completion:
--
-- Associate a heartbeat with the receiver.
--
-- Use this method to asynchronously add a heartbeat to the series.
--
-- @timeIntervalSinceStart@ — The elapsed time between the series startDate and the heartbeat occurence. Must be                                    a positive value.
--
-- @precededByGap@ — Whether or not this heartbeat was preceded by a gap in data collection.
--
-- @completion@ — The completion callback handler returns the status of the save. If the completion                                    handler success is NO, then error is non-nil. An error here is considered fatal and                                    the series builder will be complete.
--
-- ObjC selector: @- addHeartbeatWithTimeIntervalSinceSeriesStartDate:precededByGap:completion:@
addHeartbeatWithTimeIntervalSinceSeriesStartDate_precededByGap_completion :: IsHKHeartbeatSeriesBuilder hkHeartbeatSeriesBuilder => hkHeartbeatSeriesBuilder -> CDouble -> Bool -> Ptr () -> IO ()
addHeartbeatWithTimeIntervalSinceSeriesStartDate_precededByGap_completion hkHeartbeatSeriesBuilder  timeIntervalSinceStart precededByGap completion =
    sendMsg hkHeartbeatSeriesBuilder (mkSelector "addHeartbeatWithTimeIntervalSinceSeriesStartDate:precededByGap:completion:") retVoid [argCDouble timeIntervalSinceStart, argCULong (if precededByGap then 1 else 0), argPtr (castPtr completion :: Ptr ())]

-- | addMetadata:completion:
--
-- Adds new metadata to the builder instance. This method can be called more than once; each time                     the newly provided metadata will be incorporated in the same manner as                     -[NSMutableDictionary addEntriesFromDictionary:].                     This operation is performed asynchronously and the completion will be executed on an arbitrary                     background queue.
--
-- @metadata@ — The metadata to add to the builder.
--
-- @completion@ — Block to be called when the addition of metadata to the builder is complete.                                 If success is YES, the metadata has been added to the builder successfully. If success                                 is NO, error will be non-null and will contain the error encountered during the                                 insertion operation. When an error occurs, the builder's metadata will remain unchanged.
--
-- ObjC selector: @- addMetadata:completion:@
addMetadata_completion :: (IsHKHeartbeatSeriesBuilder hkHeartbeatSeriesBuilder, IsNSDictionary metadata) => hkHeartbeatSeriesBuilder -> metadata -> Ptr () -> IO ()
addMetadata_completion hkHeartbeatSeriesBuilder  metadata completion =
  withObjCPtr metadata $ \raw_metadata ->
      sendMsg hkHeartbeatSeriesBuilder (mkSelector "addMetadata:completion:") retVoid [argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | finishSeriesWithCompletion:
--
-- Method to stop data collection and return the associated HKHeartbeatSeriesSample.
--
-- Call this method when you have added all heartbeats to this builder. The completion handler will                     return the saved HKHeartbeatSeriesSample. If no heartbeat was added, then heartbeatSeries will be                     nil and an error returned. The receiver will be considered invalid afterwards and any further calls                     to it will result in an error.
--
-- @completion@ — The completion callback handler returns the saved HKHeartbeatSeriesSample object. If                                 heartbeatSeries is nil, an error will indicate why the series could not be returned                                 including database inaccessibility during device lock. Subsequent requests for the                                 HKHeartbeatSeriesSample can be made through HKSampleQuery or similar queries. To                                 retrieve the data stored with an HKHeartbeatSeriesSample use HKHeartbeatSeriesQuery.
--
-- ObjC selector: @- finishSeriesWithCompletion:@
finishSeriesWithCompletion :: IsHKHeartbeatSeriesBuilder hkHeartbeatSeriesBuilder => hkHeartbeatSeriesBuilder -> Ptr () -> IO ()
finishSeriesWithCompletion hkHeartbeatSeriesBuilder  completion =
    sendMsg hkHeartbeatSeriesBuilder (mkSelector "finishSeriesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | maximumCount
--
-- The maximum number of heartbeats that can be added to an HKHeartbeatSeriesBuilder.
--
-- Any calls to addHeartbeatWithTimeIntervalSinceSeriesStartDate:precededByGap:completion: once                     maximumCount has been reached will fail and an error will be returned in the completion handler.
--
-- ObjC selector: @+ maximumCount@
maximumCount :: IO CULong
maximumCount  =
  do
    cls' <- getRequiredClass "HKHeartbeatSeriesBuilder"
    sendClassMsg cls' (mkSelector "maximumCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHealthStore:device:startDate:@
initWithHealthStore_device_startDateSelector :: Selector
initWithHealthStore_device_startDateSelector = mkSelector "initWithHealthStore:device:startDate:"

-- | @Selector@ for @addHeartbeatWithTimeIntervalSinceSeriesStartDate:precededByGap:completion:@
addHeartbeatWithTimeIntervalSinceSeriesStartDate_precededByGap_completionSelector :: Selector
addHeartbeatWithTimeIntervalSinceSeriesStartDate_precededByGap_completionSelector = mkSelector "addHeartbeatWithTimeIntervalSinceSeriesStartDate:precededByGap:completion:"

-- | @Selector@ for @addMetadata:completion:@
addMetadata_completionSelector :: Selector
addMetadata_completionSelector = mkSelector "addMetadata:completion:"

-- | @Selector@ for @finishSeriesWithCompletion:@
finishSeriesWithCompletionSelector :: Selector
finishSeriesWithCompletionSelector = mkSelector "finishSeriesWithCompletion:"

-- | @Selector@ for @maximumCount@
maximumCountSelector :: Selector
maximumCountSelector = mkSelector "maximumCount"

