{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKAudiogramSample
--
-- A sample object representing the results of a standard hearing test.
--
-- Generated bindings for @HKAudiogramSample@.
module ObjC.HealthKit.HKAudiogramSample
  ( HKAudiogramSample
  , IsHKAudiogramSample(..)
  , audiogramSampleWithSensitivityPoints_startDate_endDate_metadata
  , audiogramSampleWithSensitivityPoints_startDate_endDate_device_metadata
  , sensitivityPoints
  , audiogramSampleWithSensitivityPoints_startDate_endDate_metadataSelector
  , audiogramSampleWithSensitivityPoints_startDate_endDate_device_metadataSelector
  , sensitivityPointsSelector


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

-- | audiogramSampleWithSensitivityPoints:startDate:endDate:metadata:
--
-- Creates a new audiogram sample with the specified attributes.
--
-- @sensitivityPoints@ — Sensitivity data associated with the sample, with a maximum limit of 30 points. Frequencies must be unique, and ordered ascending.
--
-- @startDate@ — The start date for the hearing test.
--
-- @endDate@ — The end date for the hearing test.
--
-- @metadata@ — Optional meta data associated with the sample.
--
-- Returns: A new instance of an audiogram sample.
--
-- ObjC selector: @+ audiogramSampleWithSensitivityPoints:startDate:endDate:metadata:@
audiogramSampleWithSensitivityPoints_startDate_endDate_metadata :: (IsNSArray sensitivityPoints, IsNSDate startDate, IsNSDate endDate, IsNSDictionary metadata) => sensitivityPoints -> startDate -> endDate -> metadata -> IO (Id HKAudiogramSample)
audiogramSampleWithSensitivityPoints_startDate_endDate_metadata sensitivityPoints startDate endDate metadata =
  do
    cls' <- getRequiredClass "HKAudiogramSample"
    withObjCPtr sensitivityPoints $ \raw_sensitivityPoints ->
      withObjCPtr startDate $ \raw_startDate ->
        withObjCPtr endDate $ \raw_endDate ->
          withObjCPtr metadata $ \raw_metadata ->
            sendClassMsg cls' (mkSelector "audiogramSampleWithSensitivityPoints:startDate:endDate:metadata:") (retPtr retVoid) [argPtr (castPtr raw_sensitivityPoints :: Ptr ()), argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | audiogramSampleWithSensitivityPoints:startDate:endDate:device:metadata:
--
-- Creates a new audiogram sample with the specified attributes.
--
-- @sensitivityPoints@ — Sensitivity data associated with the sample, with a maximum limit of 30 points. Frequencies must be unique, and ordered ascending.
--
-- @startDate@ — The start date of the hearing test.
--
-- @endDate@ — The end date of the hearing test.
--
-- @device@ — The device that generated the sample data.
--
-- @metadata@ — Optional metadata associated with the sample.
--
-- Returns: A new instance of an audiogram sample.
--
-- ObjC selector: @+ audiogramSampleWithSensitivityPoints:startDate:endDate:device:metadata:@
audiogramSampleWithSensitivityPoints_startDate_endDate_device_metadata :: (IsNSArray sensitivityPoints, IsNSDate startDate, IsNSDate endDate, IsHKDevice device, IsNSDictionary metadata) => sensitivityPoints -> startDate -> endDate -> device -> metadata -> IO (Id HKAudiogramSample)
audiogramSampleWithSensitivityPoints_startDate_endDate_device_metadata sensitivityPoints startDate endDate device metadata =
  do
    cls' <- getRequiredClass "HKAudiogramSample"
    withObjCPtr sensitivityPoints $ \raw_sensitivityPoints ->
      withObjCPtr startDate $ \raw_startDate ->
        withObjCPtr endDate $ \raw_endDate ->
          withObjCPtr device $ \raw_device ->
            withObjCPtr metadata $ \raw_metadata ->
              sendClassMsg cls' (mkSelector "audiogramSampleWithSensitivityPoints:startDate:endDate:device:metadata:") (retPtr retVoid) [argPtr (castPtr raw_sensitivityPoints :: Ptr ()), argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | sensitivityPoints
--
-- The hearing sensitivity readings associated with a hearing test.
--
-- ObjC selector: @- sensitivityPoints@
sensitivityPoints :: IsHKAudiogramSample hkAudiogramSample => hkAudiogramSample -> IO (Id NSArray)
sensitivityPoints hkAudiogramSample  =
    sendMsg hkAudiogramSample (mkSelector "sensitivityPoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @audiogramSampleWithSensitivityPoints:startDate:endDate:metadata:@
audiogramSampleWithSensitivityPoints_startDate_endDate_metadataSelector :: Selector
audiogramSampleWithSensitivityPoints_startDate_endDate_metadataSelector = mkSelector "audiogramSampleWithSensitivityPoints:startDate:endDate:metadata:"

-- | @Selector@ for @audiogramSampleWithSensitivityPoints:startDate:endDate:device:metadata:@
audiogramSampleWithSensitivityPoints_startDate_endDate_device_metadataSelector :: Selector
audiogramSampleWithSensitivityPoints_startDate_endDate_device_metadataSelector = mkSelector "audiogramSampleWithSensitivityPoints:startDate:endDate:device:metadata:"

-- | @Selector@ for @sensitivityPoints@
sensitivityPointsSelector :: Selector
sensitivityPointsSelector = mkSelector "sensitivityPoints"

