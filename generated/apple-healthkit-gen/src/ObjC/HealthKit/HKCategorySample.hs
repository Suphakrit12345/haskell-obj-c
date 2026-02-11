{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKCategorySample
--
-- An HKObject subclass representing an category measurement
--
-- Category samples are samples that can be categorized into an enum of concrete values
--
-- Generated bindings for @HKCategorySample@.
module ObjC.HealthKit.HKCategorySample
  ( HKCategorySample
  , IsHKCategorySample(..)
  , init_
  , categorySampleWithType_value_startDate_endDate_metadata
  , categorySampleWithType_value_startDate_endDate
  , categorySampleWithType_value_startDate_endDate_device_metadata
  , categoryType
  , value
  , initSelector
  , categorySampleWithType_value_startDate_endDate_metadataSelector
  , categorySampleWithType_value_startDate_endDateSelector
  , categorySampleWithType_value_startDate_endDate_device_metadataSelector
  , categoryTypeSelector
  , valueSelector


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

-- | @- init@
init_ :: IsHKCategorySample hkCategorySample => hkCategorySample -> IO (Id HKCategorySample)
init_ hkCategorySample  =
    sendMsg hkCategorySample (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | categorySampleWithType:value:startDate:endDate:metadata:
--
-- Creates a new HKCategorySample.
--
-- @type@ — The type of the sample.
--
-- @value@ — The enumeration value for the sample. See HKCategoryTypeIdentifier for appropriate value.
--
-- @startDate@ — The start date of the sample.
--
-- @endDate@ — The end date of the sample.
--
-- @metadata@ — Metadata for the sample (optional).
--
-- ObjC selector: @+ categorySampleWithType:value:startDate:endDate:metadata:@
categorySampleWithType_value_startDate_endDate_metadata :: (IsHKCategoryType type_, IsNSDate startDate, IsNSDate endDate, IsNSDictionary metadata) => type_ -> CLong -> startDate -> endDate -> metadata -> IO (Id HKCategorySample)
categorySampleWithType_value_startDate_endDate_metadata type_ value startDate endDate metadata =
  do
    cls' <- getRequiredClass "HKCategorySample"
    withObjCPtr type_ $ \raw_type_ ->
      withObjCPtr startDate $ \raw_startDate ->
        withObjCPtr endDate $ \raw_endDate ->
          withObjCPtr metadata $ \raw_metadata ->
            sendClassMsg cls' (mkSelector "categorySampleWithType:value:startDate:endDate:metadata:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ()), argCLong value, argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | categorySampleWithType:value:startDate:endDate:
--
-- Creates a new HKCategorySample.
--
-- @type@ — The type of the sample.
--
-- @value@ — The enumeration value for the sample. See HKCategoryTypeIdentifier for appropriate value.
--
-- @startDate@ — The start date of the sample.
--
-- @endDate@ — The end date of the sample.
--
-- ObjC selector: @+ categorySampleWithType:value:startDate:endDate:@
categorySampleWithType_value_startDate_endDate :: (IsHKCategoryType type_, IsNSDate startDate, IsNSDate endDate) => type_ -> CLong -> startDate -> endDate -> IO (Id HKCategorySample)
categorySampleWithType_value_startDate_endDate type_ value startDate endDate =
  do
    cls' <- getRequiredClass "HKCategorySample"
    withObjCPtr type_ $ \raw_type_ ->
      withObjCPtr startDate $ \raw_startDate ->
        withObjCPtr endDate $ \raw_endDate ->
          sendClassMsg cls' (mkSelector "categorySampleWithType:value:startDate:endDate:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ()), argCLong value, argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ())] >>= retainedObject . castPtr

-- | categorySampleWithType:value:startDate:endDate:device:metadata:
--
-- Creates a new HKCategorySample.
--
-- @type@ — The type of the sample.
--
-- @value@ — The enumeration value for the sample. See HKCategoryTypeIdentifier for appropriate value.
--
-- @startDate@ — The start date of the sample.
--
-- @endDate@ — The end date of the sample.
--
-- @device@ — The HKDevice that generated the sample (optional).
--
-- @metadata@ — Metadata for the sample (optional).
--
-- ObjC selector: @+ categorySampleWithType:value:startDate:endDate:device:metadata:@
categorySampleWithType_value_startDate_endDate_device_metadata :: (IsHKCategoryType type_, IsNSDate startDate, IsNSDate endDate, IsHKDevice device, IsNSDictionary metadata) => type_ -> CLong -> startDate -> endDate -> device -> metadata -> IO (Id HKCategorySample)
categorySampleWithType_value_startDate_endDate_device_metadata type_ value startDate endDate device metadata =
  do
    cls' <- getRequiredClass "HKCategorySample"
    withObjCPtr type_ $ \raw_type_ ->
      withObjCPtr startDate $ \raw_startDate ->
        withObjCPtr endDate $ \raw_endDate ->
          withObjCPtr device $ \raw_device ->
            withObjCPtr metadata $ \raw_metadata ->
              sendClassMsg cls' (mkSelector "categorySampleWithType:value:startDate:endDate:device:metadata:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ()), argCLong value, argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | @- categoryType@
categoryType :: IsHKCategorySample hkCategorySample => hkCategorySample -> IO (Id HKCategoryType)
categoryType hkCategorySample  =
    sendMsg hkCategorySample (mkSelector "categoryType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | value
--
-- The preferred enum for the value is determined by the receiver's category type.
--
-- ObjC selector: @- value@
value :: IsHKCategorySample hkCategorySample => hkCategorySample -> IO CLong
value hkCategorySample  =
    sendMsg hkCategorySample (mkSelector "value") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @categorySampleWithType:value:startDate:endDate:metadata:@
categorySampleWithType_value_startDate_endDate_metadataSelector :: Selector
categorySampleWithType_value_startDate_endDate_metadataSelector = mkSelector "categorySampleWithType:value:startDate:endDate:metadata:"

-- | @Selector@ for @categorySampleWithType:value:startDate:endDate:@
categorySampleWithType_value_startDate_endDateSelector :: Selector
categorySampleWithType_value_startDate_endDateSelector = mkSelector "categorySampleWithType:value:startDate:endDate:"

-- | @Selector@ for @categorySampleWithType:value:startDate:endDate:device:metadata:@
categorySampleWithType_value_startDate_endDate_device_metadataSelector :: Selector
categorySampleWithType_value_startDate_endDate_device_metadataSelector = mkSelector "categorySampleWithType:value:startDate:endDate:device:metadata:"

-- | @Selector@ for @categoryType@
categoryTypeSelector :: Selector
categoryTypeSelector = mkSelector "categoryType"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

