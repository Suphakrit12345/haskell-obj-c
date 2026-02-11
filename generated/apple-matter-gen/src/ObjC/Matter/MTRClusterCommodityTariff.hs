{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commodity Tariff    The CommodityTariffCluster provides the mechanism for communicating Commodity Tariff information within the premises.
--
-- Generated bindings for @MTRClusterCommodityTariff@.
module ObjC.Matter.MTRClusterCommodityTariff
  ( MTRClusterCommodityTariff
  , IsMTRClusterCommodityTariff(..)
  , getTariffComponentWithParams_expectedValues_expectedValueInterval_completion
  , getDayEntryWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeTariffInfoWithParams
  , readAttributeTariffUnitWithParams
  , readAttributeStartDateWithParams
  , readAttributeDayEntriesWithParams
  , readAttributeDayPatternsWithParams
  , readAttributeCalendarPeriodsWithParams
  , readAttributeIndividualDaysWithParams
  , readAttributeCurrentDayWithParams
  , readAttributeNextDayWithParams
  , readAttributeCurrentDayEntryWithParams
  , readAttributeCurrentDayEntryDateWithParams
  , readAttributeNextDayEntryWithParams
  , readAttributeNextDayEntryDateWithParams
  , readAttributeTariffComponentsWithParams
  , readAttributeTariffPeriodsWithParams
  , readAttributeCurrentTariffComponentsWithParams
  , readAttributeNextTariffComponentsWithParams
  , readAttributeDefaultRandomizationOffsetWithParams
  , readAttributeDefaultRandomizationTypeWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , getTariffComponentWithParams_expectedValues_expectedValueInterval_completionSelector
  , getDayEntryWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeTariffInfoWithParamsSelector
  , readAttributeTariffUnitWithParamsSelector
  , readAttributeStartDateWithParamsSelector
  , readAttributeDayEntriesWithParamsSelector
  , readAttributeDayPatternsWithParamsSelector
  , readAttributeCalendarPeriodsWithParamsSelector
  , readAttributeIndividualDaysWithParamsSelector
  , readAttributeCurrentDayWithParamsSelector
  , readAttributeNextDayWithParamsSelector
  , readAttributeCurrentDayEntryWithParamsSelector
  , readAttributeCurrentDayEntryDateWithParamsSelector
  , readAttributeNextDayEntryWithParamsSelector
  , readAttributeNextDayEntryDateWithParamsSelector
  , readAttributeTariffComponentsWithParamsSelector
  , readAttributeTariffPeriodsWithParamsSelector
  , readAttributeCurrentTariffComponentsWithParamsSelector
  , readAttributeNextTariffComponentsWithParamsSelector
  , readAttributeDefaultRandomizationOffsetWithParamsSelector
  , readAttributeDefaultRandomizationTypeWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpointID_queueSelector


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

-- | @- getTariffComponentWithParams:expectedValues:expectedValueInterval:completion:@
getTariffComponentWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRCommodityTariffClusterGetTariffComponentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCommodityTariff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getTariffComponentWithParams_expectedValues_expectedValueInterval_completion mtrClusterCommodityTariff  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCommodityTariff (mkSelector "getTariffComponentWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getDayEntryWithParams:expectedValues:expectedValueInterval:completion:@
getDayEntryWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRCommodityTariffClusterGetDayEntryParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCommodityTariff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getDayEntryWithParams_expectedValues_expectedValueInterval_completion mtrClusterCommodityTariff  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCommodityTariff (mkSelector "getDayEntryWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTariffInfoWithParams:@
readAttributeTariffInfoWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeTariffInfoWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeTariffInfoWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTariffUnitWithParams:@
readAttributeTariffUnitWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeTariffUnitWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeTariffUnitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStartDateWithParams:@
readAttributeStartDateWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeStartDateWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeStartDateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDayEntriesWithParams:@
readAttributeDayEntriesWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeDayEntriesWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeDayEntriesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDayPatternsWithParams:@
readAttributeDayPatternsWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeDayPatternsWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeDayPatternsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCalendarPeriodsWithParams:@
readAttributeCalendarPeriodsWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeCalendarPeriodsWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeCalendarPeriodsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeIndividualDaysWithParams:@
readAttributeIndividualDaysWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeIndividualDaysWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeIndividualDaysWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentDayWithParams:@
readAttributeCurrentDayWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeCurrentDayWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeCurrentDayWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNextDayWithParams:@
readAttributeNextDayWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeNextDayWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeNextDayWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentDayEntryWithParams:@
readAttributeCurrentDayEntryWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeCurrentDayEntryWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeCurrentDayEntryWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentDayEntryDateWithParams:@
readAttributeCurrentDayEntryDateWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeCurrentDayEntryDateWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeCurrentDayEntryDateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNextDayEntryWithParams:@
readAttributeNextDayEntryWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeNextDayEntryWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeNextDayEntryWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNextDayEntryDateWithParams:@
readAttributeNextDayEntryDateWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeNextDayEntryDateWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeNextDayEntryDateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTariffComponentsWithParams:@
readAttributeTariffComponentsWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeTariffComponentsWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeTariffComponentsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTariffPeriodsWithParams:@
readAttributeTariffPeriodsWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeTariffPeriodsWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeTariffPeriodsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentTariffComponentsWithParams:@
readAttributeCurrentTariffComponentsWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeCurrentTariffComponentsWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeCurrentTariffComponentsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNextTariffComponentsWithParams:@
readAttributeNextTariffComponentsWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeNextTariffComponentsWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeNextTariffComponentsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDefaultRandomizationOffsetWithParams:@
readAttributeDefaultRandomizationOffsetWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeDefaultRandomizationOffsetWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeDefaultRandomizationOffsetWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDefaultRandomizationTypeWithParams:@
readAttributeDefaultRandomizationTypeWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeDefaultRandomizationTypeWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeDefaultRandomizationTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCommodityTariff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityTariff (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterCommodityTariff mtrClusterCommodityTariff => mtrClusterCommodityTariff -> IO (Id MTRClusterCommodityTariff)
init_ mtrClusterCommodityTariff  =
    sendMsg mtrClusterCommodityTariff (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterCommodityTariff)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCommodityTariff"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCommodityTariff -> device -> endpointID -> queue -> IO (Id MTRClusterCommodityTariff)
initWithDevice_endpointID_queue mtrClusterCommodityTariff  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterCommodityTariff (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getTariffComponentWithParams:expectedValues:expectedValueInterval:completion:@
getTariffComponentWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getTariffComponentWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getTariffComponentWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getDayEntryWithParams:expectedValues:expectedValueInterval:completion:@
getDayEntryWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getDayEntryWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getDayEntryWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeTariffInfoWithParams:@
readAttributeTariffInfoWithParamsSelector :: Selector
readAttributeTariffInfoWithParamsSelector = mkSelector "readAttributeTariffInfoWithParams:"

-- | @Selector@ for @readAttributeTariffUnitWithParams:@
readAttributeTariffUnitWithParamsSelector :: Selector
readAttributeTariffUnitWithParamsSelector = mkSelector "readAttributeTariffUnitWithParams:"

-- | @Selector@ for @readAttributeStartDateWithParams:@
readAttributeStartDateWithParamsSelector :: Selector
readAttributeStartDateWithParamsSelector = mkSelector "readAttributeStartDateWithParams:"

-- | @Selector@ for @readAttributeDayEntriesWithParams:@
readAttributeDayEntriesWithParamsSelector :: Selector
readAttributeDayEntriesWithParamsSelector = mkSelector "readAttributeDayEntriesWithParams:"

-- | @Selector@ for @readAttributeDayPatternsWithParams:@
readAttributeDayPatternsWithParamsSelector :: Selector
readAttributeDayPatternsWithParamsSelector = mkSelector "readAttributeDayPatternsWithParams:"

-- | @Selector@ for @readAttributeCalendarPeriodsWithParams:@
readAttributeCalendarPeriodsWithParamsSelector :: Selector
readAttributeCalendarPeriodsWithParamsSelector = mkSelector "readAttributeCalendarPeriodsWithParams:"

-- | @Selector@ for @readAttributeIndividualDaysWithParams:@
readAttributeIndividualDaysWithParamsSelector :: Selector
readAttributeIndividualDaysWithParamsSelector = mkSelector "readAttributeIndividualDaysWithParams:"

-- | @Selector@ for @readAttributeCurrentDayWithParams:@
readAttributeCurrentDayWithParamsSelector :: Selector
readAttributeCurrentDayWithParamsSelector = mkSelector "readAttributeCurrentDayWithParams:"

-- | @Selector@ for @readAttributeNextDayWithParams:@
readAttributeNextDayWithParamsSelector :: Selector
readAttributeNextDayWithParamsSelector = mkSelector "readAttributeNextDayWithParams:"

-- | @Selector@ for @readAttributeCurrentDayEntryWithParams:@
readAttributeCurrentDayEntryWithParamsSelector :: Selector
readAttributeCurrentDayEntryWithParamsSelector = mkSelector "readAttributeCurrentDayEntryWithParams:"

-- | @Selector@ for @readAttributeCurrentDayEntryDateWithParams:@
readAttributeCurrentDayEntryDateWithParamsSelector :: Selector
readAttributeCurrentDayEntryDateWithParamsSelector = mkSelector "readAttributeCurrentDayEntryDateWithParams:"

-- | @Selector@ for @readAttributeNextDayEntryWithParams:@
readAttributeNextDayEntryWithParamsSelector :: Selector
readAttributeNextDayEntryWithParamsSelector = mkSelector "readAttributeNextDayEntryWithParams:"

-- | @Selector@ for @readAttributeNextDayEntryDateWithParams:@
readAttributeNextDayEntryDateWithParamsSelector :: Selector
readAttributeNextDayEntryDateWithParamsSelector = mkSelector "readAttributeNextDayEntryDateWithParams:"

-- | @Selector@ for @readAttributeTariffComponentsWithParams:@
readAttributeTariffComponentsWithParamsSelector :: Selector
readAttributeTariffComponentsWithParamsSelector = mkSelector "readAttributeTariffComponentsWithParams:"

-- | @Selector@ for @readAttributeTariffPeriodsWithParams:@
readAttributeTariffPeriodsWithParamsSelector :: Selector
readAttributeTariffPeriodsWithParamsSelector = mkSelector "readAttributeTariffPeriodsWithParams:"

-- | @Selector@ for @readAttributeCurrentTariffComponentsWithParams:@
readAttributeCurrentTariffComponentsWithParamsSelector :: Selector
readAttributeCurrentTariffComponentsWithParamsSelector = mkSelector "readAttributeCurrentTariffComponentsWithParams:"

-- | @Selector@ for @readAttributeNextTariffComponentsWithParams:@
readAttributeNextTariffComponentsWithParamsSelector :: Selector
readAttributeNextTariffComponentsWithParamsSelector = mkSelector "readAttributeNextTariffComponentsWithParams:"

-- | @Selector@ for @readAttributeDefaultRandomizationOffsetWithParams:@
readAttributeDefaultRandomizationOffsetWithParamsSelector :: Selector
readAttributeDefaultRandomizationOffsetWithParamsSelector = mkSelector "readAttributeDefaultRandomizationOffsetWithParams:"

-- | @Selector@ for @readAttributeDefaultRandomizationTypeWithParams:@
readAttributeDefaultRandomizationTypeWithParamsSelector :: Selector
readAttributeDefaultRandomizationTypeWithParamsSelector = mkSelector "readAttributeDefaultRandomizationTypeWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

