{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commodity Price    The Commodity Price Cluster provides the mechanism for communicating Gas, Energy, or Water pricing information within the premises.
--
-- Generated bindings for @MTRClusterCommodityPrice@.
module ObjC.Matter.MTRClusterCommodityPrice
  ( MTRClusterCommodityPrice
  , IsMTRClusterCommodityPrice(..)
  , getDetailedPriceRequestWithParams_expectedValues_expectedValueInterval_completion
  , getDetailedForecastRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeTariffUnitWithParams
  , readAttributeCurrencyWithParams
  , readAttributeCurrentPriceWithParams
  , readAttributePriceForecastWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , getDetailedPriceRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , getDetailedForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeTariffUnitWithParamsSelector
  , readAttributeCurrencyWithParamsSelector
  , readAttributeCurrentPriceWithParamsSelector
  , readAttributePriceForecastWithParamsSelector
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

-- | @- getDetailedPriceRequestWithParams:expectedValues:expectedValueInterval:completion:@
getDetailedPriceRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRCommodityPriceClusterGetDetailedPriceRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCommodityPrice -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getDetailedPriceRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterCommodityPrice  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCommodityPrice (mkSelector "getDetailedPriceRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getDetailedForecastRequestWithParams:expectedValues:expectedValueInterval:completion:@
getDetailedForecastRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRCommodityPriceClusterGetDetailedForecastRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCommodityPrice -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getDetailedForecastRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterCommodityPrice  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCommodityPrice (mkSelector "getDetailedForecastRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTariffUnitWithParams:@
readAttributeTariffUnitWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeTariffUnitWithParams mtrClusterCommodityPrice  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityPrice (mkSelector "readAttributeTariffUnitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrencyWithParams:@
readAttributeCurrencyWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeCurrencyWithParams mtrClusterCommodityPrice  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityPrice (mkSelector "readAttributeCurrencyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentPriceWithParams:@
readAttributeCurrentPriceWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeCurrentPriceWithParams mtrClusterCommodityPrice  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityPrice (mkSelector "readAttributeCurrentPriceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePriceForecastWithParams:@
readAttributePriceForecastWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributePriceForecastWithParams mtrClusterCommodityPrice  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityPrice (mkSelector "readAttributePriceForecastWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCommodityPrice  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityPrice (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCommodityPrice  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityPrice (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCommodityPrice  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityPrice (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCommodityPrice  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityPrice (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCommodityPrice  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityPrice (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterCommodityPrice mtrClusterCommodityPrice => mtrClusterCommodityPrice -> IO (Id MTRClusterCommodityPrice)
init_ mtrClusterCommodityPrice  =
    sendMsg mtrClusterCommodityPrice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterCommodityPrice)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCommodityPrice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCommodityPrice -> device -> endpointID -> queue -> IO (Id MTRClusterCommodityPrice)
initWithDevice_endpointID_queue mtrClusterCommodityPrice  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterCommodityPrice (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getDetailedPriceRequestWithParams:expectedValues:expectedValueInterval:completion:@
getDetailedPriceRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getDetailedPriceRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getDetailedPriceRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getDetailedForecastRequestWithParams:expectedValues:expectedValueInterval:completion:@
getDetailedForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getDetailedForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getDetailedForecastRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeTariffUnitWithParams:@
readAttributeTariffUnitWithParamsSelector :: Selector
readAttributeTariffUnitWithParamsSelector = mkSelector "readAttributeTariffUnitWithParams:"

-- | @Selector@ for @readAttributeCurrencyWithParams:@
readAttributeCurrencyWithParamsSelector :: Selector
readAttributeCurrencyWithParamsSelector = mkSelector "readAttributeCurrencyWithParams:"

-- | @Selector@ for @readAttributeCurrentPriceWithParams:@
readAttributeCurrentPriceWithParamsSelector :: Selector
readAttributeCurrentPriceWithParamsSelector = mkSelector "readAttributeCurrentPriceWithParams:"

-- | @Selector@ for @readAttributePriceForecastWithParams:@
readAttributePriceForecastWithParamsSelector :: Selector
readAttributePriceForecastWithParamsSelector = mkSelector "readAttributePriceForecastWithParams:"

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

