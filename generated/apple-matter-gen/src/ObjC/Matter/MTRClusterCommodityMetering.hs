{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commodity Metering    The Commodity Metering Cluster provides the mechanism for communicating commodity consumption information within a premises.
--
-- Generated bindings for @MTRClusterCommodityMetering@.
module ObjC.Matter.MTRClusterCommodityMetering
  ( MTRClusterCommodityMetering
  , IsMTRClusterCommodityMetering(..)
  , readAttributeMeteredQuantityWithParams
  , readAttributeMeteredQuantityTimestampWithParams
  , readAttributeTariffUnitWithParams
  , readAttributeMaximumMeteredQuantitiesWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeMeteredQuantityWithParamsSelector
  , readAttributeMeteredQuantityTimestampWithParamsSelector
  , readAttributeTariffUnitWithParamsSelector
  , readAttributeMaximumMeteredQuantitiesWithParamsSelector
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

-- | @- readAttributeMeteredQuantityWithParams:@
readAttributeMeteredQuantityWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeMeteredQuantityWithParams mtrClusterCommodityMetering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityMetering (mkSelector "readAttributeMeteredQuantityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeteredQuantityTimestampWithParams:@
readAttributeMeteredQuantityTimestampWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeMeteredQuantityTimestampWithParams mtrClusterCommodityMetering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityMetering (mkSelector "readAttributeMeteredQuantityTimestampWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTariffUnitWithParams:@
readAttributeTariffUnitWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeTariffUnitWithParams mtrClusterCommodityMetering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityMetering (mkSelector "readAttributeTariffUnitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaximumMeteredQuantitiesWithParams:@
readAttributeMaximumMeteredQuantitiesWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeMaximumMeteredQuantitiesWithParams mtrClusterCommodityMetering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityMetering (mkSelector "readAttributeMaximumMeteredQuantitiesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCommodityMetering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityMetering (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCommodityMetering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityMetering (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCommodityMetering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityMetering (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCommodityMetering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityMetering (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCommodityMetering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommodityMetering (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterCommodityMetering mtrClusterCommodityMetering => mtrClusterCommodityMetering -> IO (Id MTRClusterCommodityMetering)
init_ mtrClusterCommodityMetering  =
    sendMsg mtrClusterCommodityMetering (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterCommodityMetering)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCommodityMetering"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCommodityMetering -> device -> endpointID -> queue -> IO (Id MTRClusterCommodityMetering)
initWithDevice_endpointID_queue mtrClusterCommodityMetering  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterCommodityMetering (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMeteredQuantityWithParams:@
readAttributeMeteredQuantityWithParamsSelector :: Selector
readAttributeMeteredQuantityWithParamsSelector = mkSelector "readAttributeMeteredQuantityWithParams:"

-- | @Selector@ for @readAttributeMeteredQuantityTimestampWithParams:@
readAttributeMeteredQuantityTimestampWithParamsSelector :: Selector
readAttributeMeteredQuantityTimestampWithParamsSelector = mkSelector "readAttributeMeteredQuantityTimestampWithParams:"

-- | @Selector@ for @readAttributeTariffUnitWithParams:@
readAttributeTariffUnitWithParamsSelector :: Selector
readAttributeTariffUnitWithParamsSelector = mkSelector "readAttributeTariffUnitWithParams:"

-- | @Selector@ for @readAttributeMaximumMeteredQuantitiesWithParams:@
readAttributeMaximumMeteredQuantitiesWithParamsSelector :: Selector
readAttributeMaximumMeteredQuantitiesWithParamsSelector = mkSelector "readAttributeMaximumMeteredQuantitiesWithParams:"

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

