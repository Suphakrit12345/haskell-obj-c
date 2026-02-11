{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Air Quality    Attributes for reporting air quality classification
--
-- Generated bindings for @MTRClusterAirQuality@.
module ObjC.Matter.MTRClusterAirQuality
  ( MTRClusterAirQuality
  , IsMTRClusterAirQuality(..)
  , readAttributeAirQualityWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeAirQualityWithParamsSelector
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

-- | @- readAttributeAirQualityWithParams:@
readAttributeAirQualityWithParams :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRReadParams params) => mtrClusterAirQuality -> params -> IO (Id NSDictionary)
readAttributeAirQualityWithParams mtrClusterAirQuality  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAirQuality (mkSelector "readAttributeAirQualityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRReadParams params) => mtrClusterAirQuality -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterAirQuality  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAirQuality (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRReadParams params) => mtrClusterAirQuality -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterAirQuality  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAirQuality (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRReadParams params) => mtrClusterAirQuality -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterAirQuality  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAirQuality (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRReadParams params) => mtrClusterAirQuality -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterAirQuality  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAirQuality (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRReadParams params) => mtrClusterAirQuality -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterAirQuality  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAirQuality (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterAirQuality mtrClusterAirQuality => mtrClusterAirQuality -> IO (Id MTRClusterAirQuality)
init_ mtrClusterAirQuality  =
    sendMsg mtrClusterAirQuality (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterAirQuality)
new  =
  do
    cls' <- getRequiredClass "MTRClusterAirQuality"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterAirQuality -> device -> endpointID -> queue -> IO (Id MTRClusterAirQuality)
initWithDevice_endpointID_queue mtrClusterAirQuality  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterAirQuality (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeAirQualityWithParams:@
readAttributeAirQualityWithParamsSelector :: Selector
readAttributeAirQualityWithParamsSelector = mkSelector "readAttributeAirQualityWithParams:"

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

