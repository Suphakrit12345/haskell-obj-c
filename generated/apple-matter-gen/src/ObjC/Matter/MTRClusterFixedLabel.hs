{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Fixed Label    The Fixed Label Cluster provides a feature for the device to tag an endpoint with zero or more read onlylabels.
--
-- Generated bindings for @MTRClusterFixedLabel@.
module ObjC.Matter.MTRClusterFixedLabel
  ( MTRClusterFixedLabel
  , IsMTRClusterFixedLabel(..)
  , readAttributeLabelListWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeLabelListWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
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

-- | @- readAttributeLabelListWithParams:@
readAttributeLabelListWithParams :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRReadParams params) => mtrClusterFixedLabel -> params -> IO (Id NSDictionary)
readAttributeLabelListWithParams mtrClusterFixedLabel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFixedLabel (mkSelector "readAttributeLabelListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRReadParams params) => mtrClusterFixedLabel -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterFixedLabel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFixedLabel (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRReadParams params) => mtrClusterFixedLabel -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterFixedLabel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFixedLabel (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRReadParams params) => mtrClusterFixedLabel -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterFixedLabel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFixedLabel (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRReadParams params) => mtrClusterFixedLabel -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterFixedLabel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFixedLabel (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRReadParams params) => mtrClusterFixedLabel -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterFixedLabel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFixedLabel (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterFixedLabel mtrClusterFixedLabel => mtrClusterFixedLabel -> IO (Id MTRClusterFixedLabel)
init_ mtrClusterFixedLabel  =
    sendMsg mtrClusterFixedLabel (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterFixedLabel)
new  =
  do
    cls' <- getRequiredClass "MTRClusterFixedLabel"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRDevice device, IsNSObject queue) => mtrClusterFixedLabel -> device -> CUShort -> queue -> IO (Id MTRClusterFixedLabel)
initWithDevice_endpoint_queue mtrClusterFixedLabel  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterFixedLabel (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterFixedLabel -> device -> endpointID -> queue -> IO (Id MTRClusterFixedLabel)
initWithDevice_endpointID_queue mtrClusterFixedLabel  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterFixedLabel (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeLabelListWithParams:@
readAttributeLabelListWithParamsSelector :: Selector
readAttributeLabelListWithParamsSelector = mkSelector "readAttributeLabelListWithParams:"

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

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

