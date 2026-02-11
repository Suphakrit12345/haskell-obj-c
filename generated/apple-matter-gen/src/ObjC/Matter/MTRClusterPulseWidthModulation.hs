{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Pulse Width Modulation    Cluster to control pulse width modulation
--
-- Generated bindings for @MTRClusterPulseWidthModulation@.
module ObjC.Matter.MTRClusterPulseWidthModulation
  ( MTRClusterPulseWidthModulation
  , IsMTRClusterPulseWidthModulation(..)
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
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

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation, IsMTRReadParams params) => mtrClusterPulseWidthModulation -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPulseWidthModulation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPulseWidthModulation (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation, IsMTRReadParams params) => mtrClusterPulseWidthModulation -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPulseWidthModulation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPulseWidthModulation (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation, IsMTRReadParams params) => mtrClusterPulseWidthModulation -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPulseWidthModulation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPulseWidthModulation (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation, IsMTRReadParams params) => mtrClusterPulseWidthModulation -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPulseWidthModulation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPulseWidthModulation (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation, IsMTRReadParams params) => mtrClusterPulseWidthModulation -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPulseWidthModulation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPulseWidthModulation (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation => mtrClusterPulseWidthModulation -> IO (Id MTRClusterPulseWidthModulation)
init_ mtrClusterPulseWidthModulation  =
    sendMsg mtrClusterPulseWidthModulation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterPulseWidthModulation)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPulseWidthModulation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPulseWidthModulation -> device -> endpointID -> queue -> IO (Id MTRClusterPulseWidthModulation)
initWithDevice_endpointID_queue mtrClusterPulseWidthModulation  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterPulseWidthModulation (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

