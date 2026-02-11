{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Application Basic    This cluster provides information about an application running on a TV or media player device which is represented as an endpoint.
--
-- Generated bindings for @MTRClusterApplicationBasic@.
module ObjC.Matter.MTRClusterApplicationBasic
  ( MTRClusterApplicationBasic
  , IsMTRClusterApplicationBasic(..)
  , readAttributeVendorNameWithParams
  , readAttributeVendorIDWithParams
  , readAttributeApplicationNameWithParams
  , readAttributeProductIDWithParams
  , readAttributeApplicationWithParams
  , readAttributeStatusWithParams
  , readAttributeApplicationVersionWithParams
  , readAttributeAllowedVendorListWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeVendorNameWithParamsSelector
  , readAttributeVendorIDWithParamsSelector
  , readAttributeApplicationNameWithParamsSelector
  , readAttributeProductIDWithParamsSelector
  , readAttributeApplicationWithParamsSelector
  , readAttributeStatusWithParamsSelector
  , readAttributeApplicationVersionWithParamsSelector
  , readAttributeAllowedVendorListWithParamsSelector
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

-- | @- readAttributeVendorNameWithParams:@
readAttributeVendorNameWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeVendorNameWithParams mtrClusterApplicationBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationBasic (mkSelector "readAttributeVendorNameWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeVendorIDWithParams:@
readAttributeVendorIDWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeVendorIDWithParams mtrClusterApplicationBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationBasic (mkSelector "readAttributeVendorIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeApplicationNameWithParams:@
readAttributeApplicationNameWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeApplicationNameWithParams mtrClusterApplicationBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationBasic (mkSelector "readAttributeApplicationNameWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProductIDWithParams:@
readAttributeProductIDWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeProductIDWithParams mtrClusterApplicationBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationBasic (mkSelector "readAttributeProductIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeApplicationWithParams:@
readAttributeApplicationWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeApplicationWithParams mtrClusterApplicationBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationBasic (mkSelector "readAttributeApplicationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStatusWithParams:@
readAttributeStatusWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeStatusWithParams mtrClusterApplicationBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationBasic (mkSelector "readAttributeStatusWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeApplicationVersionWithParams:@
readAttributeApplicationVersionWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeApplicationVersionWithParams mtrClusterApplicationBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationBasic (mkSelector "readAttributeApplicationVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAllowedVendorListWithParams:@
readAttributeAllowedVendorListWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeAllowedVendorListWithParams mtrClusterApplicationBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationBasic (mkSelector "readAttributeAllowedVendorListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterApplicationBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationBasic (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterApplicationBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationBasic (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterApplicationBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationBasic (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterApplicationBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationBasic (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterApplicationBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationBasic (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterApplicationBasic mtrClusterApplicationBasic => mtrClusterApplicationBasic -> IO (Id MTRClusterApplicationBasic)
init_ mtrClusterApplicationBasic  =
    sendMsg mtrClusterApplicationBasic (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterApplicationBasic)
new  =
  do
    cls' <- getRequiredClass "MTRClusterApplicationBasic"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRDevice device, IsNSObject queue) => mtrClusterApplicationBasic -> device -> CUShort -> queue -> IO (Id MTRClusterApplicationBasic)
initWithDevice_endpoint_queue mtrClusterApplicationBasic  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterApplicationBasic (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterApplicationBasic -> device -> endpointID -> queue -> IO (Id MTRClusterApplicationBasic)
initWithDevice_endpointID_queue mtrClusterApplicationBasic  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterApplicationBasic (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeVendorNameWithParams:@
readAttributeVendorNameWithParamsSelector :: Selector
readAttributeVendorNameWithParamsSelector = mkSelector "readAttributeVendorNameWithParams:"

-- | @Selector@ for @readAttributeVendorIDWithParams:@
readAttributeVendorIDWithParamsSelector :: Selector
readAttributeVendorIDWithParamsSelector = mkSelector "readAttributeVendorIDWithParams:"

-- | @Selector@ for @readAttributeApplicationNameWithParams:@
readAttributeApplicationNameWithParamsSelector :: Selector
readAttributeApplicationNameWithParamsSelector = mkSelector "readAttributeApplicationNameWithParams:"

-- | @Selector@ for @readAttributeProductIDWithParams:@
readAttributeProductIDWithParamsSelector :: Selector
readAttributeProductIDWithParamsSelector = mkSelector "readAttributeProductIDWithParams:"

-- | @Selector@ for @readAttributeApplicationWithParams:@
readAttributeApplicationWithParamsSelector :: Selector
readAttributeApplicationWithParamsSelector = mkSelector "readAttributeApplicationWithParams:"

-- | @Selector@ for @readAttributeStatusWithParams:@
readAttributeStatusWithParamsSelector :: Selector
readAttributeStatusWithParamsSelector = mkSelector "readAttributeStatusWithParams:"

-- | @Selector@ for @readAttributeApplicationVersionWithParams:@
readAttributeApplicationVersionWithParamsSelector :: Selector
readAttributeApplicationVersionWithParamsSelector = mkSelector "readAttributeApplicationVersionWithParams:"

-- | @Selector@ for @readAttributeAllowedVendorListWithParams:@
readAttributeAllowedVendorListWithParamsSelector :: Selector
readAttributeAllowedVendorListWithParamsSelector = mkSelector "readAttributeAllowedVendorListWithParams:"

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

