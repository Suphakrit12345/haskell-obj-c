{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Basic Information    This cluster provides attributes and events for determining basic information about Nodes, which supports both      Commissioning and operational determination of Node characteristics, such as Vendor ID, Product ID and serial number,      which apply to the whole Node. Also allows setting user device information such as location.
--
-- Generated bindings for @MTRClusterBasicInformation@.
module ObjC.Matter.MTRClusterBasicInformation
  ( MTRClusterBasicInformation
  , IsMTRClusterBasicInformation(..)
  , readAttributeDataModelRevisionWithParams
  , readAttributeVendorNameWithParams
  , readAttributeVendorIDWithParams
  , readAttributeProductNameWithParams
  , readAttributeProductIDWithParams
  , readAttributeNodeLabelWithParams
  , writeAttributeNodeLabelWithValue_expectedValueInterval
  , writeAttributeNodeLabelWithValue_expectedValueInterval_params
  , readAttributeLocationWithParams
  , writeAttributeLocationWithValue_expectedValueInterval
  , writeAttributeLocationWithValue_expectedValueInterval_params
  , readAttributeHardwareVersionWithParams
  , readAttributeHardwareVersionStringWithParams
  , readAttributeSoftwareVersionWithParams
  , readAttributeSoftwareVersionStringWithParams
  , readAttributeManufacturingDateWithParams
  , readAttributePartNumberWithParams
  , readAttributeProductURLWithParams
  , readAttributeProductLabelWithParams
  , readAttributeSerialNumberWithParams
  , readAttributeLocalConfigDisabledWithParams
  , writeAttributeLocalConfigDisabledWithValue_expectedValueInterval
  , writeAttributeLocalConfigDisabledWithValue_expectedValueInterval_params
  , readAttributeReachableWithParams
  , readAttributeUniqueIDWithParams
  , readAttributeCapabilityMinimaWithParams
  , readAttributeProductAppearanceWithParams
  , readAttributeSpecificationVersionWithParams
  , readAttributeMaxPathsPerInvokeWithParams
  , readAttributeConfigurationVersionWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeDataModelRevisionWithParamsSelector
  , readAttributeVendorNameWithParamsSelector
  , readAttributeVendorIDWithParamsSelector
  , readAttributeProductNameWithParamsSelector
  , readAttributeProductIDWithParamsSelector
  , readAttributeNodeLabelWithParamsSelector
  , writeAttributeNodeLabelWithValue_expectedValueIntervalSelector
  , writeAttributeNodeLabelWithValue_expectedValueInterval_paramsSelector
  , readAttributeLocationWithParamsSelector
  , writeAttributeLocationWithValue_expectedValueIntervalSelector
  , writeAttributeLocationWithValue_expectedValueInterval_paramsSelector
  , readAttributeHardwareVersionWithParamsSelector
  , readAttributeHardwareVersionStringWithParamsSelector
  , readAttributeSoftwareVersionWithParamsSelector
  , readAttributeSoftwareVersionStringWithParamsSelector
  , readAttributeManufacturingDateWithParamsSelector
  , readAttributePartNumberWithParamsSelector
  , readAttributeProductURLWithParamsSelector
  , readAttributeProductLabelWithParamsSelector
  , readAttributeSerialNumberWithParamsSelector
  , readAttributeLocalConfigDisabledWithParamsSelector
  , writeAttributeLocalConfigDisabledWithValue_expectedValueIntervalSelector
  , writeAttributeLocalConfigDisabledWithValue_expectedValueInterval_paramsSelector
  , readAttributeReachableWithParamsSelector
  , readAttributeUniqueIDWithParamsSelector
  , readAttributeCapabilityMinimaWithParamsSelector
  , readAttributeProductAppearanceWithParamsSelector
  , readAttributeSpecificationVersionWithParamsSelector
  , readAttributeMaxPathsPerInvokeWithParamsSelector
  , readAttributeConfigurationVersionWithParamsSelector
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

-- | @- readAttributeDataModelRevisionWithParams:@
readAttributeDataModelRevisionWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeDataModelRevisionWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeDataModelRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeVendorNameWithParams:@
readAttributeVendorNameWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeVendorNameWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeVendorNameWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeVendorIDWithParams:@
readAttributeVendorIDWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeVendorIDWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeVendorIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProductNameWithParams:@
readAttributeProductNameWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductNameWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeProductNameWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProductIDWithParams:@
readAttributeProductIDWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductIDWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeProductIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNodeLabelWithParams:@
readAttributeNodeLabelWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeNodeLabelWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeNodeLabelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeNodeLabelWithValue:expectedValueInterval:@
writeAttributeNodeLabelWithValue_expectedValueInterval :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeNodeLabelWithValue_expectedValueInterval mtrClusterBasicInformation  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBasicInformation (mkSelector "writeAttributeNodeLabelWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeNodeLabelWithValue:expectedValueInterval:params:@
writeAttributeNodeLabelWithValue_expectedValueInterval_params :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeNodeLabelWithValue_expectedValueInterval_params mtrClusterBasicInformation  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBasicInformation (mkSelector "writeAttributeNodeLabelWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeLocationWithParams:@
readAttributeLocationWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeLocationWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeLocationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLocationWithValue:expectedValueInterval:@
writeAttributeLocationWithValue_expectedValueInterval :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLocationWithValue_expectedValueInterval mtrClusterBasicInformation  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBasicInformation (mkSelector "writeAttributeLocationWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLocationWithValue:expectedValueInterval:params:@
writeAttributeLocationWithValue_expectedValueInterval_params :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLocationWithValue_expectedValueInterval_params mtrClusterBasicInformation  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBasicInformation (mkSelector "writeAttributeLocationWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeHardwareVersionWithParams:@
readAttributeHardwareVersionWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeHardwareVersionWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeHardwareVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeHardwareVersionStringWithParams:@
readAttributeHardwareVersionStringWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeHardwareVersionStringWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeHardwareVersionStringWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSoftwareVersionWithParams:@
readAttributeSoftwareVersionWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSoftwareVersionWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeSoftwareVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSoftwareVersionStringWithParams:@
readAttributeSoftwareVersionStringWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSoftwareVersionStringWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeSoftwareVersionStringWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeManufacturingDateWithParams:@
readAttributeManufacturingDateWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeManufacturingDateWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeManufacturingDateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePartNumberWithParams:@
readAttributePartNumberWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributePartNumberWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributePartNumberWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProductURLWithParams:@
readAttributeProductURLWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductURLWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeProductURLWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProductLabelWithParams:@
readAttributeProductLabelWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductLabelWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeProductLabelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSerialNumberWithParams:@
readAttributeSerialNumberWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSerialNumberWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeSerialNumberWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLocalConfigDisabledWithParams:@
readAttributeLocalConfigDisabledWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeLocalConfigDisabledWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeLocalConfigDisabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:@
writeAttributeLocalConfigDisabledWithValue_expectedValueInterval :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLocalConfigDisabledWithValue_expectedValueInterval mtrClusterBasicInformation  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBasicInformation (mkSelector "writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:params:@
writeAttributeLocalConfigDisabledWithValue_expectedValueInterval_params :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLocalConfigDisabledWithValue_expectedValueInterval_params mtrClusterBasicInformation  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBasicInformation (mkSelector "writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeReachableWithParams:@
readAttributeReachableWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeReachableWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeReachableWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUniqueIDWithParams:@
readAttributeUniqueIDWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeUniqueIDWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeUniqueIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCapabilityMinimaWithParams:@
readAttributeCapabilityMinimaWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeCapabilityMinimaWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeCapabilityMinimaWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProductAppearanceWithParams:@
readAttributeProductAppearanceWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductAppearanceWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeProductAppearanceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSpecificationVersionWithParams:@
readAttributeSpecificationVersionWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSpecificationVersionWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeSpecificationVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxPathsPerInvokeWithParams:@
readAttributeMaxPathsPerInvokeWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeMaxPathsPerInvokeWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeMaxPathsPerInvokeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeConfigurationVersionWithParams:@
readAttributeConfigurationVersionWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeConfigurationVersionWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeConfigurationVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBasicInformation (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterBasicInformation mtrClusterBasicInformation => mtrClusterBasicInformation -> IO (Id MTRClusterBasicInformation)
init_ mtrClusterBasicInformation  =
    sendMsg mtrClusterBasicInformation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterBasicInformation)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBasicInformation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBasicInformation -> device -> endpointID -> queue -> IO (Id MTRClusterBasicInformation)
initWithDevice_endpointID_queue mtrClusterBasicInformation  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterBasicInformation (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeDataModelRevisionWithParams:@
readAttributeDataModelRevisionWithParamsSelector :: Selector
readAttributeDataModelRevisionWithParamsSelector = mkSelector "readAttributeDataModelRevisionWithParams:"

-- | @Selector@ for @readAttributeVendorNameWithParams:@
readAttributeVendorNameWithParamsSelector :: Selector
readAttributeVendorNameWithParamsSelector = mkSelector "readAttributeVendorNameWithParams:"

-- | @Selector@ for @readAttributeVendorIDWithParams:@
readAttributeVendorIDWithParamsSelector :: Selector
readAttributeVendorIDWithParamsSelector = mkSelector "readAttributeVendorIDWithParams:"

-- | @Selector@ for @readAttributeProductNameWithParams:@
readAttributeProductNameWithParamsSelector :: Selector
readAttributeProductNameWithParamsSelector = mkSelector "readAttributeProductNameWithParams:"

-- | @Selector@ for @readAttributeProductIDWithParams:@
readAttributeProductIDWithParamsSelector :: Selector
readAttributeProductIDWithParamsSelector = mkSelector "readAttributeProductIDWithParams:"

-- | @Selector@ for @readAttributeNodeLabelWithParams:@
readAttributeNodeLabelWithParamsSelector :: Selector
readAttributeNodeLabelWithParamsSelector = mkSelector "readAttributeNodeLabelWithParams:"

-- | @Selector@ for @writeAttributeNodeLabelWithValue:expectedValueInterval:@
writeAttributeNodeLabelWithValue_expectedValueIntervalSelector :: Selector
writeAttributeNodeLabelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeNodeLabelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeNodeLabelWithValue:expectedValueInterval:params:@
writeAttributeNodeLabelWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeNodeLabelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeNodeLabelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLocationWithParams:@
readAttributeLocationWithParamsSelector :: Selector
readAttributeLocationWithParamsSelector = mkSelector "readAttributeLocationWithParams:"

-- | @Selector@ for @writeAttributeLocationWithValue:expectedValueInterval:@
writeAttributeLocationWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLocationWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLocationWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLocationWithValue:expectedValueInterval:params:@
writeAttributeLocationWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLocationWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLocationWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeHardwareVersionWithParams:@
readAttributeHardwareVersionWithParamsSelector :: Selector
readAttributeHardwareVersionWithParamsSelector = mkSelector "readAttributeHardwareVersionWithParams:"

-- | @Selector@ for @readAttributeHardwareVersionStringWithParams:@
readAttributeHardwareVersionStringWithParamsSelector :: Selector
readAttributeHardwareVersionStringWithParamsSelector = mkSelector "readAttributeHardwareVersionStringWithParams:"

-- | @Selector@ for @readAttributeSoftwareVersionWithParams:@
readAttributeSoftwareVersionWithParamsSelector :: Selector
readAttributeSoftwareVersionWithParamsSelector = mkSelector "readAttributeSoftwareVersionWithParams:"

-- | @Selector@ for @readAttributeSoftwareVersionStringWithParams:@
readAttributeSoftwareVersionStringWithParamsSelector :: Selector
readAttributeSoftwareVersionStringWithParamsSelector = mkSelector "readAttributeSoftwareVersionStringWithParams:"

-- | @Selector@ for @readAttributeManufacturingDateWithParams:@
readAttributeManufacturingDateWithParamsSelector :: Selector
readAttributeManufacturingDateWithParamsSelector = mkSelector "readAttributeManufacturingDateWithParams:"

-- | @Selector@ for @readAttributePartNumberWithParams:@
readAttributePartNumberWithParamsSelector :: Selector
readAttributePartNumberWithParamsSelector = mkSelector "readAttributePartNumberWithParams:"

-- | @Selector@ for @readAttributeProductURLWithParams:@
readAttributeProductURLWithParamsSelector :: Selector
readAttributeProductURLWithParamsSelector = mkSelector "readAttributeProductURLWithParams:"

-- | @Selector@ for @readAttributeProductLabelWithParams:@
readAttributeProductLabelWithParamsSelector :: Selector
readAttributeProductLabelWithParamsSelector = mkSelector "readAttributeProductLabelWithParams:"

-- | @Selector@ for @readAttributeSerialNumberWithParams:@
readAttributeSerialNumberWithParamsSelector :: Selector
readAttributeSerialNumberWithParamsSelector = mkSelector "readAttributeSerialNumberWithParams:"

-- | @Selector@ for @readAttributeLocalConfigDisabledWithParams:@
readAttributeLocalConfigDisabledWithParamsSelector :: Selector
readAttributeLocalConfigDisabledWithParamsSelector = mkSelector "readAttributeLocalConfigDisabledWithParams:"

-- | @Selector@ for @writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:@
writeAttributeLocalConfigDisabledWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLocalConfigDisabledWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:params:@
writeAttributeLocalConfigDisabledWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLocalConfigDisabledWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeReachableWithParams:@
readAttributeReachableWithParamsSelector :: Selector
readAttributeReachableWithParamsSelector = mkSelector "readAttributeReachableWithParams:"

-- | @Selector@ for @readAttributeUniqueIDWithParams:@
readAttributeUniqueIDWithParamsSelector :: Selector
readAttributeUniqueIDWithParamsSelector = mkSelector "readAttributeUniqueIDWithParams:"

-- | @Selector@ for @readAttributeCapabilityMinimaWithParams:@
readAttributeCapabilityMinimaWithParamsSelector :: Selector
readAttributeCapabilityMinimaWithParamsSelector = mkSelector "readAttributeCapabilityMinimaWithParams:"

-- | @Selector@ for @readAttributeProductAppearanceWithParams:@
readAttributeProductAppearanceWithParamsSelector :: Selector
readAttributeProductAppearanceWithParamsSelector = mkSelector "readAttributeProductAppearanceWithParams:"

-- | @Selector@ for @readAttributeSpecificationVersionWithParams:@
readAttributeSpecificationVersionWithParamsSelector :: Selector
readAttributeSpecificationVersionWithParamsSelector = mkSelector "readAttributeSpecificationVersionWithParams:"

-- | @Selector@ for @readAttributeMaxPathsPerInvokeWithParams:@
readAttributeMaxPathsPerInvokeWithParamsSelector :: Selector
readAttributeMaxPathsPerInvokeWithParamsSelector = mkSelector "readAttributeMaxPathsPerInvokeWithParams:"

-- | @Selector@ for @readAttributeConfigurationVersionWithParams:@
readAttributeConfigurationVersionWithParamsSelector :: Selector
readAttributeConfigurationVersionWithParamsSelector = mkSelector "readAttributeConfigurationVersionWithParams:"

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

