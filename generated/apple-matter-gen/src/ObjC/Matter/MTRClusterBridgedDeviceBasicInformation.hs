{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Bridged Device Basic Information    This Cluster serves two purposes towards a Node communicating with a Bridge: indicate that the functionality on          the Endpoint where it is placed (and its Parts) is bridged from a non-CHIP technology; and provide a centralized          collection of attributes that the Node MAY collect to aid in conveying information regarding the Bridged Device to a user,          such as the vendor name, the model name, or user-assigned name.
--
-- Generated bindings for @MTRClusterBridgedDeviceBasicInformation@.
module ObjC.Matter.MTRClusterBridgedDeviceBasicInformation
  ( MTRClusterBridgedDeviceBasicInformation
  , IsMTRClusterBridgedDeviceBasicInformation(..)
  , keepActiveWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeVendorNameWithParams
  , readAttributeVendorIDWithParams
  , readAttributeProductNameWithParams
  , readAttributeProductIDWithParams
  , readAttributeNodeLabelWithParams
  , writeAttributeNodeLabelWithValue_expectedValueInterval
  , writeAttributeNodeLabelWithValue_expectedValueInterval_params
  , readAttributeHardwareVersionWithParams
  , readAttributeHardwareVersionStringWithParams
  , readAttributeSoftwareVersionWithParams
  , readAttributeSoftwareVersionStringWithParams
  , readAttributeManufacturingDateWithParams
  , readAttributePartNumberWithParams
  , readAttributeProductURLWithParams
  , readAttributeProductLabelWithParams
  , readAttributeSerialNumberWithParams
  , readAttributeReachableWithParams
  , readAttributeUniqueIDWithParams
  , readAttributeProductAppearanceWithParams
  , readAttributeConfigurationVersionWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , keepActiveWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeVendorNameWithParamsSelector
  , readAttributeVendorIDWithParamsSelector
  , readAttributeProductNameWithParamsSelector
  , readAttributeProductIDWithParamsSelector
  , readAttributeNodeLabelWithParamsSelector
  , writeAttributeNodeLabelWithValue_expectedValueIntervalSelector
  , writeAttributeNodeLabelWithValue_expectedValueInterval_paramsSelector
  , readAttributeHardwareVersionWithParamsSelector
  , readAttributeHardwareVersionStringWithParamsSelector
  , readAttributeSoftwareVersionWithParamsSelector
  , readAttributeSoftwareVersionStringWithParamsSelector
  , readAttributeManufacturingDateWithParamsSelector
  , readAttributePartNumberWithParamsSelector
  , readAttributeProductURLWithParamsSelector
  , readAttributeProductLabelWithParamsSelector
  , readAttributeSerialNumberWithParamsSelector
  , readAttributeReachableWithParamsSelector
  , readAttributeUniqueIDWithParamsSelector
  , readAttributeProductAppearanceWithParamsSelector
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

-- | @- keepActiveWithParams:expectedValues:expectedValueInterval:completion:@
keepActiveWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBridgedDeviceBasicInformation -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keepActiveWithParams_expectedValues_expectedValueInterval_completion mtrClusterBridgedDeviceBasicInformation  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "keepActiveWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeVendorNameWithParams:@
readAttributeVendorNameWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeVendorNameWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeVendorNameWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeVendorIDWithParams:@
readAttributeVendorIDWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeVendorIDWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeVendorIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProductNameWithParams:@
readAttributeProductNameWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductNameWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeProductNameWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProductIDWithParams:@
readAttributeProductIDWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductIDWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeProductIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNodeLabelWithParams:@
readAttributeNodeLabelWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeNodeLabelWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeNodeLabelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeNodeLabelWithValue:expectedValueInterval:@
writeAttributeNodeLabelWithValue_expectedValueInterval :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBridgedDeviceBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeNodeLabelWithValue_expectedValueInterval mtrClusterBridgedDeviceBasicInformation  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "writeAttributeNodeLabelWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeNodeLabelWithValue:expectedValueInterval:params:@
writeAttributeNodeLabelWithValue_expectedValueInterval_params :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBridgedDeviceBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeNodeLabelWithValue_expectedValueInterval_params mtrClusterBridgedDeviceBasicInformation  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "writeAttributeNodeLabelWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeHardwareVersionWithParams:@
readAttributeHardwareVersionWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeHardwareVersionWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeHardwareVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeHardwareVersionStringWithParams:@
readAttributeHardwareVersionStringWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeHardwareVersionStringWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeHardwareVersionStringWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSoftwareVersionWithParams:@
readAttributeSoftwareVersionWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSoftwareVersionWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeSoftwareVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSoftwareVersionStringWithParams:@
readAttributeSoftwareVersionStringWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSoftwareVersionStringWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeSoftwareVersionStringWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeManufacturingDateWithParams:@
readAttributeManufacturingDateWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeManufacturingDateWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeManufacturingDateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePartNumberWithParams:@
readAttributePartNumberWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributePartNumberWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributePartNumberWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProductURLWithParams:@
readAttributeProductURLWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductURLWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeProductURLWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProductLabelWithParams:@
readAttributeProductLabelWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductLabelWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeProductLabelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSerialNumberWithParams:@
readAttributeSerialNumberWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSerialNumberWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeSerialNumberWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeReachableWithParams:@
readAttributeReachableWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeReachableWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeReachableWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUniqueIDWithParams:@
readAttributeUniqueIDWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeUniqueIDWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeUniqueIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProductAppearanceWithParams:@
readAttributeProductAppearanceWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductAppearanceWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeProductAppearanceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeConfigurationVersionWithParams:@
readAttributeConfigurationVersionWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeConfigurationVersionWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeConfigurationVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBridgedDeviceBasicInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation => mtrClusterBridgedDeviceBasicInformation -> IO (Id MTRClusterBridgedDeviceBasicInformation)
init_ mtrClusterBridgedDeviceBasicInformation  =
    sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterBridgedDeviceBasicInformation)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBridgedDeviceBasicInformation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBridgedDeviceBasicInformation -> device -> endpointID -> queue -> IO (Id MTRClusterBridgedDeviceBasicInformation)
initWithDevice_endpointID_queue mtrClusterBridgedDeviceBasicInformation  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterBridgedDeviceBasicInformation (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keepActiveWithParams:expectedValues:expectedValueInterval:completion:@
keepActiveWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
keepActiveWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "keepActiveWithParams:expectedValues:expectedValueInterval:completion:"

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

-- | @Selector@ for @readAttributeReachableWithParams:@
readAttributeReachableWithParamsSelector :: Selector
readAttributeReachableWithParamsSelector = mkSelector "readAttributeReachableWithParams:"

-- | @Selector@ for @readAttributeUniqueIDWithParams:@
readAttributeUniqueIDWithParamsSelector :: Selector
readAttributeUniqueIDWithParamsSelector = mkSelector "readAttributeUniqueIDWithParams:"

-- | @Selector@ for @readAttributeProductAppearanceWithParams:@
readAttributeProductAppearanceWithParamsSelector :: Selector
readAttributeProductAppearanceWithParamsSelector = mkSelector "readAttributeProductAppearanceWithParams:"

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

