{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterCostStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterCostStruct
  ( MTRDeviceEnergyManagementClusterCostStruct
  , IsMTRDeviceEnergyManagementClusterCostStruct(..)
  , costType
  , setCostType
  , value
  , setValue
  , decimalPoints
  , setDecimalPoints
  , currency
  , setCurrency
  , costTypeSelector
  , setCostTypeSelector
  , valueSelector
  , setValueSelector
  , decimalPointsSelector
  , setDecimalPointsSelector
  , currencySelector
  , setCurrencySelector


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

-- | @- costType@
costType :: IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct => mtrDeviceEnergyManagementClusterCostStruct -> IO (Id NSNumber)
costType mtrDeviceEnergyManagementClusterCostStruct  =
    sendMsg mtrDeviceEnergyManagementClusterCostStruct (mkSelector "costType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCostType:@
setCostType :: (IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterCostStruct -> value -> IO ()
setCostType mtrDeviceEnergyManagementClusterCostStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterCostStruct (mkSelector "setCostType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct => mtrDeviceEnergyManagementClusterCostStruct -> IO (Id NSNumber)
value mtrDeviceEnergyManagementClusterCostStruct  =
    sendMsg mtrDeviceEnergyManagementClusterCostStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterCostStruct -> value -> IO ()
setValue mtrDeviceEnergyManagementClusterCostStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterCostStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- decimalPoints@
decimalPoints :: IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct => mtrDeviceEnergyManagementClusterCostStruct -> IO (Id NSNumber)
decimalPoints mtrDeviceEnergyManagementClusterCostStruct  =
    sendMsg mtrDeviceEnergyManagementClusterCostStruct (mkSelector "decimalPoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDecimalPoints:@
setDecimalPoints :: (IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterCostStruct -> value -> IO ()
setDecimalPoints mtrDeviceEnergyManagementClusterCostStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterCostStruct (mkSelector "setDecimalPoints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currency@
currency :: IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct => mtrDeviceEnergyManagementClusterCostStruct -> IO (Id NSNumber)
currency mtrDeviceEnergyManagementClusterCostStruct  =
    sendMsg mtrDeviceEnergyManagementClusterCostStruct (mkSelector "currency") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrency:@
setCurrency :: (IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterCostStruct -> value -> IO ()
setCurrency mtrDeviceEnergyManagementClusterCostStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterCostStruct (mkSelector "setCurrency:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @costType@
costTypeSelector :: Selector
costTypeSelector = mkSelector "costType"

-- | @Selector@ for @setCostType:@
setCostTypeSelector :: Selector
setCostTypeSelector = mkSelector "setCostType:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @decimalPoints@
decimalPointsSelector :: Selector
decimalPointsSelector = mkSelector "decimalPoints"

-- | @Selector@ for @setDecimalPoints:@
setDecimalPointsSelector :: Selector
setDecimalPointsSelector = mkSelector "setDecimalPoints:"

-- | @Selector@ for @currency@
currencySelector :: Selector
currencySelector = mkSelector "currency"

-- | @Selector@ for @setCurrency:@
setCurrencySelector :: Selector
setCurrencySelector = mkSelector "setCurrency:"

