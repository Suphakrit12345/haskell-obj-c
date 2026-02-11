{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent@.
module ObjC.Matter.MTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent
  ( MTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent
  , IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent(..)
  , energyImported
  , setEnergyImported
  , energyExported
  , setEnergyExported
  , energyImportedSelector
  , setEnergyImportedSelector
  , energyExportedSelector
  , setEnergyExportedSelector


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

-- | @- energyImported@
energyImported :: IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent => mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent -> IO (Id MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct)
energyImported mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent  =
    sendMsg mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent (mkSelector "energyImported") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnergyImported:@
setEnergyImported :: (IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent, IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct value) => mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent -> value -> IO ()
setEnergyImported mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent (mkSelector "setEnergyImported:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- energyExported@
energyExported :: IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent => mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent -> IO (Id MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct)
energyExported mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent  =
    sendMsg mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent (mkSelector "energyExported") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnergyExported:@
setEnergyExported :: (IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent, IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct value) => mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent -> value -> IO ()
setEnergyExported mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterCumulativeEnergyMeasuredEvent (mkSelector "setEnergyExported:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @energyImported@
energyImportedSelector :: Selector
energyImportedSelector = mkSelector "energyImported"

-- | @Selector@ for @setEnergyImported:@
setEnergyImportedSelector :: Selector
setEnergyImportedSelector = mkSelector "setEnergyImported:"

-- | @Selector@ for @energyExported@
energyExportedSelector :: Selector
energyExportedSelector = mkSelector "energyExported"

-- | @Selector@ for @setEnergyExported:@
setEnergyExportedSelector :: Selector
setEnergyExportedSelector = mkSelector "setEnergyExported:"

