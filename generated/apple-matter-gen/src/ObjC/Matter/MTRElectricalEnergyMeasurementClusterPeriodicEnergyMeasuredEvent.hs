{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent@.
module ObjC.Matter.MTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent
  ( MTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent
  , IsMTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent(..)
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
energyImported :: IsMTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent => mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent -> IO (Id MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct)
energyImported mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent  =
    sendMsg mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent (mkSelector "energyImported") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnergyImported:@
setEnergyImported :: (IsMTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent, IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct value) => mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent -> value -> IO ()
setEnergyImported mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent (mkSelector "setEnergyImported:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- energyExported@
energyExported :: IsMTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent => mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent -> IO (Id MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct)
energyExported mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent  =
    sendMsg mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent (mkSelector "energyExported") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnergyExported:@
setEnergyExported :: (IsMTRElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent, IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct value) => mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent -> value -> IO ()
setEnergyExported mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterPeriodicEnergyMeasuredEvent (mkSelector "setEnergyExported:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

