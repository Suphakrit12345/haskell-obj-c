{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalGridConditionsClusterElectricalGridConditionsStruct@.
module ObjC.Matter.MTRElectricalGridConditionsClusterElectricalGridConditionsStruct
  ( MTRElectricalGridConditionsClusterElectricalGridConditionsStruct
  , IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct(..)
  , periodStart
  , setPeriodStart
  , periodEnd
  , setPeriodEnd
  , gridCarbonIntensity
  , setGridCarbonIntensity
  , gridCarbonLevel
  , setGridCarbonLevel
  , localCarbonIntensity
  , setLocalCarbonIntensity
  , localCarbonLevel
  , setLocalCarbonLevel
  , periodStartSelector
  , setPeriodStartSelector
  , periodEndSelector
  , setPeriodEndSelector
  , gridCarbonIntensitySelector
  , setGridCarbonIntensitySelector
  , gridCarbonLevelSelector
  , setGridCarbonLevelSelector
  , localCarbonIntensitySelector
  , setLocalCarbonIntensitySelector
  , localCarbonLevelSelector
  , setLocalCarbonLevelSelector


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

-- | @- periodStart@
periodStart :: IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> IO (Id NSNumber)
periodStart mtrElectricalGridConditionsClusterElectricalGridConditionsStruct  =
    sendMsg mtrElectricalGridConditionsClusterElectricalGridConditionsStruct (mkSelector "periodStart") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPeriodStart:@
setPeriodStart :: (IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct, IsNSNumber value) => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> value -> IO ()
setPeriodStart mtrElectricalGridConditionsClusterElectricalGridConditionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalGridConditionsClusterElectricalGridConditionsStruct (mkSelector "setPeriodStart:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- periodEnd@
periodEnd :: IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> IO (Id NSNumber)
periodEnd mtrElectricalGridConditionsClusterElectricalGridConditionsStruct  =
    sendMsg mtrElectricalGridConditionsClusterElectricalGridConditionsStruct (mkSelector "periodEnd") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPeriodEnd:@
setPeriodEnd :: (IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct, IsNSNumber value) => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> value -> IO ()
setPeriodEnd mtrElectricalGridConditionsClusterElectricalGridConditionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalGridConditionsClusterElectricalGridConditionsStruct (mkSelector "setPeriodEnd:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- gridCarbonIntensity@
gridCarbonIntensity :: IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> IO (Id NSNumber)
gridCarbonIntensity mtrElectricalGridConditionsClusterElectricalGridConditionsStruct  =
    sendMsg mtrElectricalGridConditionsClusterElectricalGridConditionsStruct (mkSelector "gridCarbonIntensity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGridCarbonIntensity:@
setGridCarbonIntensity :: (IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct, IsNSNumber value) => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> value -> IO ()
setGridCarbonIntensity mtrElectricalGridConditionsClusterElectricalGridConditionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalGridConditionsClusterElectricalGridConditionsStruct (mkSelector "setGridCarbonIntensity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- gridCarbonLevel@
gridCarbonLevel :: IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> IO (Id NSNumber)
gridCarbonLevel mtrElectricalGridConditionsClusterElectricalGridConditionsStruct  =
    sendMsg mtrElectricalGridConditionsClusterElectricalGridConditionsStruct (mkSelector "gridCarbonLevel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGridCarbonLevel:@
setGridCarbonLevel :: (IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct, IsNSNumber value) => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> value -> IO ()
setGridCarbonLevel mtrElectricalGridConditionsClusterElectricalGridConditionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalGridConditionsClusterElectricalGridConditionsStruct (mkSelector "setGridCarbonLevel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localCarbonIntensity@
localCarbonIntensity :: IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> IO (Id NSNumber)
localCarbonIntensity mtrElectricalGridConditionsClusterElectricalGridConditionsStruct  =
    sendMsg mtrElectricalGridConditionsClusterElectricalGridConditionsStruct (mkSelector "localCarbonIntensity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalCarbonIntensity:@
setLocalCarbonIntensity :: (IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct, IsNSNumber value) => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> value -> IO ()
setLocalCarbonIntensity mtrElectricalGridConditionsClusterElectricalGridConditionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalGridConditionsClusterElectricalGridConditionsStruct (mkSelector "setLocalCarbonIntensity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localCarbonLevel@
localCarbonLevel :: IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> IO (Id NSNumber)
localCarbonLevel mtrElectricalGridConditionsClusterElectricalGridConditionsStruct  =
    sendMsg mtrElectricalGridConditionsClusterElectricalGridConditionsStruct (mkSelector "localCarbonLevel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalCarbonLevel:@
setLocalCarbonLevel :: (IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct mtrElectricalGridConditionsClusterElectricalGridConditionsStruct, IsNSNumber value) => mtrElectricalGridConditionsClusterElectricalGridConditionsStruct -> value -> IO ()
setLocalCarbonLevel mtrElectricalGridConditionsClusterElectricalGridConditionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalGridConditionsClusterElectricalGridConditionsStruct (mkSelector "setLocalCarbonLevel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @periodStart@
periodStartSelector :: Selector
periodStartSelector = mkSelector "periodStart"

-- | @Selector@ for @setPeriodStart:@
setPeriodStartSelector :: Selector
setPeriodStartSelector = mkSelector "setPeriodStart:"

-- | @Selector@ for @periodEnd@
periodEndSelector :: Selector
periodEndSelector = mkSelector "periodEnd"

-- | @Selector@ for @setPeriodEnd:@
setPeriodEndSelector :: Selector
setPeriodEndSelector = mkSelector "setPeriodEnd:"

-- | @Selector@ for @gridCarbonIntensity@
gridCarbonIntensitySelector :: Selector
gridCarbonIntensitySelector = mkSelector "gridCarbonIntensity"

-- | @Selector@ for @setGridCarbonIntensity:@
setGridCarbonIntensitySelector :: Selector
setGridCarbonIntensitySelector = mkSelector "setGridCarbonIntensity:"

-- | @Selector@ for @gridCarbonLevel@
gridCarbonLevelSelector :: Selector
gridCarbonLevelSelector = mkSelector "gridCarbonLevel"

-- | @Selector@ for @setGridCarbonLevel:@
setGridCarbonLevelSelector :: Selector
setGridCarbonLevelSelector = mkSelector "setGridCarbonLevel:"

-- | @Selector@ for @localCarbonIntensity@
localCarbonIntensitySelector :: Selector
localCarbonIntensitySelector = mkSelector "localCarbonIntensity"

-- | @Selector@ for @setLocalCarbonIntensity:@
setLocalCarbonIntensitySelector :: Selector
setLocalCarbonIntensitySelector = mkSelector "setLocalCarbonIntensity:"

-- | @Selector@ for @localCarbonLevel@
localCarbonLevelSelector :: Selector
localCarbonLevelSelector = mkSelector "localCarbonLevel"

-- | @Selector@ for @setLocalCarbonLevel:@
setLocalCarbonLevelSelector :: Selector
setLocalCarbonLevelSelector = mkSelector "setLocalCarbonLevel:"

