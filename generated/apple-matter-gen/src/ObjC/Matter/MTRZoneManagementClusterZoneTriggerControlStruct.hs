{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterZoneTriggerControlStruct@.
module ObjC.Matter.MTRZoneManagementClusterZoneTriggerControlStruct
  ( MTRZoneManagementClusterZoneTriggerControlStruct
  , IsMTRZoneManagementClusterZoneTriggerControlStruct(..)
  , zoneID
  , setZoneID
  , initialDuration
  , setInitialDuration
  , augmentationDuration
  , setAugmentationDuration
  , maxDuration
  , setMaxDuration
  , blindDuration
  , setBlindDuration
  , sensitivity
  , setSensitivity
  , zoneIDSelector
  , setZoneIDSelector
  , initialDurationSelector
  , setInitialDurationSelector
  , augmentationDurationSelector
  , setAugmentationDurationSelector
  , maxDurationSelector
  , setMaxDurationSelector
  , blindDurationSelector
  , setBlindDurationSelector
  , sensitivitySelector
  , setSensitivitySelector


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

-- | @- zoneID@
zoneID :: IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct => mtrZoneManagementClusterZoneTriggerControlStruct -> IO (Id NSNumber)
zoneID mtrZoneManagementClusterZoneTriggerControlStruct  =
    sendMsg mtrZoneManagementClusterZoneTriggerControlStruct (mkSelector "zoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZoneID:@
setZoneID :: (IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct, IsNSNumber value) => mtrZoneManagementClusterZoneTriggerControlStruct -> value -> IO ()
setZoneID mtrZoneManagementClusterZoneTriggerControlStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneTriggerControlStruct (mkSelector "setZoneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- initialDuration@
initialDuration :: IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct => mtrZoneManagementClusterZoneTriggerControlStruct -> IO (Id NSNumber)
initialDuration mtrZoneManagementClusterZoneTriggerControlStruct  =
    sendMsg mtrZoneManagementClusterZoneTriggerControlStruct (mkSelector "initialDuration") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setInitialDuration:@
setInitialDuration :: (IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct, IsNSNumber value) => mtrZoneManagementClusterZoneTriggerControlStruct -> value -> IO ()
setInitialDuration mtrZoneManagementClusterZoneTriggerControlStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneTriggerControlStruct (mkSelector "setInitialDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- augmentationDuration@
augmentationDuration :: IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct => mtrZoneManagementClusterZoneTriggerControlStruct -> IO (Id NSNumber)
augmentationDuration mtrZoneManagementClusterZoneTriggerControlStruct  =
    sendMsg mtrZoneManagementClusterZoneTriggerControlStruct (mkSelector "augmentationDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAugmentationDuration:@
setAugmentationDuration :: (IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct, IsNSNumber value) => mtrZoneManagementClusterZoneTriggerControlStruct -> value -> IO ()
setAugmentationDuration mtrZoneManagementClusterZoneTriggerControlStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneTriggerControlStruct (mkSelector "setAugmentationDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxDuration@
maxDuration :: IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct => mtrZoneManagementClusterZoneTriggerControlStruct -> IO (Id NSNumber)
maxDuration mtrZoneManagementClusterZoneTriggerControlStruct  =
    sendMsg mtrZoneManagementClusterZoneTriggerControlStruct (mkSelector "maxDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxDuration:@
setMaxDuration :: (IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct, IsNSNumber value) => mtrZoneManagementClusterZoneTriggerControlStruct -> value -> IO ()
setMaxDuration mtrZoneManagementClusterZoneTriggerControlStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneTriggerControlStruct (mkSelector "setMaxDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- blindDuration@
blindDuration :: IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct => mtrZoneManagementClusterZoneTriggerControlStruct -> IO (Id NSNumber)
blindDuration mtrZoneManagementClusterZoneTriggerControlStruct  =
    sendMsg mtrZoneManagementClusterZoneTriggerControlStruct (mkSelector "blindDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBlindDuration:@
setBlindDuration :: (IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct, IsNSNumber value) => mtrZoneManagementClusterZoneTriggerControlStruct -> value -> IO ()
setBlindDuration mtrZoneManagementClusterZoneTriggerControlStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneTriggerControlStruct (mkSelector "setBlindDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sensitivity@
sensitivity :: IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct => mtrZoneManagementClusterZoneTriggerControlStruct -> IO (Id NSNumber)
sensitivity mtrZoneManagementClusterZoneTriggerControlStruct  =
    sendMsg mtrZoneManagementClusterZoneTriggerControlStruct (mkSelector "sensitivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSensitivity:@
setSensitivity :: (IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct, IsNSNumber value) => mtrZoneManagementClusterZoneTriggerControlStruct -> value -> IO ()
setSensitivity mtrZoneManagementClusterZoneTriggerControlStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneTriggerControlStruct (mkSelector "setSensitivity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @setZoneID:@
setZoneIDSelector :: Selector
setZoneIDSelector = mkSelector "setZoneID:"

-- | @Selector@ for @initialDuration@
initialDurationSelector :: Selector
initialDurationSelector = mkSelector "initialDuration"

-- | @Selector@ for @setInitialDuration:@
setInitialDurationSelector :: Selector
setInitialDurationSelector = mkSelector "setInitialDuration:"

-- | @Selector@ for @augmentationDuration@
augmentationDurationSelector :: Selector
augmentationDurationSelector = mkSelector "augmentationDuration"

-- | @Selector@ for @setAugmentationDuration:@
setAugmentationDurationSelector :: Selector
setAugmentationDurationSelector = mkSelector "setAugmentationDuration:"

-- | @Selector@ for @maxDuration@
maxDurationSelector :: Selector
maxDurationSelector = mkSelector "maxDuration"

-- | @Selector@ for @setMaxDuration:@
setMaxDurationSelector :: Selector
setMaxDurationSelector = mkSelector "setMaxDuration:"

-- | @Selector@ for @blindDuration@
blindDurationSelector :: Selector
blindDurationSelector = mkSelector "blindDuration"

-- | @Selector@ for @setBlindDuration:@
setBlindDurationSelector :: Selector
setBlindDurationSelector = mkSelector "setBlindDuration:"

-- | @Selector@ for @sensitivity@
sensitivitySelector :: Selector
sensitivitySelector = mkSelector "sensitivity"

-- | @Selector@ for @setSensitivity:@
setSensitivitySelector :: Selector
setSensitivitySelector = mkSelector "setSensitivity:"

