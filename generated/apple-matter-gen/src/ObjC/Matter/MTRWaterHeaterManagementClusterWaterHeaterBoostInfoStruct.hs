{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct@.
module ObjC.Matter.MTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct
  ( MTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct
  , IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct(..)
  , duration
  , setDuration
  , oneShot
  , setOneShot
  , emergencyBoost
  , setEmergencyBoost
  , temporarySetpoint
  , setTemporarySetpoint
  , targetPercentage
  , setTargetPercentage
  , targetReheat
  , setTargetReheat
  , durationSelector
  , setDurationSelector
  , oneShotSelector
  , setOneShotSelector
  , emergencyBoostSelector
  , setEmergencyBoostSelector
  , temporarySetpointSelector
  , setTemporarySetpointSelector
  , targetPercentageSelector
  , setTargetPercentageSelector
  , targetReheatSelector
  , setTargetReheatSelector


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

-- | @- duration@
duration :: IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> IO (Id NSNumber)
duration mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct  =
    sendMsg mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDuration:@
setDuration :: (IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct, IsNSNumber value) => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> value -> IO ()
setDuration mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct (mkSelector "setDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- oneShot@
oneShot :: IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> IO (Id NSNumber)
oneShot mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct  =
    sendMsg mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct (mkSelector "oneShot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOneShot:@
setOneShot :: (IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct, IsNSNumber value) => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> value -> IO ()
setOneShot mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct (mkSelector "setOneShot:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- emergencyBoost@
emergencyBoost :: IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> IO (Id NSNumber)
emergencyBoost mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct  =
    sendMsg mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct (mkSelector "emergencyBoost") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEmergencyBoost:@
setEmergencyBoost :: (IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct, IsNSNumber value) => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> value -> IO ()
setEmergencyBoost mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct (mkSelector "setEmergencyBoost:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- temporarySetpoint@
temporarySetpoint :: IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> IO (Id NSNumber)
temporarySetpoint mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct  =
    sendMsg mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct (mkSelector "temporarySetpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTemporarySetpoint:@
setTemporarySetpoint :: (IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct, IsNSNumber value) => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> value -> IO ()
setTemporarySetpoint mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct (mkSelector "setTemporarySetpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- targetPercentage@
targetPercentage :: IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> IO (Id NSNumber)
targetPercentage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct  =
    sendMsg mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct (mkSelector "targetPercentage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargetPercentage:@
setTargetPercentage :: (IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct, IsNSNumber value) => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> value -> IO ()
setTargetPercentage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct (mkSelector "setTargetPercentage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- targetReheat@
targetReheat :: IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> IO (Id NSNumber)
targetReheat mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct  =
    sendMsg mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct (mkSelector "targetReheat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargetReheat:@
setTargetReheat :: (IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct, IsNSNumber value) => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> value -> IO ()
setTargetReheat mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct (mkSelector "setTargetReheat:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @oneShot@
oneShotSelector :: Selector
oneShotSelector = mkSelector "oneShot"

-- | @Selector@ for @setOneShot:@
setOneShotSelector :: Selector
setOneShotSelector = mkSelector "setOneShot:"

-- | @Selector@ for @emergencyBoost@
emergencyBoostSelector :: Selector
emergencyBoostSelector = mkSelector "emergencyBoost"

-- | @Selector@ for @setEmergencyBoost:@
setEmergencyBoostSelector :: Selector
setEmergencyBoostSelector = mkSelector "setEmergencyBoost:"

-- | @Selector@ for @temporarySetpoint@
temporarySetpointSelector :: Selector
temporarySetpointSelector = mkSelector "temporarySetpoint"

-- | @Selector@ for @setTemporarySetpoint:@
setTemporarySetpointSelector :: Selector
setTemporarySetpointSelector = mkSelector "setTemporarySetpoint:"

-- | @Selector@ for @targetPercentage@
targetPercentageSelector :: Selector
targetPercentageSelector = mkSelector "targetPercentage"

-- | @Selector@ for @setTargetPercentage:@
setTargetPercentageSelector :: Selector
setTargetPercentageSelector = mkSelector "setTargetPercentage:"

-- | @Selector@ for @targetReheat@
targetReheatSelector :: Selector
targetReheatSelector = mkSelector "targetReheat"

-- | @Selector@ for @setTargetReheat:@
setTargetReheatSelector :: Selector
setTargetReheatSelector = mkSelector "setTargetReheat:"

