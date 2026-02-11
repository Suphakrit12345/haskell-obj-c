{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterTransportTriggerOptionsStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterTransportTriggerOptionsStruct
  ( MTRPushAVStreamTransportClusterTransportTriggerOptionsStruct
  , IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct(..)
  , triggerType
  , setTriggerType
  , motionZones
  , setMotionZones
  , motionSensitivity
  , setMotionSensitivity
  , motionTimeControl
  , setMotionTimeControl
  , maxPreRollLen
  , setMaxPreRollLen
  , triggerTypeSelector
  , setTriggerTypeSelector
  , motionZonesSelector
  , setMotionZonesSelector
  , motionSensitivitySelector
  , setMotionSensitivitySelector
  , motionTimeControlSelector
  , setMotionTimeControlSelector
  , maxPreRollLenSelector
  , setMaxPreRollLenSelector


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

-- | @- triggerType@
triggerType :: IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> IO (Id NSNumber)
triggerType mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct (mkSelector "triggerType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTriggerType:@
setTriggerType :: (IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> value -> IO ()
setTriggerType mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct (mkSelector "setTriggerType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- motionZones@
motionZones :: IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> IO (Id NSArray)
motionZones mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct (mkSelector "motionZones") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMotionZones:@
setMotionZones :: (IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct, IsNSArray value) => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> value -> IO ()
setMotionZones mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct (mkSelector "setMotionZones:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- motionSensitivity@
motionSensitivity :: IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> IO (Id NSNumber)
motionSensitivity mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct (mkSelector "motionSensitivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMotionSensitivity:@
setMotionSensitivity :: (IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> value -> IO ()
setMotionSensitivity mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct (mkSelector "setMotionSensitivity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- motionTimeControl@
motionTimeControl :: IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> IO (Id MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct)
motionTimeControl mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct (mkSelector "motionTimeControl") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMotionTimeControl:@
setMotionTimeControl :: (IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct, IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct value) => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> value -> IO ()
setMotionTimeControl mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct (mkSelector "setMotionTimeControl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxPreRollLen@
maxPreRollLen :: IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> IO (Id NSNumber)
maxPreRollLen mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct (mkSelector "maxPreRollLen") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxPreRollLen:@
setMaxPreRollLen :: (IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> value -> IO ()
setMaxPreRollLen mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct (mkSelector "setMaxPreRollLen:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @triggerType@
triggerTypeSelector :: Selector
triggerTypeSelector = mkSelector "triggerType"

-- | @Selector@ for @setTriggerType:@
setTriggerTypeSelector :: Selector
setTriggerTypeSelector = mkSelector "setTriggerType:"

-- | @Selector@ for @motionZones@
motionZonesSelector :: Selector
motionZonesSelector = mkSelector "motionZones"

-- | @Selector@ for @setMotionZones:@
setMotionZonesSelector :: Selector
setMotionZonesSelector = mkSelector "setMotionZones:"

-- | @Selector@ for @motionSensitivity@
motionSensitivitySelector :: Selector
motionSensitivitySelector = mkSelector "motionSensitivity"

-- | @Selector@ for @setMotionSensitivity:@
setMotionSensitivitySelector :: Selector
setMotionSensitivitySelector = mkSelector "setMotionSensitivity:"

-- | @Selector@ for @motionTimeControl@
motionTimeControlSelector :: Selector
motionTimeControlSelector = mkSelector "motionTimeControl"

-- | @Selector@ for @setMotionTimeControl:@
setMotionTimeControlSelector :: Selector
setMotionTimeControlSelector = mkSelector "setMotionTimeControl:"

-- | @Selector@ for @maxPreRollLen@
maxPreRollLenSelector :: Selector
maxPreRollLenSelector = mkSelector "maxPreRollLen"

-- | @Selector@ for @setMaxPreRollLen:@
setMaxPreRollLenSelector :: Selector
setMaxPreRollLenSelector = mkSelector "setMaxPreRollLen:"

