{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct
  ( MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct
  , IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct(..)
  , initialDuration
  , setInitialDuration
  , augmentationDuration
  , setAugmentationDuration
  , maxDuration
  , setMaxDuration
  , blindDuration
  , setBlindDuration
  , initialDurationSelector
  , setInitialDurationSelector
  , augmentationDurationSelector
  , setAugmentationDurationSelector
  , maxDurationSelector
  , setMaxDurationSelector
  , blindDurationSelector
  , setBlindDurationSelector


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

-- | @- initialDuration@
initialDuration :: IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> IO (Id NSNumber)
initialDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct (mkSelector "initialDuration") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setInitialDuration:@
setInitialDuration :: (IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> value -> IO ()
setInitialDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct (mkSelector "setInitialDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- augmentationDuration@
augmentationDuration :: IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> IO (Id NSNumber)
augmentationDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct (mkSelector "augmentationDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAugmentationDuration:@
setAugmentationDuration :: (IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> value -> IO ()
setAugmentationDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct (mkSelector "setAugmentationDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxDuration@
maxDuration :: IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> IO (Id NSNumber)
maxDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct (mkSelector "maxDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxDuration:@
setMaxDuration :: (IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> value -> IO ()
setMaxDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct (mkSelector "setMaxDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- blindDuration@
blindDuration :: IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> IO (Id NSNumber)
blindDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct (mkSelector "blindDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBlindDuration:@
setBlindDuration :: (IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> value -> IO ()
setBlindDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct (mkSelector "setBlindDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

