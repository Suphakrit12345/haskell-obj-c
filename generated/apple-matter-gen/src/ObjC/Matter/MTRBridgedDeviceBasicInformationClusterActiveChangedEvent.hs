{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBridgedDeviceBasicInformationClusterActiveChangedEvent@.
module ObjC.Matter.MTRBridgedDeviceBasicInformationClusterActiveChangedEvent
  ( MTRBridgedDeviceBasicInformationClusterActiveChangedEvent
  , IsMTRBridgedDeviceBasicInformationClusterActiveChangedEvent(..)
  , promisedActiveDuration
  , setPromisedActiveDuration
  , promisedActiveDurationSelector
  , setPromisedActiveDurationSelector


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

-- | @- promisedActiveDuration@
promisedActiveDuration :: IsMTRBridgedDeviceBasicInformationClusterActiveChangedEvent mtrBridgedDeviceBasicInformationClusterActiveChangedEvent => mtrBridgedDeviceBasicInformationClusterActiveChangedEvent -> IO (Id NSNumber)
promisedActiveDuration mtrBridgedDeviceBasicInformationClusterActiveChangedEvent  =
    sendMsg mtrBridgedDeviceBasicInformationClusterActiveChangedEvent (mkSelector "promisedActiveDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPromisedActiveDuration:@
setPromisedActiveDuration :: (IsMTRBridgedDeviceBasicInformationClusterActiveChangedEvent mtrBridgedDeviceBasicInformationClusterActiveChangedEvent, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterActiveChangedEvent -> value -> IO ()
setPromisedActiveDuration mtrBridgedDeviceBasicInformationClusterActiveChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBridgedDeviceBasicInformationClusterActiveChangedEvent (mkSelector "setPromisedActiveDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @promisedActiveDuration@
promisedActiveDurationSelector :: Selector
promisedActiveDurationSelector = mkSelector "promisedActiveDuration"

-- | @Selector@ for @setPromisedActiveDuration:@
setPromisedActiveDurationSelector :: Selector
setPromisedActiveDurationSelector = mkSelector "setPromisedActiveDuration:"

