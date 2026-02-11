{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterActivePresetChangeEvent@.
module ObjC.Matter.MTRThermostatClusterActivePresetChangeEvent
  ( MTRThermostatClusterActivePresetChangeEvent
  , IsMTRThermostatClusterActivePresetChangeEvent(..)
  , previousPresetHandle
  , setPreviousPresetHandle
  , currentPresetHandle
  , setCurrentPresetHandle
  , previousPresetHandleSelector
  , setPreviousPresetHandleSelector
  , currentPresetHandleSelector
  , setCurrentPresetHandleSelector


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

-- | @- previousPresetHandle@
previousPresetHandle :: IsMTRThermostatClusterActivePresetChangeEvent mtrThermostatClusterActivePresetChangeEvent => mtrThermostatClusterActivePresetChangeEvent -> IO (Id NSData)
previousPresetHandle mtrThermostatClusterActivePresetChangeEvent  =
    sendMsg mtrThermostatClusterActivePresetChangeEvent (mkSelector "previousPresetHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousPresetHandle:@
setPreviousPresetHandle :: (IsMTRThermostatClusterActivePresetChangeEvent mtrThermostatClusterActivePresetChangeEvent, IsNSData value) => mtrThermostatClusterActivePresetChangeEvent -> value -> IO ()
setPreviousPresetHandle mtrThermostatClusterActivePresetChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterActivePresetChangeEvent (mkSelector "setPreviousPresetHandle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currentPresetHandle@
currentPresetHandle :: IsMTRThermostatClusterActivePresetChangeEvent mtrThermostatClusterActivePresetChangeEvent => mtrThermostatClusterActivePresetChangeEvent -> IO (Id NSData)
currentPresetHandle mtrThermostatClusterActivePresetChangeEvent  =
    sendMsg mtrThermostatClusterActivePresetChangeEvent (mkSelector "currentPresetHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentPresetHandle:@
setCurrentPresetHandle :: (IsMTRThermostatClusterActivePresetChangeEvent mtrThermostatClusterActivePresetChangeEvent, IsNSData value) => mtrThermostatClusterActivePresetChangeEvent -> value -> IO ()
setCurrentPresetHandle mtrThermostatClusterActivePresetChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterActivePresetChangeEvent (mkSelector "setCurrentPresetHandle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousPresetHandle@
previousPresetHandleSelector :: Selector
previousPresetHandleSelector = mkSelector "previousPresetHandle"

-- | @Selector@ for @setPreviousPresetHandle:@
setPreviousPresetHandleSelector :: Selector
setPreviousPresetHandleSelector = mkSelector "setPreviousPresetHandle:"

-- | @Selector@ for @currentPresetHandle@
currentPresetHandleSelector :: Selector
currentPresetHandleSelector = mkSelector "currentPresetHandle"

-- | @Selector@ for @setCurrentPresetHandle:@
setCurrentPresetHandleSelector :: Selector
setCurrentPresetHandleSelector = mkSelector "setCurrentPresetHandle:"

