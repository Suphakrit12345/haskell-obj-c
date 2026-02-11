{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterThermostatSuggestionStruct@.
module ObjC.Matter.MTRThermostatClusterThermostatSuggestionStruct
  ( MTRThermostatClusterThermostatSuggestionStruct
  , IsMTRThermostatClusterThermostatSuggestionStruct(..)
  , uniqueID
  , setUniqueID
  , presetHandle
  , setPresetHandle
  , effectiveTime
  , setEffectiveTime
  , expirationTime
  , setExpirationTime
  , uniqueIDSelector
  , setUniqueIDSelector
  , presetHandleSelector
  , setPresetHandleSelector
  , effectiveTimeSelector
  , setEffectiveTimeSelector
  , expirationTimeSelector
  , setExpirationTimeSelector


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

-- | @- uniqueID@
uniqueID :: IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct => mtrThermostatClusterThermostatSuggestionStruct -> IO (Id NSNumber)
uniqueID mtrThermostatClusterThermostatSuggestionStruct  =
    sendMsg mtrThermostatClusterThermostatSuggestionStruct (mkSelector "uniqueID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUniqueID:@
setUniqueID :: (IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct, IsNSNumber value) => mtrThermostatClusterThermostatSuggestionStruct -> value -> IO ()
setUniqueID mtrThermostatClusterThermostatSuggestionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterThermostatSuggestionStruct (mkSelector "setUniqueID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- presetHandle@
presetHandle :: IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct => mtrThermostatClusterThermostatSuggestionStruct -> IO (Id NSData)
presetHandle mtrThermostatClusterThermostatSuggestionStruct  =
    sendMsg mtrThermostatClusterThermostatSuggestionStruct (mkSelector "presetHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPresetHandle:@
setPresetHandle :: (IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct, IsNSData value) => mtrThermostatClusterThermostatSuggestionStruct -> value -> IO ()
setPresetHandle mtrThermostatClusterThermostatSuggestionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterThermostatSuggestionStruct (mkSelector "setPresetHandle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- effectiveTime@
effectiveTime :: IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct => mtrThermostatClusterThermostatSuggestionStruct -> IO (Id NSNumber)
effectiveTime mtrThermostatClusterThermostatSuggestionStruct  =
    sendMsg mtrThermostatClusterThermostatSuggestionStruct (mkSelector "effectiveTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEffectiveTime:@
setEffectiveTime :: (IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct, IsNSNumber value) => mtrThermostatClusterThermostatSuggestionStruct -> value -> IO ()
setEffectiveTime mtrThermostatClusterThermostatSuggestionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterThermostatSuggestionStruct (mkSelector "setEffectiveTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- expirationTime@
expirationTime :: IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct => mtrThermostatClusterThermostatSuggestionStruct -> IO (Id NSNumber)
expirationTime mtrThermostatClusterThermostatSuggestionStruct  =
    sendMsg mtrThermostatClusterThermostatSuggestionStruct (mkSelector "expirationTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExpirationTime:@
setExpirationTime :: (IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct, IsNSNumber value) => mtrThermostatClusterThermostatSuggestionStruct -> value -> IO ()
setExpirationTime mtrThermostatClusterThermostatSuggestionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterThermostatSuggestionStruct (mkSelector "setExpirationTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector
uniqueIDSelector = mkSelector "uniqueID"

-- | @Selector@ for @setUniqueID:@
setUniqueIDSelector :: Selector
setUniqueIDSelector = mkSelector "setUniqueID:"

-- | @Selector@ for @presetHandle@
presetHandleSelector :: Selector
presetHandleSelector = mkSelector "presetHandle"

-- | @Selector@ for @setPresetHandle:@
setPresetHandleSelector :: Selector
setPresetHandleSelector = mkSelector "setPresetHandle:"

-- | @Selector@ for @effectiveTime@
effectiveTimeSelector :: Selector
effectiveTimeSelector = mkSelector "effectiveTime"

-- | @Selector@ for @setEffectiveTime:@
setEffectiveTimeSelector :: Selector
setEffectiveTimeSelector = mkSelector "setEffectiveTime:"

-- | @Selector@ for @expirationTime@
expirationTimeSelector :: Selector
expirationTimeSelector = mkSelector "expirationTime"

-- | @Selector@ for @setExpirationTime:@
setExpirationTimeSelector :: Selector
setExpirationTimeSelector = mkSelector "setExpirationTime:"

