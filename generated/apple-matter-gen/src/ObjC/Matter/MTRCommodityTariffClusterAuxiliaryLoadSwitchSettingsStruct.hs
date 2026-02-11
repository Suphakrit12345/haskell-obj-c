{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct@.
module ObjC.Matter.MTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct
  ( MTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct
  , IsMTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct(..)
  , number
  , setNumber
  , requiredState
  , setRequiredState
  , numberSelector
  , setNumberSelector
  , requiredStateSelector
  , setRequiredStateSelector


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

-- | @- number@
number :: IsMTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct => mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct -> IO (Id NSNumber)
number mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct  =
    sendMsg mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct (mkSelector "number") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNumber:@
setNumber :: (IsMTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct, IsNSNumber value) => mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct -> value -> IO ()
setNumber mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct (mkSelector "setNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requiredState@
requiredState :: IsMTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct => mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct -> IO (Id NSNumber)
requiredState mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct  =
    sendMsg mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct (mkSelector "requiredState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequiredState:@
setRequiredState :: (IsMTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct, IsNSNumber value) => mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct -> value -> IO ()
setRequiredState mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct (mkSelector "setRequiredState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @number@
numberSelector :: Selector
numberSelector = mkSelector "number"

-- | @Selector@ for @setNumber:@
setNumberSelector :: Selector
setNumberSelector = mkSelector "setNumber:"

-- | @Selector@ for @requiredState@
requiredStateSelector :: Selector
requiredStateSelector = mkSelector "requiredState"

-- | @Selector@ for @setRequiredState:@
setRequiredStateSelector :: Selector
setRequiredStateSelector = mkSelector "setRequiredState:"

