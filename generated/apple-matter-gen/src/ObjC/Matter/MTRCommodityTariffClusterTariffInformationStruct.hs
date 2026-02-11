{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterTariffInformationStruct@.
module ObjC.Matter.MTRCommodityTariffClusterTariffInformationStruct
  ( MTRCommodityTariffClusterTariffInformationStruct
  , IsMTRCommodityTariffClusterTariffInformationStruct(..)
  , tariffLabel
  , setTariffLabel
  , providerName
  , setProviderName
  , currency
  , setCurrency
  , blockMode
  , setBlockMode
  , tariffLabelSelector
  , setTariffLabelSelector
  , providerNameSelector
  , setProviderNameSelector
  , currencySelector
  , setCurrencySelector
  , blockModeSelector
  , setBlockModeSelector


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

-- | @- tariffLabel@
tariffLabel :: IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct => mtrCommodityTariffClusterTariffInformationStruct -> IO (Id NSString)
tariffLabel mtrCommodityTariffClusterTariffInformationStruct  =
    sendMsg mtrCommodityTariffClusterTariffInformationStruct (mkSelector "tariffLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTariffLabel:@
setTariffLabel :: (IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct, IsNSString value) => mtrCommodityTariffClusterTariffInformationStruct -> value -> IO ()
setTariffLabel mtrCommodityTariffClusterTariffInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffInformationStruct (mkSelector "setTariffLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- providerName@
providerName :: IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct => mtrCommodityTariffClusterTariffInformationStruct -> IO (Id NSString)
providerName mtrCommodityTariffClusterTariffInformationStruct  =
    sendMsg mtrCommodityTariffClusterTariffInformationStruct (mkSelector "providerName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProviderName:@
setProviderName :: (IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct, IsNSString value) => mtrCommodityTariffClusterTariffInformationStruct -> value -> IO ()
setProviderName mtrCommodityTariffClusterTariffInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffInformationStruct (mkSelector "setProviderName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currency@
currency :: IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct => mtrCommodityTariffClusterTariffInformationStruct -> IO (Id MTRDataTypeCurrencyStruct)
currency mtrCommodityTariffClusterTariffInformationStruct  =
    sendMsg mtrCommodityTariffClusterTariffInformationStruct (mkSelector "currency") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrency:@
setCurrency :: (IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct, IsMTRDataTypeCurrencyStruct value) => mtrCommodityTariffClusterTariffInformationStruct -> value -> IO ()
setCurrency mtrCommodityTariffClusterTariffInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffInformationStruct (mkSelector "setCurrency:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- blockMode@
blockMode :: IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct => mtrCommodityTariffClusterTariffInformationStruct -> IO (Id NSNumber)
blockMode mtrCommodityTariffClusterTariffInformationStruct  =
    sendMsg mtrCommodityTariffClusterTariffInformationStruct (mkSelector "blockMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBlockMode:@
setBlockMode :: (IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffInformationStruct -> value -> IO ()
setBlockMode mtrCommodityTariffClusterTariffInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffInformationStruct (mkSelector "setBlockMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tariffLabel@
tariffLabelSelector :: Selector
tariffLabelSelector = mkSelector "tariffLabel"

-- | @Selector@ for @setTariffLabel:@
setTariffLabelSelector :: Selector
setTariffLabelSelector = mkSelector "setTariffLabel:"

-- | @Selector@ for @providerName@
providerNameSelector :: Selector
providerNameSelector = mkSelector "providerName"

-- | @Selector@ for @setProviderName:@
setProviderNameSelector :: Selector
setProviderNameSelector = mkSelector "setProviderName:"

-- | @Selector@ for @currency@
currencySelector :: Selector
currencySelector = mkSelector "currency"

-- | @Selector@ for @setCurrency:@
setCurrencySelector :: Selector
setCurrencySelector = mkSelector "setCurrency:"

-- | @Selector@ for @blockMode@
blockModeSelector :: Selector
blockModeSelector = mkSelector "blockMode"

-- | @Selector@ for @setBlockMode:@
setBlockModeSelector :: Selector
setBlockModeSelector = mkSelector "setBlockMode:"

