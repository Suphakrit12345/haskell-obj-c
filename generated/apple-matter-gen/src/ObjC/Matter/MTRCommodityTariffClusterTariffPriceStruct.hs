{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterTariffPriceStruct@.
module ObjC.Matter.MTRCommodityTariffClusterTariffPriceStruct
  ( MTRCommodityTariffClusterTariffPriceStruct
  , IsMTRCommodityTariffClusterTariffPriceStruct(..)
  , priceType
  , setPriceType
  , price
  , setPrice
  , priceLevel
  , setPriceLevel
  , priceTypeSelector
  , setPriceTypeSelector
  , priceSelector
  , setPriceSelector
  , priceLevelSelector
  , setPriceLevelSelector


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

-- | @- priceType@
priceType :: IsMTRCommodityTariffClusterTariffPriceStruct mtrCommodityTariffClusterTariffPriceStruct => mtrCommodityTariffClusterTariffPriceStruct -> IO (Id NSNumber)
priceType mtrCommodityTariffClusterTariffPriceStruct  =
    sendMsg mtrCommodityTariffClusterTariffPriceStruct (mkSelector "priceType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPriceType:@
setPriceType :: (IsMTRCommodityTariffClusterTariffPriceStruct mtrCommodityTariffClusterTariffPriceStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffPriceStruct -> value -> IO ()
setPriceType mtrCommodityTariffClusterTariffPriceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffPriceStruct (mkSelector "setPriceType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- price@
price :: IsMTRCommodityTariffClusterTariffPriceStruct mtrCommodityTariffClusterTariffPriceStruct => mtrCommodityTariffClusterTariffPriceStruct -> IO (Id NSNumber)
price mtrCommodityTariffClusterTariffPriceStruct  =
    sendMsg mtrCommodityTariffClusterTariffPriceStruct (mkSelector "price") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrice:@
setPrice :: (IsMTRCommodityTariffClusterTariffPriceStruct mtrCommodityTariffClusterTariffPriceStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffPriceStruct -> value -> IO ()
setPrice mtrCommodityTariffClusterTariffPriceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffPriceStruct (mkSelector "setPrice:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- priceLevel@
priceLevel :: IsMTRCommodityTariffClusterTariffPriceStruct mtrCommodityTariffClusterTariffPriceStruct => mtrCommodityTariffClusterTariffPriceStruct -> IO (Id NSNumber)
priceLevel mtrCommodityTariffClusterTariffPriceStruct  =
    sendMsg mtrCommodityTariffClusterTariffPriceStruct (mkSelector "priceLevel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPriceLevel:@
setPriceLevel :: (IsMTRCommodityTariffClusterTariffPriceStruct mtrCommodityTariffClusterTariffPriceStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffPriceStruct -> value -> IO ()
setPriceLevel mtrCommodityTariffClusterTariffPriceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffPriceStruct (mkSelector "setPriceLevel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @priceType@
priceTypeSelector :: Selector
priceTypeSelector = mkSelector "priceType"

-- | @Selector@ for @setPriceType:@
setPriceTypeSelector :: Selector
setPriceTypeSelector = mkSelector "setPriceType:"

-- | @Selector@ for @price@
priceSelector :: Selector
priceSelector = mkSelector "price"

-- | @Selector@ for @setPrice:@
setPriceSelector :: Selector
setPriceSelector = mkSelector "setPrice:"

-- | @Selector@ for @priceLevel@
priceLevelSelector :: Selector
priceLevelSelector = mkSelector "priceLevel"

-- | @Selector@ for @setPriceLevel:@
setPriceLevelSelector :: Selector
setPriceLevelSelector = mkSelector "setPriceLevel:"

