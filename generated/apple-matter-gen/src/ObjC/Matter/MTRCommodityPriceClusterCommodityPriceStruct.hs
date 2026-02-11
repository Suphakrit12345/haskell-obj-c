{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityPriceClusterCommodityPriceStruct@.
module ObjC.Matter.MTRCommodityPriceClusterCommodityPriceStruct
  ( MTRCommodityPriceClusterCommodityPriceStruct
  , IsMTRCommodityPriceClusterCommodityPriceStruct(..)
  , periodStart
  , setPeriodStart
  , periodEnd
  , setPeriodEnd
  , price
  , setPrice
  , priceLevel
  , setPriceLevel
  , descriptionString
  , setDescriptionString
  , components
  , setComponents
  , periodStartSelector
  , setPeriodStartSelector
  , periodEndSelector
  , setPeriodEndSelector
  , priceSelector
  , setPriceSelector
  , priceLevelSelector
  , setPriceLevelSelector
  , descriptionStringSelector
  , setDescriptionStringSelector
  , componentsSelector
  , setComponentsSelector


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

-- | @- periodStart@
periodStart :: IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct => mtrCommodityPriceClusterCommodityPriceStruct -> IO (Id NSNumber)
periodStart mtrCommodityPriceClusterCommodityPriceStruct  =
    sendMsg mtrCommodityPriceClusterCommodityPriceStruct (mkSelector "periodStart") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPeriodStart:@
setPeriodStart :: (IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceStruct -> value -> IO ()
setPeriodStart mtrCommodityPriceClusterCommodityPriceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityPriceClusterCommodityPriceStruct (mkSelector "setPeriodStart:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- periodEnd@
periodEnd :: IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct => mtrCommodityPriceClusterCommodityPriceStruct -> IO (Id NSNumber)
periodEnd mtrCommodityPriceClusterCommodityPriceStruct  =
    sendMsg mtrCommodityPriceClusterCommodityPriceStruct (mkSelector "periodEnd") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPeriodEnd:@
setPeriodEnd :: (IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceStruct -> value -> IO ()
setPeriodEnd mtrCommodityPriceClusterCommodityPriceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityPriceClusterCommodityPriceStruct (mkSelector "setPeriodEnd:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- price@
price :: IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct => mtrCommodityPriceClusterCommodityPriceStruct -> IO (Id NSNumber)
price mtrCommodityPriceClusterCommodityPriceStruct  =
    sendMsg mtrCommodityPriceClusterCommodityPriceStruct (mkSelector "price") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrice:@
setPrice :: (IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceStruct -> value -> IO ()
setPrice mtrCommodityPriceClusterCommodityPriceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityPriceClusterCommodityPriceStruct (mkSelector "setPrice:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- priceLevel@
priceLevel :: IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct => mtrCommodityPriceClusterCommodityPriceStruct -> IO (Id NSNumber)
priceLevel mtrCommodityPriceClusterCommodityPriceStruct  =
    sendMsg mtrCommodityPriceClusterCommodityPriceStruct (mkSelector "priceLevel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPriceLevel:@
setPriceLevel :: (IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceStruct -> value -> IO ()
setPriceLevel mtrCommodityPriceClusterCommodityPriceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityPriceClusterCommodityPriceStruct (mkSelector "setPriceLevel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- descriptionString@
descriptionString :: IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct => mtrCommodityPriceClusterCommodityPriceStruct -> IO (Id NSString)
descriptionString mtrCommodityPriceClusterCommodityPriceStruct  =
    sendMsg mtrCommodityPriceClusterCommodityPriceStruct (mkSelector "descriptionString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDescriptionString:@
setDescriptionString :: (IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct, IsNSString value) => mtrCommodityPriceClusterCommodityPriceStruct -> value -> IO ()
setDescriptionString mtrCommodityPriceClusterCommodityPriceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityPriceClusterCommodityPriceStruct (mkSelector "setDescriptionString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- components@
components :: IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct => mtrCommodityPriceClusterCommodityPriceStruct -> IO (Id NSArray)
components mtrCommodityPriceClusterCommodityPriceStruct  =
    sendMsg mtrCommodityPriceClusterCommodityPriceStruct (mkSelector "components") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setComponents:@
setComponents :: (IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct, IsNSArray value) => mtrCommodityPriceClusterCommodityPriceStruct -> value -> IO ()
setComponents mtrCommodityPriceClusterCommodityPriceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityPriceClusterCommodityPriceStruct (mkSelector "setComponents:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @periodStart@
periodStartSelector :: Selector
periodStartSelector = mkSelector "periodStart"

-- | @Selector@ for @setPeriodStart:@
setPeriodStartSelector :: Selector
setPeriodStartSelector = mkSelector "setPeriodStart:"

-- | @Selector@ for @periodEnd@
periodEndSelector :: Selector
periodEndSelector = mkSelector "periodEnd"

-- | @Selector@ for @setPeriodEnd:@
setPeriodEndSelector :: Selector
setPeriodEndSelector = mkSelector "setPeriodEnd:"

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

-- | @Selector@ for @descriptionString@
descriptionStringSelector :: Selector
descriptionStringSelector = mkSelector "descriptionString"

-- | @Selector@ for @setDescriptionString:@
setDescriptionStringSelector :: Selector
setDescriptionStringSelector = mkSelector "setDescriptionString:"

-- | @Selector@ for @components@
componentsSelector :: Selector
componentsSelector = mkSelector "components"

-- | @Selector@ for @setComponents:@
setComponentsSelector :: Selector
setComponentsSelector = mkSelector "setComponents:"

