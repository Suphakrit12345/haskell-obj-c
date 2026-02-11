{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityPriceClusterCommodityPriceComponentStruct@.
module ObjC.Matter.MTRCommodityPriceClusterCommodityPriceComponentStruct
  ( MTRCommodityPriceClusterCommodityPriceComponentStruct
  , IsMTRCommodityPriceClusterCommodityPriceComponentStruct(..)
  , price
  , setPrice
  , source
  , setSource
  , descriptionString
  , setDescriptionString
  , tariffComponentID
  , setTariffComponentID
  , priceSelector
  , setPriceSelector
  , sourceSelector
  , setSourceSelector
  , descriptionStringSelector
  , setDescriptionStringSelector
  , tariffComponentIDSelector
  , setTariffComponentIDSelector


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

-- | @- price@
price :: IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct => mtrCommodityPriceClusterCommodityPriceComponentStruct -> IO (Id NSNumber)
price mtrCommodityPriceClusterCommodityPriceComponentStruct  =
    sendMsg mtrCommodityPriceClusterCommodityPriceComponentStruct (mkSelector "price") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrice:@
setPrice :: (IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceComponentStruct -> value -> IO ()
setPrice mtrCommodityPriceClusterCommodityPriceComponentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityPriceClusterCommodityPriceComponentStruct (mkSelector "setPrice:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- source@
source :: IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct => mtrCommodityPriceClusterCommodityPriceComponentStruct -> IO (Id NSNumber)
source mtrCommodityPriceClusterCommodityPriceComponentStruct  =
    sendMsg mtrCommodityPriceClusterCommodityPriceComponentStruct (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSource:@
setSource :: (IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceComponentStruct -> value -> IO ()
setSource mtrCommodityPriceClusterCommodityPriceComponentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityPriceClusterCommodityPriceComponentStruct (mkSelector "setSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- descriptionString@
descriptionString :: IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct => mtrCommodityPriceClusterCommodityPriceComponentStruct -> IO (Id NSString)
descriptionString mtrCommodityPriceClusterCommodityPriceComponentStruct  =
    sendMsg mtrCommodityPriceClusterCommodityPriceComponentStruct (mkSelector "descriptionString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDescriptionString:@
setDescriptionString :: (IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct, IsNSString value) => mtrCommodityPriceClusterCommodityPriceComponentStruct -> value -> IO ()
setDescriptionString mtrCommodityPriceClusterCommodityPriceComponentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityPriceClusterCommodityPriceComponentStruct (mkSelector "setDescriptionString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tariffComponentID@
tariffComponentID :: IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct => mtrCommodityPriceClusterCommodityPriceComponentStruct -> IO (Id NSNumber)
tariffComponentID mtrCommodityPriceClusterCommodityPriceComponentStruct  =
    sendMsg mtrCommodityPriceClusterCommodityPriceComponentStruct (mkSelector "tariffComponentID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTariffComponentID:@
setTariffComponentID :: (IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceComponentStruct -> value -> IO ()
setTariffComponentID mtrCommodityPriceClusterCommodityPriceComponentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityPriceClusterCommodityPriceComponentStruct (mkSelector "setTariffComponentID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @price@
priceSelector :: Selector
priceSelector = mkSelector "price"

-- | @Selector@ for @setPrice:@
setPriceSelector :: Selector
setPriceSelector = mkSelector "setPrice:"

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector
setSourceSelector = mkSelector "setSource:"

-- | @Selector@ for @descriptionString@
descriptionStringSelector :: Selector
descriptionStringSelector = mkSelector "descriptionString"

-- | @Selector@ for @setDescriptionString:@
setDescriptionStringSelector :: Selector
setDescriptionStringSelector = mkSelector "setDescriptionString:"

-- | @Selector@ for @tariffComponentID@
tariffComponentIDSelector :: Selector
tariffComponentIDSelector = mkSelector "tariffComponentID"

-- | @Selector@ for @setTariffComponentID:@
setTariffComponentIDSelector :: Selector
setTariffComponentIDSelector = mkSelector "setTariffComponentID:"

