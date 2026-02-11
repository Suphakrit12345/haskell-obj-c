{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityMeteringClusterMeteredQuantityStruct@.
module ObjC.Matter.MTRCommodityMeteringClusterMeteredQuantityStruct
  ( MTRCommodityMeteringClusterMeteredQuantityStruct
  , IsMTRCommodityMeteringClusterMeteredQuantityStruct(..)
  , tariffComponentIDs
  , setTariffComponentIDs
  , quantity
  , setQuantity
  , tariffComponentIDsSelector
  , setTariffComponentIDsSelector
  , quantitySelector
  , setQuantitySelector


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

-- | @- tariffComponentIDs@
tariffComponentIDs :: IsMTRCommodityMeteringClusterMeteredQuantityStruct mtrCommodityMeteringClusterMeteredQuantityStruct => mtrCommodityMeteringClusterMeteredQuantityStruct -> IO (Id NSArray)
tariffComponentIDs mtrCommodityMeteringClusterMeteredQuantityStruct  =
    sendMsg mtrCommodityMeteringClusterMeteredQuantityStruct (mkSelector "tariffComponentIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTariffComponentIDs:@
setTariffComponentIDs :: (IsMTRCommodityMeteringClusterMeteredQuantityStruct mtrCommodityMeteringClusterMeteredQuantityStruct, IsNSArray value) => mtrCommodityMeteringClusterMeteredQuantityStruct -> value -> IO ()
setTariffComponentIDs mtrCommodityMeteringClusterMeteredQuantityStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityMeteringClusterMeteredQuantityStruct (mkSelector "setTariffComponentIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- quantity@
quantity :: IsMTRCommodityMeteringClusterMeteredQuantityStruct mtrCommodityMeteringClusterMeteredQuantityStruct => mtrCommodityMeteringClusterMeteredQuantityStruct -> IO (Id NSNumber)
quantity mtrCommodityMeteringClusterMeteredQuantityStruct  =
    sendMsg mtrCommodityMeteringClusterMeteredQuantityStruct (mkSelector "quantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setQuantity:@
setQuantity :: (IsMTRCommodityMeteringClusterMeteredQuantityStruct mtrCommodityMeteringClusterMeteredQuantityStruct, IsNSNumber value) => mtrCommodityMeteringClusterMeteredQuantityStruct -> value -> IO ()
setQuantity mtrCommodityMeteringClusterMeteredQuantityStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityMeteringClusterMeteredQuantityStruct (mkSelector "setQuantity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tariffComponentIDs@
tariffComponentIDsSelector :: Selector
tariffComponentIDsSelector = mkSelector "tariffComponentIDs"

-- | @Selector@ for @setTariffComponentIDs:@
setTariffComponentIDsSelector :: Selector
setTariffComponentIDsSelector = mkSelector "setTariffComponentIDs:"

-- | @Selector@ for @quantity@
quantitySelector :: Selector
quantitySelector = mkSelector "quantity"

-- | @Selector@ for @setQuantity:@
setQuantitySelector :: Selector
setQuantitySelector = mkSelector "setQuantity:"

