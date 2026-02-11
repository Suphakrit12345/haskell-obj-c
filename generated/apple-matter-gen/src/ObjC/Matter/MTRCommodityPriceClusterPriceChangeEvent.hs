{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityPriceClusterPriceChangeEvent@.
module ObjC.Matter.MTRCommodityPriceClusterPriceChangeEvent
  ( MTRCommodityPriceClusterPriceChangeEvent
  , IsMTRCommodityPriceClusterPriceChangeEvent(..)
  , currentPrice
  , setCurrentPrice
  , currentPriceSelector
  , setCurrentPriceSelector


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

-- | @- currentPrice@
currentPrice :: IsMTRCommodityPriceClusterPriceChangeEvent mtrCommodityPriceClusterPriceChangeEvent => mtrCommodityPriceClusterPriceChangeEvent -> IO (Id MTRCommodityPriceClusterCommodityPriceStruct)
currentPrice mtrCommodityPriceClusterPriceChangeEvent  =
    sendMsg mtrCommodityPriceClusterPriceChangeEvent (mkSelector "currentPrice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentPrice:@
setCurrentPrice :: (IsMTRCommodityPriceClusterPriceChangeEvent mtrCommodityPriceClusterPriceChangeEvent, IsMTRCommodityPriceClusterCommodityPriceStruct value) => mtrCommodityPriceClusterPriceChangeEvent -> value -> IO ()
setCurrentPrice mtrCommodityPriceClusterPriceChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityPriceClusterPriceChangeEvent (mkSelector "setCurrentPrice:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentPrice@
currentPriceSelector :: Selector
currentPriceSelector = mkSelector "currentPrice"

-- | @Selector@ for @setCurrentPrice:@
setCurrentPriceSelector :: Selector
setCurrentPriceSelector = mkSelector "setCurrentPrice:"

