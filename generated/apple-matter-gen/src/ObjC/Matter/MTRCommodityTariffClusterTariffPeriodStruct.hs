{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterTariffPeriodStruct@.
module ObjC.Matter.MTRCommodityTariffClusterTariffPeriodStruct
  ( MTRCommodityTariffClusterTariffPeriodStruct
  , IsMTRCommodityTariffClusterTariffPeriodStruct(..)
  , label
  , setLabel
  , dayEntryIDs
  , setDayEntryIDs
  , tariffComponentIDs
  , setTariffComponentIDs
  , labelSelector
  , setLabelSelector
  , dayEntryIDsSelector
  , setDayEntryIDsSelector
  , tariffComponentIDsSelector
  , setTariffComponentIDsSelector


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

-- | @- label@
label :: IsMTRCommodityTariffClusterTariffPeriodStruct mtrCommodityTariffClusterTariffPeriodStruct => mtrCommodityTariffClusterTariffPeriodStruct -> IO (Id NSString)
label mtrCommodityTariffClusterTariffPeriodStruct  =
    sendMsg mtrCommodityTariffClusterTariffPeriodStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRCommodityTariffClusterTariffPeriodStruct mtrCommodityTariffClusterTariffPeriodStruct, IsNSString value) => mtrCommodityTariffClusterTariffPeriodStruct -> value -> IO ()
setLabel mtrCommodityTariffClusterTariffPeriodStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffPeriodStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dayEntryIDs@
dayEntryIDs :: IsMTRCommodityTariffClusterTariffPeriodStruct mtrCommodityTariffClusterTariffPeriodStruct => mtrCommodityTariffClusterTariffPeriodStruct -> IO (Id NSArray)
dayEntryIDs mtrCommodityTariffClusterTariffPeriodStruct  =
    sendMsg mtrCommodityTariffClusterTariffPeriodStruct (mkSelector "dayEntryIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDayEntryIDs:@
setDayEntryIDs :: (IsMTRCommodityTariffClusterTariffPeriodStruct mtrCommodityTariffClusterTariffPeriodStruct, IsNSArray value) => mtrCommodityTariffClusterTariffPeriodStruct -> value -> IO ()
setDayEntryIDs mtrCommodityTariffClusterTariffPeriodStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffPeriodStruct (mkSelector "setDayEntryIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tariffComponentIDs@
tariffComponentIDs :: IsMTRCommodityTariffClusterTariffPeriodStruct mtrCommodityTariffClusterTariffPeriodStruct => mtrCommodityTariffClusterTariffPeriodStruct -> IO (Id NSArray)
tariffComponentIDs mtrCommodityTariffClusterTariffPeriodStruct  =
    sendMsg mtrCommodityTariffClusterTariffPeriodStruct (mkSelector "tariffComponentIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTariffComponentIDs:@
setTariffComponentIDs :: (IsMTRCommodityTariffClusterTariffPeriodStruct mtrCommodityTariffClusterTariffPeriodStruct, IsNSArray value) => mtrCommodityTariffClusterTariffPeriodStruct -> value -> IO ()
setTariffComponentIDs mtrCommodityTariffClusterTariffPeriodStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffPeriodStruct (mkSelector "setTariffComponentIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @dayEntryIDs@
dayEntryIDsSelector :: Selector
dayEntryIDsSelector = mkSelector "dayEntryIDs"

-- | @Selector@ for @setDayEntryIDs:@
setDayEntryIDsSelector :: Selector
setDayEntryIDsSelector = mkSelector "setDayEntryIDs:"

-- | @Selector@ for @tariffComponentIDs@
tariffComponentIDsSelector :: Selector
tariffComponentIDsSelector = mkSelector "tariffComponentIDs"

-- | @Selector@ for @setTariffComponentIDs:@
setTariffComponentIDsSelector :: Selector
setTariffComponentIDsSelector = mkSelector "setTariffComponentIDs:"

