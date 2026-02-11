{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterDayStruct@.
module ObjC.Matter.MTRCommodityTariffClusterDayStruct
  ( MTRCommodityTariffClusterDayStruct
  , IsMTRCommodityTariffClusterDayStruct(..)
  , date
  , setDate
  , dayType
  , setDayType
  , dayEntryIDs
  , setDayEntryIDs
  , dateSelector
  , setDateSelector
  , dayTypeSelector
  , setDayTypeSelector
  , dayEntryIDsSelector
  , setDayEntryIDsSelector


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

-- | @- date@
date :: IsMTRCommodityTariffClusterDayStruct mtrCommodityTariffClusterDayStruct => mtrCommodityTariffClusterDayStruct -> IO (Id NSNumber)
date mtrCommodityTariffClusterDayStruct  =
    sendMsg mtrCommodityTariffClusterDayStruct (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDate:@
setDate :: (IsMTRCommodityTariffClusterDayStruct mtrCommodityTariffClusterDayStruct, IsNSNumber value) => mtrCommodityTariffClusterDayStruct -> value -> IO ()
setDate mtrCommodityTariffClusterDayStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterDayStruct (mkSelector "setDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dayType@
dayType :: IsMTRCommodityTariffClusterDayStruct mtrCommodityTariffClusterDayStruct => mtrCommodityTariffClusterDayStruct -> IO (Id NSNumber)
dayType mtrCommodityTariffClusterDayStruct  =
    sendMsg mtrCommodityTariffClusterDayStruct (mkSelector "dayType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDayType:@
setDayType :: (IsMTRCommodityTariffClusterDayStruct mtrCommodityTariffClusterDayStruct, IsNSNumber value) => mtrCommodityTariffClusterDayStruct -> value -> IO ()
setDayType mtrCommodityTariffClusterDayStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterDayStruct (mkSelector "setDayType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dayEntryIDs@
dayEntryIDs :: IsMTRCommodityTariffClusterDayStruct mtrCommodityTariffClusterDayStruct => mtrCommodityTariffClusterDayStruct -> IO (Id NSArray)
dayEntryIDs mtrCommodityTariffClusterDayStruct  =
    sendMsg mtrCommodityTariffClusterDayStruct (mkSelector "dayEntryIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDayEntryIDs:@
setDayEntryIDs :: (IsMTRCommodityTariffClusterDayStruct mtrCommodityTariffClusterDayStruct, IsNSArray value) => mtrCommodityTariffClusterDayStruct -> value -> IO ()
setDayEntryIDs mtrCommodityTariffClusterDayStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterDayStruct (mkSelector "setDayEntryIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @setDate:@
setDateSelector :: Selector
setDateSelector = mkSelector "setDate:"

-- | @Selector@ for @dayType@
dayTypeSelector :: Selector
dayTypeSelector = mkSelector "dayType"

-- | @Selector@ for @setDayType:@
setDayTypeSelector :: Selector
setDayTypeSelector = mkSelector "setDayType:"

-- | @Selector@ for @dayEntryIDs@
dayEntryIDsSelector :: Selector
dayEntryIDsSelector = mkSelector "dayEntryIDs"

-- | @Selector@ for @setDayEntryIDs:@
setDayEntryIDsSelector :: Selector
setDayEntryIDsSelector = mkSelector "setDayEntryIDs:"

