{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterDayPatternStruct@.
module ObjC.Matter.MTRCommodityTariffClusterDayPatternStruct
  ( MTRCommodityTariffClusterDayPatternStruct
  , IsMTRCommodityTariffClusterDayPatternStruct(..)
  , dayPatternID
  , setDayPatternID
  , daysOfWeek
  , setDaysOfWeek
  , dayEntryIDs
  , setDayEntryIDs
  , dayPatternIDSelector
  , setDayPatternIDSelector
  , daysOfWeekSelector
  , setDaysOfWeekSelector
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

-- | @- dayPatternID@
dayPatternID :: IsMTRCommodityTariffClusterDayPatternStruct mtrCommodityTariffClusterDayPatternStruct => mtrCommodityTariffClusterDayPatternStruct -> IO (Id NSNumber)
dayPatternID mtrCommodityTariffClusterDayPatternStruct  =
    sendMsg mtrCommodityTariffClusterDayPatternStruct (mkSelector "dayPatternID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDayPatternID:@
setDayPatternID :: (IsMTRCommodityTariffClusterDayPatternStruct mtrCommodityTariffClusterDayPatternStruct, IsNSNumber value) => mtrCommodityTariffClusterDayPatternStruct -> value -> IO ()
setDayPatternID mtrCommodityTariffClusterDayPatternStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterDayPatternStruct (mkSelector "setDayPatternID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- daysOfWeek@
daysOfWeek :: IsMTRCommodityTariffClusterDayPatternStruct mtrCommodityTariffClusterDayPatternStruct => mtrCommodityTariffClusterDayPatternStruct -> IO (Id NSNumber)
daysOfWeek mtrCommodityTariffClusterDayPatternStruct  =
    sendMsg mtrCommodityTariffClusterDayPatternStruct (mkSelector "daysOfWeek") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDaysOfWeek:@
setDaysOfWeek :: (IsMTRCommodityTariffClusterDayPatternStruct mtrCommodityTariffClusterDayPatternStruct, IsNSNumber value) => mtrCommodityTariffClusterDayPatternStruct -> value -> IO ()
setDaysOfWeek mtrCommodityTariffClusterDayPatternStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterDayPatternStruct (mkSelector "setDaysOfWeek:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dayEntryIDs@
dayEntryIDs :: IsMTRCommodityTariffClusterDayPatternStruct mtrCommodityTariffClusterDayPatternStruct => mtrCommodityTariffClusterDayPatternStruct -> IO (Id NSArray)
dayEntryIDs mtrCommodityTariffClusterDayPatternStruct  =
    sendMsg mtrCommodityTariffClusterDayPatternStruct (mkSelector "dayEntryIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDayEntryIDs:@
setDayEntryIDs :: (IsMTRCommodityTariffClusterDayPatternStruct mtrCommodityTariffClusterDayPatternStruct, IsNSArray value) => mtrCommodityTariffClusterDayPatternStruct -> value -> IO ()
setDayEntryIDs mtrCommodityTariffClusterDayPatternStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterDayPatternStruct (mkSelector "setDayEntryIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dayPatternID@
dayPatternIDSelector :: Selector
dayPatternIDSelector = mkSelector "dayPatternID"

-- | @Selector@ for @setDayPatternID:@
setDayPatternIDSelector :: Selector
setDayPatternIDSelector = mkSelector "setDayPatternID:"

-- | @Selector@ for @daysOfWeek@
daysOfWeekSelector :: Selector
daysOfWeekSelector = mkSelector "daysOfWeek"

-- | @Selector@ for @setDaysOfWeek:@
setDaysOfWeekSelector :: Selector
setDaysOfWeekSelector = mkSelector "setDaysOfWeek:"

-- | @Selector@ for @dayEntryIDs@
dayEntryIDsSelector :: Selector
dayEntryIDsSelector = mkSelector "dayEntryIDs"

-- | @Selector@ for @setDayEntryIDs:@
setDayEntryIDsSelector :: Selector
setDayEntryIDsSelector = mkSelector "setDayEntryIDs:"

