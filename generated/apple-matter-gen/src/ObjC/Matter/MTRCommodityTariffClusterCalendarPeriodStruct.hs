{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterCalendarPeriodStruct@.
module ObjC.Matter.MTRCommodityTariffClusterCalendarPeriodStruct
  ( MTRCommodityTariffClusterCalendarPeriodStruct
  , IsMTRCommodityTariffClusterCalendarPeriodStruct(..)
  , startDate
  , setStartDate
  , dayPatternIDs
  , setDayPatternIDs
  , startDateSelector
  , setStartDateSelector
  , dayPatternIDsSelector
  , setDayPatternIDsSelector


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

-- | @- startDate@
startDate :: IsMTRCommodityTariffClusterCalendarPeriodStruct mtrCommodityTariffClusterCalendarPeriodStruct => mtrCommodityTariffClusterCalendarPeriodStruct -> IO (Id NSNumber)
startDate mtrCommodityTariffClusterCalendarPeriodStruct  =
    sendMsg mtrCommodityTariffClusterCalendarPeriodStruct (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartDate:@
setStartDate :: (IsMTRCommodityTariffClusterCalendarPeriodStruct mtrCommodityTariffClusterCalendarPeriodStruct, IsNSNumber value) => mtrCommodityTariffClusterCalendarPeriodStruct -> value -> IO ()
setStartDate mtrCommodityTariffClusterCalendarPeriodStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterCalendarPeriodStruct (mkSelector "setStartDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dayPatternIDs@
dayPatternIDs :: IsMTRCommodityTariffClusterCalendarPeriodStruct mtrCommodityTariffClusterCalendarPeriodStruct => mtrCommodityTariffClusterCalendarPeriodStruct -> IO (Id NSArray)
dayPatternIDs mtrCommodityTariffClusterCalendarPeriodStruct  =
    sendMsg mtrCommodityTariffClusterCalendarPeriodStruct (mkSelector "dayPatternIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDayPatternIDs:@
setDayPatternIDs :: (IsMTRCommodityTariffClusterCalendarPeriodStruct mtrCommodityTariffClusterCalendarPeriodStruct, IsNSArray value) => mtrCommodityTariffClusterCalendarPeriodStruct -> value -> IO ()
setDayPatternIDs mtrCommodityTariffClusterCalendarPeriodStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterCalendarPeriodStruct (mkSelector "setDayPatternIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @setStartDate:@
setStartDateSelector :: Selector
setStartDateSelector = mkSelector "setStartDate:"

-- | @Selector@ for @dayPatternIDs@
dayPatternIDsSelector :: Selector
dayPatternIDsSelector = mkSelector "dayPatternIDs"

-- | @Selector@ for @setDayPatternIDs:@
setDayPatternIDsSelector :: Selector
setDayPatternIDsSelector = mkSelector "setDayPatternIDs:"

