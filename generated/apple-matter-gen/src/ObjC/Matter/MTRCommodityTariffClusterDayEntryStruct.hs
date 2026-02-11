{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterDayEntryStruct@.
module ObjC.Matter.MTRCommodityTariffClusterDayEntryStruct
  ( MTRCommodityTariffClusterDayEntryStruct
  , IsMTRCommodityTariffClusterDayEntryStruct(..)
  , dayEntryID
  , setDayEntryID
  , startTime
  , setStartTime
  , duration
  , setDuration
  , randomizationOffset
  , setRandomizationOffset
  , randomizationType
  , setRandomizationType
  , dayEntryIDSelector
  , setDayEntryIDSelector
  , startTimeSelector
  , setStartTimeSelector
  , durationSelector
  , setDurationSelector
  , randomizationOffsetSelector
  , setRandomizationOffsetSelector
  , randomizationTypeSelector
  , setRandomizationTypeSelector


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

-- | @- dayEntryID@
dayEntryID :: IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct => mtrCommodityTariffClusterDayEntryStruct -> IO (Id NSNumber)
dayEntryID mtrCommodityTariffClusterDayEntryStruct  =
    sendMsg mtrCommodityTariffClusterDayEntryStruct (mkSelector "dayEntryID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDayEntryID:@
setDayEntryID :: (IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct, IsNSNumber value) => mtrCommodityTariffClusterDayEntryStruct -> value -> IO ()
setDayEntryID mtrCommodityTariffClusterDayEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterDayEntryStruct (mkSelector "setDayEntryID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startTime@
startTime :: IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct => mtrCommodityTariffClusterDayEntryStruct -> IO (Id NSNumber)
startTime mtrCommodityTariffClusterDayEntryStruct  =
    sendMsg mtrCommodityTariffClusterDayEntryStruct (mkSelector "startTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartTime:@
setStartTime :: (IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct, IsNSNumber value) => mtrCommodityTariffClusterDayEntryStruct -> value -> IO ()
setStartTime mtrCommodityTariffClusterDayEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterDayEntryStruct (mkSelector "setStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- duration@
duration :: IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct => mtrCommodityTariffClusterDayEntryStruct -> IO (Id NSNumber)
duration mtrCommodityTariffClusterDayEntryStruct  =
    sendMsg mtrCommodityTariffClusterDayEntryStruct (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDuration:@
setDuration :: (IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct, IsNSNumber value) => mtrCommodityTariffClusterDayEntryStruct -> value -> IO ()
setDuration mtrCommodityTariffClusterDayEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterDayEntryStruct (mkSelector "setDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- randomizationOffset@
randomizationOffset :: IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct => mtrCommodityTariffClusterDayEntryStruct -> IO (Id NSNumber)
randomizationOffset mtrCommodityTariffClusterDayEntryStruct  =
    sendMsg mtrCommodityTariffClusterDayEntryStruct (mkSelector "randomizationOffset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRandomizationOffset:@
setRandomizationOffset :: (IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct, IsNSNumber value) => mtrCommodityTariffClusterDayEntryStruct -> value -> IO ()
setRandomizationOffset mtrCommodityTariffClusterDayEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterDayEntryStruct (mkSelector "setRandomizationOffset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- randomizationType@
randomizationType :: IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct => mtrCommodityTariffClusterDayEntryStruct -> IO (Id NSNumber)
randomizationType mtrCommodityTariffClusterDayEntryStruct  =
    sendMsg mtrCommodityTariffClusterDayEntryStruct (mkSelector "randomizationType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRandomizationType:@
setRandomizationType :: (IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct, IsNSNumber value) => mtrCommodityTariffClusterDayEntryStruct -> value -> IO ()
setRandomizationType mtrCommodityTariffClusterDayEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterDayEntryStruct (mkSelector "setRandomizationType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dayEntryID@
dayEntryIDSelector :: Selector
dayEntryIDSelector = mkSelector "dayEntryID"

-- | @Selector@ for @setDayEntryID:@
setDayEntryIDSelector :: Selector
setDayEntryIDSelector = mkSelector "setDayEntryID:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @randomizationOffset@
randomizationOffsetSelector :: Selector
randomizationOffsetSelector = mkSelector "randomizationOffset"

-- | @Selector@ for @setRandomizationOffset:@
setRandomizationOffsetSelector :: Selector
setRandomizationOffsetSelector = mkSelector "setRandomizationOffset:"

-- | @Selector@ for @randomizationType@
randomizationTypeSelector :: Selector
randomizationTypeSelector = mkSelector "randomizationType"

-- | @Selector@ for @setRandomizationType:@
setRandomizationTypeSelector :: Selector
setRandomizationTypeSelector = mkSelector "setRandomizationType:"

