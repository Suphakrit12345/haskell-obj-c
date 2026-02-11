{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterPeakPeriodStruct@.
module ObjC.Matter.MTRCommodityTariffClusterPeakPeriodStruct
  ( MTRCommodityTariffClusterPeakPeriodStruct
  , IsMTRCommodityTariffClusterPeakPeriodStruct(..)
  , severity
  , setSeverity
  , peakPeriod
  , setPeakPeriod
  , severitySelector
  , setSeveritySelector
  , peakPeriodSelector
  , setPeakPeriodSelector


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

-- | @- severity@
severity :: IsMTRCommodityTariffClusterPeakPeriodStruct mtrCommodityTariffClusterPeakPeriodStruct => mtrCommodityTariffClusterPeakPeriodStruct -> IO (Id NSNumber)
severity mtrCommodityTariffClusterPeakPeriodStruct  =
    sendMsg mtrCommodityTariffClusterPeakPeriodStruct (mkSelector "severity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSeverity:@
setSeverity :: (IsMTRCommodityTariffClusterPeakPeriodStruct mtrCommodityTariffClusterPeakPeriodStruct, IsNSNumber value) => mtrCommodityTariffClusterPeakPeriodStruct -> value -> IO ()
setSeverity mtrCommodityTariffClusterPeakPeriodStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterPeakPeriodStruct (mkSelector "setSeverity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- peakPeriod@
peakPeriod :: IsMTRCommodityTariffClusterPeakPeriodStruct mtrCommodityTariffClusterPeakPeriodStruct => mtrCommodityTariffClusterPeakPeriodStruct -> IO (Id NSNumber)
peakPeriod mtrCommodityTariffClusterPeakPeriodStruct  =
    sendMsg mtrCommodityTariffClusterPeakPeriodStruct (mkSelector "peakPeriod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPeakPeriod:@
setPeakPeriod :: (IsMTRCommodityTariffClusterPeakPeriodStruct mtrCommodityTariffClusterPeakPeriodStruct, IsNSNumber value) => mtrCommodityTariffClusterPeakPeriodStruct -> value -> IO ()
setPeakPeriod mtrCommodityTariffClusterPeakPeriodStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterPeakPeriodStruct (mkSelector "setPeakPeriod:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @severity@
severitySelector :: Selector
severitySelector = mkSelector "severity"

-- | @Selector@ for @setSeverity:@
setSeveritySelector :: Selector
setSeveritySelector = mkSelector "setSeverity:"

-- | @Selector@ for @peakPeriod@
peakPeriodSelector :: Selector
peakPeriodSelector = mkSelector "peakPeriod"

-- | @Selector@ for @setPeakPeriod:@
setPeakPeriodSelector :: Selector
setPeakPeriodSelector = mkSelector "setPeakPeriod:"

