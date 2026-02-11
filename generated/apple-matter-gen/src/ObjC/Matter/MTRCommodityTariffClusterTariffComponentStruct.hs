{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterTariffComponentStruct@.
module ObjC.Matter.MTRCommodityTariffClusterTariffComponentStruct
  ( MTRCommodityTariffClusterTariffComponentStruct
  , IsMTRCommodityTariffClusterTariffComponentStruct(..)
  , tariffComponentID
  , setTariffComponentID
  , price
  , setPrice
  , friendlyCredit
  , setFriendlyCredit
  , auxiliaryLoad
  , setAuxiliaryLoad
  , peakPeriod
  , setPeakPeriod
  , powerThreshold
  , setPowerThreshold
  , threshold
  , setThreshold
  , label
  , setLabel
  , predicted
  , setPredicted
  , tariffComponentIDSelector
  , setTariffComponentIDSelector
  , priceSelector
  , setPriceSelector
  , friendlyCreditSelector
  , setFriendlyCreditSelector
  , auxiliaryLoadSelector
  , setAuxiliaryLoadSelector
  , peakPeriodSelector
  , setPeakPeriodSelector
  , powerThresholdSelector
  , setPowerThresholdSelector
  , thresholdSelector
  , setThresholdSelector
  , labelSelector
  , setLabelSelector
  , predictedSelector
  , setPredictedSelector


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

-- | @- tariffComponentID@
tariffComponentID :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id NSNumber)
tariffComponentID mtrCommodityTariffClusterTariffComponentStruct  =
    sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "tariffComponentID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTariffComponentID:@
setTariffComponentID :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setTariffComponentID mtrCommodityTariffClusterTariffComponentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "setTariffComponentID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- price@
price :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id MTRCommodityTariffClusterTariffPriceStruct)
price mtrCommodityTariffClusterTariffComponentStruct  =
    sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "price") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrice:@
setPrice :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsMTRCommodityTariffClusterTariffPriceStruct value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setPrice mtrCommodityTariffClusterTariffComponentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "setPrice:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- friendlyCredit@
friendlyCredit :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id NSNumber)
friendlyCredit mtrCommodityTariffClusterTariffComponentStruct  =
    sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "friendlyCredit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFriendlyCredit:@
setFriendlyCredit :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setFriendlyCredit mtrCommodityTariffClusterTariffComponentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "setFriendlyCredit:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- auxiliaryLoad@
auxiliaryLoad :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id MTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct)
auxiliaryLoad mtrCommodityTariffClusterTariffComponentStruct  =
    sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "auxiliaryLoad") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAuxiliaryLoad:@
setAuxiliaryLoad :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsMTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setAuxiliaryLoad mtrCommodityTariffClusterTariffComponentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "setAuxiliaryLoad:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- peakPeriod@
peakPeriod :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id MTRCommodityTariffClusterPeakPeriodStruct)
peakPeriod mtrCommodityTariffClusterTariffComponentStruct  =
    sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "peakPeriod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPeakPeriod:@
setPeakPeriod :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsMTRCommodityTariffClusterPeakPeriodStruct value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setPeakPeriod mtrCommodityTariffClusterTariffComponentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "setPeakPeriod:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- powerThreshold@
powerThreshold :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id MTRDataTypePowerThresholdStruct)
powerThreshold mtrCommodityTariffClusterTariffComponentStruct  =
    sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "powerThreshold") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPowerThreshold:@
setPowerThreshold :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsMTRDataTypePowerThresholdStruct value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setPowerThreshold mtrCommodityTariffClusterTariffComponentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "setPowerThreshold:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- threshold@
threshold :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id NSNumber)
threshold mtrCommodityTariffClusterTariffComponentStruct  =
    sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "threshold") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setThreshold:@
setThreshold :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setThreshold mtrCommodityTariffClusterTariffComponentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "setThreshold:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- label@
label :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id NSString)
label mtrCommodityTariffClusterTariffComponentStruct  =
    sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsNSString value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setLabel mtrCommodityTariffClusterTariffComponentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- predicted@
predicted :: IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct => mtrCommodityTariffClusterTariffComponentStruct -> IO (Id NSNumber)
predicted mtrCommodityTariffClusterTariffComponentStruct  =
    sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "predicted") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPredicted:@
setPredicted :: (IsMTRCommodityTariffClusterTariffComponentStruct mtrCommodityTariffClusterTariffComponentStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffComponentStruct -> value -> IO ()
setPredicted mtrCommodityTariffClusterTariffComponentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterTariffComponentStruct (mkSelector "setPredicted:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tariffComponentID@
tariffComponentIDSelector :: Selector
tariffComponentIDSelector = mkSelector "tariffComponentID"

-- | @Selector@ for @setTariffComponentID:@
setTariffComponentIDSelector :: Selector
setTariffComponentIDSelector = mkSelector "setTariffComponentID:"

-- | @Selector@ for @price@
priceSelector :: Selector
priceSelector = mkSelector "price"

-- | @Selector@ for @setPrice:@
setPriceSelector :: Selector
setPriceSelector = mkSelector "setPrice:"

-- | @Selector@ for @friendlyCredit@
friendlyCreditSelector :: Selector
friendlyCreditSelector = mkSelector "friendlyCredit"

-- | @Selector@ for @setFriendlyCredit:@
setFriendlyCreditSelector :: Selector
setFriendlyCreditSelector = mkSelector "setFriendlyCredit:"

-- | @Selector@ for @auxiliaryLoad@
auxiliaryLoadSelector :: Selector
auxiliaryLoadSelector = mkSelector "auxiliaryLoad"

-- | @Selector@ for @setAuxiliaryLoad:@
setAuxiliaryLoadSelector :: Selector
setAuxiliaryLoadSelector = mkSelector "setAuxiliaryLoad:"

-- | @Selector@ for @peakPeriod@
peakPeriodSelector :: Selector
peakPeriodSelector = mkSelector "peakPeriod"

-- | @Selector@ for @setPeakPeriod:@
setPeakPeriodSelector :: Selector
setPeakPeriodSelector = mkSelector "setPeakPeriod:"

-- | @Selector@ for @powerThreshold@
powerThresholdSelector :: Selector
powerThresholdSelector = mkSelector "powerThreshold"

-- | @Selector@ for @setPowerThreshold:@
setPowerThresholdSelector :: Selector
setPowerThresholdSelector = mkSelector "setPowerThreshold:"

-- | @Selector@ for @threshold@
thresholdSelector :: Selector
thresholdSelector = mkSelector "threshold"

-- | @Selector@ for @setThreshold:@
setThresholdSelector :: Selector
setThresholdSelector = mkSelector "setThreshold:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @predicted@
predictedSelector :: Selector
predictedSelector = mkSelector "predicted"

-- | @Selector@ for @setPredicted:@
setPredictedSelector :: Selector
setPredictedSelector = mkSelector "setPredicted:"

