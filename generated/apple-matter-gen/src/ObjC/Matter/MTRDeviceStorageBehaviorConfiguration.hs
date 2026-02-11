{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class that configures how MTRDevice objects persist their attributes to storage, so as to not overwhelm the underlying storage system.
--
-- Generated bindings for @MTRDeviceStorageBehaviorConfiguration@.
module ObjC.Matter.MTRDeviceStorageBehaviorConfiguration
  ( MTRDeviceStorageBehaviorConfiguration
  , IsMTRDeviceStorageBehaviorConfiguration(..)
  , configurationWithDefaultStorageBehavior
  , configurationWithStorageBehaviorOptimizationDisabled
  , configurationWithReportToPersistenceDelayTime_reportToPersistenceDelayTimeMax_recentReportTimesMaxCount_timeBetweenReportsTooShortThreshold_timeBetweenReportsTooShortMinThreshold_reportToPersistenceDelayMaxMultiplier_deviceReportingExcessivelyIntervalThreshold
  , disableStorageBehaviorOptimization
  , setDisableStorageBehaviorOptimization
  , reportToPersistenceDelayTime
  , setReportToPersistenceDelayTime
  , reportToPersistenceDelayTimeMax
  , setReportToPersistenceDelayTimeMax
  , recentReportTimesMaxCount
  , setRecentReportTimesMaxCount
  , timeBetweenReportsTooShortThreshold
  , setTimeBetweenReportsTooShortThreshold
  , timeBetweenReportsTooShortMinThreshold
  , setTimeBetweenReportsTooShortMinThreshold
  , reportToPersistenceDelayMaxMultiplier
  , setReportToPersistenceDelayMaxMultiplier
  , deviceReportingExcessivelyIntervalThreshold
  , setDeviceReportingExcessivelyIntervalThreshold
  , configurationWithDefaultStorageBehaviorSelector
  , configurationWithStorageBehaviorOptimizationDisabledSelector
  , configurationWithReportToPersistenceDelayTime_reportToPersistenceDelayTimeMax_recentReportTimesMaxCount_timeBetweenReportsTooShortThreshold_timeBetweenReportsTooShortMinThreshold_reportToPersistenceDelayMaxMultiplier_deviceReportingExcessivelyIntervalThresholdSelector
  , disableStorageBehaviorOptimizationSelector
  , setDisableStorageBehaviorOptimizationSelector
  , reportToPersistenceDelayTimeSelector
  , setReportToPersistenceDelayTimeSelector
  , reportToPersistenceDelayTimeMaxSelector
  , setReportToPersistenceDelayTimeMaxSelector
  , recentReportTimesMaxCountSelector
  , setRecentReportTimesMaxCountSelector
  , timeBetweenReportsTooShortThresholdSelector
  , setTimeBetweenReportsTooShortThresholdSelector
  , timeBetweenReportsTooShortMinThresholdSelector
  , setTimeBetweenReportsTooShortMinThresholdSelector
  , reportToPersistenceDelayMaxMultiplierSelector
  , setReportToPersistenceDelayMaxMultiplierSelector
  , deviceReportingExcessivelyIntervalThresholdSelector
  , setDeviceReportingExcessivelyIntervalThresholdSelector


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

-- | Create configuration with a default set of values. See description below for details.
--
-- ObjC selector: @+ configurationWithDefaultStorageBehavior@
configurationWithDefaultStorageBehavior :: IO (Id MTRDeviceStorageBehaviorConfiguration)
configurationWithDefaultStorageBehavior  =
  do
    cls' <- getRequiredClass "MTRDeviceStorageBehaviorConfiguration"
    sendClassMsg cls' (mkSelector "configurationWithDefaultStorageBehavior") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create configuration that disables storage behavior optimizations.
--
-- ObjC selector: @+ configurationWithStorageBehaviorOptimizationDisabled@
configurationWithStorageBehaviorOptimizationDisabled :: IO (Id MTRDeviceStorageBehaviorConfiguration)
configurationWithStorageBehaviorOptimizationDisabled  =
  do
    cls' <- getRequiredClass "MTRDeviceStorageBehaviorConfiguration"
    sendClassMsg cls' (mkSelector "configurationWithStorageBehaviorOptimizationDisabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create configuration with specified values. See description below for details, and the list of properties below for valid ranges of these values.
--
-- ObjC selector: @+ configurationWithReportToPersistenceDelayTime:reportToPersistenceDelayTimeMax:recentReportTimesMaxCount:timeBetweenReportsTooShortThreshold:timeBetweenReportsTooShortMinThreshold:reportToPersistenceDelayMaxMultiplier:deviceReportingExcessivelyIntervalThreshold:@
configurationWithReportToPersistenceDelayTime_reportToPersistenceDelayTimeMax_recentReportTimesMaxCount_timeBetweenReportsTooShortThreshold_timeBetweenReportsTooShortMinThreshold_reportToPersistenceDelayMaxMultiplier_deviceReportingExcessivelyIntervalThreshold :: CDouble -> CDouble -> CULong -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id MTRDeviceStorageBehaviorConfiguration)
configurationWithReportToPersistenceDelayTime_reportToPersistenceDelayTimeMax_recentReportTimesMaxCount_timeBetweenReportsTooShortThreshold_timeBetweenReportsTooShortMinThreshold_reportToPersistenceDelayMaxMultiplier_deviceReportingExcessivelyIntervalThreshold reportToPersistenceDelayTime reportToPersistenceDelayTimeMax recentReportTimesMaxCount timeBetweenReportsTooShortThreshold timeBetweenReportsTooShortMinThreshold reportToPersistenceDelayMaxMultiplier deviceReportingExcessivelyIntervalThreshold =
  do
    cls' <- getRequiredClass "MTRDeviceStorageBehaviorConfiguration"
    sendClassMsg cls' (mkSelector "configurationWithReportToPersistenceDelayTime:reportToPersistenceDelayTimeMax:recentReportTimesMaxCount:timeBetweenReportsTooShortThreshold:timeBetweenReportsTooShortMinThreshold:reportToPersistenceDelayMaxMultiplier:deviceReportingExcessivelyIntervalThreshold:") (retPtr retVoid) [argCDouble reportToPersistenceDelayTime, argCDouble reportToPersistenceDelayTimeMax, argCULong recentReportTimesMaxCount, argCDouble timeBetweenReportsTooShortThreshold, argCDouble timeBetweenReportsTooShortMinThreshold, argCDouble reportToPersistenceDelayMaxMultiplier, argCDouble deviceReportingExcessivelyIntervalThreshold] >>= retainedObject . castPtr

-- | If disableStorageBehaviorOptimization is set to YES, then all the waiting mechanism as described above is disabled.
--
-- ObjC selector: @- disableStorageBehaviorOptimization@
disableStorageBehaviorOptimization :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO Bool
disableStorageBehaviorOptimization mtrDeviceStorageBehaviorConfiguration  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "disableStorageBehaviorOptimization") retCULong []

-- | If disableStorageBehaviorOptimization is set to YES, then all the waiting mechanism as described above is disabled.
--
-- ObjC selector: @- setDisableStorageBehaviorOptimization:@
setDisableStorageBehaviorOptimization :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> Bool -> IO ()
setDisableStorageBehaviorOptimization mtrDeviceStorageBehaviorConfiguration  value =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "setDisableStorageBehaviorOptimization:") retVoid [argCULong (if value then 1 else 0)]

-- | If any of these properties are set to be out of the documented limits, these default values will be used to replace all of them:
--
-- reportToPersistenceDelayTimeDefault (15) reportToPersistenceDelayTimeMaxDefault (20 * 15) recentReportTimesMaxCountDefault (12) timeBetweenReportsTooShortThresholdDefault (15) timeBetweenReportsTooShortMinThresholdDefault (5) reportToPersistenceDelayMaxMultiplierDefault (10) deviceReportingExcessivelyIntervalThresholdDefault (5 * 60)
--
-- ObjC selector: @- reportToPersistenceDelayTime@
reportToPersistenceDelayTime :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CDouble
reportToPersistenceDelayTime mtrDeviceStorageBehaviorConfiguration  =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "reportToPersistenceDelayTime") retCDouble []

-- | If any of these properties are set to be out of the documented limits, these default values will be used to replace all of them:
--
-- reportToPersistenceDelayTimeDefault (15) reportToPersistenceDelayTimeMaxDefault (20 * 15) recentReportTimesMaxCountDefault (12) timeBetweenReportsTooShortThresholdDefault (15) timeBetweenReportsTooShortMinThresholdDefault (5) reportToPersistenceDelayMaxMultiplierDefault (10) deviceReportingExcessivelyIntervalThresholdDefault (5 * 60)
--
-- ObjC selector: @- setReportToPersistenceDelayTime:@
setReportToPersistenceDelayTime :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CDouble -> IO ()
setReportToPersistenceDelayTime mtrDeviceStorageBehaviorConfiguration  value =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "setReportToPersistenceDelayTime:") retVoid [argCDouble value]

-- | @- reportToPersistenceDelayTimeMax@
reportToPersistenceDelayTimeMax :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CDouble
reportToPersistenceDelayTimeMax mtrDeviceStorageBehaviorConfiguration  =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "reportToPersistenceDelayTimeMax") retCDouble []

-- | @- setReportToPersistenceDelayTimeMax:@
setReportToPersistenceDelayTimeMax :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CDouble -> IO ()
setReportToPersistenceDelayTimeMax mtrDeviceStorageBehaviorConfiguration  value =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "setReportToPersistenceDelayTimeMax:") retVoid [argCDouble value]

-- | @- recentReportTimesMaxCount@
recentReportTimesMaxCount :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CULong
recentReportTimesMaxCount mtrDeviceStorageBehaviorConfiguration  =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "recentReportTimesMaxCount") retCULong []

-- | @- setRecentReportTimesMaxCount:@
setRecentReportTimesMaxCount :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CULong -> IO ()
setRecentReportTimesMaxCount mtrDeviceStorageBehaviorConfiguration  value =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "setRecentReportTimesMaxCount:") retVoid [argCULong value]

-- | @- timeBetweenReportsTooShortThreshold@
timeBetweenReportsTooShortThreshold :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CDouble
timeBetweenReportsTooShortThreshold mtrDeviceStorageBehaviorConfiguration  =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "timeBetweenReportsTooShortThreshold") retCDouble []

-- | @- setTimeBetweenReportsTooShortThreshold:@
setTimeBetweenReportsTooShortThreshold :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CDouble -> IO ()
setTimeBetweenReportsTooShortThreshold mtrDeviceStorageBehaviorConfiguration  value =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "setTimeBetweenReportsTooShortThreshold:") retVoid [argCDouble value]

-- | @- timeBetweenReportsTooShortMinThreshold@
timeBetweenReportsTooShortMinThreshold :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CDouble
timeBetweenReportsTooShortMinThreshold mtrDeviceStorageBehaviorConfiguration  =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "timeBetweenReportsTooShortMinThreshold") retCDouble []

-- | @- setTimeBetweenReportsTooShortMinThreshold:@
setTimeBetweenReportsTooShortMinThreshold :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CDouble -> IO ()
setTimeBetweenReportsTooShortMinThreshold mtrDeviceStorageBehaviorConfiguration  value =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "setTimeBetweenReportsTooShortMinThreshold:") retVoid [argCDouble value]

-- | @- reportToPersistenceDelayMaxMultiplier@
reportToPersistenceDelayMaxMultiplier :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CDouble
reportToPersistenceDelayMaxMultiplier mtrDeviceStorageBehaviorConfiguration  =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "reportToPersistenceDelayMaxMultiplier") retCDouble []

-- | @- setReportToPersistenceDelayMaxMultiplier:@
setReportToPersistenceDelayMaxMultiplier :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CDouble -> IO ()
setReportToPersistenceDelayMaxMultiplier mtrDeviceStorageBehaviorConfiguration  value =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "setReportToPersistenceDelayMaxMultiplier:") retVoid [argCDouble value]

-- | @- deviceReportingExcessivelyIntervalThreshold@
deviceReportingExcessivelyIntervalThreshold :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CDouble
deviceReportingExcessivelyIntervalThreshold mtrDeviceStorageBehaviorConfiguration  =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "deviceReportingExcessivelyIntervalThreshold") retCDouble []

-- | @- setDeviceReportingExcessivelyIntervalThreshold:@
setDeviceReportingExcessivelyIntervalThreshold :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CDouble -> IO ()
setDeviceReportingExcessivelyIntervalThreshold mtrDeviceStorageBehaviorConfiguration  value =
    sendMsg mtrDeviceStorageBehaviorConfiguration (mkSelector "setDeviceReportingExcessivelyIntervalThreshold:") retVoid [argCDouble value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configurationWithDefaultStorageBehavior@
configurationWithDefaultStorageBehaviorSelector :: Selector
configurationWithDefaultStorageBehaviorSelector = mkSelector "configurationWithDefaultStorageBehavior"

-- | @Selector@ for @configurationWithStorageBehaviorOptimizationDisabled@
configurationWithStorageBehaviorOptimizationDisabledSelector :: Selector
configurationWithStorageBehaviorOptimizationDisabledSelector = mkSelector "configurationWithStorageBehaviorOptimizationDisabled"

-- | @Selector@ for @configurationWithReportToPersistenceDelayTime:reportToPersistenceDelayTimeMax:recentReportTimesMaxCount:timeBetweenReportsTooShortThreshold:timeBetweenReportsTooShortMinThreshold:reportToPersistenceDelayMaxMultiplier:deviceReportingExcessivelyIntervalThreshold:@
configurationWithReportToPersistenceDelayTime_reportToPersistenceDelayTimeMax_recentReportTimesMaxCount_timeBetweenReportsTooShortThreshold_timeBetweenReportsTooShortMinThreshold_reportToPersistenceDelayMaxMultiplier_deviceReportingExcessivelyIntervalThresholdSelector :: Selector
configurationWithReportToPersistenceDelayTime_reportToPersistenceDelayTimeMax_recentReportTimesMaxCount_timeBetweenReportsTooShortThreshold_timeBetweenReportsTooShortMinThreshold_reportToPersistenceDelayMaxMultiplier_deviceReportingExcessivelyIntervalThresholdSelector = mkSelector "configurationWithReportToPersistenceDelayTime:reportToPersistenceDelayTimeMax:recentReportTimesMaxCount:timeBetweenReportsTooShortThreshold:timeBetweenReportsTooShortMinThreshold:reportToPersistenceDelayMaxMultiplier:deviceReportingExcessivelyIntervalThreshold:"

-- | @Selector@ for @disableStorageBehaviorOptimization@
disableStorageBehaviorOptimizationSelector :: Selector
disableStorageBehaviorOptimizationSelector = mkSelector "disableStorageBehaviorOptimization"

-- | @Selector@ for @setDisableStorageBehaviorOptimization:@
setDisableStorageBehaviorOptimizationSelector :: Selector
setDisableStorageBehaviorOptimizationSelector = mkSelector "setDisableStorageBehaviorOptimization:"

-- | @Selector@ for @reportToPersistenceDelayTime@
reportToPersistenceDelayTimeSelector :: Selector
reportToPersistenceDelayTimeSelector = mkSelector "reportToPersistenceDelayTime"

-- | @Selector@ for @setReportToPersistenceDelayTime:@
setReportToPersistenceDelayTimeSelector :: Selector
setReportToPersistenceDelayTimeSelector = mkSelector "setReportToPersistenceDelayTime:"

-- | @Selector@ for @reportToPersistenceDelayTimeMax@
reportToPersistenceDelayTimeMaxSelector :: Selector
reportToPersistenceDelayTimeMaxSelector = mkSelector "reportToPersistenceDelayTimeMax"

-- | @Selector@ for @setReportToPersistenceDelayTimeMax:@
setReportToPersistenceDelayTimeMaxSelector :: Selector
setReportToPersistenceDelayTimeMaxSelector = mkSelector "setReportToPersistenceDelayTimeMax:"

-- | @Selector@ for @recentReportTimesMaxCount@
recentReportTimesMaxCountSelector :: Selector
recentReportTimesMaxCountSelector = mkSelector "recentReportTimesMaxCount"

-- | @Selector@ for @setRecentReportTimesMaxCount:@
setRecentReportTimesMaxCountSelector :: Selector
setRecentReportTimesMaxCountSelector = mkSelector "setRecentReportTimesMaxCount:"

-- | @Selector@ for @timeBetweenReportsTooShortThreshold@
timeBetweenReportsTooShortThresholdSelector :: Selector
timeBetweenReportsTooShortThresholdSelector = mkSelector "timeBetweenReportsTooShortThreshold"

-- | @Selector@ for @setTimeBetweenReportsTooShortThreshold:@
setTimeBetweenReportsTooShortThresholdSelector :: Selector
setTimeBetweenReportsTooShortThresholdSelector = mkSelector "setTimeBetweenReportsTooShortThreshold:"

-- | @Selector@ for @timeBetweenReportsTooShortMinThreshold@
timeBetweenReportsTooShortMinThresholdSelector :: Selector
timeBetweenReportsTooShortMinThresholdSelector = mkSelector "timeBetweenReportsTooShortMinThreshold"

-- | @Selector@ for @setTimeBetweenReportsTooShortMinThreshold:@
setTimeBetweenReportsTooShortMinThresholdSelector :: Selector
setTimeBetweenReportsTooShortMinThresholdSelector = mkSelector "setTimeBetweenReportsTooShortMinThreshold:"

-- | @Selector@ for @reportToPersistenceDelayMaxMultiplier@
reportToPersistenceDelayMaxMultiplierSelector :: Selector
reportToPersistenceDelayMaxMultiplierSelector = mkSelector "reportToPersistenceDelayMaxMultiplier"

-- | @Selector@ for @setReportToPersistenceDelayMaxMultiplier:@
setReportToPersistenceDelayMaxMultiplierSelector :: Selector
setReportToPersistenceDelayMaxMultiplierSelector = mkSelector "setReportToPersistenceDelayMaxMultiplier:"

-- | @Selector@ for @deviceReportingExcessivelyIntervalThreshold@
deviceReportingExcessivelyIntervalThresholdSelector :: Selector
deviceReportingExcessivelyIntervalThresholdSelector = mkSelector "deviceReportingExcessivelyIntervalThreshold"

-- | @Selector@ for @setDeviceReportingExcessivelyIntervalThreshold:@
setDeviceReportingExcessivelyIntervalThresholdSelector :: Selector
setDeviceReportingExcessivelyIntervalThresholdSelector = mkSelector "setDeviceReportingExcessivelyIntervalThreshold:"

