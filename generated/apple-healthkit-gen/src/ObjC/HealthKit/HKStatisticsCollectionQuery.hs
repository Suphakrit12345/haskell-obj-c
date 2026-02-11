{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKStatisticsCollectionQuery@.
module ObjC.HealthKit.HKStatisticsCollectionQuery
  ( HKStatisticsCollectionQuery
  , IsHKStatisticsCollectionQuery(..)
  , initWithQuantityType_quantitySamplePredicate_options_anchorDate_intervalComponents
  , anchorDate
  , options
  , intervalComponents
  , initialResultsHandler
  , setInitialResultsHandler
  , statisticsUpdateHandler
  , setStatisticsUpdateHandler
  , initWithQuantityType_quantitySamplePredicate_options_anchorDate_intervalComponentsSelector
  , anchorDateSelector
  , optionsSelector
  , intervalComponentsSelector
  , initialResultsHandlerSelector
  , setInitialResultsHandlerSelector
  , statisticsUpdateHandlerSelector
  , setStatisticsUpdateHandlerSelector

  -- * Enum types
  , HKStatisticsOptions(HKStatisticsOptions)
  , pattern HKStatisticsOptionNone
  , pattern HKStatisticsOptionSeparateBySource
  , pattern HKStatisticsOptionDiscreteAverage
  , pattern HKStatisticsOptionDiscreteMin
  , pattern HKStatisticsOptionDiscreteMax
  , pattern HKStatisticsOptionCumulativeSum
  , pattern HKStatisticsOptionMostRecent
  , pattern HKStatisticsOptionDiscreteMostRecent
  , pattern HKStatisticsOptionDuration

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

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithQuantityType:quantitySamplePredicate:options:anchorDate:intervalComponents:@
initWithQuantityType_quantitySamplePredicate_options_anchorDate_intervalComponents :: (IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery, IsHKQuantityType quantityType, IsNSPredicate quantitySamplePredicate, IsNSDate anchorDate, IsNSDateComponents intervalComponents) => hkStatisticsCollectionQuery -> quantityType -> quantitySamplePredicate -> HKStatisticsOptions -> anchorDate -> intervalComponents -> IO (Id HKStatisticsCollectionQuery)
initWithQuantityType_quantitySamplePredicate_options_anchorDate_intervalComponents hkStatisticsCollectionQuery  quantityType quantitySamplePredicate options anchorDate intervalComponents =
  withObjCPtr quantityType $ \raw_quantityType ->
    withObjCPtr quantitySamplePredicate $ \raw_quantitySamplePredicate ->
      withObjCPtr anchorDate $ \raw_anchorDate ->
        withObjCPtr intervalComponents $ \raw_intervalComponents ->
            sendMsg hkStatisticsCollectionQuery (mkSelector "initWithQuantityType:quantitySamplePredicate:options:anchorDate:intervalComponents:") (retPtr retVoid) [argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_quantitySamplePredicate :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_anchorDate :: Ptr ()), argPtr (castPtr raw_intervalComponents :: Ptr ())] >>= ownedObject . castPtr

-- | @- anchorDate@
anchorDate :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> IO (Id NSDate)
anchorDate hkStatisticsCollectionQuery  =
    sendMsg hkStatisticsCollectionQuery (mkSelector "anchorDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- options@
options :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> IO HKStatisticsOptions
options hkStatisticsCollectionQuery  =
    fmap (coerce :: CULong -> HKStatisticsOptions) $ sendMsg hkStatisticsCollectionQuery (mkSelector "options") retCULong []

-- | @- intervalComponents@
intervalComponents :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> IO (Id NSDateComponents)
intervalComponents hkStatisticsCollectionQuery  =
    sendMsg hkStatisticsCollectionQuery (mkSelector "intervalComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initialResultsHandler@
initialResultsHandler :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> IO (Ptr ())
initialResultsHandler hkStatisticsCollectionQuery  =
    fmap castPtr $ sendMsg hkStatisticsCollectionQuery (mkSelector "initialResultsHandler") (retPtr retVoid) []

-- | @- setInitialResultsHandler:@
setInitialResultsHandler :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> Ptr () -> IO ()
setInitialResultsHandler hkStatisticsCollectionQuery  value =
    sendMsg hkStatisticsCollectionQuery (mkSelector "setInitialResultsHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- statisticsUpdateHandler@
statisticsUpdateHandler :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> IO (Ptr ())
statisticsUpdateHandler hkStatisticsCollectionQuery  =
    fmap castPtr $ sendMsg hkStatisticsCollectionQuery (mkSelector "statisticsUpdateHandler") (retPtr retVoid) []

-- | @- setStatisticsUpdateHandler:@
setStatisticsUpdateHandler :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> Ptr () -> IO ()
setStatisticsUpdateHandler hkStatisticsCollectionQuery  value =
    sendMsg hkStatisticsCollectionQuery (mkSelector "setStatisticsUpdateHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithQuantityType:quantitySamplePredicate:options:anchorDate:intervalComponents:@
initWithQuantityType_quantitySamplePredicate_options_anchorDate_intervalComponentsSelector :: Selector
initWithQuantityType_quantitySamplePredicate_options_anchorDate_intervalComponentsSelector = mkSelector "initWithQuantityType:quantitySamplePredicate:options:anchorDate:intervalComponents:"

-- | @Selector@ for @anchorDate@
anchorDateSelector :: Selector
anchorDateSelector = mkSelector "anchorDate"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @intervalComponents@
intervalComponentsSelector :: Selector
intervalComponentsSelector = mkSelector "intervalComponents"

-- | @Selector@ for @initialResultsHandler@
initialResultsHandlerSelector :: Selector
initialResultsHandlerSelector = mkSelector "initialResultsHandler"

-- | @Selector@ for @setInitialResultsHandler:@
setInitialResultsHandlerSelector :: Selector
setInitialResultsHandlerSelector = mkSelector "setInitialResultsHandler:"

-- | @Selector@ for @statisticsUpdateHandler@
statisticsUpdateHandlerSelector :: Selector
statisticsUpdateHandlerSelector = mkSelector "statisticsUpdateHandler"

-- | @Selector@ for @setStatisticsUpdateHandler:@
setStatisticsUpdateHandlerSelector :: Selector
setStatisticsUpdateHandlerSelector = mkSelector "setStatisticsUpdateHandler:"

