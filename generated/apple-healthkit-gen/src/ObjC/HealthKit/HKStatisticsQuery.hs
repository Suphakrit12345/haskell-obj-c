{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKStatisticsQuery@.
module ObjC.HealthKit.HKStatisticsQuery
  ( HKStatisticsQuery
  , IsHKStatisticsQuery(..)
  , initWithQuantityType_quantitySamplePredicate_options_completionHandler
  , initWithQuantityType_quantitySamplePredicate_options_completionHandlerSelector

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

-- | @- initWithQuantityType:quantitySamplePredicate:options:completionHandler:@
initWithQuantityType_quantitySamplePredicate_options_completionHandler :: (IsHKStatisticsQuery hkStatisticsQuery, IsHKQuantityType quantityType, IsNSPredicate quantitySamplePredicate) => hkStatisticsQuery -> quantityType -> quantitySamplePredicate -> HKStatisticsOptions -> Ptr () -> IO (Id HKStatisticsQuery)
initWithQuantityType_quantitySamplePredicate_options_completionHandler hkStatisticsQuery  quantityType quantitySamplePredicate options handler =
  withObjCPtr quantityType $ \raw_quantityType ->
    withObjCPtr quantitySamplePredicate $ \raw_quantitySamplePredicate ->
        sendMsg hkStatisticsQuery (mkSelector "initWithQuantityType:quantitySamplePredicate:options:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_quantitySamplePredicate :: Ptr ()), argCULong (coerce options), argPtr (castPtr handler :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithQuantityType:quantitySamplePredicate:options:completionHandler:@
initWithQuantityType_quantitySamplePredicate_options_completionHandlerSelector :: Selector
initWithQuantityType_quantitySamplePredicate_options_completionHandlerSelector = mkSelector "initWithQuantityType:quantitySamplePredicate:options:completionHandler:"

