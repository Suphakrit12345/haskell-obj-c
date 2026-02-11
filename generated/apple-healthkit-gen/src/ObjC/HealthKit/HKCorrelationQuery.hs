{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKCorrelationQuery
--
-- A query to find HKCorrelations
--
-- Correlations are HKSamples that contain a set of correlated samples. HKCorrelationQuery                accepts a predicate to filter HKCorrelations and a dictionary of predicates to filter the                correlated samples.
--
-- Generated bindings for @HKCorrelationQuery@.
module ObjC.HealthKit.HKCorrelationQuery
  ( HKCorrelationQuery
  , IsHKCorrelationQuery(..)
  , correlationType
  , samplePredicates
  , correlationTypeSelector
  , samplePredicatesSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- correlationType@
correlationType :: IsHKCorrelationQuery hkCorrelationQuery => hkCorrelationQuery -> IO (Id HKCorrelationType)
correlationType hkCorrelationQuery  =
    sendMsg hkCorrelationQuery (mkSelector "correlationType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | samplePredicates
--
-- A dictionary of predicates for the HKCorrelation's objects
--
-- samplePredicates maps HKSampleTypes to NSPredicates. The predicate value will apply                to objects of the key type.
--
-- ObjC selector: @- samplePredicates@
samplePredicates :: IsHKCorrelationQuery hkCorrelationQuery => hkCorrelationQuery -> IO (Id NSDictionary)
samplePredicates hkCorrelationQuery  =
    sendMsg hkCorrelationQuery (mkSelector "samplePredicates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @correlationType@
correlationTypeSelector :: Selector
correlationTypeSelector = mkSelector "correlationType"

-- | @Selector@ for @samplePredicates@
samplePredicatesSelector :: Selector
samplePredicatesSelector = mkSelector "samplePredicates"

