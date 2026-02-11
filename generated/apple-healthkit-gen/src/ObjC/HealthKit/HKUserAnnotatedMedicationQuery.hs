{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKUserAnnotatedMedicationQuery@.
module ObjC.HealthKit.HKUserAnnotatedMedicationQuery
  ( HKUserAnnotatedMedicationQuery
  , IsHKUserAnnotatedMedicationQuery(..)
  , initWithPredicate_limit_resultsHandler
  , initWithPredicate_limit_resultsHandlerSelector


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

-- | initWithPredicate:limit:resultsHandler:
--
-- Returns a query that will retrieve HKUserAnnotatedMedications matching the given predicate and limit.
--
-- @predicate@ — The predicate which user annotated medications should match.
--
-- @limit@ — The maximum number of  user annotated medications to return.  Pass HKObjectQueryNoLimit for no limit.
--
-- @resultsHandler@ — The block to invoke with results to deliver to the client. The results handler will be called with done = YES when there are no more user annotated medications to enumerate.
--
-- ObjC selector: @- initWithPredicate:limit:resultsHandler:@
initWithPredicate_limit_resultsHandler :: (IsHKUserAnnotatedMedicationQuery hkUserAnnotatedMedicationQuery, IsNSPredicate predicate) => hkUserAnnotatedMedicationQuery -> predicate -> CULong -> Ptr () -> IO (Id HKUserAnnotatedMedicationQuery)
initWithPredicate_limit_resultsHandler hkUserAnnotatedMedicationQuery  predicate limit resultsHandler =
  withObjCPtr predicate $ \raw_predicate ->
      sendMsg hkUserAnnotatedMedicationQuery (mkSelector "initWithPredicate:limit:resultsHandler:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ()), argCULong limit, argPtr (castPtr resultsHandler :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPredicate:limit:resultsHandler:@
initWithPredicate_limit_resultsHandlerSelector :: Selector
initWithPredicate_limit_resultsHandlerSelector = mkSelector "initWithPredicate:limit:resultsHandler:"

