{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKQueryDescriptor@.
module ObjC.HealthKit.HKQueryDescriptor
  ( HKQueryDescriptor
  , IsHKQueryDescriptor(..)
  , init_
  , new
  , initWithSampleType_predicate
  , sampleType
  , predicate
  , initSelector
  , newSelector
  , initWithSampleType_predicateSelector
  , sampleTypeSelector
  , predicateSelector


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

-- | @- init@
init_ :: IsHKQueryDescriptor hkQueryDescriptor => hkQueryDescriptor -> IO (Id HKQueryDescriptor)
init_ hkQueryDescriptor  =
    sendMsg hkQueryDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKQueryDescriptor)
new  =
  do
    cls' <- getRequiredClass "HKQueryDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithSampleType:predicate:
--
-- Returns a query descriptor that describes a data type and predicate to be used in an HKQuery.
--
-- @sampleType@ — The type of sample to retrieve.
--
-- @predicate@ — The predicate which samples should match.
--
-- ObjC selector: @- initWithSampleType:predicate:@
initWithSampleType_predicate :: (IsHKQueryDescriptor hkQueryDescriptor, IsHKSampleType sampleType, IsNSPredicate predicate) => hkQueryDescriptor -> sampleType -> predicate -> IO (Id HKQueryDescriptor)
initWithSampleType_predicate hkQueryDescriptor  sampleType predicate =
  withObjCPtr sampleType $ \raw_sampleType ->
    withObjCPtr predicate $ \raw_predicate ->
        sendMsg hkQueryDescriptor (mkSelector "initWithSampleType:predicate:") (retPtr retVoid) [argPtr (castPtr raw_sampleType :: Ptr ()), argPtr (castPtr raw_predicate :: Ptr ())] >>= ownedObject . castPtr

-- | sampleType
--
-- The type of sample to retrieve in an HKQuery.
--
-- ObjC selector: @- sampleType@
sampleType :: IsHKQueryDescriptor hkQueryDescriptor => hkQueryDescriptor -> IO (Id HKSampleType)
sampleType hkQueryDescriptor  =
    sendMsg hkQueryDescriptor (mkSelector "sampleType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | predicate
--
-- The predicate which samples should match.
--
-- ObjC selector: @- predicate@
predicate :: IsHKQueryDescriptor hkQueryDescriptor => hkQueryDescriptor -> IO (Id NSPredicate)
predicate hkQueryDescriptor  =
    sendMsg hkQueryDescriptor (mkSelector "predicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSampleType:predicate:@
initWithSampleType_predicateSelector :: Selector
initWithSampleType_predicateSelector = mkSelector "initWithSampleType:predicate:"

-- | @Selector@ for @sampleType@
sampleTypeSelector :: Selector
sampleTypeSelector = mkSelector "sampleType"

-- | @Selector@ for @predicate@
predicateSelector :: Selector
predicateSelector = mkSelector "predicate"

