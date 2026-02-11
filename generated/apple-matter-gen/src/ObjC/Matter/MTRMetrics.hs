{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of a collection of metrics data for an operation.
--
-- Generated bindings for @MTRMetrics@.
module ObjC.Matter.MTRMetrics
  ( MTRMetrics
  , IsMTRMetrics(..)
  , init_
  , new
  , metricDataForKey
  , uniqueIdentifier
  , allKeys
  , initSelector
  , newSelector
  , metricDataForKeySelector
  , uniqueIdentifierSelector
  , allKeysSelector


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

-- | @- init@
init_ :: IsMTRMetrics mtrMetrics => mtrMetrics -> IO (Id MTRMetrics)
init_ mtrMetrics  =
    sendMsg mtrMetrics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRMetrics)
new  =
  do
    cls' <- getRequiredClass "MTRMetrics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns metric data corresponding to the metric identified by its key.
--
-- @key@ — Name of the metric
--
-- Returns: An object containing the metric data, nil if key is invalid.
--
-- ObjC selector: @- metricDataForKey:@
metricDataForKey :: (IsMTRMetrics mtrMetrics, IsNSString key) => mtrMetrics -> key -> IO (Id MTRMetricData)
metricDataForKey mtrMetrics  key =
  withObjCPtr key $ \raw_key ->
      sendMsg mtrMetrics (mkSelector "metricDataForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a unique identifier for the object
--
-- ObjC selector: @- uniqueIdentifier@
uniqueIdentifier :: IsMTRMetrics mtrMetrics => mtrMetrics -> IO (Id NSUUID)
uniqueIdentifier mtrMetrics  =
    sendMsg mtrMetrics (mkSelector "uniqueIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the names of all the metrics data items collected.
--
-- ObjC selector: @- allKeys@
allKeys :: IsMTRMetrics mtrMetrics => mtrMetrics -> IO (Id NSArray)
allKeys mtrMetrics  =
    sendMsg mtrMetrics (mkSelector "allKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @metricDataForKey:@
metricDataForKeySelector :: Selector
metricDataForKeySelector = mkSelector "metricDataForKey:"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @allKeys@
allKeysSelector :: Selector
allKeysSelector = mkSelector "allKeys"

