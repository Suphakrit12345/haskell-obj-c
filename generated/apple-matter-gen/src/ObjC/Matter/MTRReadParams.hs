{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTRReadParams    This is used to control the behavior of attribute/event reads and subscribes.    If not provided (i.e. nil passed for the MTRReadParams argument), will be    treated as if a default-initialized object was passed in.
--
-- Generated bindings for @MTRReadParams@.
module ObjC.Matter.MTRReadParams
  ( MTRReadParams
  , IsMTRReadParams(..)
  , filterByFabric
  , setFilterByFabric
  , minEventNumber
  , setMinEventNumber
  , assumeUnknownAttributesReportable
  , setAssumeUnknownAttributesReportable
  , fabricFiltered
  , setFabricFiltered
  , filterByFabricSelector
  , setFilterByFabricSelector
  , minEventNumberSelector
  , setMinEventNumberSelector
  , assumeUnknownAttributesReportableSelector
  , setAssumeUnknownAttributesReportableSelector
  , fabricFilteredSelector
  , setFabricFilteredSelector


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

-- | Whether the read/subscribe is fabric-filtered. The default is YES.
--
-- If YES, the read/subscribe is fabric-filtered and will only see things associated with the fabric of the reader/subscriber.
--
-- If NO, the read/subscribe is not fabric-filtered and will see all non-fabric-sensitive data for the given attribute path.
--
-- ObjC selector: @- filterByFabric@
filterByFabric :: IsMTRReadParams mtrReadParams => mtrReadParams -> IO Bool
filterByFabric mtrReadParams  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrReadParams (mkSelector "filterByFabric") retCULong []

-- | Whether the read/subscribe is fabric-filtered. The default is YES.
--
-- If YES, the read/subscribe is fabric-filtered and will only see things associated with the fabric of the reader/subscriber.
--
-- If NO, the read/subscribe is not fabric-filtered and will see all non-fabric-sensitive data for the given attribute path.
--
-- ObjC selector: @- setFilterByFabric:@
setFilterByFabric :: IsMTRReadParams mtrReadParams => mtrReadParams -> Bool -> IO ()
setFilterByFabric mtrReadParams  value =
    sendMsg mtrReadParams (mkSelector "setFilterByFabric:") retVoid [argCULong (if value then 1 else 0)]

-- | Sets a filter for which events will be reported in the read/subscribe interaction.
--
-- If nil (the default value), all of the queued events will be reported from lowest to highest event number.
--
-- If not nil, queued events with an event number smaller than minEventNumber will not be reported.
--
-- ObjC selector: @- minEventNumber@
minEventNumber :: IsMTRReadParams mtrReadParams => mtrReadParams -> IO (Id NSNumber)
minEventNumber mtrReadParams  =
    sendMsg mtrReadParams (mkSelector "minEventNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets a filter for which events will be reported in the read/subscribe interaction.
--
-- If nil (the default value), all of the queued events will be reported from lowest to highest event number.
--
-- If not nil, queued events with an event number smaller than minEventNumber will not be reported.
--
-- ObjC selector: @- setMinEventNumber:@
setMinEventNumber :: (IsMTRReadParams mtrReadParams, IsNSNumber value) => mtrReadParams -> value -> IO ()
setMinEventNumber mtrReadParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrReadParams (mkSelector "setMinEventNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether attributes without known schema (e.g. vendor-specific attributes) should be assumed to be reportable normally via subscriptions. The default is YES.
--
-- This setting is only relevant to some consumers of MTRReadParams.  One of those consumers is readAttributeWithEndpointID:clusterID:attributeID:params: on MTRDevice.
--
-- ObjC selector: @- assumeUnknownAttributesReportable@
assumeUnknownAttributesReportable :: IsMTRReadParams mtrReadParams => mtrReadParams -> IO Bool
assumeUnknownAttributesReportable mtrReadParams  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrReadParams (mkSelector "assumeUnknownAttributesReportable") retCULong []

-- | Controls whether attributes without known schema (e.g. vendor-specific attributes) should be assumed to be reportable normally via subscriptions. The default is YES.
--
-- This setting is only relevant to some consumers of MTRReadParams.  One of those consumers is readAttributeWithEndpointID:clusterID:attributeID:params: on MTRDevice.
--
-- ObjC selector: @- setAssumeUnknownAttributesReportable:@
setAssumeUnknownAttributesReportable :: IsMTRReadParams mtrReadParams => mtrReadParams -> Bool -> IO ()
setAssumeUnknownAttributesReportable mtrReadParams  value =
    sendMsg mtrReadParams (mkSelector "setAssumeUnknownAttributesReportable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- fabricFiltered@
fabricFiltered :: IsMTRReadParams mtrReadParams => mtrReadParams -> IO (Id NSNumber)
fabricFiltered mtrReadParams  =
    sendMsg mtrReadParams (mkSelector "fabricFiltered") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricFiltered:@
setFabricFiltered :: (IsMTRReadParams mtrReadParams, IsNSNumber value) => mtrReadParams -> value -> IO ()
setFabricFiltered mtrReadParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrReadParams (mkSelector "setFabricFiltered:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @filterByFabric@
filterByFabricSelector :: Selector
filterByFabricSelector = mkSelector "filterByFabric"

-- | @Selector@ for @setFilterByFabric:@
setFilterByFabricSelector :: Selector
setFilterByFabricSelector = mkSelector "setFilterByFabric:"

-- | @Selector@ for @minEventNumber@
minEventNumberSelector :: Selector
minEventNumberSelector = mkSelector "minEventNumber"

-- | @Selector@ for @setMinEventNumber:@
setMinEventNumberSelector :: Selector
setMinEventNumberSelector = mkSelector "setMinEventNumber:"

-- | @Selector@ for @assumeUnknownAttributesReportable@
assumeUnknownAttributesReportableSelector :: Selector
assumeUnknownAttributesReportableSelector = mkSelector "assumeUnknownAttributesReportable"

-- | @Selector@ for @setAssumeUnknownAttributesReportable:@
setAssumeUnknownAttributesReportableSelector :: Selector
setAssumeUnknownAttributesReportableSelector = mkSelector "setAssumeUnknownAttributesReportable:"

-- | @Selector@ for @fabricFiltered@
fabricFilteredSelector :: Selector
fabricFilteredSelector = mkSelector "fabricFiltered"

-- | @Selector@ for @setFabricFiltered:@
setFabricFilteredSelector :: Selector
setFabricFilteredSelector = mkSelector "setFabricFiltered:"

