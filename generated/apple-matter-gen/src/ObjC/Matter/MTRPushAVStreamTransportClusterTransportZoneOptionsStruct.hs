{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterTransportZoneOptionsStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterTransportZoneOptionsStruct
  ( MTRPushAVStreamTransportClusterTransportZoneOptionsStruct
  , IsMTRPushAVStreamTransportClusterTransportZoneOptionsStruct(..)
  , zone
  , setZone
  , sensitivity
  , setSensitivity
  , zoneSelector
  , setZoneSelector
  , sensitivitySelector
  , setSensitivitySelector


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

-- | @- zone@
zone :: IsMTRPushAVStreamTransportClusterTransportZoneOptionsStruct mtrPushAVStreamTransportClusterTransportZoneOptionsStruct => mtrPushAVStreamTransportClusterTransportZoneOptionsStruct -> IO (Id NSNumber)
zone mtrPushAVStreamTransportClusterTransportZoneOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportZoneOptionsStruct (mkSelector "zone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZone:@
setZone :: (IsMTRPushAVStreamTransportClusterTransportZoneOptionsStruct mtrPushAVStreamTransportClusterTransportZoneOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportZoneOptionsStruct -> value -> IO ()
setZone mtrPushAVStreamTransportClusterTransportZoneOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportZoneOptionsStruct (mkSelector "setZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sensitivity@
sensitivity :: IsMTRPushAVStreamTransportClusterTransportZoneOptionsStruct mtrPushAVStreamTransportClusterTransportZoneOptionsStruct => mtrPushAVStreamTransportClusterTransportZoneOptionsStruct -> IO (Id NSNumber)
sensitivity mtrPushAVStreamTransportClusterTransportZoneOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportZoneOptionsStruct (mkSelector "sensitivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSensitivity:@
setSensitivity :: (IsMTRPushAVStreamTransportClusterTransportZoneOptionsStruct mtrPushAVStreamTransportClusterTransportZoneOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportZoneOptionsStruct -> value -> IO ()
setSensitivity mtrPushAVStreamTransportClusterTransportZoneOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportZoneOptionsStruct (mkSelector "setSensitivity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zone@
zoneSelector :: Selector
zoneSelector = mkSelector "zone"

-- | @Selector@ for @setZone:@
setZoneSelector :: Selector
setZoneSelector = mkSelector "setZone:"

-- | @Selector@ for @sensitivity@
sensitivitySelector :: Selector
sensitivitySelector = mkSelector "sensitivity"

-- | @Selector@ for @setSensitivity:@
setSensitivitySelector :: Selector
setSensitivitySelector = mkSelector "setSensitivity:"

