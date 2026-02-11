{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterZoneInformationStruct@.
module ObjC.Matter.MTRZoneManagementClusterZoneInformationStruct
  ( MTRZoneManagementClusterZoneInformationStruct
  , IsMTRZoneManagementClusterZoneInformationStruct(..)
  , zoneID
  , setZoneID
  , zoneType
  , setZoneType
  , zoneSource
  , setZoneSource
  , twoDCartesianZone
  , setTwoDCartesianZone
  , zoneIDSelector
  , setZoneIDSelector
  , zoneTypeSelector
  , setZoneTypeSelector
  , zoneSourceSelector
  , setZoneSourceSelector
  , twoDCartesianZoneSelector
  , setTwoDCartesianZoneSelector


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

-- | @- zoneID@
zoneID :: IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct => mtrZoneManagementClusterZoneInformationStruct -> IO (Id NSNumber)
zoneID mtrZoneManagementClusterZoneInformationStruct  =
    sendMsg mtrZoneManagementClusterZoneInformationStruct (mkSelector "zoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZoneID:@
setZoneID :: (IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct, IsNSNumber value) => mtrZoneManagementClusterZoneInformationStruct -> value -> IO ()
setZoneID mtrZoneManagementClusterZoneInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneInformationStruct (mkSelector "setZoneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- zoneType@
zoneType :: IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct => mtrZoneManagementClusterZoneInformationStruct -> IO (Id NSNumber)
zoneType mtrZoneManagementClusterZoneInformationStruct  =
    sendMsg mtrZoneManagementClusterZoneInformationStruct (mkSelector "zoneType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZoneType:@
setZoneType :: (IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct, IsNSNumber value) => mtrZoneManagementClusterZoneInformationStruct -> value -> IO ()
setZoneType mtrZoneManagementClusterZoneInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneInformationStruct (mkSelector "setZoneType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- zoneSource@
zoneSource :: IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct => mtrZoneManagementClusterZoneInformationStruct -> IO (Id NSNumber)
zoneSource mtrZoneManagementClusterZoneInformationStruct  =
    sendMsg mtrZoneManagementClusterZoneInformationStruct (mkSelector "zoneSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZoneSource:@
setZoneSource :: (IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct, IsNSNumber value) => mtrZoneManagementClusterZoneInformationStruct -> value -> IO ()
setZoneSource mtrZoneManagementClusterZoneInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneInformationStruct (mkSelector "setZoneSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- twoDCartesianZone@
twoDCartesianZone :: IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct => mtrZoneManagementClusterZoneInformationStruct -> IO (Id MTRZoneManagementClusterTwoDCartesianZoneStruct)
twoDCartesianZone mtrZoneManagementClusterZoneInformationStruct  =
    sendMsg mtrZoneManagementClusterZoneInformationStruct (mkSelector "twoDCartesianZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTwoDCartesianZone:@
setTwoDCartesianZone :: (IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct, IsMTRZoneManagementClusterTwoDCartesianZoneStruct value) => mtrZoneManagementClusterZoneInformationStruct -> value -> IO ()
setTwoDCartesianZone mtrZoneManagementClusterZoneInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneInformationStruct (mkSelector "setTwoDCartesianZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @setZoneID:@
setZoneIDSelector :: Selector
setZoneIDSelector = mkSelector "setZoneID:"

-- | @Selector@ for @zoneType@
zoneTypeSelector :: Selector
zoneTypeSelector = mkSelector "zoneType"

-- | @Selector@ for @setZoneType:@
setZoneTypeSelector :: Selector
setZoneTypeSelector = mkSelector "setZoneType:"

-- | @Selector@ for @zoneSource@
zoneSourceSelector :: Selector
zoneSourceSelector = mkSelector "zoneSource"

-- | @Selector@ for @setZoneSource:@
setZoneSourceSelector :: Selector
setZoneSourceSelector = mkSelector "setZoneSource:"

-- | @Selector@ for @twoDCartesianZone@
twoDCartesianZoneSelector :: Selector
twoDCartesianZoneSelector = mkSelector "twoDCartesianZone"

-- | @Selector@ for @setTwoDCartesianZone:@
setTwoDCartesianZoneSelector :: Selector
setTwoDCartesianZoneSelector = mkSelector "setTwoDCartesianZone:"

