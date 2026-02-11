{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterAreaStruct@.
module ObjC.Matter.MTRServiceAreaClusterAreaStruct
  ( MTRServiceAreaClusterAreaStruct
  , IsMTRServiceAreaClusterAreaStruct(..)
  , areaID
  , setAreaID
  , mapID
  , setMapID
  , areaInfo
  , setAreaInfo
  , areaIDSelector
  , setAreaIDSelector
  , mapIDSelector
  , setMapIDSelector
  , areaInfoSelector
  , setAreaInfoSelector


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

-- | @- areaID@
areaID :: IsMTRServiceAreaClusterAreaStruct mtrServiceAreaClusterAreaStruct => mtrServiceAreaClusterAreaStruct -> IO (Id NSNumber)
areaID mtrServiceAreaClusterAreaStruct  =
    sendMsg mtrServiceAreaClusterAreaStruct (mkSelector "areaID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAreaID:@
setAreaID :: (IsMTRServiceAreaClusterAreaStruct mtrServiceAreaClusterAreaStruct, IsNSNumber value) => mtrServiceAreaClusterAreaStruct -> value -> IO ()
setAreaID mtrServiceAreaClusterAreaStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterAreaStruct (mkSelector "setAreaID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mapID@
mapID :: IsMTRServiceAreaClusterAreaStruct mtrServiceAreaClusterAreaStruct => mtrServiceAreaClusterAreaStruct -> IO (Id NSNumber)
mapID mtrServiceAreaClusterAreaStruct  =
    sendMsg mtrServiceAreaClusterAreaStruct (mkSelector "mapID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMapID:@
setMapID :: (IsMTRServiceAreaClusterAreaStruct mtrServiceAreaClusterAreaStruct, IsNSNumber value) => mtrServiceAreaClusterAreaStruct -> value -> IO ()
setMapID mtrServiceAreaClusterAreaStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterAreaStruct (mkSelector "setMapID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- areaInfo@
areaInfo :: IsMTRServiceAreaClusterAreaStruct mtrServiceAreaClusterAreaStruct => mtrServiceAreaClusterAreaStruct -> IO (Id MTRServiceAreaClusterAreaInfoStruct)
areaInfo mtrServiceAreaClusterAreaStruct  =
    sendMsg mtrServiceAreaClusterAreaStruct (mkSelector "areaInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAreaInfo:@
setAreaInfo :: (IsMTRServiceAreaClusterAreaStruct mtrServiceAreaClusterAreaStruct, IsMTRServiceAreaClusterAreaInfoStruct value) => mtrServiceAreaClusterAreaStruct -> value -> IO ()
setAreaInfo mtrServiceAreaClusterAreaStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterAreaStruct (mkSelector "setAreaInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @areaID@
areaIDSelector :: Selector
areaIDSelector = mkSelector "areaID"

-- | @Selector@ for @setAreaID:@
setAreaIDSelector :: Selector
setAreaIDSelector = mkSelector "setAreaID:"

-- | @Selector@ for @mapID@
mapIDSelector :: Selector
mapIDSelector = mkSelector "mapID"

-- | @Selector@ for @setMapID:@
setMapIDSelector :: Selector
setMapIDSelector = mkSelector "setMapID:"

-- | @Selector@ for @areaInfo@
areaInfoSelector :: Selector
areaInfoSelector = mkSelector "areaInfo"

-- | @Selector@ for @setAreaInfo:@
setAreaInfoSelector :: Selector
setAreaInfoSelector = mkSelector "setAreaInfo:"

