{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterAreaInfoStruct@.
module ObjC.Matter.MTRServiceAreaClusterAreaInfoStruct
  ( MTRServiceAreaClusterAreaInfoStruct
  , IsMTRServiceAreaClusterAreaInfoStruct(..)
  , locationInfo
  , setLocationInfo
  , landmarkInfo
  , setLandmarkInfo
  , locationInfoSelector
  , setLocationInfoSelector
  , landmarkInfoSelector
  , setLandmarkInfoSelector


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

-- | @- locationInfo@
locationInfo :: IsMTRServiceAreaClusterAreaInfoStruct mtrServiceAreaClusterAreaInfoStruct => mtrServiceAreaClusterAreaInfoStruct -> IO (Id MTRDataTypeLocationDescriptorStruct)
locationInfo mtrServiceAreaClusterAreaInfoStruct  =
    sendMsg mtrServiceAreaClusterAreaInfoStruct (mkSelector "locationInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocationInfo:@
setLocationInfo :: (IsMTRServiceAreaClusterAreaInfoStruct mtrServiceAreaClusterAreaInfoStruct, IsMTRDataTypeLocationDescriptorStruct value) => mtrServiceAreaClusterAreaInfoStruct -> value -> IO ()
setLocationInfo mtrServiceAreaClusterAreaInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterAreaInfoStruct (mkSelector "setLocationInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- landmarkInfo@
landmarkInfo :: IsMTRServiceAreaClusterAreaInfoStruct mtrServiceAreaClusterAreaInfoStruct => mtrServiceAreaClusterAreaInfoStruct -> IO (Id MTRServiceAreaClusterLandmarkInfoStruct)
landmarkInfo mtrServiceAreaClusterAreaInfoStruct  =
    sendMsg mtrServiceAreaClusterAreaInfoStruct (mkSelector "landmarkInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLandmarkInfo:@
setLandmarkInfo :: (IsMTRServiceAreaClusterAreaInfoStruct mtrServiceAreaClusterAreaInfoStruct, IsMTRServiceAreaClusterLandmarkInfoStruct value) => mtrServiceAreaClusterAreaInfoStruct -> value -> IO ()
setLandmarkInfo mtrServiceAreaClusterAreaInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterAreaInfoStruct (mkSelector "setLandmarkInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @locationInfo@
locationInfoSelector :: Selector
locationInfoSelector = mkSelector "locationInfo"

-- | @Selector@ for @setLocationInfo:@
setLocationInfoSelector :: Selector
setLocationInfoSelector = mkSelector "setLocationInfo:"

-- | @Selector@ for @landmarkInfo@
landmarkInfoSelector :: Selector
landmarkInfoSelector = mkSelector "landmarkInfo"

-- | @Selector@ for @setLandmarkInfo:@
setLandmarkInfoSelector :: Selector
setLandmarkInfoSelector = mkSelector "setLandmarkInfo:"

