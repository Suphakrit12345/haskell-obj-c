{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterLandmarkInfoStruct@.
module ObjC.Matter.MTRServiceAreaClusterLandmarkInfoStruct
  ( MTRServiceAreaClusterLandmarkInfoStruct
  , IsMTRServiceAreaClusterLandmarkInfoStruct(..)
  , landmarkTag
  , setLandmarkTag
  , relativePositionTag
  , setRelativePositionTag
  , landmarkTagSelector
  , setLandmarkTagSelector
  , relativePositionTagSelector
  , setRelativePositionTagSelector


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

-- | @- landmarkTag@
landmarkTag :: IsMTRServiceAreaClusterLandmarkInfoStruct mtrServiceAreaClusterLandmarkInfoStruct => mtrServiceAreaClusterLandmarkInfoStruct -> IO (Id NSNumber)
landmarkTag mtrServiceAreaClusterLandmarkInfoStruct  =
    sendMsg mtrServiceAreaClusterLandmarkInfoStruct (mkSelector "landmarkTag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLandmarkTag:@
setLandmarkTag :: (IsMTRServiceAreaClusterLandmarkInfoStruct mtrServiceAreaClusterLandmarkInfoStruct, IsNSNumber value) => mtrServiceAreaClusterLandmarkInfoStruct -> value -> IO ()
setLandmarkTag mtrServiceAreaClusterLandmarkInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterLandmarkInfoStruct (mkSelector "setLandmarkTag:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- relativePositionTag@
relativePositionTag :: IsMTRServiceAreaClusterLandmarkInfoStruct mtrServiceAreaClusterLandmarkInfoStruct => mtrServiceAreaClusterLandmarkInfoStruct -> IO (Id NSNumber)
relativePositionTag mtrServiceAreaClusterLandmarkInfoStruct  =
    sendMsg mtrServiceAreaClusterLandmarkInfoStruct (mkSelector "relativePositionTag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRelativePositionTag:@
setRelativePositionTag :: (IsMTRServiceAreaClusterLandmarkInfoStruct mtrServiceAreaClusterLandmarkInfoStruct, IsNSNumber value) => mtrServiceAreaClusterLandmarkInfoStruct -> value -> IO ()
setRelativePositionTag mtrServiceAreaClusterLandmarkInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterLandmarkInfoStruct (mkSelector "setRelativePositionTag:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @landmarkTag@
landmarkTagSelector :: Selector
landmarkTagSelector = mkSelector "landmarkTag"

-- | @Selector@ for @setLandmarkTag:@
setLandmarkTagSelector :: Selector
setLandmarkTagSelector = mkSelector "setLandmarkTag:"

-- | @Selector@ for @relativePositionTag@
relativePositionTagSelector :: Selector
relativePositionTagSelector = mkSelector "relativePositionTag"

-- | @Selector@ for @setRelativePositionTag:@
setRelativePositionTagSelector :: Selector
setRelativePositionTagSelector = mkSelector "setRelativePositionTag:"

