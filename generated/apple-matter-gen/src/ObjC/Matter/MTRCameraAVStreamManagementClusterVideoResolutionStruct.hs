{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterVideoResolutionStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterVideoResolutionStruct
  ( MTRCameraAVStreamManagementClusterVideoResolutionStruct
  , IsMTRCameraAVStreamManagementClusterVideoResolutionStruct(..)
  , width
  , setWidth
  , height
  , setHeight
  , widthSelector
  , setWidthSelector
  , heightSelector
  , setHeightSelector


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

-- | @- width@
width :: IsMTRCameraAVStreamManagementClusterVideoResolutionStruct mtrCameraAVStreamManagementClusterVideoResolutionStruct => mtrCameraAVStreamManagementClusterVideoResolutionStruct -> IO (Id NSNumber)
width mtrCameraAVStreamManagementClusterVideoResolutionStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoResolutionStruct (mkSelector "width") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidth:@
setWidth :: (IsMTRCameraAVStreamManagementClusterVideoResolutionStruct mtrCameraAVStreamManagementClusterVideoResolutionStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoResolutionStruct -> value -> IO ()
setWidth mtrCameraAVStreamManagementClusterVideoResolutionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoResolutionStruct (mkSelector "setWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- height@
height :: IsMTRCameraAVStreamManagementClusterVideoResolutionStruct mtrCameraAVStreamManagementClusterVideoResolutionStruct => mtrCameraAVStreamManagementClusterVideoResolutionStruct -> IO (Id NSNumber)
height mtrCameraAVStreamManagementClusterVideoResolutionStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoResolutionStruct (mkSelector "height") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeight:@
setHeight :: (IsMTRCameraAVStreamManagementClusterVideoResolutionStruct mtrCameraAVStreamManagementClusterVideoResolutionStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoResolutionStruct -> value -> IO ()
setHeight mtrCameraAVStreamManagementClusterVideoResolutionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoResolutionStruct (mkSelector "setHeight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

