{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct
  ( MTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct
  , IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct(..)
  , codec
  , setCodec
  , resolution
  , setResolution
  , minBitRate
  , setMinBitRate
  , codecSelector
  , setCodecSelector
  , resolutionSelector
  , setResolutionSelector
  , minBitRateSelector
  , setMinBitRateSelector


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

-- | @- codec@
codec :: IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct => mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct -> IO (Id NSNumber)
codec mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct  =
    sendMsg mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct (mkSelector "codec") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCodec:@
setCodec :: (IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct -> value -> IO ()
setCodec mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct (mkSelector "setCodec:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- resolution@
resolution :: IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct => mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
resolution mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct  =
    sendMsg mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct (mkSelector "resolution") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setResolution:@
setResolution :: (IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct -> value -> IO ()
setResolution mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct (mkSelector "setResolution:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minBitRate@
minBitRate :: IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct => mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct -> IO (Id NSNumber)
minBitRate mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct  =
    sendMsg mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct (mkSelector "minBitRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinBitRate:@
setMinBitRate :: (IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct -> value -> IO ()
setMinBitRate mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct (mkSelector "setMinBitRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @codec@
codecSelector :: Selector
codecSelector = mkSelector "codec"

-- | @Selector@ for @setCodec:@
setCodecSelector :: Selector
setCodecSelector = mkSelector "setCodec:"

-- | @Selector@ for @resolution@
resolutionSelector :: Selector
resolutionSelector = mkSelector "resolution"

-- | @Selector@ for @setResolution:@
setResolutionSelector :: Selector
setResolutionSelector = mkSelector "setResolution:"

-- | @Selector@ for @minBitRate@
minBitRateSelector :: Selector
minBitRateSelector = mkSelector "minBitRate"

-- | @Selector@ for @setMinBitRate:@
setMinBitRateSelector :: Selector
setMinBitRateSelector = mkSelector "setMinBitRate:"

