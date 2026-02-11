{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterAudioCapabilitiesStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterAudioCapabilitiesStruct
  ( MTRCameraAVStreamManagementClusterAudioCapabilitiesStruct
  , IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct(..)
  , maxNumberOfChannels
  , setMaxNumberOfChannels
  , supportedCodecs
  , setSupportedCodecs
  , supportedSampleRates
  , setSupportedSampleRates
  , supportedBitDepths
  , setSupportedBitDepths
  , maxNumberOfChannelsSelector
  , setMaxNumberOfChannelsSelector
  , supportedCodecsSelector
  , setSupportedCodecsSelector
  , supportedSampleRatesSelector
  , setSupportedSampleRatesSelector
  , supportedBitDepthsSelector
  , setSupportedBitDepthsSelector


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

-- | @- maxNumberOfChannels@
maxNumberOfChannels :: IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> IO (Id NSNumber)
maxNumberOfChannels mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct  =
    sendMsg mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct (mkSelector "maxNumberOfChannels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxNumberOfChannels:@
setMaxNumberOfChannels :: (IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> value -> IO ()
setMaxNumberOfChannels mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct (mkSelector "setMaxNumberOfChannels:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supportedCodecs@
supportedCodecs :: IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> IO (Id NSArray)
supportedCodecs mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct  =
    sendMsg mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct (mkSelector "supportedCodecs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupportedCodecs:@
setSupportedCodecs :: (IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct, IsNSArray value) => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> value -> IO ()
setSupportedCodecs mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct (mkSelector "setSupportedCodecs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supportedSampleRates@
supportedSampleRates :: IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> IO (Id NSArray)
supportedSampleRates mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct  =
    sendMsg mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct (mkSelector "supportedSampleRates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupportedSampleRates:@
setSupportedSampleRates :: (IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct, IsNSArray value) => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> value -> IO ()
setSupportedSampleRates mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct (mkSelector "setSupportedSampleRates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supportedBitDepths@
supportedBitDepths :: IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> IO (Id NSArray)
supportedBitDepths mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct  =
    sendMsg mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct (mkSelector "supportedBitDepths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupportedBitDepths:@
setSupportedBitDepths :: (IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct, IsNSArray value) => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> value -> IO ()
setSupportedBitDepths mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct (mkSelector "setSupportedBitDepths:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxNumberOfChannels@
maxNumberOfChannelsSelector :: Selector
maxNumberOfChannelsSelector = mkSelector "maxNumberOfChannels"

-- | @Selector@ for @setMaxNumberOfChannels:@
setMaxNumberOfChannelsSelector :: Selector
setMaxNumberOfChannelsSelector = mkSelector "setMaxNumberOfChannels:"

-- | @Selector@ for @supportedCodecs@
supportedCodecsSelector :: Selector
supportedCodecsSelector = mkSelector "supportedCodecs"

-- | @Selector@ for @setSupportedCodecs:@
setSupportedCodecsSelector :: Selector
setSupportedCodecsSelector = mkSelector "setSupportedCodecs:"

-- | @Selector@ for @supportedSampleRates@
supportedSampleRatesSelector :: Selector
supportedSampleRatesSelector = mkSelector "supportedSampleRates"

-- | @Selector@ for @setSupportedSampleRates:@
setSupportedSampleRatesSelector :: Selector
setSupportedSampleRatesSelector = mkSelector "setSupportedSampleRates:"

-- | @Selector@ for @supportedBitDepths@
supportedBitDepthsSelector :: Selector
supportedBitDepthsSelector = mkSelector "supportedBitDepths"

-- | @Selector@ for @setSupportedBitDepths:@
setSupportedBitDepthsSelector :: Selector
setSupportedBitDepthsSelector = mkSelector "setSupportedBitDepths:"

