{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents
  ( MTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents
  , IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents(..)
  , activeTimestampPresent
  , setActiveTimestampPresent
  , pendingTimestampPresent
  , setPendingTimestampPresent
  , masterKeyPresent
  , setMasterKeyPresent
  , networkNamePresent
  , setNetworkNamePresent
  , extendedPanIdPresent
  , setExtendedPanIdPresent
  , meshLocalPrefixPresent
  , setMeshLocalPrefixPresent
  , delayPresent
  , setDelayPresent
  , panIdPresent
  , setPanIdPresent
  , channelPresent
  , setChannelPresent
  , pskcPresent
  , setPskcPresent
  , securityPolicyPresent
  , setSecurityPolicyPresent
  , channelMaskPresent
  , setChannelMaskPresent
  , activeTimestampPresentSelector
  , setActiveTimestampPresentSelector
  , pendingTimestampPresentSelector
  , setPendingTimestampPresentSelector
  , masterKeyPresentSelector
  , setMasterKeyPresentSelector
  , networkNamePresentSelector
  , setNetworkNamePresentSelector
  , extendedPanIdPresentSelector
  , setExtendedPanIdPresentSelector
  , meshLocalPrefixPresentSelector
  , setMeshLocalPrefixPresentSelector
  , delayPresentSelector
  , setDelayPresentSelector
  , panIdPresentSelector
  , setPanIdPresentSelector
  , channelPresentSelector
  , setChannelPresentSelector
  , pskcPresentSelector
  , setPskcPresentSelector
  , securityPolicyPresentSelector
  , setSecurityPolicyPresentSelector
  , channelMaskPresentSelector
  , setChannelMaskPresentSelector


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

-- | @- activeTimestampPresent@
activeTimestampPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
activeTimestampPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  =
    sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "activeTimestampPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActiveTimestampPresent:@
setActiveTimestampPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setActiveTimestampPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "setActiveTimestampPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pendingTimestampPresent@
pendingTimestampPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
pendingTimestampPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  =
    sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "pendingTimestampPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPendingTimestampPresent:@
setPendingTimestampPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setPendingTimestampPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "setPendingTimestampPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- masterKeyPresent@
masterKeyPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
masterKeyPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  =
    sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "masterKeyPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMasterKeyPresent:@
setMasterKeyPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setMasterKeyPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "setMasterKeyPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- networkNamePresent@
networkNamePresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
networkNamePresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  =
    sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "networkNamePresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNetworkNamePresent:@
setNetworkNamePresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setNetworkNamePresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "setNetworkNamePresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- extendedPanIdPresent@
extendedPanIdPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
extendedPanIdPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  =
    sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "extendedPanIdPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtendedPanIdPresent:@
setExtendedPanIdPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setExtendedPanIdPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "setExtendedPanIdPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- meshLocalPrefixPresent@
meshLocalPrefixPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
meshLocalPrefixPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  =
    sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "meshLocalPrefixPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMeshLocalPrefixPresent:@
setMeshLocalPrefixPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setMeshLocalPrefixPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "setMeshLocalPrefixPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delayPresent@
delayPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
delayPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  =
    sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "delayPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDelayPresent:@
setDelayPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setDelayPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "setDelayPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- panIdPresent@
panIdPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
panIdPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  =
    sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "panIdPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPanIdPresent:@
setPanIdPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setPanIdPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "setPanIdPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- channelPresent@
channelPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
channelPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  =
    sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "channelPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChannelPresent:@
setChannelPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setChannelPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "setChannelPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pskcPresent@
pskcPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
pskcPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  =
    sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "pskcPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPskcPresent:@
setPskcPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setPskcPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "setPskcPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- securityPolicyPresent@
securityPolicyPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
securityPolicyPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  =
    sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "securityPolicyPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSecurityPolicyPresent:@
setSecurityPolicyPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setSecurityPolicyPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "setSecurityPolicyPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- channelMaskPresent@
channelMaskPresent :: IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> IO (Id NSNumber)
channelMaskPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  =
    sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "channelMaskPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChannelMaskPresent:@
setChannelMaskPresent :: (IsMTRThreadNetworkDiagnosticsClusterOperationalDatasetComponents mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents -> value -> IO ()
setChannelMaskPresent mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterOperationalDatasetComponents (mkSelector "setChannelMaskPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @activeTimestampPresent@
activeTimestampPresentSelector :: Selector
activeTimestampPresentSelector = mkSelector "activeTimestampPresent"

-- | @Selector@ for @setActiveTimestampPresent:@
setActiveTimestampPresentSelector :: Selector
setActiveTimestampPresentSelector = mkSelector "setActiveTimestampPresent:"

-- | @Selector@ for @pendingTimestampPresent@
pendingTimestampPresentSelector :: Selector
pendingTimestampPresentSelector = mkSelector "pendingTimestampPresent"

-- | @Selector@ for @setPendingTimestampPresent:@
setPendingTimestampPresentSelector :: Selector
setPendingTimestampPresentSelector = mkSelector "setPendingTimestampPresent:"

-- | @Selector@ for @masterKeyPresent@
masterKeyPresentSelector :: Selector
masterKeyPresentSelector = mkSelector "masterKeyPresent"

-- | @Selector@ for @setMasterKeyPresent:@
setMasterKeyPresentSelector :: Selector
setMasterKeyPresentSelector = mkSelector "setMasterKeyPresent:"

-- | @Selector@ for @networkNamePresent@
networkNamePresentSelector :: Selector
networkNamePresentSelector = mkSelector "networkNamePresent"

-- | @Selector@ for @setNetworkNamePresent:@
setNetworkNamePresentSelector :: Selector
setNetworkNamePresentSelector = mkSelector "setNetworkNamePresent:"

-- | @Selector@ for @extendedPanIdPresent@
extendedPanIdPresentSelector :: Selector
extendedPanIdPresentSelector = mkSelector "extendedPanIdPresent"

-- | @Selector@ for @setExtendedPanIdPresent:@
setExtendedPanIdPresentSelector :: Selector
setExtendedPanIdPresentSelector = mkSelector "setExtendedPanIdPresent:"

-- | @Selector@ for @meshLocalPrefixPresent@
meshLocalPrefixPresentSelector :: Selector
meshLocalPrefixPresentSelector = mkSelector "meshLocalPrefixPresent"

-- | @Selector@ for @setMeshLocalPrefixPresent:@
setMeshLocalPrefixPresentSelector :: Selector
setMeshLocalPrefixPresentSelector = mkSelector "setMeshLocalPrefixPresent:"

-- | @Selector@ for @delayPresent@
delayPresentSelector :: Selector
delayPresentSelector = mkSelector "delayPresent"

-- | @Selector@ for @setDelayPresent:@
setDelayPresentSelector :: Selector
setDelayPresentSelector = mkSelector "setDelayPresent:"

-- | @Selector@ for @panIdPresent@
panIdPresentSelector :: Selector
panIdPresentSelector = mkSelector "panIdPresent"

-- | @Selector@ for @setPanIdPresent:@
setPanIdPresentSelector :: Selector
setPanIdPresentSelector = mkSelector "setPanIdPresent:"

-- | @Selector@ for @channelPresent@
channelPresentSelector :: Selector
channelPresentSelector = mkSelector "channelPresent"

-- | @Selector@ for @setChannelPresent:@
setChannelPresentSelector :: Selector
setChannelPresentSelector = mkSelector "setChannelPresent:"

-- | @Selector@ for @pskcPresent@
pskcPresentSelector :: Selector
pskcPresentSelector = mkSelector "pskcPresent"

-- | @Selector@ for @setPskcPresent:@
setPskcPresentSelector :: Selector
setPskcPresentSelector = mkSelector "setPskcPresent:"

-- | @Selector@ for @securityPolicyPresent@
securityPolicyPresentSelector :: Selector
securityPolicyPresentSelector = mkSelector "securityPolicyPresent"

-- | @Selector@ for @setSecurityPolicyPresent:@
setSecurityPolicyPresentSelector :: Selector
setSecurityPolicyPresentSelector = mkSelector "setSecurityPolicyPresent:"

-- | @Selector@ for @channelMaskPresent@
channelMaskPresentSelector :: Selector
channelMaskPresentSelector = mkSelector "channelMaskPresent"

-- | @Selector@ for @setChannelMaskPresent:@
setChannelMaskPresentSelector :: Selector
setChannelMaskPresentSelector = mkSelector "setChannelMaskPresent:"

