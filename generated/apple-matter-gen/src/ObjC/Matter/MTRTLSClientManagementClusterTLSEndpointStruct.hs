{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSClientManagementClusterTLSEndpointStruct@.
module ObjC.Matter.MTRTLSClientManagementClusterTLSEndpointStruct
  ( MTRTLSClientManagementClusterTLSEndpointStruct
  , IsMTRTLSClientManagementClusterTLSEndpointStruct(..)
  , endpointID
  , setEndpointID
  , hostname
  , setHostname
  , port
  , setPort
  , caid
  , setCaid
  , ccdid
  , setCcdid
  , referenceCount
  , setReferenceCount
  , fabricIndex
  , setFabricIndex
  , endpointIDSelector
  , setEndpointIDSelector
  , hostnameSelector
  , setHostnameSelector
  , portSelector
  , setPortSelector
  , caidSelector
  , setCaidSelector
  , ccdidSelector
  , setCcdidSelector
  , referenceCountSelector
  , setReferenceCountSelector
  , fabricIndexSelector
  , setFabricIndexSelector


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

-- | @- endpointID@
endpointID :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSNumber)
endpointID mtrtlsClientManagementClusterTLSEndpointStruct  =
    sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "endpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpointID:@
setEndpointID :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSNumber value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setEndpointID mtrtlsClientManagementClusterTLSEndpointStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "setEndpointID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hostname@
hostname :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSData)
hostname mtrtlsClientManagementClusterTLSEndpointStruct  =
    sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "hostname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHostname:@
setHostname :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSData value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setHostname mtrtlsClientManagementClusterTLSEndpointStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "setHostname:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- port@
port :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSNumber)
port mtrtlsClientManagementClusterTLSEndpointStruct  =
    sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "port") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPort:@
setPort :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSNumber value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setPort mtrtlsClientManagementClusterTLSEndpointStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "setPort:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- caid@
caid :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSNumber)
caid mtrtlsClientManagementClusterTLSEndpointStruct  =
    sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "caid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaid:@
setCaid :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSNumber value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setCaid mtrtlsClientManagementClusterTLSEndpointStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "setCaid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ccdid@
ccdid :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSNumber)
ccdid mtrtlsClientManagementClusterTLSEndpointStruct  =
    sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "ccdid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSNumber value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setCcdid mtrtlsClientManagementClusterTLSEndpointStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "setCcdid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- referenceCount@
referenceCount :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSNumber)
referenceCount mtrtlsClientManagementClusterTLSEndpointStruct  =
    sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "referenceCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReferenceCount:@
setReferenceCount :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSNumber value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setReferenceCount mtrtlsClientManagementClusterTLSEndpointStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "setReferenceCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSNumber)
fabricIndex mtrtlsClientManagementClusterTLSEndpointStruct  =
    sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSNumber value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setFabricIndex mtrtlsClientManagementClusterTLSEndpointStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterTLSEndpointStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector
setEndpointIDSelector = mkSelector "setEndpointID:"

-- | @Selector@ for @hostname@
hostnameSelector :: Selector
hostnameSelector = mkSelector "hostname"

-- | @Selector@ for @setHostname:@
setHostnameSelector :: Selector
setHostnameSelector = mkSelector "setHostname:"

-- | @Selector@ for @port@
portSelector :: Selector
portSelector = mkSelector "port"

-- | @Selector@ for @setPort:@
setPortSelector :: Selector
setPortSelector = mkSelector "setPort:"

-- | @Selector@ for @caid@
caidSelector :: Selector
caidSelector = mkSelector "caid"

-- | @Selector@ for @setCaid:@
setCaidSelector :: Selector
setCaidSelector = mkSelector "setCaid:"

-- | @Selector@ for @ccdid@
ccdidSelector :: Selector
ccdidSelector = mkSelector "ccdid"

-- | @Selector@ for @setCcdid:@
setCcdidSelector :: Selector
setCcdidSelector = mkSelector "setCcdid:"

-- | @Selector@ for @referenceCount@
referenceCountSelector :: Selector
referenceCountSelector = mkSelector "referenceCount"

-- | @Selector@ for @setReferenceCount:@
setReferenceCountSelector :: Selector
setReferenceCountSelector = mkSelector "setReferenceCount:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

