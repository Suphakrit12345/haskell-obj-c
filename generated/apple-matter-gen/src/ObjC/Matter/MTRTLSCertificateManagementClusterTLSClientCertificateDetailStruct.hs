{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct@.
module ObjC.Matter.MTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct
  ( MTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct
  , IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct(..)
  , ccdid
  , setCcdid
  , clientCertificate
  , setClientCertificate
  , intermediateCertificates
  , setIntermediateCertificates
  , fabricIndex
  , setFabricIndex
  , ccdidSelector
  , setCcdidSelector
  , clientCertificateSelector
  , setClientCertificateSelector
  , intermediateCertificatesSelector
  , setIntermediateCertificatesSelector
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

-- | @- ccdid@
ccdid :: IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> IO (Id NSNumber)
ccdid mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct  =
    sendMsg mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct (mkSelector "ccdid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct, IsNSNumber value) => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> value -> IO ()
setCcdid mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct (mkSelector "setCcdid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- clientCertificate@
clientCertificate :: IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> IO (Id NSData)
clientCertificate mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct  =
    sendMsg mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct (mkSelector "clientCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClientCertificate:@
setClientCertificate :: (IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct, IsNSData value) => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> value -> IO ()
setClientCertificate mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct (mkSelector "setClientCertificate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- intermediateCertificates@
intermediateCertificates :: IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> IO (Id NSArray)
intermediateCertificates mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct  =
    sendMsg mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct (mkSelector "intermediateCertificates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIntermediateCertificates:@
setIntermediateCertificates :: (IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct, IsNSArray value) => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> value -> IO ()
setIntermediateCertificates mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct (mkSelector "setIntermediateCertificates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> IO (Id NSNumber)
fabricIndex mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct  =
    sendMsg mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct, IsNSNumber value) => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> value -> IO ()
setFabricIndex mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ccdid@
ccdidSelector :: Selector
ccdidSelector = mkSelector "ccdid"

-- | @Selector@ for @setCcdid:@
setCcdidSelector :: Selector
setCcdidSelector = mkSelector "setCcdid:"

-- | @Selector@ for @clientCertificate@
clientCertificateSelector :: Selector
clientCertificateSelector = mkSelector "clientCertificate"

-- | @Selector@ for @setClientCertificate:@
setClientCertificateSelector :: Selector
setClientCertificateSelector = mkSelector "setClientCertificate:"

-- | @Selector@ for @intermediateCertificates@
intermediateCertificatesSelector :: Selector
intermediateCertificatesSelector = mkSelector "intermediateCertificates"

-- | @Selector@ for @setIntermediateCertificates:@
setIntermediateCertificatesSelector :: Selector
setIntermediateCertificatesSelector = mkSelector "setIntermediateCertificates:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

