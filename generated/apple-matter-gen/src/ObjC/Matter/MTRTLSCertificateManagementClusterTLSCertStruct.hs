{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterTLSCertStruct@.
module ObjC.Matter.MTRTLSCertificateManagementClusterTLSCertStruct
  ( MTRTLSCertificateManagementClusterTLSCertStruct
  , IsMTRTLSCertificateManagementClusterTLSCertStruct(..)
  , caid
  , setCaid
  , certificate
  , setCertificate
  , fabricIndex
  , setFabricIndex
  , caidSelector
  , setCaidSelector
  , certificateSelector
  , setCertificateSelector
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

-- | @- caid@
caid :: IsMTRTLSCertificateManagementClusterTLSCertStruct mtrtlsCertificateManagementClusterTLSCertStruct => mtrtlsCertificateManagementClusterTLSCertStruct -> IO (Id NSNumber)
caid mtrtlsCertificateManagementClusterTLSCertStruct  =
    sendMsg mtrtlsCertificateManagementClusterTLSCertStruct (mkSelector "caid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaid:@
setCaid :: (IsMTRTLSCertificateManagementClusterTLSCertStruct mtrtlsCertificateManagementClusterTLSCertStruct, IsNSNumber value) => mtrtlsCertificateManagementClusterTLSCertStruct -> value -> IO ()
setCaid mtrtlsCertificateManagementClusterTLSCertStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterTLSCertStruct (mkSelector "setCaid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- certificate@
certificate :: IsMTRTLSCertificateManagementClusterTLSCertStruct mtrtlsCertificateManagementClusterTLSCertStruct => mtrtlsCertificateManagementClusterTLSCertStruct -> IO (Id NSData)
certificate mtrtlsCertificateManagementClusterTLSCertStruct  =
    sendMsg mtrtlsCertificateManagementClusterTLSCertStruct (mkSelector "certificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCertificate:@
setCertificate :: (IsMTRTLSCertificateManagementClusterTLSCertStruct mtrtlsCertificateManagementClusterTLSCertStruct, IsNSData value) => mtrtlsCertificateManagementClusterTLSCertStruct -> value -> IO ()
setCertificate mtrtlsCertificateManagementClusterTLSCertStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterTLSCertStruct (mkSelector "setCertificate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRTLSCertificateManagementClusterTLSCertStruct mtrtlsCertificateManagementClusterTLSCertStruct => mtrtlsCertificateManagementClusterTLSCertStruct -> IO (Id NSNumber)
fabricIndex mtrtlsCertificateManagementClusterTLSCertStruct  =
    sendMsg mtrtlsCertificateManagementClusterTLSCertStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRTLSCertificateManagementClusterTLSCertStruct mtrtlsCertificateManagementClusterTLSCertStruct, IsNSNumber value) => mtrtlsCertificateManagementClusterTLSCertStruct -> value -> IO ()
setFabricIndex mtrtlsCertificateManagementClusterTLSCertStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterTLSCertStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @caid@
caidSelector :: Selector
caidSelector = mkSelector "caid"

-- | @Selector@ for @setCaid:@
setCaidSelector :: Selector
setCaidSelector = mkSelector "setCaid:"

-- | @Selector@ for @certificate@
certificateSelector :: Selector
certificateSelector = mkSelector "certificate"

-- | @Selector@ for @setCertificate:@
setCertificateSelector :: Selector
setCertificateSelector = mkSelector "setCertificate:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

