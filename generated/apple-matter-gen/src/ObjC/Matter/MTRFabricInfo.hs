{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRFabricInfo@.
module ObjC.Matter.MTRFabricInfo
  ( MTRFabricInfo
  , IsMTRFabricInfo(..)
  , init_
  , new
  , rootPublicKey
  , vendorID
  , fabricID
  , nodeID
  , label
  , rootCertificate
  , rootCertificateTLV
  , intermediateCertificate
  , intermediateCertificateTLV
  , operationalCertificate
  , operationalCertificateTLV
  , fabricIndex
  , initSelector
  , newSelector
  , rootPublicKeySelector
  , vendorIDSelector
  , fabricIDSelector
  , nodeIDSelector
  , labelSelector
  , rootCertificateSelector
  , rootCertificateTLVSelector
  , intermediateCertificateSelector
  , intermediateCertificateTLVSelector
  , operationalCertificateSelector
  , operationalCertificateTLVSelector
  , fabricIndexSelector


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

-- | @- init@
init_ :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id MTRFabricInfo)
init_ mtrFabricInfo  =
    sendMsg mtrFabricInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRFabricInfo)
new  =
  do
    cls' <- getRequiredClass "MTRFabricInfo"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Root public key for the fabric.
--
-- ObjC selector: @- rootPublicKey@
rootPublicKey :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
rootPublicKey mtrFabricInfo  =
    sendMsg mtrFabricInfo (mkSelector "rootPublicKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Vendor identifier for the fabric.
--
-- ObjC selector: @- vendorID@
vendorID :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSNumber)
vendorID mtrFabricInfo  =
    sendMsg mtrFabricInfo (mkSelector "vendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Fabric identifier (scoped to the root public key) for the fabric.
--
-- ObjC selector: @- fabricID@
fabricID :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSNumber)
fabricID mtrFabricInfo  =
    sendMsg mtrFabricInfo (mkSelector "fabricID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Node identifier for the given node on the fabric.
--
-- ObjC selector: @- nodeID@
nodeID :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSNumber)
nodeID mtrFabricInfo  =
    sendMsg mtrFabricInfo (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The string label for the fabric.  May be empty.
--
-- ObjC selector: @- label@
label :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSString)
label mtrFabricInfo  =
    sendMsg mtrFabricInfo (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The root certificate for the fabric.  This might be nil if a root certificate is not available (e.g. if this is information about some remote node that we don't have root certificate information for).
--
-- ObjC selector: @- rootCertificate@
rootCertificate :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
rootCertificate mtrFabricInfo  =
    sendMsg mtrFabricInfo (mkSelector "rootCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The same root certificate as rootCertificate, in Matter TLV format.
--
-- ObjC selector: @- rootCertificateTLV@
rootCertificateTLV :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
rootCertificateTLV mtrFabricInfo  =
    sendMsg mtrFabricInfo (mkSelector "rootCertificateTLV") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The intermediate certificate for the node.  This might be nil if there is no intermediate certificate, or if the node is not on a fabric we have access to.
--
-- ObjC selector: @- intermediateCertificate@
intermediateCertificate :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
intermediateCertificate mtrFabricInfo  =
    sendMsg mtrFabricInfo (mkSelector "intermediateCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The same intermediate certificate as intermediateCertificate, in Matter TLV format.
--
-- ObjC selector: @- intermediateCertificateTLV@
intermediateCertificateTLV :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
intermediateCertificateTLV mtrFabricInfo  =
    sendMsg mtrFabricInfo (mkSelector "intermediateCertificateTLV") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The operational certificate for the node.  This might be nil if the node is not on a fabric we have access to.
--
-- ObjC selector: @- operationalCertificate@
operationalCertificate :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
operationalCertificate mtrFabricInfo  =
    sendMsg mtrFabricInfo (mkSelector "operationalCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The same operational certificate as operationalCertificate, in Matter TLV format.
--
-- ObjC selector: @- operationalCertificateTLV@
operationalCertificateTLV :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
operationalCertificateTLV mtrFabricInfo  =
    sendMsg mtrFabricInfo (mkSelector "operationalCertificateTLV") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The fabric index which identifies the fabric on the node.
--
-- ObjC selector: @- fabricIndex@
fabricIndex :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSNumber)
fabricIndex mtrFabricInfo  =
    sendMsg mtrFabricInfo (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @rootPublicKey@
rootPublicKeySelector :: Selector
rootPublicKeySelector = mkSelector "rootPublicKey"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @fabricID@
fabricIDSelector :: Selector
fabricIDSelector = mkSelector "fabricID"

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @rootCertificate@
rootCertificateSelector :: Selector
rootCertificateSelector = mkSelector "rootCertificate"

-- | @Selector@ for @rootCertificateTLV@
rootCertificateTLVSelector :: Selector
rootCertificateTLVSelector = mkSelector "rootCertificateTLV"

-- | @Selector@ for @intermediateCertificate@
intermediateCertificateSelector :: Selector
intermediateCertificateSelector = mkSelector "intermediateCertificate"

-- | @Selector@ for @intermediateCertificateTLV@
intermediateCertificateTLVSelector :: Selector
intermediateCertificateTLVSelector = mkSelector "intermediateCertificateTLV"

-- | @Selector@ for @operationalCertificate@
operationalCertificateSelector :: Selector
operationalCertificateSelector = mkSelector "operationalCertificate"

-- | @Selector@ for @operationalCertificateTLV@
operationalCertificateTLVSelector :: Selector
operationalCertificateTLVSelector = mkSelector "operationalCertificateTLV"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

