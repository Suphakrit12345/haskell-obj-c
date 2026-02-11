{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSRInfo@.
module ObjC.Matter.CSRInfo
  ( CSRInfo
  , IsCSRInfo(..)
  , initWithNonce_elements_elementsSignature_csr
  , nonce
  , setNonce
  , elements
  , setElements
  , elementsSignature
  , setElementsSignature
  , csr
  , setCsr
  , initWithNonce_elements_elementsSignature_csrSelector
  , nonceSelector
  , setNonceSelector
  , elementsSelector
  , setElementsSelector
  , elementsSignatureSelector
  , setElementsSignatureSelector
  , csrSelector
  , setCsrSelector


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

-- | @- initWithNonce:elements:elementsSignature:csr:@
initWithNonce_elements_elementsSignature_csr :: (IsCSRInfo csrInfo, IsNSData nonce, IsNSData elements, IsNSData elementsSignature, IsNSData csr) => csrInfo -> nonce -> elements -> elementsSignature -> csr -> IO (Id CSRInfo)
initWithNonce_elements_elementsSignature_csr csrInfo  nonce elements elementsSignature csr =
  withObjCPtr nonce $ \raw_nonce ->
    withObjCPtr elements $ \raw_elements ->
      withObjCPtr elementsSignature $ \raw_elementsSignature ->
        withObjCPtr csr $ \raw_csr ->
            sendMsg csrInfo (mkSelector "initWithNonce:elements:elementsSignature:csr:") (retPtr retVoid) [argPtr (castPtr raw_nonce :: Ptr ()), argPtr (castPtr raw_elements :: Ptr ()), argPtr (castPtr raw_elementsSignature :: Ptr ()), argPtr (castPtr raw_csr :: Ptr ())] >>= ownedObject . castPtr

-- | @- nonce@
nonce :: IsCSRInfo csrInfo => csrInfo -> IO (Id NSData)
nonce csrInfo  =
    sendMsg csrInfo (mkSelector "nonce") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNonce:@
setNonce :: (IsCSRInfo csrInfo, IsNSData value) => csrInfo -> value -> IO ()
setNonce csrInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg csrInfo (mkSelector "setNonce:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- elements@
elements :: IsCSRInfo csrInfo => csrInfo -> IO (Id NSData)
elements csrInfo  =
    sendMsg csrInfo (mkSelector "elements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setElements:@
setElements :: (IsCSRInfo csrInfo, IsNSData value) => csrInfo -> value -> IO ()
setElements csrInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg csrInfo (mkSelector "setElements:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- elementsSignature@
elementsSignature :: IsCSRInfo csrInfo => csrInfo -> IO (Id NSData)
elementsSignature csrInfo  =
    sendMsg csrInfo (mkSelector "elementsSignature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setElementsSignature:@
setElementsSignature :: (IsCSRInfo csrInfo, IsNSData value) => csrInfo -> value -> IO ()
setElementsSignature csrInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg csrInfo (mkSelector "setElementsSignature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- csr@
csr :: IsCSRInfo csrInfo => csrInfo -> IO (Id NSData)
csr csrInfo  =
    sendMsg csrInfo (mkSelector "csr") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCsr:@
setCsr :: (IsCSRInfo csrInfo, IsNSData value) => csrInfo -> value -> IO ()
setCsr csrInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg csrInfo (mkSelector "setCsr:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNonce:elements:elementsSignature:csr:@
initWithNonce_elements_elementsSignature_csrSelector :: Selector
initWithNonce_elements_elementsSignature_csrSelector = mkSelector "initWithNonce:elements:elementsSignature:csr:"

-- | @Selector@ for @nonce@
nonceSelector :: Selector
nonceSelector = mkSelector "nonce"

-- | @Selector@ for @setNonce:@
setNonceSelector :: Selector
setNonceSelector = mkSelector "setNonce:"

-- | @Selector@ for @elements@
elementsSelector :: Selector
elementsSelector = mkSelector "elements"

-- | @Selector@ for @setElements:@
setElementsSelector :: Selector
setElementsSelector = mkSelector "setElements:"

-- | @Selector@ for @elementsSignature@
elementsSignatureSelector :: Selector
elementsSignatureSelector = mkSelector "elementsSignature"

-- | @Selector@ for @setElementsSignature:@
setElementsSignatureSelector :: Selector
setElementsSignatureSelector = mkSelector "setElementsSignature:"

-- | @Selector@ for @csr@
csrSelector :: Selector
csrSelector = mkSelector "csr"

-- | @Selector@ for @setCsr:@
setCsrSelector :: Selector
setCsrSelector = mkSelector "setCsr:"

