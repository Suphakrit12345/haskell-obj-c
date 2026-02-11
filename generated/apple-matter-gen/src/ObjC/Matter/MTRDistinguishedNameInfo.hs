{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents the Matter-specific components of an X.509 Distinguished Name.
--
-- Generated bindings for @MTRDistinguishedNameInfo@.
module ObjC.Matter.MTRDistinguishedNameInfo
  ( MTRDistinguishedNameInfo
  , IsMTRDistinguishedNameInfo(..)
  , new
  , init_
  , nodeID
  , fabricID
  , rootCACertificateID
  , intermediateCACertificateID
  , caseAuthenticatedTags
  , newSelector
  , initSelector
  , nodeIDSelector
  , fabricIDSelector
  , rootCACertificateIDSelector
  , intermediateCACertificateIDSelector
  , caseAuthenticatedTagsSelector


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

-- | @+ new@
new :: IO (Id MTRDistinguishedNameInfo)
new  =
  do
    cls' <- getRequiredClass "MTRDistinguishedNameInfo"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMTRDistinguishedNameInfo mtrDistinguishedNameInfo => mtrDistinguishedNameInfo -> IO (Id MTRDistinguishedNameInfo)
init_ mtrDistinguishedNameInfo  =
    sendMsg mtrDistinguishedNameInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The Node ID contained in the DN, if any.  Will be non-nil for the subject of a valid node operational certificate.
--
-- ObjC selector: @- nodeID@
nodeID :: IsMTRDistinguishedNameInfo mtrDistinguishedNameInfo => mtrDistinguishedNameInfo -> IO (Id NSNumber)
nodeID mtrDistinguishedNameInfo  =
    sendMsg mtrDistinguishedNameInfo (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Fabric ID contained in the DN, if any.  Will be non-nil for the subject of a valid node operational certificate, and may be non-nil for the subject of a valid intermediate or root certificate.
--
-- ObjC selector: @- fabricID@
fabricID :: IsMTRDistinguishedNameInfo mtrDistinguishedNameInfo => mtrDistinguishedNameInfo -> IO (Id NSNumber)
fabricID mtrDistinguishedNameInfo  =
    sendMsg mtrDistinguishedNameInfo (mkSelector "fabricID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The @RCAC@ ID contained in the DN, if any.  Will be non-nil for the subject of a valid root certificate.
--
-- ObjC selector: @- rootCACertificateID@
rootCACertificateID :: IsMTRDistinguishedNameInfo mtrDistinguishedNameInfo => mtrDistinguishedNameInfo -> IO (Id NSNumber)
rootCACertificateID mtrDistinguishedNameInfo  =
    sendMsg mtrDistinguishedNameInfo (mkSelector "rootCACertificateID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The @ICAC@ ID contained in the DN, if any.  Will be non-nil for the subject of a valid intermediate certificate.
--
-- ObjC selector: @- intermediateCACertificateID@
intermediateCACertificateID :: IsMTRDistinguishedNameInfo mtrDistinguishedNameInfo => mtrDistinguishedNameInfo -> IO (Id NSNumber)
intermediateCACertificateID mtrDistinguishedNameInfo  =
    sendMsg mtrDistinguishedNameInfo (mkSelector "intermediateCACertificateID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The set of CASE Authenticated Tags contained in the DN.  Maybe be non-empty for the subject of a valid node operational certificate.
--
-- ObjC selector: @- caseAuthenticatedTags@
caseAuthenticatedTags :: IsMTRDistinguishedNameInfo mtrDistinguishedNameInfo => mtrDistinguishedNameInfo -> IO (Id NSSet)
caseAuthenticatedTags mtrDistinguishedNameInfo  =
    sendMsg mtrDistinguishedNameInfo (mkSelector "caseAuthenticatedTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @fabricID@
fabricIDSelector :: Selector
fabricIDSelector = mkSelector "fabricID"

-- | @Selector@ for @rootCACertificateID@
rootCACertificateIDSelector :: Selector
rootCACertificateIDSelector = mkSelector "rootCACertificateID"

-- | @Selector@ for @intermediateCACertificateID@
intermediateCACertificateIDSelector :: Selector
intermediateCACertificateIDSelector = mkSelector "intermediateCACertificateID"

-- | @Selector@ for @caseAuthenticatedTags@
caseAuthenticatedTagsSelector :: Selector
caseAuthenticatedTagsSelector = mkSelector "caseAuthenticatedTags"

