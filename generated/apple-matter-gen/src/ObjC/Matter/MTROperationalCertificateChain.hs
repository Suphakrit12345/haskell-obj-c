{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of the operational certificate chain for a node.
--
-- Generated bindings for @MTROperationalCertificateChain@.
module ObjC.Matter.MTROperationalCertificateChain
  ( MTROperationalCertificateChain
  , IsMTROperationalCertificateChain(..)
  , init_
  , new
  , initWithOperationalCertificate_intermediateCertificate_rootCertificate_adminSubject
  , operationalCertificate
  , setOperationalCertificate
  , intermediateCertificate
  , setIntermediateCertificate
  , rootCertificate
  , setRootCertificate
  , adminSubject
  , setAdminSubject
  , initSelector
  , newSelector
  , initWithOperationalCertificate_intermediateCertificate_rootCertificate_adminSubjectSelector
  , operationalCertificateSelector
  , setOperationalCertificateSelector
  , intermediateCertificateSelector
  , setIntermediateCertificateSelector
  , rootCertificateSelector
  , setRootCertificateSelector
  , adminSubjectSelector
  , setAdminSubjectSelector


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
init_ :: IsMTROperationalCertificateChain mtrOperationalCertificateChain => mtrOperationalCertificateChain -> IO (Id MTROperationalCertificateChain)
init_ mtrOperationalCertificateChain  =
    sendMsg mtrOperationalCertificateChain (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTROperationalCertificateChain)
new  =
  do
    cls' <- getRequiredClass "MTROperationalCertificateChain"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithOperationalCertificate:intermediateCertificate:rootCertificate:adminSubject:@
initWithOperationalCertificate_intermediateCertificate_rootCertificate_adminSubject :: (IsMTROperationalCertificateChain mtrOperationalCertificateChain, IsNSData operationalCertificate, IsNSData intermediateCertificate, IsNSData rootCertificate, IsNSNumber adminSubject) => mtrOperationalCertificateChain -> operationalCertificate -> intermediateCertificate -> rootCertificate -> adminSubject -> IO (Id MTROperationalCertificateChain)
initWithOperationalCertificate_intermediateCertificate_rootCertificate_adminSubject mtrOperationalCertificateChain  operationalCertificate intermediateCertificate rootCertificate adminSubject =
  withObjCPtr operationalCertificate $ \raw_operationalCertificate ->
    withObjCPtr intermediateCertificate $ \raw_intermediateCertificate ->
      withObjCPtr rootCertificate $ \raw_rootCertificate ->
        withObjCPtr adminSubject $ \raw_adminSubject ->
            sendMsg mtrOperationalCertificateChain (mkSelector "initWithOperationalCertificate:intermediateCertificate:rootCertificate:adminSubject:") (retPtr retVoid) [argPtr (castPtr raw_operationalCertificate :: Ptr ()), argPtr (castPtr raw_intermediateCertificate :: Ptr ()), argPtr (castPtr raw_rootCertificate :: Ptr ()), argPtr (castPtr raw_adminSubject :: Ptr ())] >>= ownedObject . castPtr

-- | @- operationalCertificate@
operationalCertificate :: IsMTROperationalCertificateChain mtrOperationalCertificateChain => mtrOperationalCertificateChain -> IO (Id NSData)
operationalCertificate mtrOperationalCertificateChain  =
    sendMsg mtrOperationalCertificateChain (mkSelector "operationalCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationalCertificate:@
setOperationalCertificate :: (IsMTROperationalCertificateChain mtrOperationalCertificateChain, IsNSData value) => mtrOperationalCertificateChain -> value -> IO ()
setOperationalCertificate mtrOperationalCertificateChain  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCertificateChain (mkSelector "setOperationalCertificate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A nil intermediateCertificate means there is no intermediate.
--
-- ObjC selector: @- intermediateCertificate@
intermediateCertificate :: IsMTROperationalCertificateChain mtrOperationalCertificateChain => mtrOperationalCertificateChain -> IO (Id NSData)
intermediateCertificate mtrOperationalCertificateChain  =
    sendMsg mtrOperationalCertificateChain (mkSelector "intermediateCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A nil intermediateCertificate means there is no intermediate.
--
-- ObjC selector: @- setIntermediateCertificate:@
setIntermediateCertificate :: (IsMTROperationalCertificateChain mtrOperationalCertificateChain, IsNSData value) => mtrOperationalCertificateChain -> value -> IO ()
setIntermediateCertificate mtrOperationalCertificateChain  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCertificateChain (mkSelector "setIntermediateCertificate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rootCertificate@
rootCertificate :: IsMTROperationalCertificateChain mtrOperationalCertificateChain => mtrOperationalCertificateChain -> IO (Id NSData)
rootCertificate mtrOperationalCertificateChain  =
    sendMsg mtrOperationalCertificateChain (mkSelector "rootCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRootCertificate:@
setRootCertificate :: (IsMTROperationalCertificateChain mtrOperationalCertificateChain, IsNSData value) => mtrOperationalCertificateChain -> value -> IO ()
setRootCertificate mtrOperationalCertificateChain  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCertificateChain (mkSelector "setRootCertificate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | adminSubject is passed to the device as part of the AddNOC command.  A nil adminSubject means the node id of the relevant MTRDeviceController will be used.
--
-- ObjC selector: @- adminSubject@
adminSubject :: IsMTROperationalCertificateChain mtrOperationalCertificateChain => mtrOperationalCertificateChain -> IO (Id NSNumber)
adminSubject mtrOperationalCertificateChain  =
    sendMsg mtrOperationalCertificateChain (mkSelector "adminSubject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | adminSubject is passed to the device as part of the AddNOC command.  A nil adminSubject means the node id of the relevant MTRDeviceController will be used.
--
-- ObjC selector: @- setAdminSubject:@
setAdminSubject :: (IsMTROperationalCertificateChain mtrOperationalCertificateChain, IsNSNumber value) => mtrOperationalCertificateChain -> value -> IO ()
setAdminSubject mtrOperationalCertificateChain  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCertificateChain (mkSelector "setAdminSubject:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithOperationalCertificate:intermediateCertificate:rootCertificate:adminSubject:@
initWithOperationalCertificate_intermediateCertificate_rootCertificate_adminSubjectSelector :: Selector
initWithOperationalCertificate_intermediateCertificate_rootCertificate_adminSubjectSelector = mkSelector "initWithOperationalCertificate:intermediateCertificate:rootCertificate:adminSubject:"

-- | @Selector@ for @operationalCertificate@
operationalCertificateSelector :: Selector
operationalCertificateSelector = mkSelector "operationalCertificate"

-- | @Selector@ for @setOperationalCertificate:@
setOperationalCertificateSelector :: Selector
setOperationalCertificateSelector = mkSelector "setOperationalCertificate:"

-- | @Selector@ for @intermediateCertificate@
intermediateCertificateSelector :: Selector
intermediateCertificateSelector = mkSelector "intermediateCertificate"

-- | @Selector@ for @setIntermediateCertificate:@
setIntermediateCertificateSelector :: Selector
setIntermediateCertificateSelector = mkSelector "setIntermediateCertificate:"

-- | @Selector@ for @rootCertificate@
rootCertificateSelector :: Selector
rootCertificateSelector = mkSelector "rootCertificate"

-- | @Selector@ for @setRootCertificate:@
setRootCertificateSelector :: Selector
setRootCertificateSelector = mkSelector "setRootCertificate:"

-- | @Selector@ for @adminSubject@
adminSubjectSelector :: Selector
adminSubjectSelector = mkSelector "adminSubject"

-- | @Selector@ for @setAdminSubject:@
setAdminSubjectSelector :: Selector
setAdminSubjectSelector = mkSelector "setAdminSubject:"

