{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceControllerExternalCertificateParameters@.
module ObjC.Matter.MTRDeviceControllerExternalCertificateParameters
  ( MTRDeviceControllerExternalCertificateParameters
  , IsMTRDeviceControllerExternalCertificateParameters(..)
  , init_
  , new
  , initWithStorageDelegate_storageDelegateQueue_uniqueIdentifier_ipk_vendorID_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate
  , rootCertificate
  , initSelector
  , newSelector
  , initWithStorageDelegate_storageDelegateQueue_uniqueIdentifier_ipk_vendorID_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector
  , rootCertificateSelector


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
init_ :: IsMTRDeviceControllerExternalCertificateParameters mtrDeviceControllerExternalCertificateParameters => mtrDeviceControllerExternalCertificateParameters -> IO (Id MTRDeviceControllerExternalCertificateParameters)
init_ mtrDeviceControllerExternalCertificateParameters  =
    sendMsg mtrDeviceControllerExternalCertificateParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRDeviceControllerExternalCertificateParameters)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceControllerExternalCertificateParameters"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Prepare to initialize a controller that is not able to sign operational certificates itself, and therefore needs to be provided with a complete operational certificate chain.
--
-- A controller created from MTRDeviceControllerStartupParams initialized with this method will not be able to commission devices unless operationalCertificateIssuer and operationalCertificateIssuerQueue are set.
--
-- The fabric id and node id to use for the controller will be derived from the provided operationalCertificate.
--
-- @storageDelegate@ — The storage to use for the controller.  This will be                        called into on storageDelegateQueue.
--
-- @storageDelegateQueue@ — The queue for calls into storageDelegate.  See                             MTRDeviceControllerStorageDelegate documentation                             for the rules about what work is allowed to be                             done on this queue.
--
-- @uniqueIdentifier@ — The unique id to assign to the controller.
--
-- @vendorID@ — The vendor ID (allocated by the Connectivity Standards Alliance) for                 this controller. Must not be the "standard" vendor id (0).
--
-- @ipk@ — The Identity Protection Key. Must be 16 bytes in length.
--
-- @intermediateCertificate@ — Must be nil if operationalCertificate is                                directly signed by rootCertificate.  Otherwise                                must be the certificate that signed                                operationalCertificate.
--
-- ObjC selector: @- initWithStorageDelegate:storageDelegateQueue:uniqueIdentifier:ipk:vendorID:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:@
initWithStorageDelegate_storageDelegateQueue_uniqueIdentifier_ipk_vendorID_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate :: (IsMTRDeviceControllerExternalCertificateParameters mtrDeviceControllerExternalCertificateParameters, IsNSObject storageDelegateQueue, IsNSUUID uniqueIdentifier, IsNSData ipk, IsNSNumber vendorID, IsNSData operationalCertificate, IsNSData intermediateCertificate, IsNSData rootCertificate) => mtrDeviceControllerExternalCertificateParameters -> RawId -> storageDelegateQueue -> uniqueIdentifier -> ipk -> vendorID -> RawId -> operationalCertificate -> intermediateCertificate -> rootCertificate -> IO (Id MTRDeviceControllerExternalCertificateParameters)
initWithStorageDelegate_storageDelegateQueue_uniqueIdentifier_ipk_vendorID_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate mtrDeviceControllerExternalCertificateParameters  storageDelegate storageDelegateQueue uniqueIdentifier ipk vendorID operationalKeypair operationalCertificate intermediateCertificate rootCertificate =
  withObjCPtr storageDelegateQueue $ \raw_storageDelegateQueue ->
    withObjCPtr uniqueIdentifier $ \raw_uniqueIdentifier ->
      withObjCPtr ipk $ \raw_ipk ->
        withObjCPtr vendorID $ \raw_vendorID ->
          withObjCPtr operationalCertificate $ \raw_operationalCertificate ->
            withObjCPtr intermediateCertificate $ \raw_intermediateCertificate ->
              withObjCPtr rootCertificate $ \raw_rootCertificate ->
                  sendMsg mtrDeviceControllerExternalCertificateParameters (mkSelector "initWithStorageDelegate:storageDelegateQueue:uniqueIdentifier:ipk:vendorID:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:") (retPtr retVoid) [argPtr (castPtr (unRawId storageDelegate) :: Ptr ()), argPtr (castPtr raw_storageDelegateQueue :: Ptr ()), argPtr (castPtr raw_uniqueIdentifier :: Ptr ()), argPtr (castPtr raw_ipk :: Ptr ()), argPtr (castPtr raw_vendorID :: Ptr ()), argPtr (castPtr (unRawId operationalKeypair) :: Ptr ()), argPtr (castPtr raw_operationalCertificate :: Ptr ()), argPtr (castPtr raw_intermediateCertificate :: Ptr ()), argPtr (castPtr raw_rootCertificate :: Ptr ())] >>= ownedObject . castPtr

-- | The root certificate we were initialized with.
--
-- ObjC selector: @- rootCertificate@
rootCertificate :: IsMTRDeviceControllerExternalCertificateParameters mtrDeviceControllerExternalCertificateParameters => mtrDeviceControllerExternalCertificateParameters -> IO (Id NSData)
rootCertificate mtrDeviceControllerExternalCertificateParameters  =
    sendMsg mtrDeviceControllerExternalCertificateParameters (mkSelector "rootCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithStorageDelegate:storageDelegateQueue:uniqueIdentifier:ipk:vendorID:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:@
initWithStorageDelegate_storageDelegateQueue_uniqueIdentifier_ipk_vendorID_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector :: Selector
initWithStorageDelegate_storageDelegateQueue_uniqueIdentifier_ipk_vendorID_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector = mkSelector "initWithStorageDelegate:storageDelegateQueue:uniqueIdentifier:ipk:vendorID:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:"

-- | @Selector@ for @rootCertificate@
rootCertificateSelector :: Selector
rootCertificateSelector = mkSelector "rootCertificate"

