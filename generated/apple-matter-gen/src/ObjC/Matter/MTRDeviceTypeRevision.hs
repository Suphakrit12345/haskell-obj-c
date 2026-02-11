{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of a "device type revision" in the sense used in the Matter specification.  This has an identifier and a version number.
--
-- Generated bindings for @MTRDeviceTypeRevision@.
module ObjC.Matter.MTRDeviceTypeRevision
  ( MTRDeviceTypeRevision
  , IsMTRDeviceTypeRevision(..)
  , init_
  , new
  , initWithDeviceTypeID_revision
  , initWithDeviceTypeStruct
  , deviceTypeID
  , deviceTypeRevision
  , typeInformation
  , initSelector
  , newSelector
  , initWithDeviceTypeID_revisionSelector
  , initWithDeviceTypeStructSelector
  , deviceTypeIDSelector
  , deviceTypeRevisionSelector
  , typeInformationSelector


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
init_ :: IsMTRDeviceTypeRevision mtrDeviceTypeRevision => mtrDeviceTypeRevision -> IO (Id MTRDeviceTypeRevision)
init_ mtrDeviceTypeRevision  =
    sendMsg mtrDeviceTypeRevision (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRDeviceTypeRevision)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceTypeRevision"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The provided deviceTypeID must be in the range 0xVVVV0000-0xVVVVBFFF, where VVVV is the vendor identifier (0 for standard device types).
--
-- The provided deviceTypeRevision must be in the range 1-65535.
--
-- ObjC selector: @- initWithDeviceTypeID:revision:@
initWithDeviceTypeID_revision :: (IsMTRDeviceTypeRevision mtrDeviceTypeRevision, IsNSNumber deviceTypeID, IsNSNumber revision) => mtrDeviceTypeRevision -> deviceTypeID -> revision -> IO (Id MTRDeviceTypeRevision)
initWithDeviceTypeID_revision mtrDeviceTypeRevision  deviceTypeID revision =
  withObjCPtr deviceTypeID $ \raw_deviceTypeID ->
    withObjCPtr revision $ \raw_revision ->
        sendMsg mtrDeviceTypeRevision (mkSelector "initWithDeviceTypeID:revision:") (retPtr retVoid) [argPtr (castPtr raw_deviceTypeID :: Ptr ()), argPtr (castPtr raw_revision :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes the receiver based on the values in the specified struct.
--
-- ObjC selector: @- initWithDeviceTypeStruct:@
initWithDeviceTypeStruct :: (IsMTRDeviceTypeRevision mtrDeviceTypeRevision, IsMTRDescriptorClusterDeviceTypeStruct deviceTypeStruct) => mtrDeviceTypeRevision -> deviceTypeStruct -> IO (Id MTRDeviceTypeRevision)
initWithDeviceTypeStruct mtrDeviceTypeRevision  deviceTypeStruct =
  withObjCPtr deviceTypeStruct $ \raw_deviceTypeStruct ->
      sendMsg mtrDeviceTypeRevision (mkSelector "initWithDeviceTypeStruct:") (retPtr retVoid) [argPtr (castPtr raw_deviceTypeStruct :: Ptr ())] >>= ownedObject . castPtr

-- | @- deviceTypeID@
deviceTypeID :: IsMTRDeviceTypeRevision mtrDeviceTypeRevision => mtrDeviceTypeRevision -> IO (Id NSNumber)
deviceTypeID mtrDeviceTypeRevision  =
    sendMsg mtrDeviceTypeRevision (mkSelector "deviceTypeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deviceTypeRevision@
deviceTypeRevision :: IsMTRDeviceTypeRevision mtrDeviceTypeRevision => mtrDeviceTypeRevision -> IO (Id NSNumber)
deviceTypeRevision mtrDeviceTypeRevision  =
    sendMsg mtrDeviceTypeRevision (mkSelector "deviceTypeRevision") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the MTRDeviceType corresponding to deviceTypeID, or nil if deviceTypeID does not represent a known device type.
--
-- ObjC selector: @- typeInformation@
typeInformation :: IsMTRDeviceTypeRevision mtrDeviceTypeRevision => mtrDeviceTypeRevision -> IO (Id MTRDeviceType)
typeInformation mtrDeviceTypeRevision  =
    sendMsg mtrDeviceTypeRevision (mkSelector "typeInformation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDeviceTypeID:revision:@
initWithDeviceTypeID_revisionSelector :: Selector
initWithDeviceTypeID_revisionSelector = mkSelector "initWithDeviceTypeID:revision:"

-- | @Selector@ for @initWithDeviceTypeStruct:@
initWithDeviceTypeStructSelector :: Selector
initWithDeviceTypeStructSelector = mkSelector "initWithDeviceTypeStruct:"

-- | @Selector@ for @deviceTypeID@
deviceTypeIDSelector :: Selector
deviceTypeIDSelector = mkSelector "deviceTypeID"

-- | @Selector@ for @deviceTypeRevision@
deviceTypeRevisionSelector :: Selector
deviceTypeRevisionSelector = mkSelector "deviceTypeRevision"

-- | @Selector@ for @typeInformation@
typeInformationSelector :: Selector
typeInformationSelector = mkSelector "typeInformation"

