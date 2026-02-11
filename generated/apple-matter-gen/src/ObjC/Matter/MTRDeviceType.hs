{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Meta-data about a device type defined in the Matter specification.
--
-- Generated bindings for @MTRDeviceType@.
module ObjC.Matter.MTRDeviceType
  ( MTRDeviceType
  , IsMTRDeviceType(..)
  , deviceTypeForID
  , init_
  , new
  , id_
  , name
  , isUtility
  , deviceTypeForIDSelector
  , initSelector
  , newSelector
  , idSelector
  , nameSelector
  , isUtilitySelector


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

-- | Returns an MTRDeviceType for the given ID, if the ID is known.  Returns nil for unknown IDs.
--
-- ObjC selector: @+ deviceTypeForID:@
deviceTypeForID :: IsNSNumber deviceTypeID => deviceTypeID -> IO (Id MTRDeviceType)
deviceTypeForID deviceTypeID =
  do
    cls' <- getRequiredClass "MTRDeviceType"
    withObjCPtr deviceTypeID $ \raw_deviceTypeID ->
      sendClassMsg cls' (mkSelector "deviceTypeForID:") (retPtr retVoid) [argPtr (castPtr raw_deviceTypeID :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRDeviceType mtrDeviceType => mtrDeviceType -> IO (Id MTRDeviceType)
init_ mtrDeviceType  =
    sendMsg mtrDeviceType (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRDeviceType)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceType"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The identifier of the device type (32-bit unsigned integer).
--
-- ObjC selector: @- id@
id_ :: IsMTRDeviceType mtrDeviceType => mtrDeviceType -> IO (Id NSNumber)
id_ mtrDeviceType  =
    sendMsg mtrDeviceType (mkSelector "id") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the name of the device type.
--
-- ObjC selector: @- name@
name :: IsMTRDeviceType mtrDeviceType => mtrDeviceType -> IO (Id NSString)
name mtrDeviceType  =
    sendMsg mtrDeviceType (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns whether this is a utility device type.
--
-- ObjC selector: @- isUtility@
isUtility :: IsMTRDeviceType mtrDeviceType => mtrDeviceType -> IO Bool
isUtility mtrDeviceType  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceType (mkSelector "isUtility") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceTypeForID:@
deviceTypeForIDSelector :: Selector
deviceTypeForIDSelector = mkSelector "deviceTypeForID:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @id@
idSelector :: Selector
idSelector = mkSelector "id"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @isUtility@
isUtilitySelector :: Selector
isUtilitySelector = mkSelector "isUtility"

