{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of an attribute implemented on a server cluster by an MTRDeviceController.  An attribute has an identifier and a value, and may or may not be writable.
--
-- MTRServerAttribute's API can be accessed from any thread.
--
-- Generated bindings for @MTRServerAttribute@.
module ObjC.Matter.MTRServerAttribute
  ( MTRServerAttribute
  , IsMTRServerAttribute(..)
  , init_
  , new
  , initReadonlyAttributeWithID_initialValue_requiredPrivilege
  , setValue
  , newFeatureMapAttributeWithInitialValue
  , attributeID
  , value
  , requiredReadPrivilege
  , writable
  , initSelector
  , newSelector
  , initReadonlyAttributeWithID_initialValue_requiredPrivilegeSelector
  , setValueSelector
  , newFeatureMapAttributeWithInitialValueSelector
  , attributeIDSelector
  , valueSelector
  , requiredReadPrivilegeSelector
  , writableSelector

  -- * Enum types
  , MTRAccessControlEntryPrivilege(MTRAccessControlEntryPrivilege)
  , pattern MTRAccessControlEntryPrivilegeView
  , pattern MTRAccessControlEntryPrivilegeProxyView
  , pattern MTRAccessControlEntryPrivilegeOperate
  , pattern MTRAccessControlEntryPrivilegeManage
  , pattern MTRAccessControlEntryPrivilegeAdminister

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
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRServerAttribute mtrServerAttribute => mtrServerAttribute -> IO (Id MTRServerAttribute)
init_ mtrServerAttribute  =
    sendMsg mtrServerAttribute (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRServerAttribute)
new  =
  do
    cls' <- getRequiredClass "MTRServerAttribute"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initialize as a readonly attribute.  The value is a data-value as documented in MTRBaseDevice.h.
--
-- Will fail if the attribute ID is not valid per the Matter specification or the attribute value is not a valid data-value.
--
-- requiredPrivilege is the privilege required to read the attribute. This initializer may fail if the provided attributeID is a global attribute and the provided requiredPrivilege value is not correct for that attribute ID.
--
-- ObjC selector: @- initReadonlyAttributeWithID:initialValue:requiredPrivilege:@
initReadonlyAttributeWithID_initialValue_requiredPrivilege :: (IsMTRServerAttribute mtrServerAttribute, IsNSNumber attributeID, IsNSDictionary value) => mtrServerAttribute -> attributeID -> value -> MTRAccessControlEntryPrivilege -> IO (Id MTRServerAttribute)
initReadonlyAttributeWithID_initialValue_requiredPrivilege mtrServerAttribute  attributeID value requiredPrivilege =
  withObjCPtr attributeID $ \raw_attributeID ->
    withObjCPtr value $ \raw_value ->
        sendMsg mtrServerAttribute (mkSelector "initReadonlyAttributeWithID:initialValue:requiredPrivilege:") (retPtr retVoid) [argPtr (castPtr raw_attributeID :: Ptr ()), argPtr (castPtr raw_value :: Ptr ()), argCUChar (coerce requiredPrivilege)] >>= ownedObject . castPtr

-- | Change the value of the attribute to a new value.  The value is a data-value as documented in MTRBaseDevice.h.
--
-- Will fail if the attribute is not a valid data-value.
--
-- ObjC selector: @- setValue:@
setValue :: (IsMTRServerAttribute mtrServerAttribute, IsNSDictionary value) => mtrServerAttribute -> value -> IO Bool
setValue mtrServerAttribute  value =
  withObjCPtr value $ \raw_value ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrServerAttribute (mkSelector "setValue:") retCULong [argPtr (castPtr raw_value :: Ptr ())]

-- | Create an attribute description for a FeatureMap attribute with the provided value (expected to be an unsigned integer representing the value of the bitmap). This will automatically set requiredPrivilege to the right value for FeatureMap.
--
-- ObjC selector: @+ newFeatureMapAttributeWithInitialValue:@
newFeatureMapAttributeWithInitialValue :: IsNSNumber value => value -> IO (Id MTRServerAttribute)
newFeatureMapAttributeWithInitialValue value =
  do
    cls' <- getRequiredClass "MTRServerAttribute"
    withObjCPtr value $ \raw_value ->
      sendClassMsg cls' (mkSelector "newFeatureMapAttributeWithInitialValue:") (retPtr retVoid) [argPtr (castPtr raw_value :: Ptr ())] >>= ownedObject . castPtr

-- | @- attributeID@
attributeID :: IsMTRServerAttribute mtrServerAttribute => mtrServerAttribute -> IO (Id NSNumber)
attributeID mtrServerAttribute  =
    sendMsg mtrServerAttribute (mkSelector "attributeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- value@
value :: IsMTRServerAttribute mtrServerAttribute => mtrServerAttribute -> IO (Id NSDictionary)
value mtrServerAttribute  =
    sendMsg mtrServerAttribute (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The privilege level necessary to read this attribute.
--
-- ObjC selector: @- requiredReadPrivilege@
requiredReadPrivilege :: IsMTRServerAttribute mtrServerAttribute => mtrServerAttribute -> IO MTRAccessControlEntryPrivilege
requiredReadPrivilege mtrServerAttribute  =
    fmap (coerce :: CUChar -> MTRAccessControlEntryPrivilege) $ sendMsg mtrServerAttribute (mkSelector "requiredReadPrivilege") retCUChar []

-- | @- writable@
writable :: IsMTRServerAttribute mtrServerAttribute => mtrServerAttribute -> IO Bool
writable mtrServerAttribute  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrServerAttribute (mkSelector "writable") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initReadonlyAttributeWithID:initialValue:requiredPrivilege:@
initReadonlyAttributeWithID_initialValue_requiredPrivilegeSelector :: Selector
initReadonlyAttributeWithID_initialValue_requiredPrivilegeSelector = mkSelector "initReadonlyAttributeWithID:initialValue:requiredPrivilege:"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @newFeatureMapAttributeWithInitialValue:@
newFeatureMapAttributeWithInitialValueSelector :: Selector
newFeatureMapAttributeWithInitialValueSelector = mkSelector "newFeatureMapAttributeWithInitialValue:"

-- | @Selector@ for @attributeID@
attributeIDSelector :: Selector
attributeIDSelector = mkSelector "attributeID"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @requiredReadPrivilege@
requiredReadPrivilegeSelector :: Selector
requiredReadPrivilegeSelector = mkSelector "requiredReadPrivilege"

-- | @Selector@ for @writable@
writableSelector :: Selector
writableSelector = mkSelector "writable"

