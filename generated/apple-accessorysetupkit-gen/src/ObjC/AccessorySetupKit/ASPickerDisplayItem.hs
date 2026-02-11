{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASPickerDisplayItem@.
module ObjC.AccessorySetupKit.ASPickerDisplayItem
  ( ASPickerDisplayItem
  , IsASPickerDisplayItem(..)
  , initWithName_productImage_descriptor
  , init_
  , new
  , name
  , descriptor
  , renameOptions
  , setRenameOptions
  , setupOptions
  , setSetupOptions
  , initWithName_productImage_descriptorSelector
  , initSelector
  , newSelector
  , nameSelector
  , descriptorSelector
  , renameOptionsSelector
  , setRenameOptionsSelector
  , setupOptionsSelector
  , setSetupOptionsSelector

  -- * Enum types
  , ASAccessoryRenameOptions(ASAccessoryRenameOptions)
  , pattern ASAccessoryRenameSSID
  , ASPickerDisplayItemSetupOptions(ASPickerDisplayItemSetupOptions)
  , pattern ASPickerDisplayItemSetupRename
  , pattern ASPickerDisplayItemSetupConfirmAuthorization
  , pattern ASPickerDisplayItemSetupFinishInApp

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

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.AccessorySetupKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a picker display item with a name and image to display and a descriptor to match discovered accessories. - Parameters:   - name: The accessory name to display in the picker.   - productImage: An image of the accessory to display in the picker.   - descriptor: A descriptor that the picker uses to determine which discovered accessories to display.
--
-- ObjC selector: @- initWithName:productImage:descriptor:@
initWithName_productImage_descriptor :: (IsASPickerDisplayItem asPickerDisplayItem, IsNSString name, IsASDiscoveryDescriptor descriptor) => asPickerDisplayItem -> name -> RawId -> descriptor -> IO (Id ASPickerDisplayItem)
initWithName_productImage_descriptor asPickerDisplayItem  name productImage descriptor =
  withObjCPtr name $ \raw_name ->
    withObjCPtr descriptor $ \raw_descriptor ->
        sendMsg asPickerDisplayItem (mkSelector "initWithName:productImage:descriptor:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId productImage) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> IO (Id ASPickerDisplayItem)
init_ asPickerDisplayItem  =
    sendMsg asPickerDisplayItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- new@
new :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> IO (Id ASPickerDisplayItem)
new asPickerDisplayItem  =
    sendMsg asPickerDisplayItem (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The accessory name to display in the picker.
--
-- ObjC selector: @- name@
name :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> IO (Id NSString)
name asPickerDisplayItem  =
    sendMsg asPickerDisplayItem (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A descriptor that the picker uses to determine which discovered accessories to display.
--
-- ObjC selector: @- descriptor@
descriptor :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> IO (Id ASDiscoveryDescriptor)
descriptor asPickerDisplayItem  =
    sendMsg asPickerDisplayItem (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Options to allow renaming a matched accessory.
--
-- To permit renaming, include ``SetupOptions-swift.struct/rename`` in the ``setupOptions-c.property``
--
-- ObjC selector: @- renameOptions@
renameOptions :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> IO ASAccessoryRenameOptions
renameOptions asPickerDisplayItem  =
    fmap (coerce :: CULong -> ASAccessoryRenameOptions) $ sendMsg asPickerDisplayItem (mkSelector "renameOptions") retCULong []

-- | Options to allow renaming a matched accessory.
--
-- To permit renaming, include ``SetupOptions-swift.struct/rename`` in the ``setupOptions-c.property``
--
-- ObjC selector: @- setRenameOptions:@
setRenameOptions :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> ASAccessoryRenameOptions -> IO ()
setRenameOptions asPickerDisplayItem  value =
    sendMsg asPickerDisplayItem (mkSelector "setRenameOptions:") retVoid [argCULong (coerce value)]

-- | Custom setup options for the accessory.
--
-- ObjC selector: @- setupOptions@
setupOptions :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> IO ASPickerDisplayItemSetupOptions
setupOptions asPickerDisplayItem  =
    fmap (coerce :: CULong -> ASPickerDisplayItemSetupOptions) $ sendMsg asPickerDisplayItem (mkSelector "setupOptions") retCULong []

-- | Custom setup options for the accessory.
--
-- ObjC selector: @- setSetupOptions:@
setSetupOptions :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> ASPickerDisplayItemSetupOptions -> IO ()
setSetupOptions asPickerDisplayItem  value =
    sendMsg asPickerDisplayItem (mkSelector "setSetupOptions:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:productImage:descriptor:@
initWithName_productImage_descriptorSelector :: Selector
initWithName_productImage_descriptorSelector = mkSelector "initWithName:productImage:descriptor:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @renameOptions@
renameOptionsSelector :: Selector
renameOptionsSelector = mkSelector "renameOptions"

-- | @Selector@ for @setRenameOptions:@
setRenameOptionsSelector :: Selector
setRenameOptionsSelector = mkSelector "setRenameOptions:"

-- | @Selector@ for @setupOptions@
setupOptionsSelector :: Selector
setupOptionsSelector = mkSelector "setupOptions"

-- | @Selector@ for @setSetupOptions:@
setSetupOptionsSelector :: Selector
setSetupOptionsSelector = mkSelector "setSetupOptions:"

