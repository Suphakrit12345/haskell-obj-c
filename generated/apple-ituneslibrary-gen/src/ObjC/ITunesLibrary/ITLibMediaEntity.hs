{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The ITLibMediaEntity class serves as the abstract superclass for ITLibMediaItem and ITLibPlaylist instances.				As the superclass, ITLibMediaEntity defines methods used by those subclasses.
--
-- Generated bindings for @ITLibMediaEntity@.
module ObjC.ITunesLibrary.ITLibMediaEntity
  ( ITLibMediaEntity
  , IsITLibMediaEntity(..)
  , valueForProperty
  , enumerateValuesForProperties_usingBlock
  , enumerateValuesExceptForProperties_usingBlock
  , persistentID
  , valueForPropertySelector
  , enumerateValuesForProperties_usingBlockSelector
  , enumerateValuesExceptForProperties_usingBlockSelector
  , persistentIDSelector


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

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Gets the value for a specified media property key.
--
-- The media property keys you can use with this property are listed in this document 			   and in Media Item Property Keys and Playlist Property Keys.
--
-- @property@ — The media property key that you want the corresponding value of.
--
-- Returns: The value for the media property key.
--
-- ObjC selector: @- valueForProperty:@
valueForProperty :: (IsITLibMediaEntity itLibMediaEntity, IsNSString property) => itLibMediaEntity -> property -> IO RawId
valueForProperty itLibMediaEntity  property =
  withObjCPtr property $ \raw_property ->
      fmap (RawId . castPtr) $ sendMsg itLibMediaEntity (mkSelector "valueForProperty:") (retPtr retVoid) [argPtr (castPtr raw_property :: Ptr ())]

-- | Executes a provided block with the fetched values for the given item properties.
--
-- Use this method to get property values in a batch fashion. 				In some cases, enumerating over a set of property keys can be more efficient 				than fetching each individual property with valueForProperty:.				The media property keys you can use with this property are listed in this document 				and in Media Item Property Keys and Playlist Property Keys.
--
-- @properties@ — A set of keys for the properties that will be enumerated, or nil to enumerate all properties.
--
-- @block@ — A block object that executes for each property in the properties set.
--
-- ObjC selector: @- enumerateValuesForProperties:usingBlock:@
enumerateValuesForProperties_usingBlock :: (IsITLibMediaEntity itLibMediaEntity, IsNSSet properties) => itLibMediaEntity -> properties -> Ptr () -> IO ()
enumerateValuesForProperties_usingBlock itLibMediaEntity  properties block =
  withObjCPtr properties $ \raw_properties ->
      sendMsg itLibMediaEntity (mkSelector "enumerateValuesForProperties:usingBlock:") retVoid [argPtr (castPtr raw_properties :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | Executes a provided block with the fetched values for all properties in the entity except for the provided set.
--
-- Use this method to get property values in a batch fashion. 				In some cases, enumerating over a set of property keys can be more efficient 				than fetching each individual property with valueForProperty:.				The media property keys you can use with this property are listed in this document 				and in Media Item Property Keys and Playlist Property Keys.
--
-- @properties@ — A set of property keys that should NOT be enumerated, or nil to enumerate all properties.
--
-- @block@ — A block object that executes for each property except for the ones in the properties set.
--
-- ObjC selector: @- enumerateValuesExceptForProperties:usingBlock:@
enumerateValuesExceptForProperties_usingBlock :: (IsITLibMediaEntity itLibMediaEntity, IsNSSet properties) => itLibMediaEntity -> properties -> Ptr () -> IO ()
enumerateValuesExceptForProperties_usingBlock itLibMediaEntity  properties block =
  withObjCPtr properties $ \raw_properties ->
      sendMsg itLibMediaEntity (mkSelector "enumerateValuesExceptForProperties:usingBlock:") retVoid [argPtr (castPtr raw_properties :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | The unique identifier of this media entity.
--
-- ObjC selector: @- persistentID@
persistentID :: IsITLibMediaEntity itLibMediaEntity => itLibMediaEntity -> IO (Id NSNumber)
persistentID itLibMediaEntity  =
    sendMsg itLibMediaEntity (mkSelector "persistentID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueForProperty:@
valueForPropertySelector :: Selector
valueForPropertySelector = mkSelector "valueForProperty:"

-- | @Selector@ for @enumerateValuesForProperties:usingBlock:@
enumerateValuesForProperties_usingBlockSelector :: Selector
enumerateValuesForProperties_usingBlockSelector = mkSelector "enumerateValuesForProperties:usingBlock:"

-- | @Selector@ for @enumerateValuesExceptForProperties:usingBlock:@
enumerateValuesExceptForProperties_usingBlockSelector :: Selector
enumerateValuesExceptForProperties_usingBlockSelector = mkSelector "enumerateValuesExceptForProperties:usingBlock:"

-- | @Selector@ for @persistentID@
persistentIDSelector :: Selector
persistentIDSelector = mkSelector "persistentID"

