{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A directory structure for files and folders.
--
-- A file system, depending on its type, provides one or more volumes to clients. The ``FSUnaryFileSystem`` by definition provides only one volume, while an ``FSFileSystem`` supports multiple volumes.
--
-- You implement a volume for your file system type by subclassing this class, and also conforming to the ``FSVolume/Operations`` and ``FSVolume/PathConfOperations`` protocols. This protocol defines the minimum set of operations supported by a volume, such as mounting, activating, creating and removing items, and more.
--
-- Your volume can provide additional functionality by conforming to other volume operations protocols. These protocols add support for operations like open and close, read and write, extended attribute (Xattr) manipulation, and more.
--
-- Generated bindings for @FSVolume@.
module ObjC.FSKit.FSVolume
  ( FSVolume
  , IsFSVolume(..)
  , init_
  , initWithVolumeID_volumeName
  , volumeID
  , name
  , setName
  , initSelector
  , initWithVolumeID_volumeNameSelector
  , volumeIDSelector
  , nameSelector
  , setNameSelector


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

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsFSVolume fsVolume => fsVolume -> IO (Id FSVolume)
init_ fsVolume  =
    sendMsg fsVolume (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a volume with the given identifier and name. - Parameters:   - volumeID: An ``FSVolumeIdentifier`` to uniquely identify the volume. For a network file system that supports multiple authenticated users, disambiguate the users by using qualifying data in the identifier.   - volumeName: A name for the volume.
--
-- ObjC selector: @- initWithVolumeID:volumeName:@
initWithVolumeID_volumeName :: (IsFSVolume fsVolume, IsFSVolumeIdentifier volumeID, IsFSFileName volumeName) => fsVolume -> volumeID -> volumeName -> IO (Id FSVolume)
initWithVolumeID_volumeName fsVolume  volumeID volumeName =
  withObjCPtr volumeID $ \raw_volumeID ->
    withObjCPtr volumeName $ \raw_volumeName ->
        sendMsg fsVolume (mkSelector "initWithVolumeID:volumeName:") (retPtr retVoid) [argPtr (castPtr raw_volumeID :: Ptr ()), argPtr (castPtr raw_volumeName :: Ptr ())] >>= ownedObject . castPtr

-- | An identifier that uniquely identifies the volume.
--
-- ObjC selector: @- volumeID@
volumeID :: IsFSVolume fsVolume => fsVolume -> IO (Id FSVolumeIdentifier)
volumeID fsVolume  =
    sendMsg fsVolume (mkSelector "volumeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the volume.
--
-- ObjC selector: @- name@
name :: IsFSVolume fsVolume => fsVolume -> IO (Id FSFileName)
name fsVolume  =
    sendMsg fsVolume (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the volume.
--
-- ObjC selector: @- setName:@
setName :: (IsFSVolume fsVolume, IsFSFileName value) => fsVolume -> value -> IO ()
setName fsVolume  value =
  withObjCPtr value $ \raw_value ->
      sendMsg fsVolume (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithVolumeID:volumeName:@
initWithVolumeID_volumeNameSelector :: Selector
initWithVolumeID_volumeNameSelector = mkSelector "initWithVolumeID:volumeName:"

-- | @Selector@ for @volumeID@
volumeIDSelector :: Selector
volumeIDSelector = mkSelector "volumeID"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

