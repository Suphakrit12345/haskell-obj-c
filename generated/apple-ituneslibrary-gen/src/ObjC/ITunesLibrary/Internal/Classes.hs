{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ITunesLibrary.Internal.Classes (
    module ObjC.ITunesLibrary.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- ITLibAlbum ----------

-- | The ITLibAlbum class represents an album where a given media item (ITLibMediaItem) is contained.
-- 
-- Phantom type for @ITLibAlbum@.
data ITLibAlbum

instance IsObjCObject (Id ITLibAlbum) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ITLibAlbum"

class IsNSObject a => IsITLibAlbum a where
  toITLibAlbum :: a -> Id ITLibAlbum

instance IsITLibAlbum (Id ITLibAlbum) where
  toITLibAlbum = unsafeCastId

instance IsNSObject (Id ITLibAlbum) where
  toNSObject = unsafeCastId

-- ---------- ITLibArtist ----------

-- | The ITLibArtist class represents an artist, such as the performer of a song.
-- 
-- Phantom type for @ITLibArtist@.
data ITLibArtist

instance IsObjCObject (Id ITLibArtist) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ITLibArtist"

class IsNSObject a => IsITLibArtist a where
  toITLibArtist :: a -> Id ITLibArtist

instance IsITLibArtist (Id ITLibArtist) where
  toITLibArtist = unsafeCastId

instance IsNSObject (Id ITLibArtist) where
  toNSObject = unsafeCastId

-- ---------- ITLibArtwork ----------

-- | The ITLibArtwork class represents a media item artwork.
-- 
-- Phantom type for @ITLibArtwork@.
data ITLibArtwork

instance IsObjCObject (Id ITLibArtwork) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ITLibArtwork"

class IsNSObject a => IsITLibArtwork a where
  toITLibArtwork :: a -> Id ITLibArtwork

instance IsITLibArtwork (Id ITLibArtwork) where
  toITLibArtwork = unsafeCastId

instance IsNSObject (Id ITLibArtwork) where
  toNSObject = unsafeCastId

-- ---------- ITLibMediaEntity ----------

-- | The ITLibMediaEntity class serves as the abstract superclass for ITLibMediaItem and ITLibPlaylist instances.				As the superclass, ITLibMediaEntity defines methods used by those subclasses.
-- 
-- Phantom type for @ITLibMediaEntity@.
data ITLibMediaEntity

instance IsObjCObject (Id ITLibMediaEntity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ITLibMediaEntity"

class IsNSObject a => IsITLibMediaEntity a where
  toITLibMediaEntity :: a -> Id ITLibMediaEntity

instance IsITLibMediaEntity (Id ITLibMediaEntity) where
  toITLibMediaEntity = unsafeCastId

instance IsNSObject (Id ITLibMediaEntity) where
  toNSObject = unsafeCastId

-- ---------- ITLibMediaItemVideoInfo ----------

-- | The ITLibMediaItemVideoInfo class encapsulates the video information of a video media item.
-- 
-- Phantom type for @ITLibMediaItemVideoInfo@.
data ITLibMediaItemVideoInfo

instance IsObjCObject (Id ITLibMediaItemVideoInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ITLibMediaItemVideoInfo"

class IsNSObject a => IsITLibMediaItemVideoInfo a where
  toITLibMediaItemVideoInfo :: a -> Id ITLibMediaItemVideoInfo

instance IsITLibMediaItemVideoInfo (Id ITLibMediaItemVideoInfo) where
  toITLibMediaItemVideoInfo = unsafeCastId

instance IsNSObject (Id ITLibMediaItemVideoInfo) where
  toNSObject = unsafeCastId

-- ---------- ITLibrary ----------

-- | A class representing an iTunes library whose metadata is being queried.
-- 
-- Phantom type for @ITLibrary@.
data ITLibrary

instance IsObjCObject (Id ITLibrary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ITLibrary"

class IsNSObject a => IsITLibrary a where
  toITLibrary :: a -> Id ITLibrary

instance IsITLibrary (Id ITLibrary) where
  toITLibrary = unsafeCastId

instance IsNSObject (Id ITLibrary) where
  toNSObject = unsafeCastId

-- ---------- ITLibMediaItem ----------

-- | A media item represents a single piece of media (such as a song, a video, a podcast, etc) in the iTunes library. 			  A media item has an overall unique identifier, accessed using the persistentID property. The media item			  metadata may be accessed through its individual properties or via the ITLibMediaEntity general property accessor			  methods.
-- 
-- Phantom type for @ITLibMediaItem@.
data ITLibMediaItem

instance IsObjCObject (Id ITLibMediaItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ITLibMediaItem"

class IsITLibMediaEntity a => IsITLibMediaItem a where
  toITLibMediaItem :: a -> Id ITLibMediaItem

instance IsITLibMediaItem (Id ITLibMediaItem) where
  toITLibMediaItem = unsafeCastId

instance IsITLibMediaEntity (Id ITLibMediaItem) where
  toITLibMediaEntity = unsafeCastId

instance IsNSObject (Id ITLibMediaItem) where
  toNSObject = unsafeCastId

-- ---------- ITLibPlaylist ----------

-- | A playlist is a collection of related media items. (Media items are described in ITLibMediaItem Class Reference.) 			Each playlist has a name, a set of attributes, and a unique identifier that persists across application launches.
-- 
-- Phantom type for @ITLibPlaylist@.
data ITLibPlaylist

instance IsObjCObject (Id ITLibPlaylist) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ITLibPlaylist"

class IsITLibMediaEntity a => IsITLibPlaylist a where
  toITLibPlaylist :: a -> Id ITLibPlaylist

instance IsITLibPlaylist (Id ITLibPlaylist) where
  toITLibPlaylist = unsafeCastId

instance IsITLibMediaEntity (Id ITLibPlaylist) where
  toITLibMediaEntity = unsafeCastId

instance IsNSObject (Id ITLibPlaylist) where
  toNSObject = unsafeCastId
