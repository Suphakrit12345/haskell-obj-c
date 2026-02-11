{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A playlist is a collection of related media items. (Media items are described in ITLibMediaItem Class Reference.) 			Each playlist has a name, a set of attributes, and a unique identifier that persists across application launches.
--
-- Generated bindings for @ITLibPlaylist@.
module ObjC.ITunesLibrary.ITLibPlaylist
  ( ITLibPlaylist
  , IsITLibPlaylist(..)
  , name
  , primary
  , parentID
  , visible
  , allItemsPlaylist
  , items
  , distinguishedKind
  , kind
  , master
  , nameSelector
  , primarySelector
  , parentIDSelector
  , visibleSelector
  , allItemsPlaylistSelector
  , itemsSelector
  , distinguishedKindSelector
  , kindSelector
  , masterSelector

  -- * Enum types
  , ITLibDistinguishedPlaylistKind(ITLibDistinguishedPlaylistKind)
  , pattern ITLibDistinguishedPlaylistKindNone
  , pattern ITLibDistinguishedPlaylistKindMovies
  , pattern ITLibDistinguishedPlaylistKindTVShows
  , pattern ITLibDistinguishedPlaylistKindMusic
  , pattern ITLibDistinguishedPlaylistKindAudiobooks
  , pattern ITLibDistinguishedPlaylistKindBooks
  , pattern ITLibDistinguishedPlaylistKindRingtones
  , pattern ITLibDistinguishedPlaylistKindPodcasts
  , pattern ITLibDistinguishedPlaylistKindVoiceMemos
  , pattern ITLibDistinguishedPlaylistKindPurchases
  , pattern ITLibDistinguishedPlaylistKindiTunesU
  , pattern ITLibDistinguishedPlaylistKind90sMusic
  , pattern ITLibDistinguishedPlaylistKindMyTopRated
  , pattern ITLibDistinguishedPlaylistKindTop25MostPlayed
  , pattern ITLibDistinguishedPlaylistKindRecentlyPlayed
  , pattern ITLibDistinguishedPlaylistKindRecentlyAdded
  , pattern ITLibDistinguishedPlaylistKindMusicVideos
  , pattern ITLibDistinguishedPlaylistKindClassicalMusic
  , pattern ITLibDistinguishedPlaylistKindLibraryMusicVideos
  , pattern ITLibDistinguishedPlaylistKindHomeVideos
  , pattern ITLibDistinguishedPlaylistKindApplications
  , pattern ITLibDistinguishedPlaylistKindLovedSongs
  , pattern ITLibDistinguishedPlaylistKindMusicShowsAndMovies
  , ITLibPlaylistKind(ITLibPlaylistKind)
  , pattern ITLibPlaylistKindRegular
  , pattern ITLibPlaylistKindSmart
  , pattern ITLibPlaylistKindGenius
  , pattern ITLibPlaylistKindFolder
  , pattern ITLibPlaylistKindGeniusMix

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
import ObjC.ITunesLibrary.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The name or title of this playlist.
--
-- ObjC selector: @- name@
name :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO (Id NSString)
name itLibPlaylist  =
    sendMsg itLibPlaylist (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether this playlist is the primary playlist.
--
-- ObjC selector: @- primary@
primary :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO Bool
primary itLibPlaylist  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibPlaylist (mkSelector "primary") retCULong []

-- | The unique identifier of this playlist' parent.
--
-- ObjC selector: @- parentID@
parentID :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO (Id NSNumber)
parentID itLibPlaylist  =
    sendMsg itLibPlaylist (mkSelector "parentID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether this playlist is visible.
--
-- ObjC selector: @- visible@
visible :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO Bool
visible itLibPlaylist  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibPlaylist (mkSelector "visible") retCULong []

-- | Whether or not every item in this playlist is exposed via this API.  Generally true but not that useful.
--
-- ObjC selector: @- allItemsPlaylist@
allItemsPlaylist :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO Bool
allItemsPlaylist itLibPlaylist  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibPlaylist (mkSelector "allItemsPlaylist") retCULong []

-- | The media items contained within this playlist.
--
-- ObjC selector: @- items@
items :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO (Id NSArray)
items itLibPlaylist  =
    sendMsg itLibPlaylist (mkSelector "items") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The distinguished kind of this playlist.
--
-- ObjC selector: @- distinguishedKind@
distinguishedKind :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO ITLibDistinguishedPlaylistKind
distinguishedKind itLibPlaylist  =
    fmap (coerce :: CULong -> ITLibDistinguishedPlaylistKind) $ sendMsg itLibPlaylist (mkSelector "distinguishedKind") retCULong []

-- | The kind of this playlist.
--
-- ObjC selector: @- kind@
kind :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO ITLibPlaylistKind
kind itLibPlaylist  =
    fmap (coerce :: CULong -> ITLibPlaylistKind) $ sendMsg itLibPlaylist (mkSelector "kind") retCULong []

-- | Whether this playlist is the primary playlist.
--
-- ObjC selector: @- master@
master :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO Bool
master itLibPlaylist  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibPlaylist (mkSelector "master") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @primary@
primarySelector :: Selector
primarySelector = mkSelector "primary"

-- | @Selector@ for @parentID@
parentIDSelector :: Selector
parentIDSelector = mkSelector "parentID"

-- | @Selector@ for @visible@
visibleSelector :: Selector
visibleSelector = mkSelector "visible"

-- | @Selector@ for @allItemsPlaylist@
allItemsPlaylistSelector :: Selector
allItemsPlaylistSelector = mkSelector "allItemsPlaylist"

-- | @Selector@ for @items@
itemsSelector :: Selector
itemsSelector = mkSelector "items"

-- | @Selector@ for @distinguishedKind@
distinguishedKindSelector :: Selector
distinguishedKindSelector = mkSelector "distinguishedKind"

-- | @Selector@ for @kind@
kindSelector :: Selector
kindSelector = mkSelector "kind"

-- | @Selector@ for @master@
masterSelector :: Selector
masterSelector = mkSelector "master"

