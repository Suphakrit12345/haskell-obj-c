{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The ITLibArtist class represents an artist, such as the performer of a song.
--
-- Generated bindings for @ITLibArtist@.
module ObjC.ITunesLibrary.ITLibArtist
  ( ITLibArtist
  , IsITLibArtist(..)
  , name
  , sortName
  , persistentID
  , nameSelector
  , sortNameSelector
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

-- | The name of this artist.
--
-- ObjC selector: @- name@
name :: IsITLibArtist itLibArtist => itLibArtist -> IO (Id NSString)
name itLibArtist  =
    sendMsg itLibArtist (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of this artist that should be used for sorting purposes.
--
-- ObjC selector: @- sortName@
sortName :: IsITLibArtist itLibArtist => itLibArtist -> IO (Id NSString)
sortName itLibArtist  =
    sendMsg itLibArtist (mkSelector "sortName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The unique identifier of this artist.
--
-- ObjC selector: @- persistentID@
persistentID :: IsITLibArtist itLibArtist => itLibArtist -> IO (Id NSNumber)
persistentID itLibArtist  =
    sendMsg itLibArtist (mkSelector "persistentID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @sortName@
sortNameSelector :: Selector
sortNameSelector = mkSelector "sortName"

-- | @Selector@ for @persistentID@
persistentIDSelector :: Selector
persistentIDSelector = mkSelector "persistentID"

