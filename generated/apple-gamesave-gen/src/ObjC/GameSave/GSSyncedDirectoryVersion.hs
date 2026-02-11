{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GSSyncedDirectoryVersion@.
module ObjC.GameSave.GSSyncedDirectoryVersion
  ( GSSyncedDirectoryVersion
  , IsGSSyncedDirectoryVersion(..)
  , init_
  , new
  , isLocal
  , localizedNameOfSavingComputer
  , modifiedDate
  , url
  , description
  , initSelector
  , newSelector
  , isLocalSelector
  , localizedNameOfSavingComputerSelector
  , modifiedDateSelector
  , urlSelector
  , descriptionSelector


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

import ObjC.GameSave.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO (Id GSSyncedDirectoryVersion)
init_ gsSyncedDirectoryVersion  =
    sendMsg gsSyncedDirectoryVersion (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- new@
new :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO (Id GSSyncedDirectoryVersion)
new gsSyncedDirectoryVersion  =
    sendMsg gsSyncedDirectoryVersion (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @YES@ if the directory version is local; otherwise @NO@.
--
-- ObjC selector: @- isLocal@
isLocal :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO Bool
isLocal gsSyncedDirectoryVersion  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gsSyncedDirectoryVersion (mkSelector "isLocal") retCULong []

-- | The localized name of the device that saved this version.
--
-- ObjC selector: @- localizedNameOfSavingComputer@
localizedNameOfSavingComputer :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO (Id NSString)
localizedNameOfSavingComputer gsSyncedDirectoryVersion  =
    sendMsg gsSyncedDirectoryVersion (mkSelector "localizedNameOfSavingComputer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date that this version was last modified.
--
-- ObjC selector: @- modifiedDate@
modifiedDate :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO (Id NSDate)
modifiedDate gsSyncedDirectoryVersion  =
    sendMsg gsSyncedDirectoryVersion (mkSelector "modifiedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The URL of a directory where you read and write game-save data.
--
-- You define the format and structure of files you write in this directory.
--
-- ObjC selector: @- url@
url :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO (Id NSURL)
url gsSyncedDirectoryVersion  =
    sendMsg gsSyncedDirectoryVersion (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- description@
description :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO (Id NSString)
description gsSyncedDirectoryVersion  =
    sendMsg gsSyncedDirectoryVersion (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @isLocal@
isLocalSelector :: Selector
isLocalSelector = mkSelector "isLocal"

-- | @Selector@ for @localizedNameOfSavingComputer@
localizedNameOfSavingComputerSelector :: Selector
localizedNameOfSavingComputerSelector = mkSelector "localizedNameOfSavingComputer"

-- | @Selector@ for @modifiedDate@
modifiedDateSelector :: Selector
modifiedDateSelector = mkSelector "modifiedDate"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

