{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a snapshot of changes made to the cinematic script since recording. Can be used as a snapshot to quickly revert to previously saved edits via @-[CNScript reloadWithChanges:]@
--
-- Generated bindings for @CNScriptChanges@.
module ObjC.Cinematic.CNScriptChanges
  ( CNScriptChanges
  , IsCNScriptChanges(..)
  , initWithDataRepresentation
  , init_
  , new
  , dataRepresentation
  , fNumber
  , userDecisions
  , addedDetectionTracks
  , initWithDataRepresentationSelector
  , initSelector
  , newSelector
  , dataRepresentationSelector
  , fNumberSelector
  , userDecisionsSelector
  , addedDetectionTracksSelector


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

import ObjC.Cinematic.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create from previously saved data representation
--
-- ObjC selector: @- initWithDataRepresentation:@
initWithDataRepresentation :: (IsCNScriptChanges cnScriptChanges, IsNSData dataRepresentation) => cnScriptChanges -> dataRepresentation -> IO (Id CNScriptChanges)
initWithDataRepresentation cnScriptChanges  dataRepresentation =
  withObjCPtr dataRepresentation $ \raw_dataRepresentation ->
      sendMsg cnScriptChanges (mkSelector "initWithDataRepresentation:") (retPtr retVoid) [argPtr (castPtr raw_dataRepresentation :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCNScriptChanges cnScriptChanges => cnScriptChanges -> IO (Id CNScriptChanges)
init_ cnScriptChanges  =
    sendMsg cnScriptChanges (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNScriptChanges)
new  =
  do
    cls' <- getRequiredClass "CNScriptChanges"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Get persistent data representation of these changes for later restoration.
--
-- The changes can only be used with the original cinematic asset from which the CNScript was created.
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsCNScriptChanges cnScriptChanges => cnScriptChanges -> IO (Id NSData)
dataRepresentation cnScriptChanges  =
    sendMsg cnScriptChanges (mkSelector "dataRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The f/number to apply to the entire movie.
--
-- ObjC selector: @- fNumber@
fNumber :: IsCNScriptChanges cnScriptChanges => cnScriptChanges -> IO CFloat
fNumber cnScriptChanges  =
    sendMsg cnScriptChanges (mkSelector "fNumber") retCFloat []

-- | All active user decisions, including those made at recording time, unless they have been removed.
--
-- ObjC selector: @- userDecisions@
userDecisions :: IsCNScriptChanges cnScriptChanges => cnScriptChanges -> IO (Id NSArray)
userDecisions cnScriptChanges  =
    sendMsg cnScriptChanges (mkSelector "userDecisions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | All detection tracks that have been added. Does not include those created at recording time.
--
-- ObjC selector: @- addedDetectionTracks@
addedDetectionTracks :: IsCNScriptChanges cnScriptChanges => cnScriptChanges -> IO (Id NSArray)
addedDetectionTracks cnScriptChanges  =
    sendMsg cnScriptChanges (mkSelector "addedDetectionTracks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDataRepresentation:@
initWithDataRepresentationSelector :: Selector
initWithDataRepresentationSelector = mkSelector "initWithDataRepresentation:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector
dataRepresentationSelector = mkSelector "dataRepresentation"

-- | @Selector@ for @fNumber@
fNumberSelector :: Selector
fNumberSelector = mkSelector "fNumber"

-- | @Selector@ for @userDecisions@
userDecisionsSelector :: Selector
userDecisionsSelector = mkSelector "userDecisions"

-- | @Selector@ for @addedDetectionTracks@
addedDetectionTracksSelector :: Selector
addedDetectionTracksSelector = mkSelector "addedDetectionTracks"

