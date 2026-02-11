{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BETextAlternatives@.
module ObjC.BrowserEngineKit.BETextAlternatives
  ( BETextAlternatives
  , IsBETextAlternatives(..)
  , new
  , init_
  , primaryString
  , alternativeStrings
  , newSelector
  , initSelector
  , primaryStringSelector
  , alternativeStringsSelector


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

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- new@
new :: IsBETextAlternatives beTextAlternatives => beTextAlternatives -> IO (Id BETextAlternatives)
new beTextAlternatives  =
    sendMsg beTextAlternatives (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsBETextAlternatives beTextAlternatives => beTextAlternatives -> IO (Id BETextAlternatives)
init_ beTextAlternatives  =
    sendMsg beTextAlternatives (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Original text for which alternative strings are provided
--
-- ObjC selector: @- primaryString@
primaryString :: IsBETextAlternatives beTextAlternatives => beTextAlternatives -> IO (Id NSString)
primaryString beTextAlternatives  =
    sendMsg beTextAlternatives (mkSelector "primaryString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array of available aternative strings
--
-- ObjC selector: @- alternativeStrings@
alternativeStrings :: IsBETextAlternatives beTextAlternatives => beTextAlternatives -> IO (Id NSArray)
alternativeStrings beTextAlternatives  =
    sendMsg beTextAlternatives (mkSelector "alternativeStrings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @primaryString@
primaryStringSelector :: Selector
primaryStringSelector = mkSelector "primaryString"

-- | @Selector@ for @alternativeStrings@
alternativeStringsSelector :: Selector
alternativeStringsSelector = mkSelector "alternativeStrings"

