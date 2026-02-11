{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object to describe and configure parameters to be used in a nearby interaction session.
--
-- Generated bindings for @NIConfiguration@.
module ObjC.NearbyInteraction.NIConfiguration
  ( NIConfiguration
  , IsNIConfiguration(..)
  , init_
  , new
  , initSelector
  , newSelector


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

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNIConfiguration niConfiguration => niConfiguration -> IO (Id NIConfiguration)
init_ niConfiguration  =
    sendMsg niConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NIConfiguration)
new  =
  do
    cls' <- getRequiredClass "NIConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

