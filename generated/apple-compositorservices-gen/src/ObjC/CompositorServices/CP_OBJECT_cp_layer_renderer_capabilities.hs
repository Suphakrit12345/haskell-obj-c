{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that stores the supported configurations for a layer.
--
-- Generated bindings for @CP_OBJECT_cp_layer_renderer_capabilities@.
module ObjC.CompositorServices.CP_OBJECT_cp_layer_renderer_capabilities
  ( CP_OBJECT_cp_layer_renderer_capabilities
  , IsCP_OBJECT_cp_layer_renderer_capabilities(..)
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

import ObjC.CompositorServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCP_OBJECT_cp_layer_renderer_capabilities cP_OBJECT_cp_layer_renderer_capabilities => cP_OBJECT_cp_layer_renderer_capabilities -> IO (Id CP_OBJECT_cp_layer_renderer_capabilities)
init_ cP_OBJECT_cp_layer_renderer_capabilities  =
    sendMsg cP_OBJECT_cp_layer_renderer_capabilities (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CP_OBJECT_cp_layer_renderer_capabilities)
new  =
  do
    cls' <- getRequiredClass "CP_OBJECT_cp_layer_renderer_capabilities"
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

