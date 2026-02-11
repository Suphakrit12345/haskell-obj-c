{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @JRSDrag@.
module ObjC.JavaRuntimeSupport.JRSDrag
  ( JRSDrag
  , IsJRSDrag(..)
  , currentAllowableActions
  , currentModifiers
  , currentAllowableActionsSelector
  , currentModifiersSelector


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

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ currentAllowableActions@
currentAllowableActions :: IO CInt
currentAllowableActions  =
  do
    cls' <- getRequiredClass "JRSDrag"
    sendClassMsg cls' (mkSelector "currentAllowableActions") retCInt []

-- | @+ currentModifiers@
currentModifiers :: IO CULong
currentModifiers  =
  do
    cls' <- getRequiredClass "JRSDrag"
    sendClassMsg cls' (mkSelector "currentModifiers") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentAllowableActions@
currentAllowableActionsSelector :: Selector
currentAllowableActionsSelector = mkSelector "currentAllowableActions"

-- | @Selector@ for @currentModifiers@
currentModifiersSelector :: Selector
currentModifiersSelector = mkSelector "currentModifiers"

