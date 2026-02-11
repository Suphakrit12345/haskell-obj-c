{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A task meant to perform processing on behalf of health research studies.
--
-- Health research tasks may only be used by applications entitled to perform studies and user's have opted in to the relevant study. These apps must have the @com.apple.developer.backgroundtasks.healthresearch@ entitlement.
--
-- Generated bindings for @BGHealthResearchTask@.
module ObjC.BackgroundTasks.BGHealthResearchTask
  ( BGHealthResearchTask
  , IsBGHealthResearchTask(..)


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

import ObjC.BackgroundTasks.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

