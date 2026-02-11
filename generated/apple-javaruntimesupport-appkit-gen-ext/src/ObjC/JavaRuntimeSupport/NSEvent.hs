{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSEvent@.
module ObjC.JavaRuntimeSupport.NSEvent
  ( NSEvent
  , IsNSEvent(..)
  , deadKeyCharacter
  , willBeHandledByComplexInputMethod
  , deadKeyCharacterSelector
  , willBeHandledByComplexInputMethodSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- deadKeyCharacter@
deadKeyCharacter :: IsNSEvent nsEvent => nsEvent -> IO CUShort
deadKeyCharacter nsEvent  =
    fmap fromIntegral $ sendMsg nsEvent (mkSelector "deadKeyCharacter") retCUInt []

-- | @- willBeHandledByComplexInputMethod@
willBeHandledByComplexInputMethod :: IsNSEvent nsEvent => nsEvent -> IO Bool
willBeHandledByComplexInputMethod nsEvent  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEvent (mkSelector "willBeHandledByComplexInputMethod") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deadKeyCharacter@
deadKeyCharacterSelector :: Selector
deadKeyCharacterSelector = mkSelector "deadKeyCharacter"

-- | @Selector@ for @willBeHandledByComplexInputMethod@
willBeHandledByComplexInputMethodSelector :: Selector
willBeHandledByComplexInputMethodSelector = mkSelector "willBeHandledByComplexInputMethod"

