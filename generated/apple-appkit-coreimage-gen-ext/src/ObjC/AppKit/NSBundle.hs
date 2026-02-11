{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBundle@.
module ObjC.AppKit.NSBundle
  ( NSBundle
  , IsNSBundle(..)
  , imageForResource
  , imageForResourceSelector


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

import ObjC.AppKit.Internal.Classes

-- | @- imageForResource:@
imageForResource :: IsNSBundle nsBundle => nsBundle -> RawId -> IO (Id NSImage)
imageForResource nsBundle  name =
    sendMsg nsBundle (mkSelector "imageForResource:") (retPtr retVoid) [argPtr (castPtr (unRawId name) :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageForResource:@
imageForResourceSelector :: Selector
imageForResourceSelector = mkSelector "imageForResource:"

