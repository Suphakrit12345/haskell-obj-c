{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMenu@.
module ObjC.JavaRuntimeSupport.NSMenu
  ( NSMenu
  , IsNSMenu(..)
  , setJavaMenuDelegate
  , isJavaMenu
  , setJavaMenuDelegateSelector
  , isJavaMenuSelector


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

-- | @- setJavaMenuDelegate:@
setJavaMenuDelegate :: IsNSMenu nsMenu => nsMenu -> RawId -> IO ()
setJavaMenuDelegate nsMenu  delegate =
    sendMsg nsMenu (mkSelector "setJavaMenuDelegate:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ())]

-- | @- isJavaMenu@
isJavaMenu :: IsNSMenu nsMenu => nsMenu -> IO Bool
isJavaMenu nsMenu  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenu (mkSelector "isJavaMenu") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setJavaMenuDelegate:@
setJavaMenuDelegateSelector :: Selector
setJavaMenuDelegateSelector = mkSelector "setJavaMenuDelegate:"

-- | @Selector@ for @isJavaMenu@
isJavaMenuSelector :: Selector
isJavaMenuSelector = mkSelector "isJavaMenu"

