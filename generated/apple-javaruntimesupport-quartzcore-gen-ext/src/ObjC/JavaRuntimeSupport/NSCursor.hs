{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCursor@.
module ObjC.JavaRuntimeSupport.NSCursor
  ( NSCursor
  , IsNSCursor(..)
  , javaSetAllowsCursorSetInBackground
  , javaSetAllowsCursorSetInBackgroundSelector


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

-- | @+ javaSetAllowsCursorSetInBackground:@
javaSetAllowsCursorSetInBackground :: Bool -> IO ()
javaSetAllowsCursorSetInBackground allows =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "javaSetAllowsCursorSetInBackground:") retVoid [argCULong (if allows then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @javaSetAllowsCursorSetInBackground:@
javaSetAllowsCursorSetInBackgroundSelector :: Selector
javaSetAllowsCursorSetInBackgroundSelector = mkSelector "javaSetAllowsCursorSetInBackground:"

