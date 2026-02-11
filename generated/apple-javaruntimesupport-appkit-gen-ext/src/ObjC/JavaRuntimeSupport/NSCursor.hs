{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCursor@.
module ObjC.JavaRuntimeSupport.NSCursor
  ( NSCursor
  , IsNSCursor(..)
  , javaBusyButClickableCursor
  , javaResizeNECursor
  , javaResizeNWCursor
  , javaResizeSECursor
  , javaResizeSWCursor
  , javaMoveCursor
  , javaSetAllowsCursorSetInBackground
  , javaBusyButClickableCursorSelector
  , javaResizeNECursorSelector
  , javaResizeNWCursorSelector
  , javaResizeSECursorSelector
  , javaResizeSWCursorSelector
  , javaMoveCursorSelector
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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ javaBusyButClickableCursor@
javaBusyButClickableCursor :: IO (Id NSCursor)
javaBusyButClickableCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "javaBusyButClickableCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ javaResizeNECursor@
javaResizeNECursor :: IO (Id NSCursor)
javaResizeNECursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "javaResizeNECursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ javaResizeNWCursor@
javaResizeNWCursor :: IO (Id NSCursor)
javaResizeNWCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "javaResizeNWCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ javaResizeSECursor@
javaResizeSECursor :: IO (Id NSCursor)
javaResizeSECursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "javaResizeSECursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ javaResizeSWCursor@
javaResizeSWCursor :: IO (Id NSCursor)
javaResizeSWCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "javaResizeSWCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ javaMoveCursor@
javaMoveCursor :: IO (Id NSCursor)
javaMoveCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "javaMoveCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ javaSetAllowsCursorSetInBackground:@
javaSetAllowsCursorSetInBackground :: Bool -> IO ()
javaSetAllowsCursorSetInBackground allows =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "javaSetAllowsCursorSetInBackground:") retVoid [argCULong (if allows then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @javaBusyButClickableCursor@
javaBusyButClickableCursorSelector :: Selector
javaBusyButClickableCursorSelector = mkSelector "javaBusyButClickableCursor"

-- | @Selector@ for @javaResizeNECursor@
javaResizeNECursorSelector :: Selector
javaResizeNECursorSelector = mkSelector "javaResizeNECursor"

-- | @Selector@ for @javaResizeNWCursor@
javaResizeNWCursorSelector :: Selector
javaResizeNWCursorSelector = mkSelector "javaResizeNWCursor"

-- | @Selector@ for @javaResizeSECursor@
javaResizeSECursorSelector :: Selector
javaResizeSECursorSelector = mkSelector "javaResizeSECursor"

-- | @Selector@ for @javaResizeSWCursor@
javaResizeSWCursorSelector :: Selector
javaResizeSWCursorSelector = mkSelector "javaResizeSWCursor"

-- | @Selector@ for @javaMoveCursor@
javaMoveCursorSelector :: Selector
javaMoveCursorSelector = mkSelector "javaMoveCursor"

-- | @Selector@ for @javaSetAllowsCursorSetInBackground:@
javaSetAllowsCursorSetInBackgroundSelector :: Selector
javaSetAllowsCursorSetInBackgroundSelector = mkSelector "javaSetAllowsCursorSetInBackground:"

