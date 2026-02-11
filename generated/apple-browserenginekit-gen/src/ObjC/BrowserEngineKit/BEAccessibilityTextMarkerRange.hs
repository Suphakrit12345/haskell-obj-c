{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | BEAccessibilityTextMarkerRange holds the start and end markers for a text range.
--
-- Generated bindings for @BEAccessibilityTextMarkerRange@.
module ObjC.BrowserEngineKit.BEAccessibilityTextMarkerRange
  ( BEAccessibilityTextMarkerRange
  , IsBEAccessibilityTextMarkerRange(..)
  , startMarker
  , setStartMarker
  , endMarker
  , setEndMarker
  , startMarkerSelector
  , setStartMarkerSelector
  , endMarkerSelector
  , setEndMarkerSelector


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

-- | @- startMarker@
startMarker :: IsBEAccessibilityTextMarkerRange beAccessibilityTextMarkerRange => beAccessibilityTextMarkerRange -> IO (Id BEAccessibilityTextMarker)
startMarker beAccessibilityTextMarkerRange  =
    sendMsg beAccessibilityTextMarkerRange (mkSelector "startMarker") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartMarker:@
setStartMarker :: (IsBEAccessibilityTextMarkerRange beAccessibilityTextMarkerRange, IsBEAccessibilityTextMarker value) => beAccessibilityTextMarkerRange -> value -> IO ()
setStartMarker beAccessibilityTextMarkerRange  value =
  withObjCPtr value $ \raw_value ->
      sendMsg beAccessibilityTextMarkerRange (mkSelector "setStartMarker:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endMarker@
endMarker :: IsBEAccessibilityTextMarkerRange beAccessibilityTextMarkerRange => beAccessibilityTextMarkerRange -> IO (Id BEAccessibilityTextMarker)
endMarker beAccessibilityTextMarkerRange  =
    sendMsg beAccessibilityTextMarkerRange (mkSelector "endMarker") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndMarker:@
setEndMarker :: (IsBEAccessibilityTextMarkerRange beAccessibilityTextMarkerRange, IsBEAccessibilityTextMarker value) => beAccessibilityTextMarkerRange -> value -> IO ()
setEndMarker beAccessibilityTextMarkerRange  value =
  withObjCPtr value $ \raw_value ->
      sendMsg beAccessibilityTextMarkerRange (mkSelector "setEndMarker:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startMarker@
startMarkerSelector :: Selector
startMarkerSelector = mkSelector "startMarker"

-- | @Selector@ for @setStartMarker:@
setStartMarkerSelector :: Selector
setStartMarkerSelector = mkSelector "setStartMarker:"

-- | @Selector@ for @endMarker@
endMarkerSelector :: Selector
endMarkerSelector = mkSelector "endMarker"

-- | @Selector@ for @setEndMarker:@
setEndMarkerSelector :: Selector
setEndMarkerSelector = mkSelector "setEndMarker:"

