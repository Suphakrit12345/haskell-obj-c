{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAttributedString@.
module ObjC.AppKit.NSAttributedString
  ( NSAttributedString
  , IsNSAttributedString(..)
  , nextWordFromIndex_forward
  , itemNumberInTextList_atIndex
  , containsAttachments
  , nextWordFromIndex_forwardSelector
  , itemNumberInTextList_atIndexSelector
  , containsAttachmentsSelector

  -- * Enum types
  , NSStringDrawingOptions(NSStringDrawingOptions)
  , pattern NSStringDrawingUsesLineFragmentOrigin
  , pattern NSStringDrawingUsesFontLeading
  , pattern NSStringDrawingUsesDeviceMetrics
  , pattern NSStringDrawingTruncatesLastVisibleLine
  , pattern NSStringDrawingOptionsResolvesNaturalAlignmentWithBaseWritingDirection
  , pattern NSStringDrawingDisableScreenFontSubstitution
  , pattern NSStringDrawingOneShot

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums

-- | @- nextWordFromIndex:forward:@
nextWordFromIndex_forward :: IsNSAttributedString nsAttributedString => nsAttributedString -> CULong -> Bool -> IO CULong
nextWordFromIndex_forward nsAttributedString  location isForward =
    sendMsg nsAttributedString (mkSelector "nextWordFromIndex:forward:") retCULong [argCULong location, argCULong (if isForward then 1 else 0)]

-- | @- itemNumberInTextList:atIndex:@
itemNumberInTextList_atIndex :: (IsNSAttributedString nsAttributedString, IsNSTextList list) => nsAttributedString -> list -> CULong -> IO CLong
itemNumberInTextList_atIndex nsAttributedString  list location =
  withObjCPtr list $ \raw_list ->
      sendMsg nsAttributedString (mkSelector "itemNumberInTextList:atIndex:") retCLong [argPtr (castPtr raw_list :: Ptr ()), argCULong location]

-- | @- containsAttachments@
containsAttachments :: IsNSAttributedString nsAttributedString => nsAttributedString -> IO Bool
containsAttachments nsAttributedString  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAttributedString (mkSelector "containsAttachments") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nextWordFromIndex:forward:@
nextWordFromIndex_forwardSelector :: Selector
nextWordFromIndex_forwardSelector = mkSelector "nextWordFromIndex:forward:"

-- | @Selector@ for @itemNumberInTextList:atIndex:@
itemNumberInTextList_atIndexSelector :: Selector
itemNumberInTextList_atIndexSelector = mkSelector "itemNumberInTextList:atIndex:"

-- | @Selector@ for @containsAttachments@
containsAttachmentsSelector :: Selector
containsAttachmentsSelector = mkSelector "containsAttachments"

