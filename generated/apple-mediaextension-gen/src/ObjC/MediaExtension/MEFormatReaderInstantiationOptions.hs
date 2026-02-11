{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MEFormatReaderInstantiationOptions
--
-- A class that encapsulates options to be passed to MEFormatReaderExtension
--
-- The class MEFormatReaderInstantiationOptions is mutable, with options set through instance properties.
--
-- Generated bindings for @MEFormatReaderInstantiationOptions@.
module ObjC.MediaExtension.MEFormatReaderInstantiationOptions
  ( MEFormatReaderInstantiationOptions
  , IsMEFormatReaderInstantiationOptions(..)
  , allowIncrementalFragmentParsing
  , allowIncrementalFragmentParsingSelector


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

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | allowIncrementalFragmentParsing
--
-- Enables support for parsing additional fragments
--
-- If YES, requests that the MEFormatReader be configured to support calls to parseAdditionalFragments. By default the MEFormatReader does not support calls to parseAdditionalFragments.
--
-- ObjC selector: @- allowIncrementalFragmentParsing@
allowIncrementalFragmentParsing :: IsMEFormatReaderInstantiationOptions meFormatReaderInstantiationOptions => meFormatReaderInstantiationOptions -> IO Bool
allowIncrementalFragmentParsing meFormatReaderInstantiationOptions  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meFormatReaderInstantiationOptions (mkSelector "allowIncrementalFragmentParsing") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowIncrementalFragmentParsing@
allowIncrementalFragmentParsingSelector :: Selector
allowIncrementalFragmentParsingSelector = mkSelector "allowIncrementalFragmentParsing"

