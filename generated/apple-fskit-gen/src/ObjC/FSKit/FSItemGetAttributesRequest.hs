{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request to get attributes from an item.
--
-- Methods that retrieve attributes use this type and inspect the ``wantedAttributes`` property to determine which attributes to provide. FSKit calls the ``isAttributeWanted(_:)`` method to determine whether the request requires a given attribute.
--
-- Generated bindings for @FSItemGetAttributesRequest@.
module ObjC.FSKit.FSItemGetAttributesRequest
  ( FSItemGetAttributesRequest
  , IsFSItemGetAttributesRequest(..)
  , isAttributeWanted
  , wantedAttributes
  , setWantedAttributes
  , isAttributeWantedSelector
  , wantedAttributesSelector
  , setWantedAttributesSelector

  -- * Enum types
  , FSItemAttribute(FSItemAttribute)
  , pattern FSItemAttributeType
  , pattern FSItemAttributeMode
  , pattern FSItemAttributeLinkCount
  , pattern FSItemAttributeUID
  , pattern FSItemAttributeGID
  , pattern FSItemAttributeFlags
  , pattern FSItemAttributeSize
  , pattern FSItemAttributeAllocSize
  , pattern FSItemAttributeFileID
  , pattern FSItemAttributeParentID
  , pattern FSItemAttributeAccessTime
  , pattern FSItemAttributeModifyTime
  , pattern FSItemAttributeChangeTime
  , pattern FSItemAttributeBirthTime
  , pattern FSItemAttributeBackupTime
  , pattern FSItemAttributeAddedTime
  , pattern FSItemAttributeSupportsLimitedXAttrs
  , pattern FSItemAttributeInhibitKernelOffloadedIO

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

import ObjC.FSKit.Internal.Classes
import ObjC.FSKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | A method that indicates whether the request wants given attribute.
--
-- - Parameter attribute: The ``FSItemAttribute`` to check.
--
-- ObjC selector: @- isAttributeWanted:@
isAttributeWanted :: IsFSItemGetAttributesRequest fsItemGetAttributesRequest => fsItemGetAttributesRequest -> FSItemAttribute -> IO Bool
isAttributeWanted fsItemGetAttributesRequest  attribute =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsItemGetAttributesRequest (mkSelector "isAttributeWanted:") retCULong [argCLong (coerce attribute)]

-- | The attributes requested by the request.
--
-- This property is a bit field in Objective-C and an <doc://com.apple.documentation/documentation/Swift/OptionSet> in Swift.
--
-- ObjC selector: @- wantedAttributes@
wantedAttributes :: IsFSItemGetAttributesRequest fsItemGetAttributesRequest => fsItemGetAttributesRequest -> IO FSItemAttribute
wantedAttributes fsItemGetAttributesRequest  =
    fmap (coerce :: CLong -> FSItemAttribute) $ sendMsg fsItemGetAttributesRequest (mkSelector "wantedAttributes") retCLong []

-- | The attributes requested by the request.
--
-- This property is a bit field in Objective-C and an <doc://com.apple.documentation/documentation/Swift/OptionSet> in Swift.
--
-- ObjC selector: @- setWantedAttributes:@
setWantedAttributes :: IsFSItemGetAttributesRequest fsItemGetAttributesRequest => fsItemGetAttributesRequest -> FSItemAttribute -> IO ()
setWantedAttributes fsItemGetAttributesRequest  value =
    sendMsg fsItemGetAttributesRequest (mkSelector "setWantedAttributes:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isAttributeWanted:@
isAttributeWantedSelector :: Selector
isAttributeWantedSelector = mkSelector "isAttributeWanted:"

-- | @Selector@ for @wantedAttributes@
wantedAttributesSelector :: Selector
wantedAttributesSelector = mkSelector "wantedAttributes"

-- | @Selector@ for @setWantedAttributes:@
setWantedAttributesSelector :: Selector
setWantedAttributesSelector = mkSelector "setWantedAttributes:"

