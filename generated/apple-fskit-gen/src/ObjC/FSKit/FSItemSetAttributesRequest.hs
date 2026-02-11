{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request to set attributes on an item.
--
-- Methods that take attributes use this type to receive attribute values and to indicate which attributes they support. The various members of the parent type, ``FSItemAttributes``, contain the values of the attributes to set.
--
-- Modify the ``consumedAttributes`` property to indicate which attributes your file system successfully used. FSKit calls the ``wasAttributeConsumed(_:)`` method to determine whether the file system successfully used a given attribute. Only set the attributes that your file system supports.
--
-- Generated bindings for @FSItemSetAttributesRequest@.
module ObjC.FSKit.FSItemSetAttributesRequest
  ( FSItemSetAttributesRequest
  , IsFSItemSetAttributesRequest(..)
  , wasAttributeConsumed
  , consumedAttributes
  , setConsumedAttributes
  , wasAttributeConsumedSelector
  , consumedAttributesSelector
  , setConsumedAttributesSelector

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

-- | A method that indicates whether the file system used the given attribute.
--
-- - Parameter attribute: The ``FSItemAttribute`` to check.
--
-- ObjC selector: @- wasAttributeConsumed:@
wasAttributeConsumed :: IsFSItemSetAttributesRequest fsItemSetAttributesRequest => fsItemSetAttributesRequest -> FSItemAttribute -> IO Bool
wasAttributeConsumed fsItemSetAttributesRequest  attribute =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsItemSetAttributesRequest (mkSelector "wasAttributeConsumed:") retCULong [argCLong (coerce attribute)]

-- | The attributes successfully used by the file system.
--
-- This property is a bit field in Objective-C and an <doc://com.apple.documentation/documentation/Swift/OptionSet> in Swift.
--
-- ObjC selector: @- consumedAttributes@
consumedAttributes :: IsFSItemSetAttributesRequest fsItemSetAttributesRequest => fsItemSetAttributesRequest -> IO FSItemAttribute
consumedAttributes fsItemSetAttributesRequest  =
    fmap (coerce :: CLong -> FSItemAttribute) $ sendMsg fsItemSetAttributesRequest (mkSelector "consumedAttributes") retCLong []

-- | The attributes successfully used by the file system.
--
-- This property is a bit field in Objective-C and an <doc://com.apple.documentation/documentation/Swift/OptionSet> in Swift.
--
-- ObjC selector: @- setConsumedAttributes:@
setConsumedAttributes :: IsFSItemSetAttributesRequest fsItemSetAttributesRequest => fsItemSetAttributesRequest -> FSItemAttribute -> IO ()
setConsumedAttributes fsItemSetAttributesRequest  value =
    sendMsg fsItemSetAttributesRequest (mkSelector "setConsumedAttributes:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wasAttributeConsumed:@
wasAttributeConsumedSelector :: Selector
wasAttributeConsumedSelector = mkSelector "wasAttributeConsumed:"

-- | @Selector@ for @consumedAttributes@
consumedAttributesSelector :: Selector
consumedAttributesSelector = mkSelector "consumedAttributes"

-- | @Selector@ for @setConsumedAttributes:@
setConsumedAttributesSelector :: Selector
setConsumedAttributesSelector = mkSelector "setConsumedAttributes:"

