{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKFitzpatrickSkinTypeObject
--
-- A wrapper object for HKFitzpatrickSkinType enumeration.
--
-- Generated bindings for @HKFitzpatrickSkinTypeObject@.
module ObjC.HealthKit.HKFitzpatrickSkinTypeObject
  ( HKFitzpatrickSkinTypeObject
  , IsHKFitzpatrickSkinTypeObject(..)
  , skinType
  , skinTypeSelector

  -- * Enum types
  , HKFitzpatrickSkinType(HKFitzpatrickSkinType)
  , pattern HKFitzpatrickSkinTypeNotSet
  , pattern HKFitzpatrickSkinTypeI
  , pattern HKFitzpatrickSkinTypeII
  , pattern HKFitzpatrickSkinTypeIII
  , pattern HKFitzpatrickSkinTypeIV
  , pattern HKFitzpatrickSkinTypeV
  , pattern HKFitzpatrickSkinTypeVI

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

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- skinType@
skinType :: IsHKFitzpatrickSkinTypeObject hkFitzpatrickSkinTypeObject => hkFitzpatrickSkinTypeObject -> IO HKFitzpatrickSkinType
skinType hkFitzpatrickSkinTypeObject  =
    fmap (coerce :: CLong -> HKFitzpatrickSkinType) $ sendMsg hkFitzpatrickSkinTypeObject (mkSelector "skinType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @skinType@
skinTypeSelector :: Selector
skinTypeSelector = mkSelector "skinType"

