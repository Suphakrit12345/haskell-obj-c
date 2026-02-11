{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterProgramCategoryStruct@.
module ObjC.Matter.MTRChannelClusterProgramCategoryStruct
  ( MTRChannelClusterProgramCategoryStruct
  , IsMTRChannelClusterProgramCategoryStruct(..)
  , category
  , setCategory
  , subCategory
  , setSubCategory
  , categorySelector
  , setCategorySelector
  , subCategorySelector
  , setSubCategorySelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- category@
category :: IsMTRChannelClusterProgramCategoryStruct mtrChannelClusterProgramCategoryStruct => mtrChannelClusterProgramCategoryStruct -> IO (Id NSString)
category mtrChannelClusterProgramCategoryStruct  =
    sendMsg mtrChannelClusterProgramCategoryStruct (mkSelector "category") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCategory:@
setCategory :: (IsMTRChannelClusterProgramCategoryStruct mtrChannelClusterProgramCategoryStruct, IsNSString value) => mtrChannelClusterProgramCategoryStruct -> value -> IO ()
setCategory mtrChannelClusterProgramCategoryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramCategoryStruct (mkSelector "setCategory:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subCategory@
subCategory :: IsMTRChannelClusterProgramCategoryStruct mtrChannelClusterProgramCategoryStruct => mtrChannelClusterProgramCategoryStruct -> IO (Id NSString)
subCategory mtrChannelClusterProgramCategoryStruct  =
    sendMsg mtrChannelClusterProgramCategoryStruct (mkSelector "subCategory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubCategory:@
setSubCategory :: (IsMTRChannelClusterProgramCategoryStruct mtrChannelClusterProgramCategoryStruct, IsNSString value) => mtrChannelClusterProgramCategoryStruct -> value -> IO ()
setSubCategory mtrChannelClusterProgramCategoryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramCategoryStruct (mkSelector "setSubCategory:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @category@
categorySelector :: Selector
categorySelector = mkSelector "category"

-- | @Selector@ for @setCategory:@
setCategorySelector :: Selector
setCategorySelector = mkSelector "setCategory:"

-- | @Selector@ for @subCategory@
subCategorySelector :: Selector
subCategorySelector = mkSelector "subCategory"

-- | @Selector@ for @setSubCategory:@
setSubCategorySelector :: Selector
setSubCategorySelector = mkSelector "setSubCategory:"

