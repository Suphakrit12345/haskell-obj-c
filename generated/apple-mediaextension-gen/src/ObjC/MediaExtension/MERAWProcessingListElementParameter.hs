{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MERAWProcessingListElementParameter
--
-- An object implementing this protocol is implemented by the RAW Processor to describe each processing parameter the Processor exposes.
--
-- The MERAWProcessingListElementParameter protocol provides an interface for VideoToolbox to query descriptions of the different elements in a parameter list  for a List element in a MERAWProcessingParameter.  A distinct MERAWProcessingListElementParameter is created for each list element.
--
-- Generated bindings for @MERAWProcessingListElementParameter@.
module ObjC.MediaExtension.MERAWProcessingListElementParameter
  ( MERAWProcessingListElementParameter
  , IsMERAWProcessingListElementParameter(..)
  , initWithName_description_elementID
  , listElementID
  , initWithName_description_elementIDSelector
  , listElementIDSelector


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

-- | @- initWithName:description:elementID:@
initWithName_description_elementID :: (IsMERAWProcessingListElementParameter merawProcessingListElementParameter, IsNSString name, IsNSString description) => merawProcessingListElementParameter -> name -> description -> CLong -> IO (Id MERAWProcessingListElementParameter)
initWithName_description_elementID merawProcessingListElementParameter  name description elementID =
  withObjCPtr name $ \raw_name ->
    withObjCPtr description $ \raw_description ->
        sendMsg merawProcessingListElementParameter (mkSelector "initWithName:description:elementID:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argCLong elementID] >>= ownedObject . castPtr

-- | listElementID
--
-- A unique number in this list which represents this list option.
--
-- The set of elements in the list may change depending on other configuration parameters, so while the index of an element in this list may change, this ID never changes and is used to report list element selection
--
-- ObjC selector: @- listElementID@
listElementID :: IsMERAWProcessingListElementParameter merawProcessingListElementParameter => merawProcessingListElementParameter -> IO CLong
listElementID merawProcessingListElementParameter  =
    sendMsg merawProcessingListElementParameter (mkSelector "listElementID") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:description:elementID:@
initWithName_description_elementIDSelector :: Selector
initWithName_description_elementIDSelector = mkSelector "initWithName:description:elementID:"

-- | @Selector@ for @listElementID@
listElementIDSelector :: Selector
listElementIDSelector = mkSelector "listElementID"

