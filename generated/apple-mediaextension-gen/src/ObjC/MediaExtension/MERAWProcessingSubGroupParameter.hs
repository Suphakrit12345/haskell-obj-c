{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MERAWProcessingSubGroupParameter@.
module ObjC.MediaExtension.MERAWProcessingSubGroupParameter
  ( MERAWProcessingSubGroupParameter
  , IsMERAWProcessingSubGroupParameter(..)
  , initWithName_description_parameters
  , subGroupParameters
  , initWithName_description_parametersSelector
  , subGroupParametersSelector


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

-- | @- initWithName:description:parameters:@
initWithName_description_parameters :: (IsMERAWProcessingSubGroupParameter merawProcessingSubGroupParameter, IsNSString name, IsNSString description, IsNSArray parameters) => merawProcessingSubGroupParameter -> name -> description -> parameters -> IO (Id MERAWProcessingSubGroupParameter)
initWithName_description_parameters merawProcessingSubGroupParameter  name description parameters =
  withObjCPtr name $ \raw_name ->
    withObjCPtr description $ \raw_description ->
      withObjCPtr parameters $ \raw_parameters ->
          sendMsg merawProcessingSubGroupParameter (mkSelector "initWithName:description:parameters:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argPtr (castPtr raw_parameters :: Ptr ())] >>= ownedObject . castPtr

-- | @- subGroupParameters@
subGroupParameters :: IsMERAWProcessingSubGroupParameter merawProcessingSubGroupParameter => merawProcessingSubGroupParameter -> IO (Id NSArray)
subGroupParameters merawProcessingSubGroupParameter  =
    sendMsg merawProcessingSubGroupParameter (mkSelector "subGroupParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:description:parameters:@
initWithName_description_parametersSelector :: Selector
initWithName_description_parametersSelector = mkSelector "initWithName:description:parameters:"

-- | @Selector@ for @subGroupParameters@
subGroupParametersSelector :: Selector
subGroupParametersSelector = mkSelector "subGroupParameters"

