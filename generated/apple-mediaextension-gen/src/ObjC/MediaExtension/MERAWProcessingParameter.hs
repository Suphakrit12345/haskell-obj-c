{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MERAWProcessingParameter
--
-- An object implementing this protocol is implemented by the RAW Processor to describe each processing parameter the Processor exposes.
--
-- The MERAWProcessingParameter protocol provides an interface for the VideoToolbox to query descriptions of the different parameters that can be used to influence Processor operation.  A distinct MERAWProcessingParameter is created for each parameter supported by the Processor, and the set of supported parameters is returned by the MERAWProcessor's processingParameters interface.
--
-- Generated bindings for @MERAWProcessingParameter@.
module ObjC.MediaExtension.MERAWProcessingParameter
  ( MERAWProcessingParameter
  , IsMERAWProcessingParameter(..)
  , name
  , key
  , longDescription
  , enabled
  , setEnabled
  , nameSelector
  , keySelector
  , longDescriptionSelector
  , enabledSelector
  , setEnabledSelector


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

-- | name
--
-- A localized human-readable name for the parameter, suitable for displaying in application UI.
--
-- ObjC selector: @- name@
name :: IsMERAWProcessingParameter merawProcessingParameter => merawProcessingParameter -> IO (Id NSString)
name merawProcessingParameter  =
    sendMsg merawProcessingParameter (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | key
--
-- A unique key string identifying this parameter.
--
-- ObjC selector: @- key@
key :: IsMERAWProcessingParameter merawProcessingParameter => merawProcessingParameter -> IO (Id NSString)
key merawProcessingParameter  =
    sendMsg merawProcessingParameter (mkSelector "key") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | longDescription
--
-- A localized description of the parameter, suitable for displaying in a tool tip or similar explanatory UI.
--
-- ObjC selector: @- longDescription@
longDescription :: IsMERAWProcessingParameter merawProcessingParameter => merawProcessingParameter -> IO (Id NSString)
longDescription merawProcessingParameter  =
    sendMsg merawProcessingParameter (mkSelector "longDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | enabled
--
-- Indicates whether the parameter is enabled or disabled by the extension.
--
-- This parameter can only be modified by the extension.  From the application-facing interface, VTRAWProcessingSession, this is a read-only value which indicates whether the parameter should be greyed out and disabled in any UI being generated.
--
-- ObjC selector: @- enabled@
enabled :: IsMERAWProcessingParameter merawProcessingParameter => merawProcessingParameter -> IO Bool
enabled merawProcessingParameter  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg merawProcessingParameter (mkSelector "enabled") retCULong []

-- | enabled
--
-- Indicates whether the parameter is enabled or disabled by the extension.
--
-- This parameter can only be modified by the extension.  From the application-facing interface, VTRAWProcessingSession, this is a read-only value which indicates whether the parameter should be greyed out and disabled in any UI being generated.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsMERAWProcessingParameter merawProcessingParameter => merawProcessingParameter -> Bool -> IO ()
setEnabled merawProcessingParameter  value =
    sendMsg merawProcessingParameter (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @key@
keySelector :: Selector
keySelector = mkSelector "key"

-- | @Selector@ for @longDescription@
longDescriptionSelector :: Selector
longDescriptionSelector = mkSelector "longDescription"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

