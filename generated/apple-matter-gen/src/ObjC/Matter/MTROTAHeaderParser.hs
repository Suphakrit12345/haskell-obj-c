{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTAHeaderParser@.
module ObjC.Matter.MTROTAHeaderParser
  ( MTROTAHeaderParser
  , IsMTROTAHeaderParser(..)
  , headerFromData_error
  , headerFromData_errorSelector


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

-- | @+ headerFromData:error:@
headerFromData_error :: (IsNSData data_, IsNSError error_) => data_ -> error_ -> IO (Id MTROTAHeader)
headerFromData_error data_ error_ =
  do
    cls' <- getRequiredClass "MTROTAHeaderParser"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "headerFromData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @headerFromData:error:@
headerFromData_errorSelector :: Selector
headerFromData_errorSelector = mkSelector "headerFromData:error:"

