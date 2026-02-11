{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @JRSSymbolicator@.
module ObjC.JavaRuntimeSupport.JRSSymbolicator
  ( JRSSymbolicator
  , IsJRSSymbolicator(..)
  , symbolicatorForPid
  , addressForSymbol
  , symbolicatorForPidSelector
  , addressForSymbolSelector


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

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ symbolicatorForPid:@
symbolicatorForPid :: CInt -> IO (Id JRSSymbolicator)
symbolicatorForPid pid =
  do
    cls' <- getRequiredClass "JRSSymbolicator"
    sendClassMsg cls' (mkSelector "symbolicatorForPid:") (retPtr retVoid) [argCInt pid] >>= retainedObject . castPtr

-- | @- addressForSymbol:@
addressForSymbol :: (IsJRSSymbolicator jrsSymbolicator, IsNSString symbolName) => jrsSymbolicator -> symbolName -> IO CULong
addressForSymbol jrsSymbolicator  symbolName =
  withObjCPtr symbolName $ \raw_symbolName ->
      sendMsg jrsSymbolicator (mkSelector "addressForSymbol:") retCULong [argPtr (castPtr raw_symbolName :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @symbolicatorForPid:@
symbolicatorForPidSelector :: Selector
symbolicatorForPidSelector = mkSelector "symbolicatorForPid:"

-- | @Selector@ for @addressForSymbol:@
addressForSymbolSelector :: Selector
addressForSymbolSelector = mkSelector "addressForSymbol:"

