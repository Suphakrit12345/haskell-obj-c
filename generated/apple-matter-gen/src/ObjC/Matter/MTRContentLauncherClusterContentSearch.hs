{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterContentSearch@.
module ObjC.Matter.MTRContentLauncherClusterContentSearch
  ( MTRContentLauncherClusterContentSearch
  , IsMTRContentLauncherClusterContentSearch(..)
  , parameterList
  , setParameterList
  , parameterListSelector
  , setParameterListSelector


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

-- | @- parameterList@
parameterList :: IsMTRContentLauncherClusterContentSearch mtrContentLauncherClusterContentSearch => mtrContentLauncherClusterContentSearch -> IO (Id NSArray)
parameterList mtrContentLauncherClusterContentSearch  =
    sendMsg mtrContentLauncherClusterContentSearch (mkSelector "parameterList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParameterList:@
setParameterList :: (IsMTRContentLauncherClusterContentSearch mtrContentLauncherClusterContentSearch, IsNSArray value) => mtrContentLauncherClusterContentSearch -> value -> IO ()
setParameterList mtrContentLauncherClusterContentSearch  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterContentSearch (mkSelector "setParameterList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @parameterList@
parameterListSelector :: Selector
parameterListSelector = mkSelector "parameterList"

-- | @Selector@ for @setParameterList:@
setParameterListSelector :: Selector
setParameterListSelector = mkSelector "setParameterList:"

