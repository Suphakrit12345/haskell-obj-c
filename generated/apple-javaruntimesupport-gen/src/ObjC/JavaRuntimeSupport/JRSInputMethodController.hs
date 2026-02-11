{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @JRSInputMethodController@.
module ObjC.JavaRuntimeSupport.JRSInputMethodController
  ( JRSInputMethodController
  , IsJRSInputMethodController(..)
  , controller
  , availableInputMethodLocales
  , currentInputMethodName
  , currentInputMethodLocale
  , setCurrentInputMethodForLocale
  , controllerSelector
  , availableInputMethodLocalesSelector
  , currentInputMethodNameSelector
  , currentInputMethodLocaleSelector
  , setCurrentInputMethodForLocaleSelector


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

-- | @+ controller@
controller :: IO (Id JRSInputMethodController)
controller  =
  do
    cls' <- getRequiredClass "JRSInputMethodController"
    sendClassMsg cls' (mkSelector "controller") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- availableInputMethodLocales@
availableInputMethodLocales :: IsJRSInputMethodController jrsInputMethodController => jrsInputMethodController -> IO (Id NSArray)
availableInputMethodLocales jrsInputMethodController  =
    sendMsg jrsInputMethodController (mkSelector "availableInputMethodLocales") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentInputMethodName@
currentInputMethodName :: IsJRSInputMethodController jrsInputMethodController => jrsInputMethodController -> IO (Id NSString)
currentInputMethodName jrsInputMethodController  =
    sendMsg jrsInputMethodController (mkSelector "currentInputMethodName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentInputMethodLocale@
currentInputMethodLocale :: IsJRSInputMethodController jrsInputMethodController => jrsInputMethodController -> IO (Id NSString)
currentInputMethodLocale jrsInputMethodController  =
    sendMsg jrsInputMethodController (mkSelector "currentInputMethodLocale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentInputMethodForLocale:@
setCurrentInputMethodForLocale :: (IsJRSInputMethodController jrsInputMethodController, IsNSString theLocale) => jrsInputMethodController -> theLocale -> IO ()
setCurrentInputMethodForLocale jrsInputMethodController  theLocale =
  withObjCPtr theLocale $ \raw_theLocale ->
      sendMsg jrsInputMethodController (mkSelector "setCurrentInputMethodForLocale:") retVoid [argPtr (castPtr raw_theLocale :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @controller@
controllerSelector :: Selector
controllerSelector = mkSelector "controller"

-- | @Selector@ for @availableInputMethodLocales@
availableInputMethodLocalesSelector :: Selector
availableInputMethodLocalesSelector = mkSelector "availableInputMethodLocales"

-- | @Selector@ for @currentInputMethodName@
currentInputMethodNameSelector :: Selector
currentInputMethodNameSelector = mkSelector "currentInputMethodName"

-- | @Selector@ for @currentInputMethodLocale@
currentInputMethodLocaleSelector :: Selector
currentInputMethodLocaleSelector = mkSelector "currentInputMethodLocale"

-- | @Selector@ for @setCurrentInputMethodForLocale:@
setCurrentInputMethodForLocaleSelector :: Selector
setCurrentInputMethodForLocaleSelector = mkSelector "setCurrentInputMethodForLocale:"

