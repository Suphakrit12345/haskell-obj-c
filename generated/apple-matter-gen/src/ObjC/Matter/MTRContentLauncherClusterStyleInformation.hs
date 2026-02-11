{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterStyleInformation@.
module ObjC.Matter.MTRContentLauncherClusterStyleInformation
  ( MTRContentLauncherClusterStyleInformation
  , IsMTRContentLauncherClusterStyleInformation(..)
  , color
  , setColor
  , size
  , setSize
  , colorSelector
  , setColorSelector
  , sizeSelector
  , setSizeSelector


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

-- | @- color@
color :: IsMTRContentLauncherClusterStyleInformation mtrContentLauncherClusterStyleInformation => mtrContentLauncherClusterStyleInformation -> IO (Id NSString)
color mtrContentLauncherClusterStyleInformation  =
    sendMsg mtrContentLauncherClusterStyleInformation (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColor:@
setColor :: (IsMTRContentLauncherClusterStyleInformation mtrContentLauncherClusterStyleInformation, IsNSString value) => mtrContentLauncherClusterStyleInformation -> value -> IO ()
setColor mtrContentLauncherClusterStyleInformation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterStyleInformation (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- size@
size :: IsMTRContentLauncherClusterStyleInformation mtrContentLauncherClusterStyleInformation => mtrContentLauncherClusterStyleInformation -> IO (Id MTRContentLauncherClusterDimensionStruct)
size mtrContentLauncherClusterStyleInformation  =
    sendMsg mtrContentLauncherClusterStyleInformation (mkSelector "size") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSize:@
setSize :: (IsMTRContentLauncherClusterStyleInformation mtrContentLauncherClusterStyleInformation, IsMTRContentLauncherClusterDimensionStruct value) => mtrContentLauncherClusterStyleInformation -> value -> IO ()
setSize mtrContentLauncherClusterStyleInformation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterStyleInformation (mkSelector "setSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector
setSizeSelector = mkSelector "setSize:"

