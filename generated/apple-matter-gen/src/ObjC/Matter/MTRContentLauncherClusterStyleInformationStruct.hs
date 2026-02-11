{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterStyleInformationStruct@.
module ObjC.Matter.MTRContentLauncherClusterStyleInformationStruct
  ( MTRContentLauncherClusterStyleInformationStruct
  , IsMTRContentLauncherClusterStyleInformationStruct(..)
  , imageURL
  , setImageURL
  , imageUrl
  , setImageUrl
  , color
  , setColor
  , size
  , setSize
  , imageURLSelector
  , setImageURLSelector
  , imageUrlSelector
  , setImageUrlSelector
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

-- | @- imageURL@
imageURL :: IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct => mtrContentLauncherClusterStyleInformationStruct -> IO (Id NSString)
imageURL mtrContentLauncherClusterStyleInformationStruct  =
    sendMsg mtrContentLauncherClusterStyleInformationStruct (mkSelector "imageURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImageURL:@
setImageURL :: (IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct, IsNSString value) => mtrContentLauncherClusterStyleInformationStruct -> value -> IO ()
setImageURL mtrContentLauncherClusterStyleInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterStyleInformationStruct (mkSelector "setImageURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- imageUrl@
imageUrl :: IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct => mtrContentLauncherClusterStyleInformationStruct -> IO (Id NSString)
imageUrl mtrContentLauncherClusterStyleInformationStruct  =
    sendMsg mtrContentLauncherClusterStyleInformationStruct (mkSelector "imageUrl") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImageUrl:@
setImageUrl :: (IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct, IsNSString value) => mtrContentLauncherClusterStyleInformationStruct -> value -> IO ()
setImageUrl mtrContentLauncherClusterStyleInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterStyleInformationStruct (mkSelector "setImageUrl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- color@
color :: IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct => mtrContentLauncherClusterStyleInformationStruct -> IO (Id NSString)
color mtrContentLauncherClusterStyleInformationStruct  =
    sendMsg mtrContentLauncherClusterStyleInformationStruct (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColor:@
setColor :: (IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct, IsNSString value) => mtrContentLauncherClusterStyleInformationStruct -> value -> IO ()
setColor mtrContentLauncherClusterStyleInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterStyleInformationStruct (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- size@
size :: IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct => mtrContentLauncherClusterStyleInformationStruct -> IO (Id MTRContentLauncherClusterDimensionStruct)
size mtrContentLauncherClusterStyleInformationStruct  =
    sendMsg mtrContentLauncherClusterStyleInformationStruct (mkSelector "size") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSize:@
setSize :: (IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct, IsMTRContentLauncherClusterDimensionStruct value) => mtrContentLauncherClusterStyleInformationStruct -> value -> IO ()
setSize mtrContentLauncherClusterStyleInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterStyleInformationStruct (mkSelector "setSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageURL@
imageURLSelector :: Selector
imageURLSelector = mkSelector "imageURL"

-- | @Selector@ for @setImageURL:@
setImageURLSelector :: Selector
setImageURLSelector = mkSelector "setImageURL:"

-- | @Selector@ for @imageUrl@
imageUrlSelector :: Selector
imageUrlSelector = mkSelector "imageUrl"

-- | @Selector@ for @setImageUrl:@
setImageUrlSelector :: Selector
setImageUrlSelector = mkSelector "setImageUrl:"

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

