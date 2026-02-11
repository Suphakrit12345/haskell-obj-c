{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterBrandingInformationStruct@.
module ObjC.Matter.MTRContentLauncherClusterBrandingInformationStruct
  ( MTRContentLauncherClusterBrandingInformationStruct
  , IsMTRContentLauncherClusterBrandingInformationStruct(..)
  , providerName
  , setProviderName
  , background
  , setBackground
  , logo
  , setLogo
  , progressBar
  , setProgressBar
  , splash
  , setSplash
  , waterMark
  , setWaterMark
  , providerNameSelector
  , setProviderNameSelector
  , backgroundSelector
  , setBackgroundSelector
  , logoSelector
  , setLogoSelector
  , progressBarSelector
  , setProgressBarSelector
  , splashSelector
  , setSplashSelector
  , waterMarkSelector
  , setWaterMarkSelector


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

-- | @- providerName@
providerName :: IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct => mtrContentLauncherClusterBrandingInformationStruct -> IO (Id NSString)
providerName mtrContentLauncherClusterBrandingInformationStruct  =
    sendMsg mtrContentLauncherClusterBrandingInformationStruct (mkSelector "providerName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProviderName:@
setProviderName :: (IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct, IsNSString value) => mtrContentLauncherClusterBrandingInformationStruct -> value -> IO ()
setProviderName mtrContentLauncherClusterBrandingInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterBrandingInformationStruct (mkSelector "setProviderName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- background@
background :: IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct => mtrContentLauncherClusterBrandingInformationStruct -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
background mtrContentLauncherClusterBrandingInformationStruct  =
    sendMsg mtrContentLauncherClusterBrandingInformationStruct (mkSelector "background") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackground:@
setBackground :: (IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformationStruct -> value -> IO ()
setBackground mtrContentLauncherClusterBrandingInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterBrandingInformationStruct (mkSelector "setBackground:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- logo@
logo :: IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct => mtrContentLauncherClusterBrandingInformationStruct -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
logo mtrContentLauncherClusterBrandingInformationStruct  =
    sendMsg mtrContentLauncherClusterBrandingInformationStruct (mkSelector "logo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLogo:@
setLogo :: (IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformationStruct -> value -> IO ()
setLogo mtrContentLauncherClusterBrandingInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterBrandingInformationStruct (mkSelector "setLogo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- progressBar@
progressBar :: IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct => mtrContentLauncherClusterBrandingInformationStruct -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
progressBar mtrContentLauncherClusterBrandingInformationStruct  =
    sendMsg mtrContentLauncherClusterBrandingInformationStruct (mkSelector "progressBar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProgressBar:@
setProgressBar :: (IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformationStruct -> value -> IO ()
setProgressBar mtrContentLauncherClusterBrandingInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterBrandingInformationStruct (mkSelector "setProgressBar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- splash@
splash :: IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct => mtrContentLauncherClusterBrandingInformationStruct -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
splash mtrContentLauncherClusterBrandingInformationStruct  =
    sendMsg mtrContentLauncherClusterBrandingInformationStruct (mkSelector "splash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSplash:@
setSplash :: (IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformationStruct -> value -> IO ()
setSplash mtrContentLauncherClusterBrandingInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterBrandingInformationStruct (mkSelector "setSplash:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- waterMark@
waterMark :: IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct => mtrContentLauncherClusterBrandingInformationStruct -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
waterMark mtrContentLauncherClusterBrandingInformationStruct  =
    sendMsg mtrContentLauncherClusterBrandingInformationStruct (mkSelector "waterMark") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWaterMark:@
setWaterMark :: (IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformationStruct -> value -> IO ()
setWaterMark mtrContentLauncherClusterBrandingInformationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterBrandingInformationStruct (mkSelector "setWaterMark:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @providerName@
providerNameSelector :: Selector
providerNameSelector = mkSelector "providerName"

-- | @Selector@ for @setProviderName:@
setProviderNameSelector :: Selector
setProviderNameSelector = mkSelector "setProviderName:"

-- | @Selector@ for @background@
backgroundSelector :: Selector
backgroundSelector = mkSelector "background"

-- | @Selector@ for @setBackground:@
setBackgroundSelector :: Selector
setBackgroundSelector = mkSelector "setBackground:"

-- | @Selector@ for @logo@
logoSelector :: Selector
logoSelector = mkSelector "logo"

-- | @Selector@ for @setLogo:@
setLogoSelector :: Selector
setLogoSelector = mkSelector "setLogo:"

-- | @Selector@ for @progressBar@
progressBarSelector :: Selector
progressBarSelector = mkSelector "progressBar"

-- | @Selector@ for @setProgressBar:@
setProgressBarSelector :: Selector
setProgressBarSelector = mkSelector "setProgressBar:"

-- | @Selector@ for @splash@
splashSelector :: Selector
splashSelector = mkSelector "splash"

-- | @Selector@ for @setSplash:@
setSplashSelector :: Selector
setSplashSelector = mkSelector "setSplash:"

-- | @Selector@ for @waterMark@
waterMarkSelector :: Selector
waterMarkSelector = mkSelector "waterMark"

-- | @Selector@ for @setWaterMark:@
setWaterMarkSelector :: Selector
setWaterMarkSelector = mkSelector "setWaterMark:"

