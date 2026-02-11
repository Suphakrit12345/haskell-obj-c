{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterBrandingInformation@.
module ObjC.Matter.MTRContentLauncherClusterBrandingInformation
  ( MTRContentLauncherClusterBrandingInformation
  , IsMTRContentLauncherClusterBrandingInformation(..)
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
providerName :: IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation => mtrContentLauncherClusterBrandingInformation -> IO (Id NSString)
providerName mtrContentLauncherClusterBrandingInformation  =
    sendMsg mtrContentLauncherClusterBrandingInformation (mkSelector "providerName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProviderName:@
setProviderName :: (IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation, IsNSString value) => mtrContentLauncherClusterBrandingInformation -> value -> IO ()
setProviderName mtrContentLauncherClusterBrandingInformation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterBrandingInformation (mkSelector "setProviderName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- background@
background :: IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation => mtrContentLauncherClusterBrandingInformation -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
background mtrContentLauncherClusterBrandingInformation  =
    sendMsg mtrContentLauncherClusterBrandingInformation (mkSelector "background") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackground:@
setBackground :: (IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformation -> value -> IO ()
setBackground mtrContentLauncherClusterBrandingInformation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterBrandingInformation (mkSelector "setBackground:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- logo@
logo :: IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation => mtrContentLauncherClusterBrandingInformation -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
logo mtrContentLauncherClusterBrandingInformation  =
    sendMsg mtrContentLauncherClusterBrandingInformation (mkSelector "logo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLogo:@
setLogo :: (IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformation -> value -> IO ()
setLogo mtrContentLauncherClusterBrandingInformation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterBrandingInformation (mkSelector "setLogo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- progressBar@
progressBar :: IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation => mtrContentLauncherClusterBrandingInformation -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
progressBar mtrContentLauncherClusterBrandingInformation  =
    sendMsg mtrContentLauncherClusterBrandingInformation (mkSelector "progressBar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProgressBar:@
setProgressBar :: (IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformation -> value -> IO ()
setProgressBar mtrContentLauncherClusterBrandingInformation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterBrandingInformation (mkSelector "setProgressBar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- splash@
splash :: IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation => mtrContentLauncherClusterBrandingInformation -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
splash mtrContentLauncherClusterBrandingInformation  =
    sendMsg mtrContentLauncherClusterBrandingInformation (mkSelector "splash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSplash:@
setSplash :: (IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformation -> value -> IO ()
setSplash mtrContentLauncherClusterBrandingInformation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterBrandingInformation (mkSelector "setSplash:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- waterMark@
waterMark :: IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation => mtrContentLauncherClusterBrandingInformation -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
waterMark mtrContentLauncherClusterBrandingInformation  =
    sendMsg mtrContentLauncherClusterBrandingInformation (mkSelector "waterMark") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWaterMark:@
setWaterMark :: (IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformation -> value -> IO ()
setWaterMark mtrContentLauncherClusterBrandingInformation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterBrandingInformation (mkSelector "setWaterMark:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

