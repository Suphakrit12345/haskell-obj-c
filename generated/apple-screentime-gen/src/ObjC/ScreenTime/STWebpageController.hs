{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @STWebpageController@.
module ObjC.ScreenTime.STWebpageController
  ( STWebpageController
  , IsSTWebpageController(..)
  , setBundleIdentifier_error
  , initWithNibName_bundle
  , initWithCoder
  , suppressUsageRecording
  , setSuppressUsageRecording
  , url
  , setURL
  , urlIsPlayingVideo
  , setURLIsPlayingVideo
  , urlIsPictureInPicture
  , setURLIsPictureInPicture
  , urlIsBlocked
  , profileIdentifier
  , setProfileIdentifier
  , setBundleIdentifier_errorSelector
  , initWithNibName_bundleSelector
  , initWithCoderSelector
  , suppressUsageRecordingSelector
  , setSuppressUsageRecordingSelector
  , urlSelector
  , setURLSelector
  , urlIsPlayingVideoSelector
  , setURLIsPlayingVideoSelector
  , urlIsPictureInPictureSelector
  , setURLIsPictureInPictureSelector
  , urlIsBlockedSelector
  , profileIdentifierSelector
  , setProfileIdentifierSelector


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

import ObjC.ScreenTime.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Changes the bundle identifier used to report web usage.
--
-- This is only supported for web browsers that have been properly registered with Screen Time.
--
-- - Parameters:   - bundleIdentifier: The bundle identifier that can be changed to facilitate web usage     reporting for a parent web browser from one of its helper processes or extensions.   - error: Any error that occurred while changing the bundle identifier.
--
-- ObjC selector: @- setBundleIdentifier:error:@
setBundleIdentifier_error :: (IsSTWebpageController stWebpageController, IsNSString bundleIdentifier, IsNSError error_) => stWebpageController -> bundleIdentifier -> error_ -> IO Bool
setBundleIdentifier_error stWebpageController  bundleIdentifier error_ =
  withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg stWebpageController (mkSelector "setBundleIdentifier:error:") retCULong [argPtr (castPtr raw_bundleIdentifier :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- initWithNibName:bundle:@
initWithNibName_bundle :: (IsSTWebpageController stWebpageController, IsNSString nibNameOrNil, IsNSBundle nibBundleOrNil) => stWebpageController -> nibNameOrNil -> nibBundleOrNil -> IO (Id STWebpageController)
initWithNibName_bundle stWebpageController  nibNameOrNil nibBundleOrNil =
  withObjCPtr nibNameOrNil $ \raw_nibNameOrNil ->
    withObjCPtr nibBundleOrNil $ \raw_nibBundleOrNil ->
        sendMsg stWebpageController (mkSelector "initWithNibName:bundle:") (retPtr retVoid) [argPtr (castPtr raw_nibNameOrNil :: Ptr ()), argPtr (castPtr raw_nibBundleOrNil :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsSTWebpageController stWebpageController, IsNSCoder aDecoder) => stWebpageController -> aDecoder -> IO (Id STWebpageController)
initWithCoder stWebpageController  aDecoder =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg stWebpageController (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | A Boolean that indicates whether the webpage controller is not recording web usage.
--
-- Set to <doc://com.apple.documentation/documentation/objectivec/yes> to stop recording and reporting web-usage data.
--
-- ObjC selector: @- suppressUsageRecording@
suppressUsageRecording :: IsSTWebpageController stWebpageController => stWebpageController -> IO Bool
suppressUsageRecording stWebpageController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg stWebpageController (mkSelector "suppressUsageRecording") retCULong []

-- | A Boolean that indicates whether the webpage controller is not recording web usage.
--
-- Set to <doc://com.apple.documentation/documentation/objectivec/yes> to stop recording and reporting web-usage data.
--
-- ObjC selector: @- setSuppressUsageRecording:@
setSuppressUsageRecording :: IsSTWebpageController stWebpageController => stWebpageController -> Bool -> IO ()
setSuppressUsageRecording stWebpageController  value =
    sendMsg stWebpageController (mkSelector "setSuppressUsageRecording:") retVoid [argCULong (if value then 1 else 0)]

-- | The URL for the webpage.
--
-- Set this value to the webpage’s URL when the user navigates to a new URL.
--
-- ObjC selector: @- URL@
url :: IsSTWebpageController stWebpageController => stWebpageController -> IO (Id NSURL)
url stWebpageController  =
    sendMsg stWebpageController (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The URL for the webpage.
--
-- Set this value to the webpage’s URL when the user navigates to a new URL.
--
-- ObjC selector: @- setURL:@
setURL :: (IsSTWebpageController stWebpageController, IsNSURL value) => stWebpageController -> value -> IO ()
setURL stWebpageController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg stWebpageController (mkSelector "setURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A Boolean that indicates whether there are one or more videos currently playing in the webpage.
--
-- The default value is <doc://com.apple.documentation/documentation/objectivec/no>. Set this value when the webpage starts or stops playing video.
--
-- - Important: Set this value to <doc://com.apple.documentation/documentation/objectivec/no> prior to changing ``ScreenTime/STWebpageController/URL`` if the new webpage at that URL stops currently playing media and won’t immediately start playing new media.
--
-- ObjC selector: @- URLIsPlayingVideo@
urlIsPlayingVideo :: IsSTWebpageController stWebpageController => stWebpageController -> IO Bool
urlIsPlayingVideo stWebpageController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg stWebpageController (mkSelector "URLIsPlayingVideo") retCULong []

-- | A Boolean that indicates whether there are one or more videos currently playing in the webpage.
--
-- The default value is <doc://com.apple.documentation/documentation/objectivec/no>. Set this value when the webpage starts or stops playing video.
--
-- - Important: Set this value to <doc://com.apple.documentation/documentation/objectivec/no> prior to changing ``ScreenTime/STWebpageController/URL`` if the new webpage at that URL stops currently playing media and won’t immediately start playing new media.
--
-- ObjC selector: @- setURLIsPlayingVideo:@
setURLIsPlayingVideo :: IsSTWebpageController stWebpageController => stWebpageController -> Bool -> IO ()
setURLIsPlayingVideo stWebpageController  value =
    sendMsg stWebpageController (mkSelector "setURLIsPlayingVideo:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean that indicates whether the webpage is currently displaying a floating picture in picture window.
--
-- The default value is <doc://com.apple.documentation/documentation/objectivec/no>. Set this value when the webpage starts or stops displaying a Picture in Picture window.
--
-- - Important: Set this value to <doc://com.apple.documentation/documentation/objectivec/no> prior to changing ``ScreenTime/STWebpageController/URL`` if the new webpage at that URL ends all currently displayed Picture in Picture windows, and won’t immediately display a new one.
--
-- ObjC selector: @- URLIsPictureInPicture@
urlIsPictureInPicture :: IsSTWebpageController stWebpageController => stWebpageController -> IO Bool
urlIsPictureInPicture stWebpageController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg stWebpageController (mkSelector "URLIsPictureInPicture") retCULong []

-- | A Boolean that indicates whether the webpage is currently displaying a floating picture in picture window.
--
-- The default value is <doc://com.apple.documentation/documentation/objectivec/no>. Set this value when the webpage starts or stops displaying a Picture in Picture window.
--
-- - Important: Set this value to <doc://com.apple.documentation/documentation/objectivec/no> prior to changing ``ScreenTime/STWebpageController/URL`` if the new webpage at that URL ends all currently displayed Picture in Picture windows, and won’t immediately display a new one.
--
-- ObjC selector: @- setURLIsPictureInPicture:@
setURLIsPictureInPicture :: IsSTWebpageController stWebpageController => stWebpageController -> Bool -> IO ()
setURLIsPictureInPicture stWebpageController  value =
    sendMsg stWebpageController (mkSelector "setURLIsPictureInPicture:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean that indicates whether a parent or guardian has blocked the URL.
--
-- When a parent or guardian blocks the webpage’s URL, the webpage controller displays a blocking UI and then sets this property to <doc://com.apple.documentation/documentation/objectivec/yes>.
--
-- ObjC selector: @- URLIsBlocked@
urlIsBlocked :: IsSTWebpageController stWebpageController => stWebpageController -> IO Bool
urlIsBlocked stWebpageController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg stWebpageController (mkSelector "URLIsBlocked") retCULong []

-- | An optional identifier for the current browsing profile.
--
-- The default value is @nil@. This identifier represents a profile and allows you to keep your browsing separate for topics like work, personal, or school. Using @nil@ will report web history without a profile identifier. Web browsers with a "default" profile may want to use @nil@ in order to match any web history reported prior to this API.
--
-- ObjC selector: @- profileIdentifier@
profileIdentifier :: IsSTWebpageController stWebpageController => stWebpageController -> IO (Id NSString)
profileIdentifier stWebpageController  =
    sendMsg stWebpageController (mkSelector "profileIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An optional identifier for the current browsing profile.
--
-- The default value is @nil@. This identifier represents a profile and allows you to keep your browsing separate for topics like work, personal, or school. Using @nil@ will report web history without a profile identifier. Web browsers with a "default" profile may want to use @nil@ in order to match any web history reported prior to this API.
--
-- ObjC selector: @- setProfileIdentifier:@
setProfileIdentifier :: (IsSTWebpageController stWebpageController, IsNSString value) => stWebpageController -> value -> IO ()
setProfileIdentifier stWebpageController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg stWebpageController (mkSelector "setProfileIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setBundleIdentifier:error:@
setBundleIdentifier_errorSelector :: Selector
setBundleIdentifier_errorSelector = mkSelector "setBundleIdentifier:error:"

-- | @Selector@ for @initWithNibName:bundle:@
initWithNibName_bundleSelector :: Selector
initWithNibName_bundleSelector = mkSelector "initWithNibName:bundle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @suppressUsageRecording@
suppressUsageRecordingSelector :: Selector
suppressUsageRecordingSelector = mkSelector "suppressUsageRecording"

-- | @Selector@ for @setSuppressUsageRecording:@
setSuppressUsageRecordingSelector :: Selector
setSuppressUsageRecordingSelector = mkSelector "setSuppressUsageRecording:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @URLIsPlayingVideo@
urlIsPlayingVideoSelector :: Selector
urlIsPlayingVideoSelector = mkSelector "URLIsPlayingVideo"

-- | @Selector@ for @setURLIsPlayingVideo:@
setURLIsPlayingVideoSelector :: Selector
setURLIsPlayingVideoSelector = mkSelector "setURLIsPlayingVideo:"

-- | @Selector@ for @URLIsPictureInPicture@
urlIsPictureInPictureSelector :: Selector
urlIsPictureInPictureSelector = mkSelector "URLIsPictureInPicture"

-- | @Selector@ for @setURLIsPictureInPicture:@
setURLIsPictureInPictureSelector :: Selector
setURLIsPictureInPictureSelector = mkSelector "setURLIsPictureInPicture:"

-- | @Selector@ for @URLIsBlocked@
urlIsBlockedSelector :: Selector
urlIsBlockedSelector = mkSelector "URLIsBlocked"

-- | @Selector@ for @profileIdentifier@
profileIdentifierSelector :: Selector
profileIdentifierSelector = mkSelector "profileIdentifier"

-- | @Selector@ for @setProfileIdentifier:@
setProfileIdentifierSelector :: Selector
setProfileIdentifierSelector = mkSelector "setProfileIdentifier:"

