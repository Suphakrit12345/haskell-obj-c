{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterProgramStruct@.
module ObjC.Matter.MTRChannelClusterProgramStruct
  ( MTRChannelClusterProgramStruct
  , IsMTRChannelClusterProgramStruct(..)
  , identifier
  , setIdentifier
  , channel
  , setChannel
  , startTime
  , setStartTime
  , endTime
  , setEndTime
  , title
  , setTitle
  , subtitle
  , setSubtitle
  , descriptionString
  , setDescriptionString
  , audioLanguages
  , setAudioLanguages
  , ratings
  , setRatings
  , thumbnailUrl
  , setThumbnailUrl
  , posterArtUrl
  , setPosterArtUrl
  , dvbiUrl
  , setDvbiUrl
  , releaseDate
  , setReleaseDate
  , parentalGuidanceText
  , setParentalGuidanceText
  , recordingFlag
  , setRecordingFlag
  , seriesInfo
  , setSeriesInfo
  , categoryList
  , setCategoryList
  , castList
  , setCastList
  , externalIDList
  , setExternalIDList
  , identifierSelector
  , setIdentifierSelector
  , channelSelector
  , setChannelSelector
  , startTimeSelector
  , setStartTimeSelector
  , endTimeSelector
  , setEndTimeSelector
  , titleSelector
  , setTitleSelector
  , subtitleSelector
  , setSubtitleSelector
  , descriptionStringSelector
  , setDescriptionStringSelector
  , audioLanguagesSelector
  , setAudioLanguagesSelector
  , ratingsSelector
  , setRatingsSelector
  , thumbnailUrlSelector
  , setThumbnailUrlSelector
  , posterArtUrlSelector
  , setPosterArtUrlSelector
  , dvbiUrlSelector
  , setDvbiUrlSelector
  , releaseDateSelector
  , setReleaseDateSelector
  , parentalGuidanceTextSelector
  , setParentalGuidanceTextSelector
  , recordingFlagSelector
  , setRecordingFlagSelector
  , seriesInfoSelector
  , setSeriesInfoSelector
  , categoryListSelector
  , setCategoryListSelector
  , castListSelector
  , setCastListSelector
  , externalIDListSelector
  , setExternalIDListSelector


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

-- | @- identifier@
identifier :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
identifier mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setIdentifier mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- channel@
channel :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id MTRChannelClusterChannelInfoStruct)
channel mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "channel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChannel:@
setChannel :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsMTRChannelClusterChannelInfoStruct value) => mtrChannelClusterProgramStruct -> value -> IO ()
setChannel mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setChannel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startTime@
startTime :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSNumber)
startTime mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "startTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartTime:@
setStartTime :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSNumber value) => mtrChannelClusterProgramStruct -> value -> IO ()
setStartTime mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endTime@
endTime :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSNumber)
endTime mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "endTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndTime:@
setEndTime :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSNumber value) => mtrChannelClusterProgramStruct -> value -> IO ()
setEndTime mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setEndTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- title@
title :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
title mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setTitle mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subtitle@
subtitle :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
subtitle mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubtitle:@
setSubtitle :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setSubtitle mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- descriptionString@
descriptionString :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
descriptionString mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "descriptionString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDescriptionString:@
setDescriptionString :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setDescriptionString mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setDescriptionString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioLanguages@
audioLanguages :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSArray)
audioLanguages mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "audioLanguages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioLanguages:@
setAudioLanguages :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSArray value) => mtrChannelClusterProgramStruct -> value -> IO ()
setAudioLanguages mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setAudioLanguages:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ratings@
ratings :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSArray)
ratings mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "ratings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRatings:@
setRatings :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSArray value) => mtrChannelClusterProgramStruct -> value -> IO ()
setRatings mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setRatings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- thumbnailUrl@
thumbnailUrl :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
thumbnailUrl mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "thumbnailUrl") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setThumbnailUrl:@
setThumbnailUrl :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setThumbnailUrl mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setThumbnailUrl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- posterArtUrl@
posterArtUrl :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
posterArtUrl mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "posterArtUrl") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPosterArtUrl:@
setPosterArtUrl :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setPosterArtUrl mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setPosterArtUrl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dvbiUrl@
dvbiUrl :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
dvbiUrl mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "dvbiUrl") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDvbiUrl:@
setDvbiUrl :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setDvbiUrl mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setDvbiUrl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- releaseDate@
releaseDate :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
releaseDate mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "releaseDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReleaseDate:@
setReleaseDate :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setReleaseDate mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setReleaseDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- parentalGuidanceText@
parentalGuidanceText :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
parentalGuidanceText mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "parentalGuidanceText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParentalGuidanceText:@
setParentalGuidanceText :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setParentalGuidanceText mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setParentalGuidanceText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recordingFlag@
recordingFlag :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSNumber)
recordingFlag mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "recordingFlag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecordingFlag:@
setRecordingFlag :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSNumber value) => mtrChannelClusterProgramStruct -> value -> IO ()
setRecordingFlag mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setRecordingFlag:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- seriesInfo@
seriesInfo :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id MTRChannelClusterSeriesInfoStruct)
seriesInfo mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "seriesInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSeriesInfo:@
setSeriesInfo :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsMTRChannelClusterSeriesInfoStruct value) => mtrChannelClusterProgramStruct -> value -> IO ()
setSeriesInfo mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setSeriesInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- categoryList@
categoryList :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSArray)
categoryList mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "categoryList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCategoryList:@
setCategoryList :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSArray value) => mtrChannelClusterProgramStruct -> value -> IO ()
setCategoryList mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setCategoryList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- castList@
castList :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSArray)
castList mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "castList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCastList:@
setCastList :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSArray value) => mtrChannelClusterProgramStruct -> value -> IO ()
setCastList mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setCastList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- externalIDList@
externalIDList :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSArray)
externalIDList mtrChannelClusterProgramStruct  =
    sendMsg mtrChannelClusterProgramStruct (mkSelector "externalIDList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExternalIDList:@
setExternalIDList :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSArray value) => mtrChannelClusterProgramStruct -> value -> IO ()
setExternalIDList mtrChannelClusterProgramStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramStruct (mkSelector "setExternalIDList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @channel@
channelSelector :: Selector
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector
setChannelSelector = mkSelector "setChannel:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @endTime@
endTimeSelector :: Selector
endTimeSelector = mkSelector "endTime"

-- | @Selector@ for @setEndTime:@
setEndTimeSelector :: Selector
setEndTimeSelector = mkSelector "setEndTime:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @descriptionString@
descriptionStringSelector :: Selector
descriptionStringSelector = mkSelector "descriptionString"

-- | @Selector@ for @setDescriptionString:@
setDescriptionStringSelector :: Selector
setDescriptionStringSelector = mkSelector "setDescriptionString:"

-- | @Selector@ for @audioLanguages@
audioLanguagesSelector :: Selector
audioLanguagesSelector = mkSelector "audioLanguages"

-- | @Selector@ for @setAudioLanguages:@
setAudioLanguagesSelector :: Selector
setAudioLanguagesSelector = mkSelector "setAudioLanguages:"

-- | @Selector@ for @ratings@
ratingsSelector :: Selector
ratingsSelector = mkSelector "ratings"

-- | @Selector@ for @setRatings:@
setRatingsSelector :: Selector
setRatingsSelector = mkSelector "setRatings:"

-- | @Selector@ for @thumbnailUrl@
thumbnailUrlSelector :: Selector
thumbnailUrlSelector = mkSelector "thumbnailUrl"

-- | @Selector@ for @setThumbnailUrl:@
setThumbnailUrlSelector :: Selector
setThumbnailUrlSelector = mkSelector "setThumbnailUrl:"

-- | @Selector@ for @posterArtUrl@
posterArtUrlSelector :: Selector
posterArtUrlSelector = mkSelector "posterArtUrl"

-- | @Selector@ for @setPosterArtUrl:@
setPosterArtUrlSelector :: Selector
setPosterArtUrlSelector = mkSelector "setPosterArtUrl:"

-- | @Selector@ for @dvbiUrl@
dvbiUrlSelector :: Selector
dvbiUrlSelector = mkSelector "dvbiUrl"

-- | @Selector@ for @setDvbiUrl:@
setDvbiUrlSelector :: Selector
setDvbiUrlSelector = mkSelector "setDvbiUrl:"

-- | @Selector@ for @releaseDate@
releaseDateSelector :: Selector
releaseDateSelector = mkSelector "releaseDate"

-- | @Selector@ for @setReleaseDate:@
setReleaseDateSelector :: Selector
setReleaseDateSelector = mkSelector "setReleaseDate:"

-- | @Selector@ for @parentalGuidanceText@
parentalGuidanceTextSelector :: Selector
parentalGuidanceTextSelector = mkSelector "parentalGuidanceText"

-- | @Selector@ for @setParentalGuidanceText:@
setParentalGuidanceTextSelector :: Selector
setParentalGuidanceTextSelector = mkSelector "setParentalGuidanceText:"

-- | @Selector@ for @recordingFlag@
recordingFlagSelector :: Selector
recordingFlagSelector = mkSelector "recordingFlag"

-- | @Selector@ for @setRecordingFlag:@
setRecordingFlagSelector :: Selector
setRecordingFlagSelector = mkSelector "setRecordingFlag:"

-- | @Selector@ for @seriesInfo@
seriesInfoSelector :: Selector
seriesInfoSelector = mkSelector "seriesInfo"

-- | @Selector@ for @setSeriesInfo:@
setSeriesInfoSelector :: Selector
setSeriesInfoSelector = mkSelector "setSeriesInfo:"

-- | @Selector@ for @categoryList@
categoryListSelector :: Selector
categoryListSelector = mkSelector "categoryList"

-- | @Selector@ for @setCategoryList:@
setCategoryListSelector :: Selector
setCategoryListSelector = mkSelector "setCategoryList:"

-- | @Selector@ for @castList@
castListSelector :: Selector
castListSelector = mkSelector "castList"

-- | @Selector@ for @setCastList:@
setCastListSelector :: Selector
setCastListSelector = mkSelector "setCastList:"

-- | @Selector@ for @externalIDList@
externalIDListSelector :: Selector
externalIDListSelector = mkSelector "externalIDList"

-- | @Selector@ for @setExternalIDList:@
setExternalIDListSelector :: Selector
setExternalIDListSelector = mkSelector "setExternalIDList:"

