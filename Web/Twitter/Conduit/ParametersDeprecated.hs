{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Twitter.Conduit.ParametersDeprecated where

import Data.Text (Text)
import Web.Twitter.Types
import Data.Time.Calendar (Day)
import Web.Twitter.Conduit.Request.Internal
import Control.Lens

count :: (Parameters p, HasParam "count" Integer (SupportParameters p)) => Lens' p (Maybe Integer)
count = rawParam "count"

sinceId :: (Parameters p, HasParam "since_id" Integer (SupportParameters p)) => Lens' p (Maybe Integer)
sinceId = rawParam "since_id"

maxId :: (Parameters p, HasParam "max_id" Integer (SupportParameters p)) => Lens' p (Maybe Integer)
maxId = rawParam "max_id"

page :: (Parameters p, HasParam "page" Integer (SupportParameters p)) => Lens' p (Maybe Integer)
page = rawParam "page"

trimUser :: (Parameters p, HasParam "trim_user" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
trimUser = rawParam "trim_user"

excludeReplies :: (Parameters p, HasParam "exclude_replies" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
excludeReplies = rawParam "exclude_replies"

contributorDetails :: (Parameters p, HasParam "contributor_details" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
contributorDetails = rawParam "contributor_details"

includeEntities :: (Parameters p, HasParam "include_entities" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
includeEntities = rawParam "include_entities"

includeEmail :: (Parameters p, HasParam "include_email" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
includeEmail = rawParam "include_email"

includeUserEntities :: (Parameters p, HasParam "include_user_entities" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
includeUserEntities = rawParam "include_user_entities"

includeRts :: (Parameters p, HasParam "include_rts" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
includeRts = rawParam "include_rts"

includeMyRetweet :: (Parameters p, HasParam "include_my_retweet" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
includeMyRetweet = rawParam "include_my_retweet"

includeExtAltText :: (Parameters p, HasParam "include_ext_alt_text" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
includeExtAltText = rawParam "include_ext_alt_text"

inReplyToStatusId :: (Parameters p, HasParam "in_reply_to_status_id" Integer (SupportParameters p)) => Lens' p (Maybe Integer)
inReplyToStatusId = rawParam "in_reply_to_status_id"

displayCoordinates :: (Parameters p, HasParam "display_coordinates" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
displayCoordinates = rawParam "display_coordinates"

possiblySensitive :: (Parameters p, HasParam "possibly_sensitive" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
possiblySensitive = rawParam "possibly_sensitive"

lang :: (Parameters p, HasParam "lang" Text (SupportParameters p)) => Lens' p (Maybe Text)
lang = rawParam "lang"

language :: (Parameters p, HasParam "language" Text (SupportParameters p)) => Lens' p (Maybe Text)
language = rawParam "language"

locale :: (Parameters p, HasParam "locale" Text (SupportParameters p)) => Lens' p (Maybe Text)
locale = rawParam "locale"

filterLevel :: (Parameters p, HasParam "filter_level" Text (SupportParameters p)) => Lens' p (Maybe Text)
filterLevel = rawParam "filter_level"

stallWarnings :: (Parameters p, HasParam "stall_warnings" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
stallWarnings = rawParam "stall_warnings"

replies :: (Parameters p, HasParam "replies" Text (SupportParameters p)) => Lens' p (Maybe Text)
replies = rawParam "replies"

until :: (Parameters p, HasParam "until" Day (SupportParameters p)) => Lens' p (Maybe Day)
until = rawParam "until"

skipStatus :: (Parameters p, HasParam "skip_status" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
skipStatus = rawParam "skip_status"

follow :: (Parameters p, HasParam "follow" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
follow = rawParam "follow"

map :: (Parameters p, HasParam "map" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
map = rawParam "map"

mediaIds :: (Parameters p, HasParam "media_ids" [Integer] (SupportParameters p)) => Lens' p (Maybe [Integer])
mediaIds = rawParam "media_ids"

description :: (Parameters p, HasParam "description" Text (SupportParameters p)) => Lens' p (Maybe Text)
description = rawParam "description"

name :: (Parameters p, HasParam "name" Text (SupportParameters p)) => Lens' p (Maybe Text)
name = rawParam "name"

profileLinkColor :: (Parameters p, HasParam "profile_link_color" Text (SupportParameters p)) => Lens' p (Maybe Text)
profileLinkColor = rawParam "profile_link_color"

location :: (Parameters p, HasParam "location" Text (SupportParameters p)) => Lens' p (Maybe Text)
location = rawParam "location"

url :: (Parameters p, HasParam "url" URIString (SupportParameters p)) => Lens' p (Maybe URIString)
url = rawParam "url"

fullText :: (Parameters p, HasParam "full_text" Bool (SupportParameters p)) => Lens' p (Maybe Bool)
fullText = rawParam "full_text"

with :: (Parameters p, HasParam "with" Text (SupportParameters p)) => Lens' p (Maybe Text)
with = rawParam "with"
