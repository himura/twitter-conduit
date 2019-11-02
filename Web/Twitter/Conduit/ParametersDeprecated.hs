{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Web.Twitter.Conduit.ParametersDeprecated where

import Data.Text (Text)
import Web.Twitter.Types
import Data.Time.Calendar (Day)
import Web.Twitter.Conduit.Request.Internal
import Control.Lens

count :: Parameters p => Lens' p (Maybe Integer)
count = rawParam "count"

sinceId :: Parameters p => Lens' p (Maybe Integer)
sinceId = rawParam "since_id"

maxId :: Parameters p => Lens' p (Maybe Integer)
maxId = rawParam "max_id"

page :: Parameters p => Lens' p (Maybe Integer)
page = rawParam "page"

trimUser :: Parameters p => Lens' p (Maybe Bool)
trimUser = rawParam "trim_user"

excludeReplies :: Parameters p => Lens' p (Maybe Bool)
excludeReplies = rawParam "exclude_replies"

contributorDetails :: Parameters p => Lens' p (Maybe Bool)
contributorDetails = rawParam "contributor_details"

includeEntities :: Parameters p => Lens' p (Maybe Bool)
includeEntities = rawParam "include_entities"

includeEmail :: Parameters p => Lens' p (Maybe Bool)
includeEmail = rawParam "include_email"

includeUserEntities :: Parameters p => Lens' p (Maybe Bool)
includeUserEntities = rawParam "include_user_entities"

includeRts :: Parameters p => Lens' p (Maybe Bool)
includeRts = rawParam "include_rts"

includeMyRetweet :: Parameters p => Lens' p (Maybe Bool)
includeMyRetweet = rawParam "include_my_retweet"

includeExtAltText :: Parameters p => Lens' p (Maybe Bool)
includeExtAltText = rawParam "include_ext_alt_text"

inReplyToStatusId :: Parameters p => Lens' p (Maybe Integer)
inReplyToStatusId = rawParam "in_reply_to_status_id"

displayCoordinates :: Parameters p => Lens' p (Maybe Bool)
displayCoordinates = rawParam "display_coordinates"

possiblySensitive :: Parameters p => Lens' p (Maybe Bool)
possiblySensitive = rawParam "possibly_sensitive"

lang :: Parameters p => Lens' p (Maybe Text)
lang = rawParam "lang"

language :: Parameters p => Lens' p (Maybe Text)
language = rawParam "language"

locale :: Parameters p => Lens' p (Maybe Text)
locale = rawParam "locale"

filterLevel :: Parameters p => Lens' p (Maybe Text)
filterLevel = rawParam "filter_level"

stallWarnings :: Parameters p => Lens' p (Maybe Bool)
stallWarnings = rawParam "stall_warnings"

replies :: Parameters p => Lens' p (Maybe Text)
replies = rawParam "replies"

until :: Parameters p => Lens' p (Maybe Day)
until = rawParam "until"

skipStatus :: Parameters p => Lens' p (Maybe Bool)
skipStatus = rawParam "skip_status"

follow :: Parameters p => Lens' p (Maybe Bool)
follow = rawParam "follow"

map :: Parameters p => Lens' p (Maybe Bool)
map = rawParam "map"

mediaIds :: Parameters p => Lens' p (Maybe [Integer])
mediaIds = rawParam "media_ids"

description :: Parameters p => Lens' p (Maybe Text)
description = rawParam "description"

name :: Parameters p => Lens' p (Maybe Text)
name = rawParam "name"

profileLinkColor :: Parameters p => Lens' p (Maybe Text)
profileLinkColor = rawParam "profile_link_color"

location :: Parameters p => Lens' p (Maybe Text)
location = rawParam "location"

url :: Parameters p => Lens' p (Maybe URIString)
url = rawParam "url"

fullText :: Parameters p => Lens' p (Maybe Bool)
fullText = rawParam "full_text"

with :: Parameters p => Lens' p (Maybe Text)
with = rawParam "with"
