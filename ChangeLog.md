## 0.3.0

* Upgrade http-conduit dependencies to:
  http-conduit >= 2.0 && < 2.4 [#59](https://github.com/himura/twitter-conduit/pull/59)

## 0.2.2

* Upgrade http-conduit and http-client dependencies to:
  http-conduit >= 2.1.8 && http-client >= 0.4.30 [#51](https://github.com/himura/twitter-conduit/pull/51)
* Added `include_email` parameter to `AccountVerifyCredentials` [#49](https://github.com/himura/twitter-conduit/pull/49)
* Added `extAltText` parameter to `showId` [#50](https://github.com/himura/twitter-conduit/pull/50)

## 0.2.1

* Added `fullText` parameter to direct message calls [#47](https://github.com/himura/twitter-conduit/pull/47)
* Replaced `SearchStatus` with `Status` type [#46](https://github.com/himura/twitter-conduit/pull/46)
* Added `accountUpdateProfile` [#45](https://github.com/himura/twitter-conduit/pull/45)
* Added `listsMembersCreateAll` and `listsMembersDestroyAll`
* Parameter lenses in `Web.Twitter.Conduit` re-exported from `Web.Twitter.Conduit.Parameters` are deprecated

## 0.2.0

* Changed the signature of functions defined in Web.Twitter.Conduit.Base,
  because Manager of http-conduit 2.0 and later does not need MonadResource.
  [#43](https://github.com/himura/twitter-conduit/issues/43)
* Removed `TwitterBaseM`
* Removed `TW` monad
* Re-exported `OAuth (..)` and `Credential (..)` from authenticate-oauth
* Re-exported `def` from data-default
* Re-exported `Manager`, `newManager` and `tlsManagerSettings` from http-conduit

## 0.1.3

* Make TWToken and TWInfo an instance of Read and Typeable [#42](https://github.com/himura/twitter-conduit/issues/42)

## 0.1.2

* Streaming API: Support multiple filters [#41](https://github.com/himura/twitter-conduit/issues/41)
* Include parameters in body for POST requests [#40](https://github.com/himura/twitter-conduit/issues/40)

## 0.1.1.1

* Fix warnings on GHC 7.10
* Fix doctest when twitter-feed package exists

## 0.1.1

* Add `sourceWithSearchResult` and `sourceWithSearchResult'`
