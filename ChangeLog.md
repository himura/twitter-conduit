## 0.5.0 (wip)

* Support for OverloadedLabels

  `twitter-conduit` now supports the OverloadedLabels extensions for overloaded parameters in `APIRequest` (e.g.: `#count`, `#max_id`).

  We can now write:

  ```haskell
  homeTimeline & #count ?~ 200
  ```

  instead of:

  ```haskell
  import qualified Web.Twitter.Conduit.Parameters as P

  homeTimeline & P.count ?~ 200
  ```

  NOTE: See `Web.Twitter.Conduit.ParametersDeprecated` module if you would like to use classic value lenses.

* Drop supports conduit < 1.3 and http-conduit < 2.3 [#69](https://github.com/himura/twitter-conduit/pull/69).
* `Web.Twitter.Conduit.Status` is no longer re-exported by Web.Twitter.Conduit in order to avoid name conflictions (e.g. `update`, `lookup`) [#71](https://github.com/himura/twitter-conduit/pull/71).
* Add alias for functions in `Web.Twitter.Conduit.Status` with statuses- prefix [#71](https://github.com/himura/twitter-conduit/pull/71).
  (e.g. `Web.Twitter.Conduit.Api.statusesHomeTimeline` for `Web.Twitter.Conduit.Status.homeTimeline`)

## 0.4.0

* Follow direct message API changes [#65](https://github.com/himura/twitter-conduit/pull/65) [#62](https://github.com/himura/twitter-conduit/pull/62)
* Changed WithCursor type [5b9e9d7a](https://github.com/himura/twitter-conduit/commit/5b9e9d7a13d33327fe637cae8e2359a38fce92b5)
    * Added type parameter to WithCursor to supports `Text` as the next cursor type.
    * Changed {previous,next}Cursor in WithCursor to be optional
* Changed APIRequest type to take HTTP Method [f25fd9b3](https://github.com/himura/twitter-conduit/commit/f25fd9b3b860032f384d01b3457ea896e596366b)

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
