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
