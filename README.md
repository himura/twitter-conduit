# twitter-conduit: An Conduit based Twitter API library for Haskell #

[![Travis](https://img.shields.io/travis/himura/twitter-conduit.svg)](https://travis-ci.org/himura/twitter-conduit)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/twitter-conduit.svg)](http://packdeps.haskellers.com/feed?needle=twitter-conduit)

## About ##

This is an conduit based Twitter API library for Haskell, including Streaming API supports.

Documentation is available in [hackage](http://hackage.haskell.org/package/twitter-conduit).

## Usage ##

    $ cabal update
    $ cabal install twitter-conduit

## Quick Start ##

For a runnable example, see [sample/simple.hs](https://github.com/himura/twitter-conduit/blob/master/sample/simple.hs).
You can find other various examples in [sample](https://github.com/himura/twitter-conduit/tree/master/sample/) directory.

## Run Samples ##

### Build ###

If you would like to use cabal sandbox, prepare sandbox as below:

~~~~
$ cabal sandbox init
~~~~

and then,

~~~~
$ cabal install --only-dependencies -fbuild-samples
$ cabal configure -fbuild-samples
$ cabal build
~~~~

### Run ###

First, you must obtain consumer key and secret from [Twitter Application Management](https://apps.twitter.com/) page,
and you have to set those values to environment variables as shown below:

~~~~
$ export OAUTH_CONSUMER_KEY="YOUR APPLICATION CONSUMER KEY"
$ export OAUTH_CONSUMER_SECRET="YOUR APPLICATION CONSUMER SECRET"
~~~~

Before you run examples, you must prepare OAuth access token and secret.
You can obtain access token and secret by using either PIN or web callback.

If you would like to use the PIN method, you run simply as below, and follow instructions:

~~~~
$ cabal run oauth_pin
~~~~

On the other hand, If you would like to use the callback method, do as follows:

~~~~
$ cabal run oauth_callback
~~~~

and open http://localhost:3000/signIn in your browser.

In both cases, you can obtain OAUTH_ACCESSS_TOKEN and OAUTH_ACCESS_SECRET variables,
then you should set those values to environment variables as shown below:

~~~~
$ export OAUTH_ACCESS_TOKEN="YOUR ACCESS TOKEN"
$ export OAUTH_ACCESS_SECRET="YOUR ACCESS SECRET"
~~~~

Finally, you can access Twitter UserStream as follows:

~~~~
$ cabal run userstream
~~~~

## Examples ##

TODO

## Authors and Credits ##

`twitter-conduit` initially was written by Takahiro Himura.
We would like to thank everyone who supported and contributed to the development of this library.
