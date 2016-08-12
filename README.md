[![GitHub tag](https://img.shields.io/github/tag/xenolinguist/hoax.svg)]()
[![Build Status](https://secure.travis-ci.org/xenolinguist/hoax.png)](http://travis-ci.org/xenolinguist/hoax)
[![Hex.pm](https://img.shields.io/hexpm/v/hoax.svg)]()

  * [Build](#build)

hoax
====

[![Join the chat at https://gitter.im/xenolinguist/hoax](https://badges.gitter.im/xenolinguist/hoax.svg)](https://gitter.im/xenolinguist/hoax?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
Yet another mocking library for Erlang.

Build
-----

hoax requires [rebar][1] to build. To build hoax, go to the hoax
directory and simply type:

```sh
rebar compile
```

To make sure hoax works on your platform, run the tests:

```sh
rebar eunit
```

_Note that hoax's unit tests require Eunit >= 2.2.1 (OTP R14B04)._

  [1]: https://github.com/rebar/rebar "Rebar - A build tool for Erlang"
