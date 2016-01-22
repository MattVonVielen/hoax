[![Build Status](https://secure.travis-ci.org/xenolinguist/hoax.png)](http://travis-ci.org/xenolinguist/hoax)

  * [Build](#build)

# hoax

_Yet another mocking library for Erlang._

## Build

hoax requires [rebar](https://github.com/rebar/rebar) to build. To build hoax, go to the hoax
directory and simply type:

```sh
rebar compile
```

To make sure hoax works on your platform, run the tests:

```sh
rebar eunit
```

_Note that hoax's unit tests require Eunit >= 2.2.1 (OTP R14B04)._

## Features

### Simple, natural expectation syntax
hoax uses a parse_transform that allows you to define expectations nearly identically to how they are called in the tested code. This helps you to write short, easy to read tests.

### Enforces API conformity while mocking
Never get burned by a forgotten argument again! For each expectation you specify, hoax will ensure that the named function and arity are actually exported from the named module. If it isn't, your test will fail with `no_such_function_to_mock`.

### TDD friendly
"But wait", you say - "I do TDD and define my modules from consumption - as a result, I often write tests that mock modules I havent even written yet!"

Well, rock on with your bad self! When the module you're mocking doesn't exist then there's no existing API to check against - so you're free to mock any function you can imagine.

### Generates detailed error/failure messages
Better failure messages means faster debugging. hoax should never fail for any reason with `badmatch`, `function_clause`, `undef`, the dreaded `badarg` or any of their ilk. Instead, error messages like `unexpected_invocation`, `unexpected_arguments` and `unmet_expectation` should help you zero in on problems quickly.

## Examples

Let's contrive some examples. Suppose we have a record, and a database module handling persistence of those records.

```erlang
-record(user, [email :: string(), name :: string(), password_hash :: string()]).

-spec select_by_email(Email :: string()) -> #user{} | {error, Reason :: term()}.
-spec insert(Record :: #user{}) -> ok | {error, Reason :: term()}.
-spec update(Record :: #user{}) -> ok | {error, Reason :: term()}.
-spec delete(Record :: #user{}) -> ok | {error, Reason :: term()}.
```



### Basic mocking

### wildcard expectation arguments and actual argument lookup

## Limitations: What can't I mock?
While hoax can safely mock most Erlang modules, there are exceptions. The general rule of thumb is this:

> a module can be safely mocked as long as it does not live in the `kernel` or `stdlib` applications

Mocking of any module contained in those applications is officially unsupported. This is because a good chunk of the important machinery that powers the Erlang runtime itself is defined in modules from those two applications. For example, trying to mock `gen_server` or `ets` will cause the Erlang VM running your tests to crash in a truly spectacular fashion.

## Coming soon
* special support for mocking behaviour callback modules
* ability to specify and enforce counts on invocations
*
