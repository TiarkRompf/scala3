# Scala 3 with Revocable Capabilities

## Overview
This is a fork of the [Scala 3 compiler](https://github.com/scala/scala3)
that implements typestate based on implicit capabilties and destructive effects
as described in *Typestate via Revocable Capabilities*.

Instructions on how to set up the compiler are in
the "Setup" section, and are the same as in
the Scala 3 [Getting Started User Guide](https://docs.scala-lang.org/scala3/getting-started.html). Instructions on how to use the language extensions are in the "Using Typestate Extensions" section. The compiler extensions are documented in the "Modifications" section. Instructions on running tests
and examples are detailed in "Tests and Examples".

## Setup

### Requirements

- **JDK version**: Eclipse Adoptium Temurin 17.0.14
- **sbt version**: 1.10.7

Following the [Getting Started User Guide](https://docs.scala-lang.org/scala3/getting-started.html), any JDK version listed
in [JDK Compatability](https://docs.scala-lang.org/overviews/jdk-compatibility/overview.html) should work, as well as the latest sbt version.

### Compiling and Running

The Scala 3 compiler provides a standard `sbt` build.
To use the compiler, first start an `sbt` shell by
invoking `sbt` in the root directory. Within the `sbt` shell,
compiling and running a file can be done with

1. `scalac tests/pos/HelloWorld.scala`
2. `scala HelloWorld`

Note that just compiling the compiler can be done by running `compile` in the `sbt` shell.

### Using Compiler in Local Projects

To use the compiler in a local project, run

`sbt publishLocal`

Then in the `build.sbt` file of the local project set

`ThisBuild / scalaVersion := "3.7.2-RC1-bin-SNAPSHOT"`

See [publishing to local repository](https://dotty.epfl.ch/docs/contributing/getting-started.html#publish-to-local-repository).

## Using Typestate Extensions

First, enable [capture checking](https://docs.scala-lang.org/scala3/reference/experimental/cc.html) with `import language.experimental.captureChecking`.
Then import the typestate definitions with `import typestate.*`. The definitions can be found in `library/src/scala/typestate/package.scala`.

## Modifications

There were two major modifications to the compiler:

- An effect checking phase (`compiler/src/dotty/tools/dotc/eff/`) was added for destructive effects. It runs after
  the capture checking phase. The main effect checker is in `compiler/src/dotty/tools/dotc/eff/CheckEffects.scala`.

- The typer (`compiler/src/dotty/tools/dotc/typer/Typer.scala`)
  was modified to support `Sigma`types. The typer performs
  a type-directed ANF transform triggered by `Sigma` types.

In addition, the capture checker (`compiler/src/dotty/tools/dotc/cc/`)
was also modified for better support of higher-order functions and `Sigma` types.

## Tests and Examples

Running capture checker tests can be done by starting an `sbt` shell and then invoking `testCompilation captures` for capture checking tests. Note that test case `tests/run-custom-args/captures/minicheck.scala` may fail when running the test suite, but succeeds when running on its own (follow the reproduction
instructions after running the test suite to reproduce on its own).

The `examples` directory contains code examples, including
all case studies from the paper:

| Paper Section        |  File (in `examples/`)    |
|----------------------|----------|
| 3.1 Table Locking  | `TableLock.scala` |
| 3.2 DOM Trees      | `DOM.scala`   |
| 3.3 Session Types  | `SessionImp.scala`   |
| 3.4 Control Flow   | `ControlFlow.scala`   |
