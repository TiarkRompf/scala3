# Scala 3 with Capability-Based Typestate

## Overview
This is a fork of the [Scala 3 compiler](https://github.com/scala/scala3)
that implements typestate based on capabilties
and a destructive effect system.

Instructions on how to set up the compiler are in
the "Setup" section, and are the same as in
the Scala 3 [Getting Started User Guide](https://docs.scala-lang.org/scala3/getting-started.html). Instructions on how to use the language extensions are in
the "Usage" section. The changes to the compiler are documented in the
"Modifications" section.

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

### Use in Local Projects

To use the compiler in a local project, run

`sbt publishLocal`

Then in the `build.sbt` file of the local project set

`ThisBuild / scalaVersion := "3.7.2-RC1-bin-SNAPSHOT"`

See [publishing to local repository](https://dotty.epfl.ch/docs/contributing/getting-started.html#publish-to-local-repository).

## Usage

First, enable [capture checking](https://docs.scala-lang.org/scala3/reference/experimental/cc.html) with `import language.experimental.captureChecking`.
Then import the typestate definitions with `import typestate.*`. These definitions can be found in `library/src/scala/typestate/package.scala`.

### Destructive Effect System

We provide an annotation `@kill(xs: Any*)`, which "kills" anything passed
as an argument, rendering it unusable for the rest of the program. It is
meant to be used on the return type of functions.

```scala
import language.experimental.captureChecking
import typestate.*

trait File
class OpenFile extends File
class ClosedFile extends File

def read(f: OpenFile^): String
def open(f: ClosedFile^): OpenFile^ @kill(f)
def close(f: OpenFile^): ClosedFile^ @kill(f)

def foo(f: ClosedFile^) =
  val f2 = open(f)
  val line = read(f2)
  close(f2)
  val bad = read(f2) // error! using killed f
```

Importantly, the arguments must be *capabilities*, following the [definition](https://docs.scala-lang.org/scala3/reference/experimental/cc.html#capabilities-and-capturing-types) used by
the Scala 3 capture checker. Passing a non-capability to `@kill` will not do anything.

### Returning Implicits

We allow implicit generations of implicits
with a new `Sigma` type, defined as follows:
```scala
class Sigma:
  type A
  type B
  val a: A
  val b: B
```

When a `Sigma` is returned, the compiler will explicitly return `val a`,
and generate a new implicit for `val b`.

```scala
  def bar(x: Int): Sigma { type A = String, type B = Int} =
    new Sigma:
      type A = String
      type B = Int
      val a: String = "Number is "
      val b: Int = x

  def useBar() =
    val x = bar(20) // x: String
    println(x + summon[Int]) // prints "Number is 20"
```

## Modifications

There were two major modifications to the compiler:

- A new effect checking phase (`compiler/src/dotty/tools/dotc/eff/`)
  was added for destructive effects. It runs after
  the capture checking phase.

- The typer (`compiler/src/dotty/tools/dotc/typer/Typer.scala`)
  was modified to support `Sigma` types. The typer now performs
  a type-directed ANF transform triggered by `Sigma` types.

In addition, the capture checker was also modified for better support
of higher-order functions, and `Sigma` types.
Examples are found in the `ts-test/` folder.

Running tests can be done by starting an `sbt` shell and then
invoking `testCompilation captures` for capture checking tests,
as well as `testCompilation typestate` for typestate tests.