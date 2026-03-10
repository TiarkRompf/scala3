# Artifact for "Typestate via Revocable Capabilities"

## Overview

This artifact is for the paper "Typestate via Revocable Capabilities".

Folder [`scala3`](scala3) contains the implementation of our Scala 3 compiler prototype
as described in Section 5.

<!-- add file by file comparison + talk about limitations, known bugs, what subset of Scala.
     need to talk about differences between our work and capturing types! -->

## Getting Started Guide

## Setup

### Requirements

- **JDK version**: Eclipse Adoptium Temurin 17.0.14
- **sbt version**: 1.10.7

Following the [Getting Started User Guide](http://nightly.scala-lang.org/docs/contributing/index.html), any JDK version listed
in [JDK Compatability](https://docs.scala-lang.org/overviews/jdk-compatibility/overview.html) should work, as well as the latest sbt version.

### Compiling and Running

The Scala 3 compiler provides a standard `sbt` build.

To compile and run the compiler, first start an `sbt` shell in the `scala3` directory:

```
  cd scala3
  sbt
```

Once within the `sbt` shell, compiling a file is achieved by invoking `scalac` on
the desired file:
```
scalac tests/pos/HelloWorld.scala
```

Running a compiled file within the `sbt` shell is achieved by invoking `scala` on
the classpath of the file:
```
scala HelloWorld
```
### Using Compiler in Local Projects

To use the compiler in a local project, run

`sbt publishLocal`

(you may ignore error messages during publishing process.)

Then in the `build.sbt` file of the local project set

`ThisBuild / scalaVersion := "3.7.2-RC1-bin-SNAPSHOT"`

See [publishing to local repository](https://nightly.scala-lang.org/docs/contributing/getting-started.html#publish-to-local-repository).

### Enabling Typestate Extensions

This section details how to enable our compiler extensions in a particular file.
First, enable [capture checking](https://docs.scala-lang.org/scala3/reference/experimental/cc.html) with an
import at the top of the file:
```
import language.experimental.captureChecking
```

Then import the typestate definitions with `import typestate.*`. The definitions can be found in directory
`library/src/scala/typestate/package.scala`.

## Step-by-Step Instructions

### Paper Figure to Artifact Correspondence

The following table lists the correspondence between each code
figure in the paper that uses our prototype and file in the artifact.

| **Paper Figure**    |  **File Name**               |
|  ----               |  ----                        |
| Figure 1b           | `examples/TableLock.scala`   |
| Figure 2            | `examples/TableLock.scala`   |
| Figure 3            | `examples/DOM.scala`         |
| Figure 4            | `examples/DOM.scala`         |
| Figure 5            | `examples/DOM.scala`         |
| Figure 6            | `examples/SessionImp.scala`  |
| Figure 7            | `examples/SessionImp.scala`  |
| Figure 8            | `examples/SessionImp.scala`  |
| Figure 9            | `examples/SessionImp.scala`  |
| Figure 10           | `examples/ControlFlow.scala` |
| Figure 11           | `examples/ControlFlow.scala` |
| Figure 12           | `examples/ControlFlow.scala` |

The `examples/` directory also contains more examples:
| **File Name**        | **Description**              |
|  ----                | ----                         |
| `File.scala`         | Final version of running file example as detailed in section 2 |
| `SessionExp.scala`   | Session-typed channels without path-dependent capabilities (e.g. destroying channel itself.) |
| `Drone.scala`        | Drone example from [Trindade et al. 2020](https://arxiv.org/pdf/2009.08769). |

All files compile under `scalac`. It is possible to run `examples/File.scala` with `scala`. However, other
files rely on `???` as implementations of the methods. This is because we regard the implementation of 

### Deviations from the Paper

### Limitations? (rename)

### Tests

Running capture checker tests can be done by starting an `sbt` shell and then invoking `testCompilation captures` for capture checking tests. Note that test case `tests/run-custom-args/captures/minicheck.scala` may fail when running the test suite, but succeeds when running on its own (follow the reproduction
instructions after running the test suite to reproduce on its own).

### Compiler Modifications

The following table describes each file in the compiler that
our prototype either adds or modifies.

| **File Name**     | **Description**   |
| ----              | ---               |

There were two major modifications to the compiler:

- An effect checking phase (`compiler/src/dotty/tools/dotc/eff/`) was added for destructive effects. It runs after
  the capture checking phase. The main effect checker is in `compiler/src/dotty/tools/dotc/eff/CheckEffects.scala`.

- The typer (`compiler/src/dotty/tools/dotc/typer/Typer.scala`)
  was modified to support `Sigma`types. The typer performs
  a type-directed ANF transform triggered by `Sigma` types.

In addition, the capture checker (`compiler/src/dotty/tools/dotc/cc/`)
was also modified for better support of higher-order functions and `Sigma` types.