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

### Running Compilation Tests

To run the 373 compilation tests for capture checking (as stated in Section 5), invoke:
```
testCompilation captures
```

The test case
```tests/run-custom-args/captures/minicheck.scala``` may fail when running the test suite,
but succeeds when ran on its own. If this test case fails during the test suite, then the
`testCompilation captures` command will provide reproduction instructions for this test case.
Note that the reproduction instructions state that the command to run the test case will end
with `'tests/run-custom-args/captures/minicheck.scala'`. Please remove the apostrophes
when running the command.

## Step-by-Step Instructions

### Enabling Typestate Extensions

This section details how to enable our compiler extensions in a particular file.
First, enable [capture checking](https://docs.scala-lang.org/scala3/reference/experimental/cc.html) with an
import at the top of the file:
```
import language.experimental.captureChecking
```

Then import the typestate definitions with
```
import typestate.*
```

The typestate definitions can be found in the file
```
library/src/scala/typestate/package.scala
```

### Paper Figure to Artifact Correspondence

The following table lists the correspondence between each major code example
in the paper and file in the artifact. All code figures are found in the `examples/` directory.

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

The `examples/` directory also contains more examples not found in the paper:
| **File Name**        | **Description**              |
|  ----                | ----                         |
| `File.scala`         | Final version of running file example as detailed in section 2 |
| `SessionExp.scala`   | Session-typed channels without path-dependent capabilities (kills channel itself.) |
| `Drone.scala`        | Drone example from [Trindade et al. 2020](https://arxiv.org/pdf/2009.08769). |

All files compile with `scalac`.
It is possible to run `examples/File.scala` and `examples/TableLock.scala` with `scala`. To do so,
first compile both files with `scalac`, and then invoke `scala Main`.

Other examples use `???` to implement certain methods, which will result in a run-time `NotImplementedError`.
We do this because the full implementation of these examples is both non-novel and orthogonal to our contribution.
This does not deviate from any figure shown in the paper.

### Compiler Modifications

This section describes the major modifications made to the Scala 3 compiler.
The major modifications are all made in the directory `compiler/src/dotty/tools/dotc`.
The following table lists each file that is either new or modified significantly.
Note that the file name is specified relative to `compiler/src/dotty/tools/dotc` (e.g. `typer/Typer.scala`
is `compiler/src/dotty/tools/dotc/typer/Typer.scala`):

| **File Name**             | **Description of Changes**                                           |
| ----                      | ----                                                                 |
|`core/TypeComparer.scala`  | Added support for destructive effect subtyping.                      |
|`eff/CheckEffects.scala`   | New file: Main checker for destructive effects.                      |
|`eff/EffectOps.scala`      | New file: Contains helpful operations for effect checking            |
|`eff/FXSetup.scala`        | New file: Sets up AST for effect checking                            |
|`typer/SigmaOps.scala`     | New file: Contains helpful operations for working with `Sigma` types |
|`typer/Typer.scala`        | Added support for `Sigma`-type directed ANF transform.               |

### Deviations from the Paper

The Scala compiler requires parenthesization between `^` and `@`:
```
def open(f: ClosedFile^): (OpenFile^) @kill(f)
```
whereas the paper omits parantheses for readability:
```
def open(f: ClosedFile^): OpenFile^ @kill(f)
```

Scala 3 does not currently support curried implicit dependent function types.
This means examples involving curried implicit dependent higher-order functions, such as `makeDOM` (Section 3.2)
need to explicitly bind arguments:

```
makeDOM { dom => c => //
  ...
}
```

Code examples in the paper omit this:
```
makeDOM { dom =>
  ...
}
```

but there is a footnote describing this omission.


### Using the Prototype in Local Projects

To use our prototype in a local project, first run

`sbt publishLocal`

Error messages that occur during the publishing process can be ignored.
Then, in the `build.sbt` file of the local project, add the following line:

`ThisBuild / scalaVersion := "3.7.2-RC1-bin-SNAPSHOT"`

See [publishing to local repository](https://nightly.scala-lang.org/docs/contributing/getting-started.html#publish-to-local-repository).
