# Artifact for "Typestate via Revocable Capabilities"

This artifact is for the paper "Typestate via Revocable Capabilities".

Folder [`scala3`](scala3) contains the implementation of our Scala 3 compiler prototype
as described in Section 5.

## Getting Started Guide

### Requirements

- **JDK version**: Eclipse Adoptium Temurin 17.0.14
- **sbt version**: 1.10.7

Following the [Getting Started User Guide](http://nightly.scala-lang.org/docs/contributing/index.html), any JDK version listed
in [JDK Compatability](https://docs.scala-lang.org/overviews/jdk-compatibility/overview.html) should work, as well as the latest sbt version.

For the purpose of artifact evaluation, we provide the environment in a Docker image:

    docker image load -i image.tar.gz
    docker run -it --rm typestate-scala3:latest

### Kick the Tires

To quickly check all examples, in the Docker image (default path `/work`):

    ./checkExamples.sh


## Step-by-Step Instructions

### Compiling and Running

The Scala 3 compiler provides a standard `sbt` build.

To compile and run the compiler, first start an `sbt` shell in the `scala3` directory:

    cd scala3
    sbt

Once within the `sbt` shell, compiling a file is achieved by invoking `scalac` on
the desired file:

    scalac tests/pos/HelloWorld.scala

Running a compiled file within the `sbt` shell is achieved by invoking `scala` on
the classpath of the file:

    scala HelloWorld

### Running Compilation Tests

To run the 373 compilation tests for capture checking (as stated in Section 5), invoke:

    testCompilation captures

The test case
`tests/run-custom-args/captures/minicheck.scala` may fail when running the test suite,
but succeeds when ran on its own. If this test case fails during the test suite, then the
`testCompilation captures` command will provide reproduction instructions for this test case.
Some thing like:

    ...
    ================================================================================
    Test Report
    ================================================================================

    2 suites passed, 1 failed, 3 total
        tests/run-custom-args/captures/minicheck.scala failed

    --------------------------------------------------------------------------------
    Note - reproduction instructions have been dumped to log file:
        /work/scala3/testlogs/tests-2026-03-14/tests-2026-03-14-T20-39-45.log
    --------------------------------------------------------------------------------
    ...

And inside the log file:

    ...
    ================================================================================
    Test Report
    ================================================================================

    2 suites passed, 1 failed, 3 total
        tests/run-custom-args/captures/minicheck.scala failed

    Test 'tests/run-custom-args/captures/minicheck.scala' compiled with 1 error(s) and 0 warning(s),
    the test can be reproduced by running from SBT (prefix it with ./bin/ if you
    want to run from the command line):

    scalac -classpath /root/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.16/scala-library-2.13.16.jar:/work/scala3/library/../out/bootstrap/scala3-library-bootstrapped/scala-3.7.2-RC1-bin-SNAPSHOT-nonbootstrapped/scala3-library_3-3.7.2-RC1-bin-SNAPSHOT.jar  -indent -Yno-double-bindings -Yforce-sbt-phases -Xsemanticdb -Xverify-signatures -pagewidth  120 -color:never -Xtarget 9 -Ycheck:all -language:experimental.captureChecking  'tests/run-custom-args/captures/minicheck.scala'

Note that the reproduction instructions state that the command to run the test case will end
with `'tests/run-custom-args/captures/minicheck.scala'`. Please remove the apostrophes
when running the command.

### Paper Figure to Artifact Correspondence

The following table lists the correspondence between each major code example (divided by section)
in the paper and file in the artifact. Each file is meant to contain the final version of the
"running example" for the corresponding section. All such files are found in the `examples/` directory.

| **Paper Section**   | **File Name**                |
| ----                | ----                         |
| Section 2           | `examples/File.scala`        |
| Section 3.1         | `examples/TableLock.scala`   |
| Section 3.2         | `examples/DOM.scala`         |
| Section 3.3         | `examples/SessionImp.scala`  |
| Section 3.4         | `examples/ControlFlow.scala` |

The following table provides a figure-to-file correspondence:

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

The `examples/` directory also contains more examples not found in the paper:
| **File Name**        | **Description**              |
|  ----                | ----                         |
| `SessionExp.scala`   | Session-typed channels without path-dependent capabilities (kills channel itself.) |
| `Drone.scala`        | Drone example from [Trindade et al. 2020](https://arxiv.org/pdf/2009.08769). |

All files compile with `scalac`.
In addition, it is possible to run `examples/File.scala` and `examples/TableLock.scala` with `scala`. To do so,
first compile both files with `scalac`, and then invoke `scala Main`. Running `examples/File.scala` will
edit `examples/samples.txt`.

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

1. The Scala compiler requires parenthesization between `^` and `@`:

        def open(f: ClosedFile^): (OpenFile^) @kill(f)

    whereas the paper omits parantheses for readability:

        def open(f: ClosedFile^): OpenFile^ @kill(f)

2. Scala 3 does not currently support curried implicit dependent function types.
    This means examples involving curried implicit dependent higher-order functions, such as `makeDOM` (Section 3.2)
    need to explicitly bind arguments:

        makeDOM { dom => c =>
          ...
        }

    Code examples in the paper omit this:

        makeDOM { dom =>
          ...
        }

    Note that there is a footnote in the paper describing this omission.

3. In Figure 7, some upper type bounds are elided for space (this is stated in the paper).
    For example, method `send` in the figure has signature

        def send[T, E, P](x: T): chan.PCap[E, Send[T, P]] ?=!>? chan.PCap[E, P]

    but in Scala will have the signature

        def send[T, E <: PList, P <: Session](x: T): chan.PCap[E, Send[T, P]] ?=!>? chan.PCap[E, P]

    Method return types are omitted for the same reason in figure 8.


## Reusability Guide

### Enable the Typestate Extension in a New File

This section details how to enable our compiler extensions.
First, enable [capture checking](https://docs.scala-lang.org/scala3/reference/experimental/cc.html) with an
import at the top of the file:

    import language.experimental.captureChecking

Then import the typestate definitions with

    import typestate.*

The typestate definitions can be found in the file

    library/src/scala/typestate/package.scala

### Prototype Status

Our modifications to the Scala compiler and capturing checker are meant to be a prototype
demonstrating the paradigm of typestate programming, but not a full language
development ready for practical adoption.
As stated in Section 5, our prototype concerns a core subset of Scala
where reachability types align closely to capturing types.
Therefore, there are major features of Scala omitted from the prototype.

Specifically, our effect checker focuses on a functional core of Scala, and it
works correctly with respect to higher-order functions regarding capabilities passed
as parameters and stored as local variables. Beyond this core,
destructive effects do not work on mutable variables and object fields; attempting to
kill them will pass silently without actually inducing the effect.

We also restrict destructive effects on polymorphic variables
This is because capturing types posses a "boxing" discipline, where upon a type
entering a generic context, its capture set is hidden:

    def foo[T](x: T): Unit =
      kill(x)

    def bar() =
      val f: File^ = ...
      val f2 = f

      foo(f2) // f2 will appear with no capture set inside foo

Inside `foo`, we cannot mark `x` killed without knowing its capturing information.
In our prototype, destroying such a variable will result in a compile-time error.
More generally, as reachability types and capturing types employ different
mechanisms to represent resources that are not fully named–i.e., fresh or
existential, our prototype cannot be true to both but leave those cases unhandled.

With our limitations regarding object fields and polymorphism, our prototype
does not understand `Sigma` as a generic datatype, but a transient wrapper for
ANF transformation, unpacked immediately after returning. We do not expect it to work,
for example, storing a  list of Sigma values.
The type-directed ANF transform does not cover all Scala features, either.
It is currently triggered by an application node having type `Sigma`. Thus,
some cases where a transform should be triggered may actually not,
such as nesting an application inside a block:

    val k = {
      open(f)
    }

Nevertheless, we believe that a full implementation of `Sigma` could be achieved with
additional engineering efforts, but its current role suffices for our use cases.

### Using the Prototype in Local Projects

To use our prototype in a local project, first run

    sbt publishLocal

Error messages that occur during the publishing process can be ignored.
Then, in the `build.sbt` file of the local project, add the following line:

    ThisBuild / scalaVersion := "3.7.2-RC1-bin-SNAPSHOT"

See also [publishing to local repository](https://nightly.scala-lang.org/docs/contributing/getting-started.html#publish-to-local-repository).