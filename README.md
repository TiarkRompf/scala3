# Artifact for "Typestate via Revocable Capabilities"

This artifact is for the paper "Typestate via Revocable Capabilities".

Folder [`scala3`](scala3) contains the implementation of our Scala 3 compiler prototype
as described in Section 5.

## Getting Started Guide

### Requirements

- **JDK version**: 17.0.* (Temurin or OpenJDK)
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

### Paper Example to Artifact Correspondence

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

### Performance Benchmark (Figure 13)
This section describes how to reproduce the performance benchmark obtained in Figure 13 of the paper. For a quick start, run

    ./runAllBenchmarks.sh

#### Benchmark Organization
The benchmark statistics can found in the `bench-ts/` directory.
Each subdirectory of `bench-ts/` contains logs, code, and statistics for a particular set of benchmarks.
The following table shows the correspondence between benchmark and location of relevant information relative
to `bench-ts`:

| **Benchmark**  | **Source Code File**  | **Benchmark Logs**         |
| ----           | ----                  | ----                       |
| 17F            | `bench-DOM17/F.scala` | `bench-DOM17/F-logs`       |
| 17I            | `bench-DOM17/I.scala` | `bench-DOM17/I-logs`       |
| 17E            | `bench-DOM17/E.scala` | `bench-DOM17/E-logs`       |
| 25F            | `bench-DOM25/F.scala` | `bench-DOM25/F-logs`       |
| 25I            | `bench-DOM25/I.scala` | `bench-DOM25/I-logs`       |
| 25E            | `bench-DOM25/E.scala` | `bench-DOM25/E-logs`       |
| 33F            | `bench-DOM33/F.scala` | `bench-DOM33/F-logs`       |
| 33I            | `bench-DOM33/I.scala` | `bench-DOM33/I-logs`       |
| 33E            | `bench-DOM33/E.scala` | `bench-DOM33/E-logs`       |

As shown from the table, each benchmark is placed in a separate subdirectory of `bench-ts/` corresponding to the number.

The existing benchmarks were each compiled 15 times with `scalac` on a Mac with the following specs:
- CPU: Apple M3 Pro
- Memory: 18 GB
- OS: macOS Sequoia 15.7.4

The statistics used to construct Figure 13 were the mean and standard deviation of the runtime for
the last 10 runs (the first 5 were used to warm up the JVM). The benchmarks used were **17F**, **17I**, **17E**, **25F**, **33FF**.
Each subdirectory of `bench-ts/` also contains a `results.txt` which displays these statistics for each benchmark in the subdirectory.

#### To re-calculate benchmark statistics
The statistics for each benchmark were computed from the log files generated by the Scala compiler.
File `bcalc.py` re-calculates the benchmark statistics for a particular benchmark.
The following instructions detail how to use `bcalc.py`:

1. Open `bcalc.py` with a text editor.
2. Set variable `LOG_DIR` on line 6 of `bcalc.py` to the path of the directory (as a string) that
   holds the log files for a specific benchmark. For example, to calculate the benchmark statistics
   for benchmark **17F**, set `LOG_DIR` to `bench-ts/bench-DOM17/F-logs`.
3. Exit the text editor and invoke `python3 bcalc.py`. This will print the benchmark
   statistics for the last 10 runs.


#### To re-run benchmarks
To re-run a given benchmark, first remove all log files from the corresponding log directory.
For example, if you want to re-run benchmark `17F`, first remove all log files from
from directory `bench-ts/bench-DOM17/F-logs`.

File `benchmarks.py` runs the benchmarks. The following instructions detail how
to use `benchmarks.py`:

1. Open `benchmarks.py` with at ext editor.
2. Set variable `FILE_NAME` on line 5 of `benchmarks.py` to the path of the source code file (as a string)
   corresponding to the benchmark. For example, running benchmark **17F** would require set
   `FILE_NAME` to `"bench-ts/bench-DOM17/F.scala"`.
3. Set variable `LOG_DIR` on line 6 of `benchmarks.py` to the path of the directory to output
   the log files to.
4. Exit the text editor and invoke `python3 benchmarks.py`. This will fill the specified
   `LOG_DIR` with the log files.

After running the benchmarks with `benchmarks.py`, the statistics can be viewed with `bcalc.py`.

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