# Project Usage

This document provides instructions on how to install and run the GLaDOS project. It serves as a comprehensive guide for users to set up the project environment and execute the necessary binaries to get the project up and running.

## Install the Project

There are two ways to install the project: you can either download the released binary or build it manually from the source code.
A guide explaining how to build the binary manually is available [below](#manual-build).

### Manual Build

This section provides a short guide explaining how to build the GLaDOS binary file from the command line.

#### Prerequisites

Before proceeding, you must ensure [Stack](https://docs.haskellstack.org/en/stable/README/) and [Make](https://www.gnu.org/software/make/) are installed on your system.

#### Compile the Project

First, clone the project repository on your system:
```bash
git clone https://github.com/yourusername/glados.git
cd glados
```
Then, open a terminal under the project folder and run the following command:
```bash
make
```

It must produce a binary file named `glados` at the root of the project folder.

## Run the Project

To run the project, follow these steps:

1. Open a terminal in the folder where the project binary (`glados`) is located.
2. Run the following command to execute the project with an input file:
    ```bash
    ./glados < file.scm
    ```
    Replace `file.scm` with the path to your input file.

3. Alternatively, you can run the project and enter your LISP code directly on the command line:
    ```bash
    ./glados
    ```
    This will start an interactive session where you can input your LISP code.

## Examples

Here are some examples to demonstrate how to use the GLaDOS interpreter.
Some example files containing LISP code can be found in the `examples` directory under the root of the project repository.

### Example 1: Running a Script from a File

You can run a Scheme script from a file using the following command:

```bash
./glados < ./examples/example1.scm
```

**Content of `example1.scm`:**
```scheme
(define foo 21)
(* foo 2)
```

**Expected Output:**
```
42
```

### Example 2: Interactive Session

You can also start an interactive session to input your LISP code directly:

```bash
./glados
```

This will start an interactive session where you can input your LISP code. For example:

```
=================
  Glados scheme
=================
> (define foo 21)
> (* foo 2)
42
```
