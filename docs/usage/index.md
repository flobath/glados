# Project Usage

This document provides instructions on how to install and run the GLaDOS project. It serves as a comprehensive guide for users to set up the project environment and execute the necessary binaries to get the project up and running.

## Install the Project

There are many ways to install the project: you can download the released binary or build it manually from the source code. Alternatively you can get a working copy of program with [nix](https://nixos.org/) as described in the [Nix Build](#nix-build) section.
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

### Nix Build

If you like to work with nix, you can get a build of our program by running `nix build` in a clone of the repository, or by running `nix build github:flobath/glados` anywhere. You can even build any branch you like with `nix build github:flobath/glados/<branchname>`, replacing `<branchname>` with the actual name of the branch. This allows you to access any release at any time.
If you just want to run the project to test it out, you don't need to bother using `nix build` to get an executable that you can run. You can just use `nix run`, as described in [Running with Nix](running-with-nix).

## Run the Project

To run the project, follow these steps:

### Running with nix

If you're working with [nix](https://nixos.org/) and nix flakes, you can run our program from anywhere, by simply running `nix run github:flobath/glados` in your shell. You can even run any specific version by specifying a branch name, for example `nix run github:flobath/glados/Release-1.0`.

## Examples

Here are some examples to demonstrate how to use the GLaDOS.
