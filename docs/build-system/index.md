# Build System

This section provides information about the chosen build system for the GLaDOS project.

## Introduction

The GLaDOS project uses [Stack](https://docs.haskellstack.org/en/stable/README/) as its build system. Stack is a powerful tool for managing Haskell projects, offering several advantages that make it an excellent choice for this project.

## Why Use Stack for the GLaDOS Project

Stack is a powerful build system for Haskell projects that offers several advantages:

1. **Ease of Use**: Stack simplifies the process of setting up and managing Haskell projects. It handles dependencies, builds, and tests with minimal configuration.

2. **Reproducible Builds**: Stack ensures that builds are reproducible by using curated package sets (Stackage) and allowing to specify exact versions of dependencies. This reduces the "it works on my machine" problem.

3. **Isolation**: Stack creates isolated environments for each project, preventing dependency conflicts between projects. This isolation ensures that each project has its own set of dependencies and does not interfere with others.

4. **Integration with Stackage**: Stackage is a stable source of Haskell packages that are tested to work well together. Stack's integration with Stackage ensures that projects are using a set of packages that are known to be compatible, reducing the likelihood of build issues.

5. **Build Automation**: Stack automates many aspects of the build process, including dependency resolution, compilation, and testing. This automation saves time and reduces the potential for human error.

6. **Community Support**: Stack has a large and active community, providing a wealth of resources, tutorials, and support. This makes it easier to find help and solutions to common problems.

7. **Cross-Platform**: Stack works on multiple platforms, including Windows, macOS, and Linux. This cross-platform support ensures that projects can be built and run on different operating systems without modification.

By using Stack as the build system for the GLaDOS project, we take advantage of these benefits to streamline development, ensure consistency, and improve the overall quality of our Haskell codebase.
