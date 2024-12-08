# GLaDOS functional tests

This directory contains a tester script which will run functional tests on a given executable.

It is parametrized by two environment variables:
- `GLaDOS_EXEC_PATH`: the path to the executable
- `TEST_CASES_DIRECTORY`: the path to the directory containing test cases.

The directory containing the test cases should contain a subdirectory for each test, containing info about the test. In each subdirectory, there must be two files:
- `in`: The standard input that will be passed to the executable
- `out`: The expected standard output of the executable

The executable will be run given `in` in standard input, and its standard output will be compared to `out`. The test will be marked as failed if the standard output does not match. Additionally, the executable is expected to return a 0 status code by default, which can be changed by putting a file named `exit` containing the expected exit code.

## Example

```
cases
├── simple_test
│   ├── in
│   └── out
└── test_with_exit_status
    ├── exit
    ├── in
    └── out
```
