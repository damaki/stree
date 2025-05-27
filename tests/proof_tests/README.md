# Proof Tests

This directory contains tests that verify usage of the SPARK contracts.
I.e., that it is possible to prove interesting properties in code using the
containers.

The test cases consist of small pieces of SPARK code that uses the Stree library
to construct trees in various ways, with contracts and assertions to verify
that expected properties on the tree hold.
For each test case, Alire is used to verify that the test case compiles, then
GNATprove is run to verify that all assertions and contracts in the test case
are proved by GNATprove as expected.

## Structure

Each test case lives in its own subdirectory under the `tests` directory.
Each test case subdirectory contains:
* The SPARK source files to be proved.
* A file called `prove.expected.stderr` that contains the expected standard
  error output from GNATprove. The test fails if the actual output from
  GNATprove does not exactly match the contents of this file.

## Prerequisites

* [Alire](https://alire.ada.dev/)
* [pytest](https://docs.pytest.org/en/stable/)

## Running the Tests

The tests are run by executing `pytest` in this directory:
```sh
pytest .
```

Pytest will automatically find the test cases in the `tests` directory, and
will build and prove them.

By default, each test will be run sequentially and GNATprove will only use a
single thread. To speed up the tests, use `--gnatprove-jobs` to set the number
of threads used when running GNATprove. For example, to use all available CPU
threads when running GNATprove:
```sh
pytest . --gnatprove-jobs=0
```

Pytest's `-n` switch can also be used to run multiple tests in parallel, but
note that each test case will run GNATprove independently with the same
`--gnatprove-jobs` value. So running `-n=2` `--gnatprove-jobs=16` will result
in 2 instances of GNATprove running up to 32 threads.

## Generating Baselines

As mentioned above, each test case contains a file called
`prove.expected.stderr` that contains the expected output from GNATprove when
proving the test case. These files may need to be regenerated when changes are
made to the library, as the line numbers or number of verification conditions
(VCs) may change.

To regenerate the files, pass the `--baseline` switch to pytest along with any
other desired arguments:
```sh
pytest . --baseline
```

> :warning: When generating a baseline you must manually verify that the
  changes to the GNATprove are correct (i.e. that there are no unexpected failed
  checks/proofs). `git diff` can be useful to verify the changes after the
  baseline is generated.