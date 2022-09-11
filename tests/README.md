This directory contains tests aiming to check for correctness of the implementation.

Tests are run using the a custom rust test harness. To run the test suite, navigate to the root directory
of this repository and run:
```
cargo test
```
(You can also restrict the tests to only these tests by running `cargo test samples`)

To run skipped tests (mainly tests that take very long to run and are therefore skipped by default), run:
```
cargo test -- --ignored
```