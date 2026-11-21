# Contributing to cusumtests

Open an issue for a bug report or proposed change. For bugs, include a
minimal example with the fitted model, the test call, the result you
expected, and your R and package versions. Use simulated data when the
original data cannot be shared.

For a code change, create a branch and open a pull request against
`main`. Add or update tests in `tests/testthat/` when changing behavior.
For changes to the statistical procedures, explain the assumptions and
how you verified the result against a reference calculation.

From the package directory, run:

```r
testthat::test_local()
```

Regenerate the help files with `roxygen2::roxygenise()` after editing
roxygen comments. Install `roxygen2` and `srr` for documentation work.
Run `Rscript checks.R` before opening a pull request. The README lists
what the checks cover.
