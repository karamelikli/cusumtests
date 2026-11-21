# cusumtests

Run recursive CUSUM and CUSUM of squares tests on an ordered linear regression.
For each diagnostic, the same finite-sample Gaussian Monte Carlo draws
produce the two-sided p-value and simultaneous significance bands at 1%, 5%,
and 10%. A strict crossing agrees with rejection by the p-value at that
level. Each result returns a decision table and the data used to plot the
path and its bands.

## Install

From the package directory, install the runtime dependency if needed with
`install.packages("strucchange")`. To build and install the package with
its vignette, also install `knitr`, `rmarkdown`, and Pandoc.

```sh
R CMD build .
R CMD INSTALL cusumtests_0.1.0.tar.gz
```

For a quick installation from the source directory, run
`R CMD INSTALL .`.

## Run both tests

```r
library(cusumtests)

belts <- as.data.frame(datasets::Seatbelts)
fit <- lm(DriversKilled ~ log(kms) + PetrolPrice, data = belts)
signed <- cusum(fit, B = 4999, seed = 17)
squared <- cusumq(fit, B = 4999, seed = 17)

signed
squared$decisions
```

`cusumQ()` is an alias for `cusumq()`. The fit must use unweighted least
squares with a single numeric response in the desired observation order.
Compatible `lm` subclasses such as `kardl_lm` can supply recursive residuals.
The finite-sample calibration assumes fixed regressors and independent
Gaussian errors with constant variance. Models with lagged outcomes or
serially dependent errors can require a different reference.

## Plot with dates and inspect a single band

```r
month <- seq(as.Date("1969-01-01"), by = "month", length.out = nrow(belts))
squared <- cusumq(fit, B = 4999, seed = 17, time = month)

plot(signed)
plot(squared, alpha = 0.05, xlab = "Month")

band_5 <- subset(squared$data, alpha == 0.05)
head(band_5)
with(band_5, any(cusumq < lower | cusumq > upper))
squared$p.value <= 0.05
```

Use `alpha = 0.05` in a test call to compute only that level's bands and
decision. Use `seed` to reproduce the Monte Carlo draws with the same
random-number settings. If the fit drops rows, provide dates for the
rows used in the fit.

## Documentation

```r
help("cusum", package = "cusumtests")
vignette("cusumtests", package = "cusumtests")
citation("cusumtests")
```

The [vignette source](vignettes/cusumtests.Rmd) includes a simulated mean
shift and explains what band crossings can tell you.

## Package checks

To regenerate the help files and namespace, install `roxygen2` and `srr`.
From the package directory, run `roxygen2::roxygenise()`. The package
configuration loads `srr` so roxygen2 can read the statistical review tags.

From the package directory, run `Rscript checks.R` to lint the package and
check script, run the unit tests, enforce 100% test line coverage, run
automatic API diagnostics, and run `R CMD check`. Add `--review` to request
the `pkgcheck` and `srr` reports. The review tools may need network access.

## Lifecycle

Version 0.1.0 is the initial development release. The current API provides
retrospective diagnostics for ordered linear regressions. Future releases
may refine the interface as the package develops.

## Companion paper

The manuscript, references, and reproducible R Markdown source are in
the `paper/` directory of the development workspace, alongside the package
directory. From that workspace, regenerate the paper with
`rmarkdown::render("paper/paper.Rmd")` after installing the package,
Pandoc, and LaTeX.
