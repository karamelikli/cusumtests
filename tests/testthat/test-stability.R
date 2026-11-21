#' @srrstats {G5.1, G5.5} This deterministic fixture is defined in the suite.
stability_data <- function() {
    x <- seq_len(25)
    data.frame(x = x, y = 2 + x / 4 + sin(x) + cos(2 * x) / 2)
}

#' @srrstats {G5.4, G5.5, TS1.4} Check both paths against independent OLS refits
#'   and verify the retained Date indices using a fixed calibration seed.
test_that("both paths match standardized forecast errors from OLS refits", {
    data <- stability_data()
    fit <- lm(y ~ x, data)
    design <- model.matrix(fit)
    k <- ncol(design)
    rows <- seq.int(k + 1L, nrow(design))
    m <- length(rows)

    w <- vapply(rows, function(i) {
        #' @srrstats {G5.4} Calculate an independent forecast error per row.
        previous <- seq_len(i - 1L)
        train <- design[previous, , drop = FALSE]
        beta <- lm.fit(train, data$y[previous])$coefficients
        row <- design[i, ]
        (data$y[i] - sum(row * beta)) /
            sqrt(1 + drop(row %*% solve(crossprod(train), row)))
    }, numeric(1))
    fraction <- seq_len(m) / m
    signed_path <- cumsum(w) / sqrt(sum(w^2))
    squared_path <- cumsum(w^2) / sum(w^2)
    dates <- as.Date("2000-01-01") + seq_len(nobs(fit))

    signed <- cusum(fit, B = 137, seed = 3, time = dates)
    squared <- cusumq(fit, B = 137, seed = 3, time = dates)

    expect_equal(signed$data$cusum[seq_len(m)], signed_path, tolerance = 1e-12)
    expect_equal(squared$data$cusumq[seq_len(m)], squared_path,
                 tolerance = 1e-12)
    expect_equal(signed$statistic, max(abs(signed_path) / (1 + 2 * fraction)),
                 tolerance = 1e-12)
    expect_equal(squared$statistic, max(abs(squared_path - fraction)),
                 tolerance = 1e-12)
    expect_identical(signed$data$time[seq_len(m)], dates[rows])
    expect_identical(squared$data$time[seq_len(m)], dates[rows])
})

#' @srrstats {G5.4, G5.7} Compare the band and p-value decisions on baseline,
#'   mean-shift, and highly concentrated residual-energy inputs.
test_that("bands, cutoffs, and p-values agree at all three levels", {
    data <- stability_data()
    stable_fit <- lm(y ~ x, data)
    signed_fit <- lm(y ~ 1, data.frame(y = rep(c(0, 1), each = 50)))
    squared_fit <- lm(y ~ 1, data.frame(y = c(rep(0, 99), 1)))
    signed_shift <- cusum(signed_fit, B = 137, seed = 3)
    squared_shift <- cusumq(squared_fit, B = 137, seed = 3)

    expect_true(signed_shift$reject[2L])
    expect_true(squared_shift$reject[2L])

    signed <- cusum(stable_fit, B = 137, seed = 3)
    squared <- cusumq(stable_fit, B = 137, seed = 3)
    expect_false(signed$reject[2L])
    expect_false(squared$reject[2L])

    for (result in list(signed, squared, signed_shift, squared_shift)) {
        path <- result$data[[tolower(result$type)]]
        crossed <- path > result$data$upper | path < result$data$lower
        crossings <- tapply(crossed, result$data$alpha, any)

        expect_identical(result$alpha, c(0.01, 0.05, 0.10))
        expect_identical(result$reject, result$p.value <= result$alpha)
        expect_identical(as.vector(crossings), result$reject)
        expect_true(all(diff(result$critical_value) <= 0))
    }
})

#' @srrstats {G2.3a, G2.4b} Check that a valid character code selects the
#'   identical level and cutoff from the full result.
test_that("the CUSUMQ alias and single character level work", {
    fit <- lm(y ~ x, stability_data())
    squared <- cusumq(fit, B = 137, seed = 3)
    expect_identical(cusumQ(fit, B = 137, seed = 3), squared)

    signed <- cusum(fit, B = 137, seed = 3)
    single <- cusum(fit, alpha = "0.10", B = 137, seed = 3)
    expected <- signed$decisions[3L, , drop = FALSE]
    rownames(expected) <- NULL
    expect_identical(single$decisions, expected)
})

#' @srrstats {G5.4} Equivalent model responses give the same offset-corrected
#'   recursive statistic.
test_that("known offsets are removed before calculating recursive residuals", {
    data <- stability_data()
    fit <- lm(y ~ x, data)
    offset_fit <- lm(y + x ~ x + offset(x), data)

    expect_equal(cusum(offset_fit, B = 137, seed = 3)$statistic,
                 cusum(fit, B = 137, seed = 3)$statistic, tolerance = 1e-12)
})

#' @srrstats {G5.5} Check the seeded and unseeded random-stream behavior.
test_that("only unseeded simulations advance the caller's random stream", {
    fit <- lm(y ~ x, stability_data())

    #' @srrstats {G5.5} Verify and restore the caller's random-number state.
    check_rng <- function() {
        seed_name <- ".Random.seed"
        original <- get0(seed_name, envir = .GlobalEnv, inherits = FALSE)
        on.exit({
            if (!is.null(original)) {
                assign(seed_name, original, envir = .GlobalEnv)
            } else if (exists(seed_name, envir = .GlobalEnv,
                              inherits = FALSE)) {
                rm(list = seed_name, envir = .GlobalEnv)
            }
        })

        set.seed(712)
        rng <- .Random.seed
        cusum(fit, B = 99, seed = 1)
        expect_identical(.Random.seed, rng)

        rm(list = seed_name, envir = .GlobalEnv)
        cusum(fit, B = 99, seed = 1)
        expect_false(exists(seed_name, envir = .GlobalEnv, inherits = FALSE))

        cusumq(fit, B = 99)
        expect_true(exists(seed_name, envir = .GlobalEnv, inherits = FALSE))
        rng <- .Random.seed
        cusumq(fit, B = 99)
        expect_false(identical(.Random.seed, rng))
    }

    check_rng()
})

#' @srrstats {TS5.0, TS5.2} Render both S3 plots with Date indices.
test_that("results can be plotted with all bands or one level", {
    fit <- lm(y ~ x, stability_data())
    dates <- as.Date("2000-01-01") + seq_len(nobs(fit))
    signed <- cusum(fit, B = 99, seed = 3, time = dates)
    squared <- cusumq(fit, B = 99, seed = 3, time = dates)
    plot_file <- tempfile(fileext = ".pdf")

    tryCatch({
        grDevices::pdf(plot_file)
        tryCatch({
            expect_identical(plot(signed), signed)
            expect_identical(plot(squared, alpha = 0.05), squared)
        }, finally = grDevices::dev.off())
        expect_gt(file.info(plot_file)$size, 0)
    }, finally = unlink(plot_file))
})

#' @srrstats {G5.2b, G5.8c, G5.8d, TS1.6} Invalid levels, weights, ordering,
#'   and degenerate fits produce their documented errors.
test_that("unsupported levels, time indices, and fits are rejected", {
    data <- stability_data()
    fit <- lm(y ~ x, data)

    expect_error(cusum(fit, alpha = 0.025), "alpha")
    expect_error(cusum(fit, time = rev(seq_len(nobs(fit)))),
                 "strictly increasing")
    expect_error(cusum(fit, time = rep(NA_real_, nobs(fit))),
                 "strictly increasing")
    duplicate <- c(1, seq_len(nobs(fit) - 1L))
    expect_error(cusum(fit, time = duplicate), "strictly increasing")
    expect_error(cusum(lm(y ~ x, data, weights = x)), "unweighted")
    expect_error(cusum(lm(y ~ I(x > 10), data)), "first k fitted rows")
    expect_error(cusumq(lm(I(2 * x) ~ x, data)),
                 "nonzero recursive residual variation")
})

#' @srrstats {TS4.2} Verify the printed summary and invisible return value.
test_that("printing reports calibration details and returns invisibly", {
    fit <- lm(y ~ x, stability_data())
    result <- cusum(fit, B = 99, seed = 3)

    output <- capture.output(printed <- withVisible(print(result)))
    expect_match(output[1L], "CUSUM test for regression stability",
                 fixed = TRUE)
    expect_true(any(grepl("Monte Carlo p-value", output, fixed = TRUE)))
    expect_true(any(grepl("99 null simulations", output, fixed = TRUE)))
    expect_true(all(vapply(c("1%", "5%", "10%"), function(level) {
        #' @srrstats {TS4.2} Verify the summary reports each level.
        any(grepl(level, output, fixed = TRUE))
    }, logical(1))))
    expect_identical(printed$value, result)
    expect_false(printed$visible)
})

#' @srrstats {G5.2b, G5.8a} Empty or incompatible plot levels are rejected.
test_that("plot levels must be present and distinct", {
    fit <- lm(y ~ x, stability_data())
    result <- cusum(fit, alpha = 0.05, B = 99, seed = 3)
    message <- "Choose distinct significance levels"

    expect_error(plot(result, alpha = numeric(0)), message)
    expect_error(plot(result, alpha = 0.10), message)
    expect_error(plot(result, alpha = c(0.05, 0.05)), message)
    expect_error(plot(result, alpha = matrix(0.05)), message)
})

#' @srrstats {G5.4, G5.8} Check the closed-form paths for a zero-column design.
test_that("a zero-column design uses the response as recursive residuals", {
    data <- data.frame(y = c(4, -2, 1, 2))
    fit <- lm(y ~ 0, data)
    signed <- cusum(fit, alpha = 0.05, B = 99, seed = 3)
    squared <- cusumq(fit, alpha = 0.05, B = 99, seed = 3)

    expect_equal(signed$data$cusum, cumsum(data$y) / sqrt(sum(data$y^2)),
                 tolerance = 1e-12)
    expect_equal(squared$data$cusumq, cumsum(data$y^2) / sum(data$y^2),
                 tolerance = 1e-12)
    expect_identical(signed$data$time, seq_len(nrow(data)))
    expect_identical(squared$n.recursive, nrow(data))
})

#' @srrstats {G2.14b, TS1.4, TS2.1b} The same retained rows give the same
#'   paths whether filtering happens explicitly or through the fitted model.
test_that("POSIXct times follow the fitted rows after missing data removal", {
    data <- stability_data()
    data$y[5L] <- NA_real_
    times <- as.POSIXct("2000-01-01", tz = "UTC") + 3600 * seq_len(nrow(data))
    fit <- lm(y ~ x, data, na.action = na.omit)
    fitted_times <- times[-as.integer(na.action(fit))]
    result <- cusum(fit, alpha = 0.05, B = 99, seed = 3, time = fitted_times)
    complete_fit <- lm(y ~ x, data[!is.na(data$y), ])
    complete_result <- cusum(complete_fit, alpha = 0.05, B = 99, seed = 3,
                             time = fitted_times)

    expect_identical(result$data$time, fitted_times[-seq_len(fit$rank)])
    expect_identical(result, complete_result)
    expect_error(cusum(fit, time = times), "one entry per fitted row")
})

#' @srrstats {G2.0, G2.2, G5.2b, G5.8a, G5.8b} Counts, seeds, and levels
#'   are exercised with malformed numeric, empty, and unsupported inputs.
test_that("invalid simulation counts, seeds, and levels are rejected", {
    fit <- lm(y ~ x, stability_data())
    for (count in list(98, 99.5, Inf, .Machine$integer.max, matrix(99))) {
        expect_error(cusum(fit, B = count), "B must be an integer")
    }
    for (seed in list(-1, 1.5, NA_real_, Inf, "3", c(1, 2), matrix(3))) {
        expect_error(cusum(fit, seed = seed), "seed must be")
    }
    expect_error(cusum(fit, alpha = c(0.01, 0.01)), "distinct values")
    expect_error(cusum(fit, alpha = "invalid"), "distinct values")
    expect_error(cusum(fit, alpha = numeric(0)), "distinct values")
    expect_error(cusum(fit, alpha = matrix(0.05)), "distinct values")
    expect_error(cusum(fit, alpha = matrix("0.05")), "distinct values")
})

#' @srrstats {G2.2, G5.2b, G5.8d} Unsupported fit classes, multivariate
#'   responses, and inadequate designs fail validation.
test_that("model classes and full-sample rank are validated", {
    data <- stability_data()
    data$duplicate <- data$x
    data$other <- data$y + data$x
    multivariate <- lm(cbind(y, other) ~ x, data)
    rank_deficient <- lm(y ~ x + duplicate, data)
    too_short <- lm(y ~ x, data[seq_len(3), ])

    expect_error(cusum(NULL), "single-response lm model")
    expect_error(cusumq(multivariate), "single-response lm model")
    expect_error(cusum(rank_deficient), "full rank")
    expect_error(cusumq(too_short), "at least two recursive residuals")
})

#' @srrstats {G2.13, G2.16, G5.2b} Nonfinite responses and overflowing offset
#'   subtraction produce distinct input errors.
test_that("non-finite model responses and offset arithmetic are rejected", {
    data <- stability_data()
    fit <- lm(y ~ x, data)
    fit$model$y[1L] <- Inf
    expect_error(cusum(fit), "finite numeric response and design")

    fit <- lm(y ~ x + offset(duplicate), transform(data, duplicate = x))
    fit$model$y[1L] <- .Machine$double.xmax
    fit$model[["offset(duplicate)"]][1L] <- -.Machine$double.xmax
    expect_error(cusumq(fit), "offset-adjusted response must be finite")
})

#' @srrstats {G5.0, G5.3, G5.4b} Verify complete results on Seatbelts and
#'   compare the signed path with strucchange using a common normalization.
test_that("Seatbelts paths are finite and the signed benchmark agrees", {
    data <- as.data.frame(datasets::Seatbelts)
    fit <- lm(DriversKilled ~ log(kms) + PetrolPrice, data)
    dates <- seq(as.Date("1969-01-01"), by = "month", length.out = nrow(data))
    signed <- cusum(fit, B = 99, seed = 3, time = dates)
    squared <- cusumq(fit, B = 99, seed = 3, time = dates)

    for (result in list(signed, squared)) {
        expect_false(anyNA(result))
        expect_true(all(is.finite(unlist(lapply(result$data, as.numeric)))))
        expect_true(all(is.finite(unlist(result$decisions))))
        expect_true(all(is.finite(c(result$statistic, result$p.value))))
    }

    reference <- strucchange::efp(
        DriversKilled ~ log(kms) + PetrolPrice, data, type = "Rec-CUSUM"
    )
    w <- strucchange::recresid(fit)
    m <- length(w)
    scale_ratio <- sd(w) * sqrt(m) / sqrt(sum(w^2))
    expect_equal(signed$data$cusum[seq_len(m)],
                 as.numeric(reference$process)[-1L] * scale_ratio,
                 tolerance = 1e-12)
})

#' @srrstats {G5.9, G5.9a, G5.9b} Tiny perturbations preserve statistics to a
#'   stated tolerance, and decisions far from the thresholds agree by seed.
test_that("small response perturbations and seed changes are predictable", {
    data <- stability_data()
    fit <- lm(y ~ x, data)
    data$y[9L] <- data$y[9L] + .Machine$double.eps * max(abs(data$y))
    perturbed_fit <- lm(y ~ x, data)

    for (test in list(cusum, cusumq)) {
        baseline <- test(fit, B = 499, seed = 3)
        perturbed <- test(perturbed_fit, B = 499, seed = 3)
        expect_equal(perturbed$statistic, baseline$statistic, tolerance = 1e-12)
        expect_identical(perturbed$reject, baseline$reject)
        for (seed in c(7, 17)) {
            expect_identical(test(fit, B = 499, seed = seed)$reject,
                             baseline$reject)
        }
    }
})

#' @srrstats {G2.0, G5.9a} Whole-number simulation controls tolerate roundoff.
test_that("simulation controls normalize near-integer floating-point inputs", {
    fit <- lm(y ~ x, stability_data())
    noise <- 100 * .Machine$double.eps
    result <- cusum(fit, B = 99, seed = 17)

    expect_identical(cusum(fit, B = 99 + noise, seed = 17 + noise), result)
    expect_identical(cusum(fit, B = 99L, seed = 17L)$B, 99L)
    expect_error(cusum(fit, B = 99 + 1e-6), "B must be an integer")
    expect_error(cusum(fit, seed = 17 + 1e-6), "seed must be")
})

#' @srrstats {G5.2b, G5.8d} A caller-defined simulation ceiling is enforced.
test_that("simulation budgets are validated and enforced", {
    fit <- lm(y ~ x, stability_data())
    previous <- options(cusumtests.max_simulations = 100L)
    tryCatch({
        expect_identical(cusum(fit, B = 100L, seed = 17)$B, 100L)
        expect_error(cusum(fit, B = 101L), "between 99 and 100")
        options(cusumtests.max_simulations = 98L)
        expect_error(cusumq(fit, B = 99L), "max_simulations option")
    }, finally = options(previous))
})
