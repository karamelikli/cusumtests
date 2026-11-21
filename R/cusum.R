#' Test an Ordered Regression for Instability
#'
#' Compute the recursive cumulative sum (CUSUM) or cumulative sum of squares
#' (CUSUMQ) test. Both functions report two-sided decisions and simultaneous
#' significance bands at 1%, 5%, and 10% by default.
#'
#' @param model A fitted unweighted, single-response [stats::lm()]
#'   model or compatible subclass. Fit the model with rows in the desired
#'   order. For a design with `k` columns, the first `k` fitted rows must
#'   identify all coefficients, and at least two rows must remain.
#' @param OptModel A fitted [stats::lm()] model for CUSUMQ, with the same
#'   requirements as `model`.
#' @param alpha A vector of distinct values from `0.01`, `0.05`, and `0.10`.
#'   Supply at least one level. Equivalent character values are accepted.
#' @param B Number of null simulations. Use a whole number with a lower limit
#'   of 99 and an upper limit of 2147483646. A lower ceiling can be set with
#'   `options(cusumtests.max_simulations = ...)`.
#' @param seed Optional nonnegative whole-number seed from 0 to 2147483647.
#'   A supplied seed restores
#'   the caller's `.Random.seed` on exit. With `NULL`, the simulations advance
#'   the current random stream.
#' @param time Optional strictly increasing numeric, `Date`, or `POSIXct`
#'   vector with one value per fitted row after subsetting and missing-data
#'   removal. The default uses fitted row positions.
#'
#' @details
#' For `B` and `seed`, values within 128 machine epsilons of an integer
#' are rounded to that integer to allow for floating-point roundoff.
#'
#' `cusum()` accumulates signed recursive residuals and scales the sum by the
#' square root of their total sum of squares. Its bands widen linearly from
#' zero. `cusumq()` accumulates each squared recursive residual's share of the
#' total sum of squares. Its bands run parallel to the fraction of residuals
#' accumulated. `cusumQ()` is an alias for `cusumq()`.
#'
#' The null reference assumes fixed regressors and independent, homoskedastic
#' Gaussian errors. The simulation draws independent standard normal recursive
#' residuals with the same count as in the fit. Both tests rank the observed
#' maximum deviation together with their simulated maximum deviations. The
#' p-value is one plus the number of simulated statistics at least as large as
#' the observed statistic, divided by `B + 1`. The corresponding band cutoffs
#' give the same decision as `p.value <= alpha` up to numerical precision.
#'
#' A crossing is evidence against the stated reference model. Serial
#' dependence, lagged outcomes, heteroskedasticity, and heavy tails can affect
#' calibration. The first crossing may occur after the underlying change.
#'
#' @return An object of class `"cusum"` with these components:
#'   - `type`, `statistic`, and `p.value` give the test name, observed
#'     statistic, and two-sided Monte Carlo p-value.
#'   - `alpha`, `critical_value`, `reject`, and `stability` give the requested
#'     levels, corresponding cutoffs, rejection indicators, and labels.
#'     `"Stable"` means failure to reject at that level.
#'   - `decisions` is a data frame with one row per significance level.
#'   - `data` is a data frame with one row per significance level and
#'     recursive residual. Its columns are `time`, `fraction`, `center`,
#'     `cusum` or `cusumq`, `alpha`, `lower`, and `upper`.
#'   - `B` and `n.recursive` give the simulation and recursive residual counts.
#' @references
#' Brown, R. L., Durbin, J., and Evans, J. M. (1975).
#' Techniques for Testing the Constancy of Regression Relationships Over Time.
#' *Journal of the Royal Statistical Society, Series B*, 37(2), 149--163.
#' \doi{10.1111/j.2517-6161.1975.tb01532.x}
#'
#' Zeileis, A., Leisch, F., Hornik, K., and Kleiber, C. (2002).
#' strucchange: An R Package for Testing for Structural Change in Linear
#' Regression Models. *Journal of Statistical Software*, 7(2), 1--38.
#' \doi{10.18637/jss.v007.i02}
#' @seealso [plot.cusum()] for printing and plotting result objects.
#' @examples
#' fit <- lm(DriversKilled ~ log(kms) + PetrolPrice, data = Seatbelts)
#' months <- seq(as.Date("1969-01-01"), by = "month", length.out = nobs(fit))
#' signed <- cusum(model = fit, B = 999, seed = 17, time = months)
#' squared <- cusumq(OptModel = fit, B = 999, seed = 17, time = months)
#' signed
#' squared$decisions
#' plot(signed)
#' plot(squared, alpha = 0.05)
#' @srrstats {G1.0, G1.3, TS2.3} The help cites the original methods,
#'   defines the signed path and its bands, and states the Gaussian null.
#' @export
cusum <- function(model, alpha = c(0.01, 0.05, 0.10), B = 5000L,
                  seed = NULL, time = NULL) {
    stability_test(model, alpha, B, seed, time, "CUSUM")
}

#' @rdname cusum
#' @srrstats {G1.3, TS2.2} The squared path measures how recursive residual
#'   energy accumulates under the constant-variance reference.
#' @export
cusumq <- function(OptModel, alpha = c(0.01, 0.05, 0.10), B = 5000L,
                   seed = NULL, time = NULL) {
    stability_test(OptModel, alpha, B, seed, time, "CUSUMQ")
}

#' @rdname cusum
#' @srrstats {G1.4} The CUSUMQ alias shares the primary CUSUMQ documentation.
#' @export
cusumQ <- cusumq

#' Calibrate a recursive stability path and assemble the result
#'
#' Dispatch to the signed or squared path, simulate matching null maxima,
#' and use their ranks in the test decisions and bands.
#' @srrstats {G2.0, G2.3a, G2.4b} Simulation settings and the requested
#'   level vector are checked before estimating the reference cutoffs.
#' @srrstats {G3.0, TS4.0b} Strict crossings of the rank-based cutoffs yield
#'   the decision table and corresponding bands in the cusum object.
#' @noRd
stability_test <- function(model, alpha, B, seed, time, type) {
    allowed <- c(0.01, 0.05, 0.10)
    if (is.character(alpha) && is.null(dim(alpha))) {
        alpha <- suppressWarnings(as.numeric(alpha))
    }
    if (!is.numeric(alpha) || !is.null(dim(alpha)) || !length(alpha) ||
            !all(alpha %in% allowed) ||
            anyDuplicated(alpha)) {
        stop("alpha must contain distinct values from 0.01, 0.05, and 0.10.",
             call. = FALSE)
    }
    max_simulations <- getOption(
        "cusumtests.max_simulations", .Machine$integer.max - 1L
    )
    if (!is_integer_in_range(max_simulations, 99, .Machine$integer.max - 1L)) {
        stop(
            "The cusumtests.max_simulations option must be an integer ",
            "between 99 and 2147483646.",
            call. = FALSE
        )
    }
    max_simulations <- round(max_simulations)
    if (!is_integer_in_range(B, 99, max_simulations)) {
        stop("B must be an integer between 99 and ", max_simulations, ".",
             call. = FALSE)
    }
    if (!is.null(seed) && !is_integer_in_range(seed, 0, .Machine$integer.max)) {
        stop("seed must be NULL or a nonnegative integer up to 2147483647.",
             call. = FALSE)
    }
    if (is.double(B)) {
        B <- round(B)
    }

    residuals <- ordered_residuals(model, time)
    m <- length(residuals$values)
    fraction <- seq_len(m) / m
    squared <- type == "CUSUMQ"
    center <- if (squared) fraction else numeric(m)
    boundary_shape <- if (squared) rep(1, m) else 1 + 2 * fraction

    path <- residual_path(residuals$values, squared)
    statistic <- max(abs(path - center) / boundary_shape)
    simulated <- simulate_statistics(
        m, B, seed, squared, center, boundary_shape
    )
    p_value <- (1 + sum(simulated >= statistic)) / (B + 1)

    # A strict crossing agrees with p <= alpha, even when statistics tie.
    rank <- B + 1 - floor(round(100 * alpha) * (B + 1) / 100)
    critical <- sort(simulated)[rank]
    reject <- statistic > critical
    decisions <- data.frame(alpha = alpha, critical_value = critical,
                            reject = reject)
    series <- data.frame(time = residuals$time, fraction = fraction,
                         center = center)
    series[[tolower(type)]] <- path
    bands <- do.call(rbind, lapply(seq_along(alpha), function(i) {
        #' @srrstats {TS4.3} Attach the original row index to each level.
        data.frame(series, alpha = alpha[i],
                   lower = center - critical[i] * boundary_shape,
                   upper = center + critical[i] * boundary_shape)
    }))

    structure(list(type = type, statistic = statistic, p.value = p_value,
                   alpha = alpha, critical_value = critical, reject = reject,
                   stability = ifelse(reject, "Unstable", "Stable"),
                   decisions = decisions, data = bands, B = B, n.recursive = m),
              class = "cusum")
}

#' Validate a bounded integer-valued scalar
#'
#' Check type, dimensions, length, finiteness, bounds, and whole-valuedness.
#' @srrstats {G2.0, G2.1, G2.2} Both simulation controls use this scalar check.
#' @noRd
is_integer_in_range <- function(x, min, max) {
    if (!is.numeric(x) || !is.null(dim(x)) || length(x) != 1L ||
            !is.finite(x)) {
        return(FALSE)
    }
    rounded <- round(x)
    rounded >= min && rounded <= max &&
        abs(x - rounded) <= 128 * .Machine$double.eps
}

#' Extract validated recursive residuals and their observation indices
#'
#' Use the fitted response and design, remove any known offset, and align
#' the labels to rows after the initial fit.
#' @srrstats {G2.8, G2.10, G2.13, G2.15} The preprocessor extracts model
#'   components and supplies checked residuals to both diagnostics.
#' @srrstats {TS1.2, TS1.3, TS1.4, TS1.5, TS1.6} A supplied time index is
#'   validated for ordering, and its class is kept when selecting rows.
#' @noRd
ordered_residuals <- function(model, time) {
    if (!inherits(model, "lm") || inherits(model, "mlm")) {
        stop("Supply a fitted single-response lm model.", call. = FALSE)
    }
    if (!is.null(model$weights) && any(model$weights != 1)) {
        stop("The tests require an unweighted regression.", call. = FALSE)
    }

    frame <- stats::model.frame(model)
    x <- stats::model.matrix(model)
    y <- stats::model.response(frame)
    if (!is.numeric(y) || is.matrix(y) || !all(is.finite(y)) ||
            !all(is.finite(x))) {
        stop("The model must have a finite numeric response and design.",
             call. = FALSE)
    }
    offset <- stats::model.offset(frame)
    if (!is.null(offset)) {
        y <- y - offset
        if (!all(is.finite(y))) {
            stop("The offset-adjusted response must be finite.", call. = FALSE)
        }
    }
    n <- nrow(x)
    k <- ncol(x)
    if (n - k < 2L || model$rank != k) {
        stop(
            "The regression must have full rank and ",
            "at least two recursive residuals.",
            call. = FALSE
        )
    }
    if (k > 0L && qr(x[seq_len(k), , drop = FALSE])$rank != k) {
        stop(
            "The first k fitted rows must identify ",
            "all k regression coefficients.",
            call. = FALSE
        )
    }

    if (is.null(time)) {
        time <- seq_len(n)
    }
    if ((!is.numeric(time) && !inherits(time, c("Date", "POSIXct"))) ||
            !is.null(dim(time)) || length(time) != n ||
            !all(is.finite(as.numeric(time))) ||
            is.unsorted(time, strictly = TRUE)) {
        stop(
            "time must be a strictly increasing numeric, Date, ",
            "or POSIXct vector with one entry per fitted row.",
            call. = FALSE
        )
    }
    rows <- seq.int(k + 1L, n)
    w <- if (k == 0L) y else strucchange::recresid(x, y)
    if (!all(is.finite(w)) ||
            max(abs(w)) <= 8 * .Machine$double.eps * max(abs(y))) {
        stop(
            "The regression must yield finite, nonzero ",
            "recursive residual variation.",
            call. = FALSE
        )
    }
    list(values = w, time = time[rows])
}

#' Construct a cumulative path from finite recursive residuals
#'
#' Rescale the residuals before squaring and compute the signed or squared
#' cumulative path.
#' @srrstats {G1.3, G2.15} The dimensionless paths use the same definition
#'   for observed and simulated, validated residuals.
#' @noRd
residual_path <- function(w, squared) {
    # Rescaling first avoids overflow or underflow when squaring residuals.
    w <- w / max(abs(w))
    if (squared) {
        cumulative <- cumsum(w^2)
        cumulative / cumulative[length(w)]
    } else {
        cumsum(w) / sqrt(sum(w^2))
    }
}

#' Simulate the null maximum-deviation statistic
#'
#' Draw independent Gaussian recursive residuals and optionally preserve
#' the caller's random-number state while simulating the reference maxima.
#' @srrstats {G1.3, TS2.3} The reference uses the observed residual count and
#'   applies the observed path and maximum-deviation definitions.
#' @noRd
simulate_statistics <- function(m, B, seed, squared, center, boundary_shape) {
    if (!is.null(seed)) {
        old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        on.exit({
            if (is.null(old_seed)) {
                rm(".Random.seed", envir = .GlobalEnv)
            } else {
                assign(".Random.seed", old_seed, envir = .GlobalEnv)
            }
        })
        set.seed(round(seed))
    }
    replicate(B, {
        path <- residual_path(stats::rnorm(m), squared)
        max(abs(path - center) / boundary_shape)
    })
}
