#' Print and Plot CUSUM Test Results
#'
#' Print the test statistic, p-value, and decisions. Plot the observed path,
#' its null reference, and both limits for each selected significance level.
#'
#' @param x Result from [cusum()] or [cusumq()].
#' @param alpha Numeric significance levels present in the result. The default
#'   shows all levels in `x`.
#' @param main,xlab,ylab Plot title and axis labels.
#' @param ylim Optional numeric vector with two limits for the vertical axis.
#' @param ... Additional arguments to the underlying `print()` or `plot()` call.
#' @return Both methods return `x` invisibly.
#' @examples
#' fit <- lm(DriversKilled ~ log(kms) + PetrolPrice, data = Seatbelts)
#' result <- cusum(fit, B = 999, seed = 17)
#' print(result)
#' plot(result, alpha = 0.05)
#' @rdname cusum-methods
#' @srrstats {TS4.2} The print method reports the test, simulation count,
#'   Monte Carlo p-value, and decisions, and returns the original object.
#' @export
print.cusum <- function(x, ...) {
    cat(x$type, "test for regression stability\n")
    cat("Statistic =", format(x$statistic, digits = 5),
        "| Monte Carlo p-value =", format(x$p.value, digits = 5), "\n")
    cat(x$n.recursive, "recursive residuals |", x$B, "null simulations\n\n")
    table <- x$decisions
    table$alpha <- paste0(round(100 * table$alpha), "%")
    print(table, row.names = FALSE, ...)
    invisible(x)
}

#' @rdname cusum-methods
#' @srrstats {TS5.0, TS5.1, TS5.2, TS5.3} The registered plot method places
#'   fitted-row, Date, or POSIXct values on the horizontal axis.
#' @export
plot.cusum <- function(x, alpha = x$alpha,
                       main = paste(x$type, "stability test"),
                       xlab = "Observation", ylab = x$type, ylim = NULL, ...) {
    if (!is.null(dim(alpha)) || !length(alpha) || !all(alpha %in% x$alpha) ||
            anyDuplicated(alpha)) {
        stop("Choose distinct significance levels present in the result.",
             call. = FALSE)
    }
    path <- x$data[x$data$alpha == x$alpha[1L], ]
    values <- path[[tolower(x$type)]]
    bands <- x$data[x$data$alpha %in% alpha, ]
    if (is.null(ylim)) {
        ylim <- range(values, path$center, bands$lower, bands$upper)
    }
    graphics::plot(path$time, values, type = "n", main = main,
                   xlab = xlab, ylab = ylab, ylim = ylim, ...)
    colors <- c("#762A83", "#007A69", "#A45108")
    styles <- c(2L, 3L, 4L)
    ids <- match(alpha, c(0.01, 0.05, 0.10))
    for (i in seq_along(alpha)) {
        band <- bands[bands$alpha == alpha[i], ]
        graphics::lines(band$time, band$lower, col = colors[ids[i]],
                        lty = styles[ids[i]])
        graphics::lines(band$time, band$upper, col = colors[ids[i]],
                        lty = styles[ids[i]])
    }
    graphics::lines(path$time, path$center, col = "gray45", lty = 5)
    graphics::lines(path$time, values, col = "#17365D", lwd = 2)
    # Use the numeric axis coordinates for legends on Date and POSIXct plots.
    graphics::legend("topleft",
                     legend = c("Observed", "Null reference",
                                paste0(round(100 * alpha), "% bands")),
                     col = c("#17365D", "gray45", colors[ids]),
                     lty = c(1, 5, styles[ids]),
                     lwd = c(2, 1, rep(1, length(alpha))),
                     bty = "n", cex = 0.8)
    invisible(x)
}
