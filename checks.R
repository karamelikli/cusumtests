# Run Rscript checks.R for local checks or add --review for review reports.
main <- function(package) {
    args <- commandArgs(trailingOnly = TRUE)
    if (!identical(args, character()) && !identical(args, "--review")) {
        stop("Usage: Rscript checks.R [--review]", call. = FALSE)
    }
    review <- identical(args, "--review")
    dependencies <- c("testthat", "lintr", "covr", "autotest", "rcmdcheck")
    if (review) {
        dependencies <- c(dependencies, "pkgcheck", "srr")
    }
    available <- vapply(dependencies, requireNamespace, logical(1),
                        quietly = TRUE)
    if (!all(available)) {
        stop("Install the check packages: ",
             paste(dependencies[!available], collapse = ", "), call. = FALSE)
    }

    package <- normalizePath(package, mustWork = TRUE)
    script <- file.path(package, "checks.R")
    work <- tempfile("cusumtests-checks-")
    dir.create(work)
    old_dir <- setwd(work)
    on.exit({
        setwd(old_dir)
        unlink(work, recursive = TRUE)
    })

    message("Lint")
    linters <- lintr::linters_with_defaults(
        indentation_linter = lintr::indentation_linter(indent = 4L),
        object_name_linter = lintr::object_name_linter(
            styles = c("snake_case", "symbols"),
            regexes = c("^B$", "^OptModel$", "^cusumQ$",
                        "^p[.]value$", "^[.]Random[.]seed$")
        )
    )
    lints <- c(lintr::lint(script, linters = linters),
               lintr::lint_package(package, linters = linters))
    if (length(lints)) {
        print(lints)
        stop("Lint found ", length(lints), " issues.", call. = FALSE)
    }
    message("No lint issues.")

    message("Unit tests")
    testthat::test_local(package, reporter = "summary", stop_on_failure = TRUE)

    message("Test coverage")
    coverage <- covr::package_coverage(package, type = "tests")
    print(coverage)
    uncovered <- covr::zero_coverage(coverage)
    if (!isTRUE(covr::percent_coverage(coverage) == 100)) {
        print(uncovered)
        stop("Test coverage must be 100%.", call. = FALSE)
    }

    message("Automatic API diagnostics")
    # Bound the simulation workload while autotest probes the valid range.
    previous <- options(cusumtests.max_simulations = 5000L)
    diagnostics <- tryCatch(
        autotest::autotest_package(package, test = TRUE, quiet = TRUE),
        finally = options(previous)
    )
    if (is.null(diagnostics) || !nrow(diagnostics)) {
        message("No automatic API diagnostics.")
    } else {
        print(as.data.frame(diagnostics), row.names = FALSE)
    }

    message("R CMD check")
    rcmdcheck::rcmdcheck(package, check_dir = work, error_on = "note")

    if (review) {
        message("Package review reports")
        reports <- list(
            pkgcheck = function() {
                cache <- file.path(work, "pkgcheck")
                dir.create(cache)
                previous <- Sys.getenv("PKGCHECK_CACHE_DIR", unset = NA)
                on.exit({
                    if (is.na(previous)) {
                        Sys.unsetenv("PKGCHECK_CACHE_DIR")
                    } else {
                        Sys.setenv(PKGCHECK_CACHE_DIR = previous)
                    }
                })
                Sys.setenv(PKGCHECK_CACHE_DIR = cache)
                print(summary(pkgcheck::pkgcheck(package, use_cache = FALSE)))
            },
            srr = function() {
                report <- srr::srr_report(
                    package, view = FALSE, roxygenise = FALSE
                )
                output <- attr(report, "file")
                if (is.null(output)) {
                    message(report)
                } else {
                    destination <- tempfile(
                        "srr-report-", tmpdir = dirname(script),
                        fileext = ".html"
                    )
                    if (!file.copy(output, destination)) {
                        stop("Could not save the srr report.", call. = FALSE)
                    }
                    message("srr report: ", destination)
                }
            }
        )
        errors <- lapply(names(reports), function(name) {
            tryCatch({
                reports[[name]]()
                NULL
            }, error = function(err) {
                paste(name, conditionMessage(err), sep = ": ")
            })
        })
        errors <- unlist(errors)
        if (length(errors)) {
            stop(paste(errors, collapse = "\n"), call. = FALSE)
        }
    }
}

if (sys.nframe() == 0L) {
    script_arg <- grep("^--file=", commandArgs(), value = TRUE)
    script <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
    main(dirname(script))
}
