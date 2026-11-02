## |
## |  *Test Execution*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |

#' @include f_quality_assurance.R
NULL

# Test selection, serial and parallel execution, and worker management.

# See testthat::skip_on_cran()
.skipTestIfDisabled <- function(msg = "Test is disabled", ..., ignoreInTestPlan = FALSE) {
    if (!isTRUE(.isCompleteUnitTestSetEnabled()) &&
            .isPackageNamespaceLoaded("testthat", quietly = TRUE)) {
        if (isTRUE(ignoreInTestPlan)) {
            msg <- paste(msg, "and ignored in test plan")
        }
        testthat::skip(msg)
    }
}

.skipTestIfCppCompilerIsMissing <- function() {
    if (.Platform$OS.type != "windows") {
        return(invisible())
    }

    if (.isPackageInstalled("pkgbuild") &&
            isTRUE(eval(parse(text = "pkgbuild::has_build_tools(debug = FALSE)")))) {
        return(invisible())
    }

    testthat::skip("The test requires a C++ compiler")
}

.skipTestIfNotX64 <- function() {
    if (!.isMachine64Bit() && !.isMinimumRVersion4() && .isPackageNamespaceLoaded("testthat", quietly = TRUE)) {
        testthat::skip("The test is only intended for R version 4.x or 64-bit computers (x86-64)")
    }
}

.skipTestIfPipeOperatorNotAvailable <- function() {
    if (!.isPipeOperatorAvailable()) {
        testthat::skip(paste0(
            "The test is disabled because it works only for ",
            "R version >= 4.1.0 (pipe operator is available)"
        ))
    }
}

.isMachine64Bit <- function() {
    return(Sys.info()[["machine"]] == "x86-64")
}

.isMinimumRVersion4 <- function() {
    return(R.Version()$major >= 4)
}

.isPipeOperatorAvailable <- function() {
    rVersion <- R.Version()
    return(rVersion$major >= 4 && rVersion$minor >= "1.0")
}

.testInstalledBasicPackages <- function(scope = c("basic", "devel", "both", "internet", "all"), ..., headingLevel = 4) {
    scope <- match.arg(scope)
    # Keep locale/environment changes outside the testthat host, and avoid
    # standard-stream pipes affected by Windows R bug 19104.
    stdout <- tempfile("rpact-basic-stdout-")
    stderr <- tempfile("rpact-basic-stderr-")
    on.exit(unlink(c(stdout, stderr)), add = TRUE)
    tryCatch(
        callr::r(function(scope, headingLevel) {
            rpact:::.testInstalledBasicPackagesInProcess(scope = scope, headingLevel = headingLevel)
        }, args = list(scope = scope, headingLevel = headingLevel), libpath = .libPaths(),
        stdout = stdout, stderr = stderr, stdin = NULL, poll_connection = FALSE,
        user_profile = FALSE),
        error = function(error) paste0("Failed to test installed R basic packages: ", conditionMessage(error)),
        finally = {
            for (path in c(stdout, stderr)) if (file.exists(path)) writeLines(readLines(path, warn = FALSE))
        }
    )
}

.testInstalledBasicPackagesInProcess <- function(scope = c("basic", "devel", "both", "internet", "all"), ..., headingLevel = 4) {
    scope <- match.arg(scope)
    basicTestDirectory <- tempfile("rpact-basic-tests-")
    dir.create(basicTestDirectory)
    on.exit(unlink(basicTestDirectory, recursive = TRUE), add = TRUE)
    tryCatch(
        {
            result <- utils::capture.output(
                tools::testInstalledBasic(scope = scope, outDir = basicTestDirectory,
                    testSrcdir = file.path(R.home(), "tests")),
                type = "message"
            )
            if (length(result) == 0) {
                return("Failed to test installed R basic packages: no test results available")
            }

            heading <- paste(rep("#", headingLevel), collapse = "")
            result <- gsub("^  ", "- ", result)
            headingIndices <- grep("^running.*", result)
            result[headingIndices] <- paste0("\n", heading, " ", .toCapitalized(gsub("$running", "", result[headingIndices])), "\n")
            cat(paste(result, collapse = "\n"))
            return(paste(result, collapse = "\n"))
        },
        error = function(e) {
            return(paste0("Failed to test installed R basic packages: ", e$message))
        }
    )
}

.getTestExecutionMode <- function(testFileDirectory, credentialsAvailable, downloadOnlyModeEnabled) {
    if (!is.na(testFileDirectory)) {
        # run test files from the user defined directory 'testFileDirectory'
        return("runTestsInTestFileDirectory")
    }

    if (credentialsAvailable) {
        if (downloadOnlyModeEnabled) {
            # download test files only
            return("downloadOnly")
        }

        # download test files and run them
        return("downloadAndRunTests")
    }

    # run test from directory 'inst/tests'
    return("default")
}

.getTestFileTargetDirectory <- function(outDir, testFileDirectory, executionMode, pkgName) {
    if (executionMode == "runTestsInTestFileDirectory") {
        return(testFileDirectory)
    }

    if (executionMode %in% c("downloadAndRunTests", "downloadOnly")) {
        return(file.path(outDir, paste0(pkgName, "-tests")))
    }

    return(file.path(find.package("rpact"), "tests"))
}

.getPackageTestWorkers <- function(workers = NULL, numberOfFiles = Inf) {
    if (is.null(workers)) {
        available <- if (requireNamespace("parallelly", quietly = TRUE)) {
            parallelly::availableCores()
        } else {
            parallel::detectCores(logical = TRUE)
        }
        if (length(available) != 1L || is.na(available) || !is.finite(available)) {
            available <- 1L
        }
        workers <- min(16L, max(1L, available - 2L))
    }
    if (!is.numeric(workers) || is.complex(workers) || length(workers) != 1L || is.na(workers) ||
            !is.finite(workers) || workers < 1 || workers != floor(workers) ||
            workers > .Machine$integer.max) {
        stopIllegalArgument("'workers' must be NULL or a positive integer",
            functionName = "testPackage", parameter = "workers", value = workers
        )
    }
    as.integer(max(1L, min(workers, numberOfFiles)))
}

.getPackageTestFiles <- function(testFileDirectory) {
    files <- list.files(file.path(testFileDirectory, "testthat"), pattern = "^test.*\\.[rR]$")
    # Local scheduling metadata takes precedence over the installed rpact metadata.
    descriptionFile <- file.path(dirname(normalizePath(testFileDirectory)), "DESCRIPTION")
    priorities <- NULL
    if (file.exists(descriptionFile)) {
        description <- read.dcf(descriptionFile)
        field <- "Config/testthat/start-first"
        if (field %in% colnames(description)) {
            priorities <- trimws(strsplit(description[1, field], ",", fixed = TRUE)[[1]])
        }
    }
    if (is.null(priorities)) {
        priorities <- utils::packageDescription("rpact")[["Config/testthat/start-first"]]
        if (!is.null(priorities)) priorities <- trimws(strsplit(priorities, ",", fixed = TRUE)[[1]])
    }
    contexts <- sub("\\.[rR]$", "", sub("^test[-_.]?", "", files))
    first <- unlist(lapply(priorities, function(pattern) {
        files[grepl(utils::glob2rx(pattern), contexts)]
    }), use.names = FALSE)
    unique(c(first, files))
}

# Count the same expectation events as the report, including expectations outside
# test_that() and repeated skips/errors that ListReporter summaries can omit.
.PackageTestCounter <- R6::R6Class("PackageTestCounter",
    inherit = testthat::Reporter,
    public = list(
        totals = c(failed = 0L, error = 0L, warning = 0L, skipped = 0L, passed = 0L),
        initialize = function(...) {
            super$initialize(...)
            self$capabilities$parallel_support <- TRUE
            self$capabilities$parallel_updates <- FALSE
        },
        add_result = function(context, test, result) {
            type <- if (inherits(result, "expectation_error")) {
                "error"
            } else if (.isTestExpectationBroken(result)) {
                "failed"
            } else if (.isTestExpectationWarning(result)) {
                "warning"
            } else if (.isTestExpectationSkip(result)) {
                "skipped"
            } else {
                "passed"
            }
            self$totals[[type]] <- self$totals[[type]] + 1L
        }
    )
)

# Windows R has an intermittent access violation when inspecting standard-stream
# pipes (R bug 19104, testthat issue 2284). File-backed standard streams avoid
# that path; each file returns reporter events through callr's result file.
.runPackageTestFile <- function(testDirectory, testFile, package) {
    events <- list()
    record <- function(method, args) {
        events[[length(events) + 1L]] <<- list(method = method, args = args)
    }
    collectorClass <- R6::R6Class("PackageTestFileReporter", inherit = testthat::Reporter,
        public = list(
            start_file = function(file) record("start_file", list(file)),
            end_file = function() record("end_file", list()),
            start_context = function(context) record("start_context", list(context)),
            end_context = function(context) record("end_context", list(context)),
            start_test = function(context, test) record("start_test", list(context, test)),
            end_test = function(context, test) record("end_test", list(context, test)),
            add_result = function(context, test, result) {
                if (inherits(result, "expectation_success")) result$srcref <- NULL
                record("add_result", list(context, test, result))
            }
        )
    )
    results <- testthat::test_file(file.path(testDirectory, testFile), package = package,
        load_package = "installed", reporter = collectorClass$new(), stop_on_failure = FALSE)
    list(events = events, results = results)
}

.runPackageTestFilesInProcesses <- function(test_dir, test_paths, test_package, load_package,
        reporter, parallel, stop_on_failure) {
    testDirectory <- normalizePath(test_dir, winslash = "/")
    workers <- .getPackageTestWorkers(getOption("Ncpus", 1L), length(test_paths))
    workDirectory <- tempfile("rpact-test-processes-")
    dir.create(workDirectory)
    active <- list()
    on.exit({
        for (job in active) if (job$process$is_alive()) job$process$kill()
        unlink(workDirectory, recursive = TRUE)
    }, add = TRUE)
    cat("Starting ", workers, " test processes with file-backed output (Windows R bug 19104 workaround).\n", sep = "")
    results <- vector("list", length(test_paths))
    nextFile <- 1L
    reporter$start_reporter()
    repeat {
        while (nextFile <= length(test_paths) && length(active) < workers) {
            index <- nextFile
            prefix <- file.path(workDirectory, as.character(index))
            config <- paste0(prefix, "-config")
            dir.create(config)
            stdout <- paste0(prefix, "-stdout.txt")
            stderr <- paste0(prefix, "-stderr.txt")
            process <- callr::r_bg(.runPackageTestFile,
                args = list(testDirectory = testDirectory, testFile = test_paths[index], package = test_package),
                libpath = .libPaths(), stdout = stdout, stderr = stderr, stdin = NULL,
                poll_connection = FALSE, supervise = TRUE, user_profile = FALSE,
                env = c(callr::rcmd_safe_env(), RPACT_CONFIG_DIR_LOAD = config, RPACT_CONFIG_DIR_SAVE = config))
            active[[as.character(index)]] <- list(process = process, stdout = stdout, stderr = stderr)
            nextFile <- nextFile + 1L
        }
        if (!length(active)) break
        for (key in names(active)) {
            job <- active[[key]]
            if (job$process$is_alive()) next
            index <- as.integer(key)
            for (path in c(job$stdout, job$stderr)) {
                if (file.exists(path)) {
                    output <- readLines(path, warn = FALSE)
                    if (length(output)) writeLines(paste0("> ", test_paths[index], ": ", output))
                }
            }
            value <- tryCatch(job$process$get_result(), error = function(error) {
                exitCode <- job$process$get_exit_status()
                if (is.null(exitCode)) exitCode <- "unknown"
                # Preserve the worker error class and file even without a configured factory.
                factory <- getOption("rpact.error.factory.runtime.issue.condition")
                if (!is.function(factory)) factory <- getOption("rpact.error.factory.condition")
                oldOptions <- options(rpact.error.factory.runtime.issue.condition =
                    function(message, ..., class = NULL) {
                        condition <- if (is.function(factory)) {
                            tryCatch(factory(message = message, ..., class = class),
                                error = function(e) NULL)
                        } else NULL
                        if (!inherits(condition, "condition")) condition <- simpleError(message, call = NULL)
                        class(condition) <- unique(c("rpact_test_process_error", class, class(condition)))
                        condition$test_file <- test_paths[index]
                        condition
                    })
                on.exit(options(oldOptions), add = TRUE)
                stopRuntimeIssue("Test process exited in file '", test_paths[index],
                    "' with exit code ", exitCode, ": ", conditionMessage(error),
                    functionName = ".runPackageTestFilesInProcesses",
                    constraint = "each test process must return its results successfully",
                    diagnosticId = "qa.test_process_failed",
                    context = list(testFile = test_paths[index], exitCode = exitCode,
                        cause = conditionMessage(error), workers = workers)
                )
            })
            for (event in value$events) do.call(reporter[[event$method]], event$args)
            results[[index]] <- unclass(value$results)
            active[[key]] <- NULL
        }
        # No standard-stream pipes are opened for polling or result transport.
        if (length(active)) active[[1]]$process$wait(timeout = 50)
    }
    reporter$end_reporter()
    structure(unlist(results, recursive = FALSE), class = "testthat_results")
}

# Runs in the isolated R CMD BATCH process, using its installed rpact namespace.
.runPackageUnitTests <- function(files, workers, reporterArguments, resultFile) {
    oldOptions <- options(
        Ncpus = workers, testthat.progress.max_fails = Inf,
        device = function(...) grDevices::pdf(file = NULL, ...)
    )
    on.exit(options(oldOptions), add = TRUE)
    configPath <- tempfile("rpact-test-config-")
    dir.create(configPath)
    on.exit(unlink(configPath, recursive = TRUE), add = TRUE)
    variables <- c(
        "RPACT_CONFIG_DIR_LOAD", "RPACT_CONFIG_DIR_SAVE", "RPACT_LOG_LEVEL",
        "TESTTHAT_PARALLEL", "TESTTHAT_CPUS", "TESTTHAT_IS_CHECKING"
    )
    oldEnvironment <- Sys.getenv(variables, unset = NA_character_)
    on.exit(
        {
            Sys.unsetenv(variables[is.na(oldEnvironment)])
            if (any(!is.na(oldEnvironment))) {
                do.call(Sys.setenv, as.list(oldEnvironment[!is.na(oldEnvironment)]))
            }
        },
        add = TRUE
    )
    Sys.setenv(
        RPACT_CONFIG_DIR_LOAD = configPath, RPACT_CONFIG_DIR_SAVE = configPath,
        TESTTHAT_PARALLEL = if (workers > 1L) "TRUE" else "FALSE",
        TESTTHAT_CPUS = workers, TESTTHAT_IS_CHECKING = "true"
    )
    # Implicit plots must not make parallel workers write the same Rplots.pdf.
    # Helpers are sourced in both serial and parallel installed-package sessions.
    graphicsHelper <- tempfile("helper-rpact-qa-graphics-", tmpdir = "testthat", fileext = ".R")
    writeLines(c(
        "options(device = function(...) grDevices::pdf(file = NULL, ...))",
        "local({",
        "    config <- file.path(Sys.getenv('RPACT_CONFIG_DIR_SAVE'), paste0('process-', Sys.getpid()))",
        "    dir.create(config, recursive = TRUE, showWarnings = FALSE)",
        "    Sys.setenv(RPACT_CONFIG_DIR_LOAD = config, RPACT_CONFIG_DIR_SAVE = config)",
        "})"
    ), graphicsHelper)
    on.exit(unlink(graphicsHelper), add = TRUE)
    # This guarded internal interface permits local file priorities while loading
    # the installed package in every worker (test_dir reads installed priorities).
    testFiles <- if (.Platform$OS.type == "windows" && workers > 1L) {
        .runPackageTestFilesInProcesses
    } else {
        getFromNamespace("test_files", "testthat")
    }
    required <- c(
        "test_dir", "test_paths", "test_package", "load_package",
        "reporter", "parallel", "stop_on_failure"
    )
    if (!all(required %in% names(formals(testFiles)))) {
        stopRuntimeIssue("Unsupported testthat file-runner interface; cannot run package tests safely.",
            functionName = ".runPackageUnitTests",
            constraint = "the testthat runner must support all required arguments",
            diagnosticId = "qa.test_runner_interface_unsupported",
            context = list(requiredArguments = required, availableArguments = names(formals(testFiles)),
                testthatVersion = as.character(utils::packageVersion("testthat")))
        )
    }
    reportType <- reporterArguments$outputSize
    reporter <- if (reportType == "Rout") {
        testthat::CheckReporter$new()
    } else {
        do.call(MarkdownReporter$new, reporterArguments)
    }
    if (workers > 1L && !isTRUE(reporter$capabilities$parallel_support)) {
        stopConflictingArguments("The testthat reporter does not support parallel tests; use workers = 1.",
            functionName = ".runPackageUnitTests", parameter = "workers", value = workers,
            relatedParameter = "outputSize", relatedValue = reportType,
            constraint = "workers must be 1 when the reporter does not support parallel tests",
            diagnosticId = "qa.reporter_parallel_unsupported",
            context = list(reporterClass = class(reporter), parallelSupport = reporter$capabilities$parallel_support)
        )
    }
    cat("R: ", R.version.string, "\nR home: ", R.home(),
        "\nrpact: ", as.character(utils::packageVersion("rpact")),
        "\nPackage path: ", find.package("rpact"),
        "\ntestthat: ", as.character(utils::packageVersion("testthat")),
        "\nTest files: ", length(files), "\nWorkers: ", workers, "\n",
        sep = ""
    )
    counter <- .PackageTestCounter$new()
    executionReporter <- testthat::MultiReporter$new(reporters = list(reporter, counter))
    started <- proc.time()[["elapsed"]]
    # The worker queue starts before start_reporter(). Run basic R tests first,
    # so synchronous R CMD tests cannot stall communication with live workers.
    if (reportType != "Rout") reporter$prepareBasicPackageTests()
    executionError <- NULL
    failedTestFile <- NULL
    # A missing final checkpoint must never be mistaken for a successful run.
    saveRDS(list(completed = FALSE, workers = workers, totals = counter$totals,
        executionError = "The test process did not write a final completion record."), resultFile)
    results <- tryCatch(
        testFiles(
            test_dir = "testthat", test_paths = files, test_package = "rpact",
            load_package = "installed", reporter = executionReporter, parallel = workers > 1L,
            stop_on_failure = FALSE
        ),
        error = function(error) {
            executionError <<- conditionMessage(error)
            failedTestFile <<- error$test_file
            NULL
        }
    )
    if (!is.null(executionError)) {
        cat("\nTest execution aborted:\n", executionError, "\n", sep = "")
        if (reportType != "Rout" && !reporter$reportFinalized) {
            reporter$executionError <- executionError
            tryCatch(reporter$end_reporter(), error = function(error) {
                cat("Failed to finalize the partial report: ", conditionMessage(error), "\n", sep = "")
            })
        }
    }
    totals <- counter$totals
    timings <- if (reportType == "Rout") {
        c(tests = proc.time()[["elapsed"]] - started, basicPackages = 0, html = 0, pdf = 0)
    } else {
        reporter$timings
    }
    # Include worker startup and result collection in the unit test timing.
    timings[["tests"]] <- max(0, proc.time()[["elapsed"]] - started -
        sum(timings[c("basicPackages", "html", "pdf")]))
    resultMessage <- sprintf(
        "[ FAIL %d | WARN %d | SKIP %d | PASS %d ]",
        totals[["failed"]] + totals[["error"]], totals[["warning"]],
        totals[["skipped"]], totals[["passed"]]
    )
    saveRDS(list(
        workers = workers, timings = timings, totals = totals,
        resultMessage = resultMessage, completed = is.null(executionError),
        executionError = executionError, failedTestFile = failedTestFile
    ), resultFile)
    cat("\n", resultMessage, "\n", sep = "")
    for (name in names(timings)) cat(sprintf("Runtime %s: %.2f seconds\n", name, timings[[name]]))
    if (!is.null(executionError)) {
        stopRuntimeIssue("Test execution aborted; the counts above are partial results.",
            functionName = ".runPackageUnitTests",
            constraint = "test execution must complete before final counts can be reported",
            diagnosticId = "qa.test_execution_aborted",
            context = list(cause = executionError, testFile = failedTestFile,
                totals = totals, resultFile = resultFile, workers = workers)
        )
    }
    if (totals[["failed"]] > 0 || totals[["error"]] > 0) {
        stopRuntimeIssue("Test failures.",
            functionName = ".runPackageUnitTests",
            constraint = "test results must contain no failures or errors",
            diagnosticId = "qa.test_expectations_failed",
            context = list(totals = totals, resultFile = resultFile, workers = workers)
        )
    }
    invisible(results)
}

#' @title
#' Test and Validate the rpact Package Installation
#'
#' @description
#' This function ensures the correct installation of the \code{rpact} package by performing
#' various tests. It supports a comprehensive validation process, essential for GxP compliance
#' and other regulatory requirements.
#'
#' @param outDir The absolute path to the output directory where all test results will be saved.
#'     By default, the current working directory is used.
#' @param completeUnitTestSetEnabled If \code{TRUE} (default), all existing unit tests will
#'     be executed; if \code{FALSE}, only a subset of tests is run.
#' @param connection A \code{list} allowing owners of the rpact validation documentation
#'     to provide \code{token} and \code{secret} credentials for full access to unit tests,
#'     enabling them to meet regulatory requirements (see \href{https://www.rpact.com}{www.rpact.com} for more information).
#' @param testFileDirectory An optional path pointing to a local directory containing test files.
#' @param downloadTestsOnly If \code{TRUE}, the unit test files are only downloaded and not executed.
#'     Default is \code{FALSE}.
#' @param addWarningDetailsToReport If \code{TRUE}, additional warning details are included in the test report.
#'     Default is \code{TRUE}.
#' @param reportType The type of report to generate.
#'     Can be \code{"compact"}, \code{"detailed"}, or \code{"Rout"}.
#' @param testInstalledBasicPackages If \code{TRUE}, tests for installed
#'     basic R packages are included, default is \code{TRUE}.
#'     For more information, see \code{\link[tools]{testInstalledBasic}}.
#' @param scope The scope of the basic R package tests to run. Can be \code{"basic"},
#'     \code{"devel"}, \code{"both"}, \code{"internet"}, or \code{"all"}. Default is \code{"basic"}.
#'     For more information, see \code{\link[tools]{testInstalledBasic}}.
#'     Only available if \code{testInstalledBasicPackages = TRUE}.
#' @param openHtmlReport If \code{TRUE}, the HTML report is opened after the tests are completed, default is \code{TRUE}.
#' @param keepSourceFiles If \code{TRUE}, the source files are kept after the tests are completed.
#'     A copy of them can be found in the subdirectory \code{src}.
#' @param reportFileBaseName The base name for the report files (without path or extension).
#' @param metaData A named \code{list} containing additional metadata to be included in the test report.
#'    This can include information such as \code{company}, \code{container name}, \code{host name}, etc.
#'    Default is an empty list.
#'    For more information, see \code{\link{writeKeyValueFile}} and \code{\link{readKeyValueFile}}.
#' @param metaDataFile An optional path to a text file containing metadata: one entry per line in the form \code{KEY=VALUE}.
#'    For more information, see \code{\link{writeKeyValueFile}} and \code{\link{readKeyValueFile}}.
#' @param workers Number of parallel R test processes. The default \code{NULL} uses
#'     the available logical CPUs minus two, with a minimum of one and a maximum of
#'     16, limited to the number of test files. Uses \code{parallelly::availableCores()}
#'     when installed (including resource limits), otherwise \code{parallel::detectCores()}.
#'     An explicit positive integer overrides the automatic selection; use \code{1}
#'     for serial execution. Parallel tests are supported on Windows, Linux, and macOS.
#' @inheritParams param_three_dots
#'
#' @details
#' This function is integral to the installation qualification (IQ) process of the \code{rpact} package,
#' ensuring it meets quality standards and functions as expected. A directory named \code{rpact-tests}
#' is created within the specified output directory, where all test files are downloaded from a secure
#' resource and executed. Results are saved in the file \code{testthat.Rout}, located in the
#' \code{rpact-tests} directory.
#'
#' Unit test files are distributed across separate R processes.
#' On Windows, standard output and error use files to avoid the pipe-related access
#' violation in R bug 19104. Linux and macOS use the testthat parallel runner. Scheduling priorities
#' are read from the \code{DESCRIPTION} above a local test directory, when present,
#' and otherwise from the installed rpact package. Report sections follow file
#' completion order. Personal rpact configuration files are isolated for the run.
#' R basic package tests run in a separate R process before parallel workers start,
#' using a temporary output directory. If execution aborts, partial results are marked
#' as incomplete and the diagnostic log path is printed; use \code{workers = 1} to
#' investigate persistent parallel-worker failures.
#' The returned object includes \code{workers} and \code{timings}, with elapsed
#' seconds for unit tests (including worker startup), R basic package tests,
#' HTML rendering, and PDF rendering.
#'
#' Installation qualification is a critical step in the validation process. Without successful IQ,
#' the package cannot be considered fully validated. To gain access to the full set of unit tests,
#' users must provide \code{token} and \code{secret} credentials, which are distributed to
#' members of the rpact user group as part of the validation documentation.
#' For more information, see vignette \href{https://www.rpact.org/vignettes/utilities/rpact_installation_qualification/}{rpact_installation_qualification}.
#'
#' @return Invisibly returns an \code{\link[=InstallationQualificationResult]{InstallationQualificationResult}}) object.
#'
#' @references For more information, please visit: <https://www.rpact.org/vignettes/utilities/rpact_installation_qualification/>
#'
#' @examples
#' \dontrun{
#' # Set the output directory
#' setwd("/path/to/output")
#'
#' # Basic usage
#' testPackage()
#'
#' # Perform all unit tests with access credentials
#' testPackage(
#'     connection = list(
#'         token = "your_token_here",
#'         secret = "your_secret_here"
#'     )
#' )
#'
#' # Download test files without executing them
#' testPackage(downloadTestsOnly = TRUE)
#' }
#'
#' @export
#'
testPackage <- function(
        outDir = ".",
        ...,
        completeUnitTestSetEnabled = TRUE,
        connection = list(token = NULL, secret = NULL),
        testFileDirectory = NA_character_,
        downloadTestsOnly = FALSE,
        addWarningDetailsToReport = TRUE,
        reportType = c("compact", "detailed", "Rout"),
        testInstalledBasicPackages = TRUE,
        scope = c("basic", "devel", "both", "internet", "all"),
        openHtmlReport = TRUE,
        keepSourceFiles = FALSE,
        reportFileBaseName = "rpact_test_result_report",
        metaData = list(),
        metaDataFile = NULL,
        workers = NULL) {
    .assertTestthatIsInstalled()
    workers <- .getPackageTestWorkers(workers)
    .assertIsSingleCharacter(outDir, "outDir", naAllowed = FALSE)
    .assertIsSingleLogical(completeUnitTestSetEnabled, "completeUnitTestSetEnabled", naAllowed = FALSE)
    .assertIsSingleCharacter(testFileDirectory, "testFileDirectory", naAllowed = TRUE)
    .assertIsSingleCharacter(reportFileBaseName, "reportFileBaseName", naAllowed = FALSE)
    .assertIsSingleLogical(downloadTestsOnly, "downloadTestsOnly", naAllowed = FALSE)
    .assertIsSingleLogical(addWarningDetailsToReport, "addWarningDetailsToReport", naAllowed = FALSE)
    .assertIsSingleLogical(testInstalledBasicPackages, "testInstalledBasicPackages", naAllowed = FALSE)
    .assertIsSingleLogical(openHtmlReport, "openHtmlReport", naAllowed = FALSE)
    .assertIsSingleLogical(keepSourceFiles, "keepSourceFiles", naAllowed = FALSE)

    token <- .getConnectionArgument(connection, "token")
    secret <- .getConnectionArgument(connection, "secret")
    credentialsAvailable <- (!is.null(token) && !is.null(secret) &&
        length(token) == 1 && length(secret) == 1 &&
        !is.na(token) && !is.na(secret))

    downloadOnlyModeEnabled <- is.na(testFileDirectory) &&
        isTRUE(downloadTestsOnly) && isTRUE(credentialsAvailable)

    executionMode <- .getTestExecutionMode(testFileDirectory, credentialsAvailable, downloadOnlyModeEnabled)

    pkgName <- "rpact"
    testFileTargetDirectory <- .getTestFileTargetDirectory(outDir, testFileDirectory, executionMode, pkgName)
    if (!is.null(metaData) && length(metaData) > 0) {
        if (is.null(metaDataFile)) {
            metaDataFile <- file.path(testFileTargetDirectory, "meta_data.txt")
        }
        writeKeyValueFile(
            keyValueList = metaData,
            filePath = metaDataFile,
            safeKeyCheck = FALSE
        )
    }

    if (!is.null(metaDataFile)) {
        metaData <- readKeyValueFile(metaDataFile)
    }

    reportType <- match.arg(reportType)
    scope <- match.arg(scope)

    if (grepl("/|:|\\\\", reportFileBaseName)) {
        stopIllegalArgument("'reportFileBaseName' (", dQuote(reportFileBaseName), ") ", 
            "must not contain path separators or drive specifiers",
            functionName = "testPackage",
            parameter = "reportFileBaseName", value = reportFileBaseName
        )
    }

    if (!dir.exists(outDir)) {
        stopIllegalArgument("test output directory ", .vQuote(outDir), " does not exist",
            functionName = "testPackage",
            parameter = "outDir", value = outDir
        )
    }

    if (outDir != "." && !grepl("^([a-zA-Z]{1,1}:)?(/|\\\\)", outDir, ignore.case = TRUE)) {
        stopIllegalArgument("test output directory ", .vQuote(outDir), " must be an absolute path",
            functionName = "testPackage",
            parameter = "outDir", value = outDir
        )
    }

    if (outDir == ".") {
        outDir <- getwd()
    }

    if (!is.na(testFileDirectory) && !dir.exists(testFileDirectory)) {
        stopIllegalArgument("test file directory ", .vQuote(testFileDirectory), " does not exist",
            functionName = "testPackage",
            parameter = "testFileDirectory", value = testFileDirectory
        )
    }

    inheritedVariables <- c("NOT_CRAN", "LANGUAGE", "RPACT_COMPLETE_UNIT_TEST_SET_ENABLED", "RPACT_LOG_LEVEL")
    inheritedEnvironment <- Sys.getenv(inheritedVariables, unset = NA_character_)
    on.exit(
        {
            Sys.unsetenv(inheritedVariables[is.na(inheritedEnvironment)])
            if (any(!is.na(inheritedEnvironment))) {
                do.call(Sys.setenv, as.list(inheritedEnvironment[!is.na(inheritedEnvironment)]))
            }
        },
        add = TRUE
    )
    if (isTRUE(addWarningDetailsToReport)) Sys.setenv(NOT_CRAN = "true")

    startTime <- Sys.time()

    if (!is.na(testFileDirectory) && !dir.exists(file.path(testFileDirectory, "testthat"))) {
        warnArgumentIgnored("'testFileDirectory' (", testFileDirectory, ") will be ignored ",
            "because it does not contain a 'testthat' subfolder",
            parameter = "testFileDirectory",
            value = testFileDirectory,
            diagnosticId = "qa.test_directory_ignored"
        )
        testFileDirectory <- NA_character_
    }

    Sys.setenv("LANGUAGE" = "EN")
    Sys.setenv("RPACT_COMPLETE_UNIT_TEST_SET_ENABLED" = completeUnitTestSetEnabled)

    debug <- .getOptionalArgument("debug", ...)
    if (!is.null(debug) && length(debug) == 1 && isTRUE(as.logical(debug))) {
        setLogLevel(C_LOG_LEVEL_DEBUG)
    } else {
        setLogLevel(C_LOG_LEVEL_DISABLED)
    }

    author <- NA_character_
    if (credentialsAvailable) {
        author <- .getReportAuthor(
            token, secret,
            ifelse(downloadTestsOnly, "downloadTestsOnly", "testPackage")
        )
    }
    if (is.null(author) || length(author) != 1 || is.na(author)) {
        author <- .getOptionalArgument("author", ...)
    }
    if (is.null(author) || length(author) != 1 || is.na(author) || nchar(trimws(author)) == 0) {
        author <- "RPACT"
    }

    if (!is.na(testFileDirectory)) {
        if (credentialsAvailable) {
            warnArgumentIgnored("The connection token and secret will be ignored ",
                "because 'testFileDirectory' is defined",
                diagnosticId = "qa.credentials_ignored_for_local_tests"
            )
        }
        credentialsAvailable <- TRUE
    }

    if (executionMode == "downloadOnly") {
        if (testInstalledBasicPackages) {
            warnResultUnavailable("The test of installed R basic packages is not supported in 'downloadOnly' mode",
                diagnosticId = "qa.basic_tests_download_only_unsupported"
            )
        }
        testInstalledBasicPackages <- FALSE
    }

    if (executionMode == "default") {
        cat("Run a small subset of the available rpact unit tests. Please wait...\n")
        cat("This is just a quick test for demonstration purposes.\n")
        cat("The test will take about a minute.\n")
    } else if (executionMode == "downloadOnly") {
        cat("The unit test files are only downloaded and not executed.\n")
    } else if (executionMode %in% c("downloadAndRunTests", "runTestsInTestFileDirectory")) {
        if (completeUnitTestSetEnabled) {
            cat("Run all tests. Please wait...\n")
            cat("Test runtime depends on the worker count and the test workload.\n")
            cat("Execution of all available unit tests started at ",
                format(startTime, "%H:%M (%d-%B-%Y)"), "\n",
                sep = ""
            )
        } else {
            cat("Run a small subset of the available rpact unit tests. Please wait...\n")
            cat("This is just a quick test, i.e., all time consuming tests will be skipped.\n")
            cat("The test will take about a minute.\n")
        }
    }

    if (!is.null(metaData) && length(metaData) > 0) {
        cat("The following additional metadata will be included in the test report:\n")
        for (key in names(metaData)) {
            cat("- ", key, ": ", metaData[[key]], "\n", sep = "")
        }
    }

    oldResultFiles <- unique(c(
        .getExistingTestResultFiles(outDir),
        .getExistingTestResultFiles(testFileTargetDirectory)
    ))
    for (oldResultFile in oldResultFiles) {
        if (file.exists(oldResultFile)) {
            file.remove(oldResultFile)
        }
    }

    if (executionMode %in% c("downloadAndRunTests", "downloadOnly")) {
        .downloadUnitTests(
            testFileTargetDirectory = testFileTargetDirectory,
            token = token,
            secret = secret,
            method = .getConnectionArgument(connection, "method"),
            mode = .getConnectionArgument(connection, "mode"),
            cacheOK = .getConnectionArgument(connection, "cacheEnabled"),
            extra = .getConnectionArgument(connection, "extra"),
            cleanOldFiles = .getConnectionArgument(connection, "cleanOldFiles"),
            connectionType = .getConnectionArgument(connection, "connectionType")
        )
        if (downloadOnlyModeEnabled) {
            return(invisible(NULL))
        }
    }

    files <- .getPackageTestFiles(testFileTargetDirectory)
    if (!length(files)) {
        stopRuntimeIssue("No test files found in directory 'testthat'",
            functionName = "testPackage", parameter = "testFileDirectory", value = testFileDirectory,
            constraint = "the testthat directory must contain test files",
            diagnosticId = "qa.test_files_missing",
            context = list(testDirectory = file.path(testFileTargetDirectory, "testthat"),
                executionMode = executionMode)
        )
    }
    
    workers <- .getPackageTestWorkers(workers, length(files))
    minNumberOfExpectedTests <- .getMinNumberOfExpectedTests()
    cat("Expected number of unit tests in the complete set: ", minNumberOfExpectedTests, "\n", sep = "")
    cat("Number of test files: ", length(files), "\n", sep = "")
    cat("Number of workers: ", workers, "\n", sep = "")
    testFileTargetDirectory <- normalizePath(testFileTargetDirectory, winslash = "/")
    resultFile <- tempfile("rpact-test-run-", tmpdir = testFileTargetDirectory, fileext = ".rds")
    on.exit(unlink(resultFile), add = TRUE)
    markdownReportFileName <- if (reportType == "Rout") NULL else paste0(reportFileBaseName, ".md")
    testthatMainFile <- file.path(testFileTargetDirectory, "testthat.R")
    if (file.exists(testthatMainFile)) {
        testthatMainFileBak <- tempfile("testthat-backup-", tmpdir = testFileTargetDirectory)
        if (!file.rename(testthatMainFile, testthatMainFileBak)) {
            stopRuntimeIssue("Cannot back up testthat.R",
                functionName = "testPackage",
                constraint = "the existing testthat.R must be backed up before running tests",
                diagnosticId = "qa.test_entry_point_backup_failed",
                context = list(sourceFile = testthatMainFile, backupFile = testthatMainFileBak)
            )
        }
        on.exit(
            {
                unlink(testthatMainFile)
                file.rename(testthatMainFileBak, testthatMainFile)
            },
            add = TRUE
        )
    } else {
        on.exit(unlink(testthatMainFile), add = TRUE)
    }
    reporterArguments <- list(
        outputFile = file.path(testFileTargetDirectory, markdownReportFileName),
        outputSize = reportType, openHtmlReport = openHtmlReport,
        keepSourceFiles = keepSourceFiles, testInstalledBasicPackages = testInstalledBasicPackages,
        addWarningDetailsToReport = addWarningDetailsToReport,
        author = author, scope = scope, metaDataFile = metaDataFile
    )
    # Serialize R expressions instead of interpolating paths, names, or metadata.
    commands <- c(
        deparse(substitute(.libPaths(paths), list(paths = .libPaths())), width.cutoff = 500L),
        deparse(
            substitute(rpact:::.runPackageUnitTests(
                files = testFiles, workers = count, reporterArguments = arguments, resultFile = path
            ), list(testFiles = files, count = workers, arguments = reporterArguments, path = resultFile)),
            width.cutoff = 500L
        )
    )
    writeLines(commands, testthatMainFile)
    # Isolate configuration before rpact is loaded in the batch process.
    configPath <- tempfile("rpact-test-config-")
    dir.create(configPath)
    on.exit(unlink(configPath, recursive = TRUE), add = TRUE)
    variables <- c("RPACT_CONFIG_DIR_LOAD", "RPACT_CONFIG_DIR_SAVE")
    oldEnvironment <- Sys.getenv(variables, unset = NA_character_)
    on.exit(
        {
            Sys.unsetenv(variables[is.na(oldEnvironment)])
            if (any(!is.na(oldEnvironment))) {
                do.call(Sys.setenv, as.list(oldEnvironment[!is.na(oldEnvironment)]))
            }
        },
        add = TRUE
    )
    Sys.setenv(RPACT_CONFIG_DIR_LOAD = configPath, RPACT_CONFIG_DIR_SAVE = configPath)
    executionStatus <- .testInstalledPackage(
        testFileDirectory = testFileTargetDirectory, pkgName = pkgName
    )
    # Qualification persistence belongs to the caller's configuration, not the
    # temporary configuration used only by the test processes.
    Sys.unsetenv(variables[is.na(oldEnvironment)])
    if (any(!is.na(oldEnvironment))) {
        do.call(Sys.setenv, as.list(oldEnvironment[!is.na(oldEnvironment)]))
    }
    runDetails <- if (file.exists(resultFile)) readRDS(resultFile) else NULL

    newResultFiles <- .getExistingTestResultFiles(outDir)
    if (length(newResultFiles) == 0) {
        newResultFiles <- .getExistingTestResultFiles(testFileTargetDirectory)
    }

    resultDir <- file.path(outDir, paste0(pkgName, "-tests"))
    if (!dir.exists(resultDir)) {
        dir.create(resultDir)
    }
    if (length(newResultFiles) > 0) {
        file.copy(newResultFiles, resultDir)
    }

    endTime <- Sys.time()

    if (completeUnitTestSetEnabled) {
        cat("Test execution ended at ",
            format(endTime, "%H:%M (%d-%B-%Y)"), "\n",
            sep = ""
        )
    }

    cat("Total runtime for testing: ", .getRuntimeString(startTime,
        endTime = endTime, runtimeUnits = "auto"
    ), ".\n", sep = "")

    reportFileNames <- .getTestReportFileNames(
        reportType, markdownReportFileName,
        keepSourceFiles, testFileTargetDirectory, resultDir, reportFileBaseName
    )

    totalNumberOfTests <- NA_integer_
    numberOfFailedTests <- 0
    numberOfSkippedTests <- 0
    executionCompleted <- !is.null(runDetails) && isTRUE(runDetails$completed)
    executionError <- if (is.null(runDetails)) NULL else runDetails$executionError
    resultOuputFile <- if (file.exists(file.path(resultDir, "testthat.Rout.fail"))) {
        "testthat.Rout.fail"
    } else {
        "testthat.Rout"
    }
    inputFileName <- file.path(resultDir, resultOuputFile)
    resultMessage <- NA_character_
    statusMessage <- NA_character_
    status <- "incomplete"
    if (file.exists(inputFileName) && identical(executionStatus, 0L) && executionCompleted) {
        fileContent <- base::readChar(inputFileName, file.info(inputFileName)$size)
        totalNumberOfTests <- if (is.null(runDetails)) {
            .getTestthatResultNumberOfTests(fileContent)
        } else {
            sum(runDetails$totals)
        }
        if (completeUnitTestSetEnabled && minNumberOfExpectedTests <= totalNumberOfTests &&
                identical(executionStatus, 0L)) {
            statusMessage <- "Installation qualification successfully completed"
            status <- "success"
            .setSystemIdentifier()
            cat("All unit tests were completed successfully, i.e., the installation \n",
                "qualification was successful.\n",
                sep = ""
            )
        } else {
            cat("Unit tests were completed successfully.\n")
            if (totalNumberOfTests < minNumberOfExpectedTests) {
                cat("Only a subset of the ", minNumberOfExpectedTests,
                    " available rpact unit tests were executed.\n",
                    sep = ""
                )
                cat("This means the package is not yet validated for use in GxP-compliant settings.\n")
                statusMessage <- "Installation qualification not completed (only a subset of tests executed)"
            }
        }
        cat("\n")

        cat("Results:\n")
        resultMessage <- if (is.null(runDetails$resultMessage)) .getTestthatResultLine(fileContent) else runDetails$resultMessage
        cat(resultMessage, "\n")
        cat("\n")
        .showResultsWereWrittenToDirectoryMessage(resultDir, reportFileNames, resultOuputFile)
        numberOfSkippedTests <- if (is.null(runDetails)) {
            .getTestthatResultNumberOfSkippedTests(fileContent)
        } else {
            unname(runDetails$totals[["skipped"]])
        }
        if (numberOfSkippedTests > 0) {
            cat("-------------------------------------------------------------------------\n")
            cat("Note that ", numberOfSkippedTests, " tests were skipped; ",
                "a possible reason may be that expected \n",
                "error messages could not be tested ",
                "because of local translation.\n",
                sep = ""
            )
        }
        cat("-------------------------------------------------------------------------\n")
        cat("Please visit www.rpact.org/iq to learn how to use rpact on FDA/GxP-compliant \n",
            "validated corporate computer systems and how to get a copy of the formal \n",
            "validation documentation that is customized and licensed for exclusive use \n",
            "by your organization, e.g., to fulfill regulatory requirements.\n",
            sep = ""
        )
    } else {
        status <- "failed"
        if (file.exists(inputFileName)) {
            fileContent <- base::readChar(inputFileName, file.info(inputFileName)$size)
            numberOfFailedTests <- if (is.null(runDetails)) {
                totalNumberOfTests <- .getTestthatResultNumberOfTests(fileContent)
                numberOfSkippedTests <- .getTestthatResultNumberOfSkippedTests(fileContent)
                .getTestthatResultNumberOfFailures(fileContent)
            } else {
                totalNumberOfTests <- sum(runDetails$totals)
                numberOfSkippedTests <- unname(runDetails$totals[["skipped"]])
                unname(runDetails$totals[["failed"]] + runDetails$totals[["error"]])
            }
            if (completeUnitTestSetEnabled && executionCompleted) {
                if (numberOfFailedTests > 0) {
                    cat(numberOfFailedTests,
                        " unit test", ifelse(numberOfFailedTests == 1, "", "s"),
                        " failed, i.e., the installation qualification was not successful.\n",
                        sep = ""
                    )
                    statusMessage <- paste0("Installation qualification was not successful (", numberOfFailedTests, " failures)")
                } else {
                    cat("Unexpected failures, i.e., the installation qualification was not successful.\n")
                    statusMessage <- "Installation qualification was not successful (unexpected failures)"
                }
            } else if (executionCompleted) {
                cat(numberOfFailedTests, " unit test",
                    ifelse(numberOfFailedTests == 1, "", "s"), " failed :(\n",
                    sep = ""
                )
                statusMessage <- paste0("Installation qualification was not successful (subset with ", numberOfFailedTests, " failures)")
            }
            if (!executionCompleted) {
                statusMessage <- "Installation qualification was not successful (test execution aborted)"
                cat("The test execution did not complete. Results below are partial.\n")
                if (!is.null(executionError)) cat("Execution error: ", executionError, "\n", sep = "")
                cat("Results received: ", totalNumberOfTests, "; expected complete test set (META): ",
                    minNumberOfExpectedTests, ".\n", sep = "")
                if (workers > 1L) {
                    cat("If the problem persists, try testPackage(..., workers = 1) for serial execution.\n")
                }
            }
            cat("Results:\n")
            resultMessage <- if (is.null(runDetails$resultMessage)) .getTestthatResultLine(fileContent) else runDetails$resultMessage
            cat(resultMessage, "\n")
            .showResultsWereWrittenToDirectoryMessage(resultDir, reportFileNames, resultOuputFile)
        } else {
            cat("No test results found in directory ", .vQuote(resultDir), "\n", sep = "")
            statusMessage <- "Installation qualification was not successful (no test results found)"
            .showResultsWereWrittenToDirectoryMessage(resultDir, reportFileNames, resultOuputFile)
        }
    }
    if (!credentialsAvailable) {
        cat("-------------------------------------------------------------------------\n")
        cat("Note that only a small subset of all available unit tests were executed.\n")
        cat("You need a personal 'token' and 'secret' to perform all unit tests.\n")
        cat("You can find these data in the appendix of the validation documentation \n")
        cat("licensed for your company/organization or at https://connect.rpact.com.\n")
    } else if (!completeUnitTestSetEnabled) {
        cat("Note that only a small subset of all available unit tests were executed.\n")
        cat("Use testPackage(completeUnitTestSetEnabled = TRUE) to perform all unit tests.\n")
    }

    result <- list(
        completeUnitTestSetEnabled = completeUnitTestSetEnabled,
        testFileDirectory = testFileDirectory,
        testFileTargetDirectory = testFileTargetDirectory,
        reportType = reportType,
        executionMode = executionMode,
        scope = scope,
        resultDir = resultDir,
        resultOuputFile = resultOuputFile,
        logFile = normalizePath(inputFileName, winslash = "/", mustWork = FALSE),
        executionCompleted = executionCompleted,
        executionError = executionError,
        workers = workers,
        timings = if (is.null(runDetails)) NULL else runDetails$timings,
        reportFileNames = reportFileNames,
        minNumberOfExpectedTests = minNumberOfExpectedTests,
        totalNumberOfTests = totalNumberOfTests,
        numberOfFailedTests = numberOfFailedTests,
        numberOfSkippedTests = numberOfSkippedTests,
        resultMessage = resultMessage,
        statusMessage = statusMessage,
        status = status
    )

    result <- structure(result, class = "InstallationQualificationResult")

    return(invisible(result))
}

.testInstalledPackage <- function(testFileDirectory, ..., pkgName = "rpact", Ropts = character()) {
    .assertIsSingleCharacter(testFileDirectory, "testFileDirectory", naAllowed = FALSE)
    if (!dir.exists(testFileDirectory)) {
        stopIllegalArgument("'testFileDirectory' (", testFileDirectory, ") does not exist",
            functionName = ".testInstalledPackage",
            parameter = "testFileDirectory", value = testFileDirectory
        )
    }

    workingDirectoryBefore <- getwd()
    on.exit(setwd(workingDirectoryBefore))

    setwd(testFileDirectory)
    message(gettextf("Running specific tests for package %s", sQuote(pkgName)), domain = NA)
    testFiles <- dir(".", pattern = "\\.R$")
    for (testFile in testFiles) {
        message(gettextf("  Running %s", sQuote(testFile)), domain = NA)
        outfile <- paste0(testFile, "out")
        cmd <- paste(
            shQuote(file.path(R.home("bin"), "R")),
            "CMD BATCH",
            "--vanilla",
            "--no-timing",
            Ropts,
            shQuote(testFile),
            shQuote(outfile)
        )
        cmd <- if (.Platform$OS.type == "windows") paste(cmd, "LANGUAGE=C") else paste("LANGUAGE=C", cmd)
        res <- base::system(cmd)
        if (res) {
            file.rename(outfile, paste(outfile, "fail", sep = "."))
            return(invisible(1L))
        }

        savefile <- paste(outfile, "save", sep = ".")
        if (file.exists(savefile)) {
            message(
                gettextf(
                    "  comparing %s to %s ...",
                    sQuote(outfile), sQuote(savefile)
                ),
                appendLF = FALSE, domain = NA
            )
            res <- Rdiff(outfile, savefile)
            if (!res) message(" OK")
        }
    }

    return(invisible(0L))
}

.isCompleteUnitTestSetEnabled <- function() {
    completeUnitTestSetEnabled <- as.logical(Sys.getenv("RPACT_COMPLETE_UNIT_TEST_SET_ENABLED"))
    if (is.na(completeUnitTestSetEnabled)) {
        completeUnitTestSetEnabled <- FALSE
        Sys.setenv("RPACT_COMPLETE_UNIT_TEST_SET_ENABLED" = completeUnitTestSetEnabled)
    }
    return(isTRUE(completeUnitTestSetEnabled))
}

.enableCompleteUnitTestSet <- function() {
    Sys.setenv("RPACT_COMPLETE_UNIT_TEST_SET_ENABLED" = TRUE)
}

.getMinNumberOfExpectedTests <- function() {
    minNumberOfExpectedTestsDefault <- 30000
    tryCatch(
        {
            metaFilePath <- system.file("tests", "META", package = "rpact")
            if (identical(metaFilePath, "") || !file.exists(metaFilePath)) {
                metaFilePath <- file.path(getwd(), "inst", "tests", "META")
            }
            if (file.exists(metaFilePath)) {
                metaLines <- trimws(.readLinesFromFile(metaFilePath))
                metaLines <- metaLines[metaLines != ""]
                if (length(metaLines) > 0) {
                    minNumberOfExpectedTests <- as.integer(metaLines[1])
                    if (length(minNumberOfExpectedTests) == 1 &&
                            !is.na(minNumberOfExpectedTests) &&
                            minNumberOfExpectedTests > minNumberOfExpectedTestsDefault) {
                        return(minNumberOfExpectedTests)
                    }
                }
            }
        },
        error = function(e) {
            warnRuntimeIssue("Failed to get the minimum number of expected tests: ", e$message,
                diagnosticId = "qa.test_metadata_failed"
            )
        }
    )
    return(minNumberOfExpectedTestsDefault)
}
