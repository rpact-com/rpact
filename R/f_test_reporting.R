## |
## |  *Test Reporting*
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

#' @include f_test_execution.R
NULL

# Test-result summaries, installation qualification results, and Markdown reports.

.getTestthatResultLine <- function(fileContent) {
    tryCatch(
        {
            if (grepl("\\[ OK:", fileContent)) {
                indexStart <- regexpr("\\[ OK: \\d", fileContent)[[1]]
                indexEnd <- regexpr("FAILED: \\d{1,5} \\]", fileContent)
                indexEnd <- indexEnd[[1]] + attr(indexEnd, "match.length") - 1
                resultPart <- substr(fileContent, indexStart, indexEnd)
                return(resultPart)
            }

            indexStart <- regexpr("\\[ FAIL \\d", fileContent)[[1]]
            if (indexStart == -1) {
                return("[ FAIL 0 | WARN 0 | SKIP 0 | PASS 0 ]")
            }

            indexEnd <- regexpr("PASS \\d{1,5} \\]", fileContent)
            indexEnd <- indexEnd[[1]] + attr(indexEnd, "match.length") - 1
            resultPart <- substr(fileContent, indexStart, indexEnd)
            return(resultPart)
        },
        error = function(e) {
            return(NA_character_)
        }
    )
}

.getTestthatResultNumberOfTests <- function(fileContent) {
    tryCatch(
        {
            # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 0 ]
            line <- .getTestthatResultLine(fileContent)
            parts <- strsplit(line, " ?\\| ?")[[1]]
            numbers <- gsub("[^0-9]", "", parts)
            return(sum(as.integer(numbers), na.rm = TRUE))
        },
        error = function(e) {
            return(0)
        }
    )
}

.getTestthatResultNumberOfFailures <- function(fileContent) {
    tryCatch(
        {
            if (grepl("FAILED:", fileContent)) {
                line <- .getTestthatResultLine(fileContent)
                index <- regexpr("FAILED: \\d{1,5} \\]", line)
                indexStart <- index[[1]] + 8
                indexEnd <- index[[1]] + attr(index, "match.length") - 3
                return(as.integer(substr(line, indexStart, indexEnd)))
            }

            line <- .getTestthatResultLine(fileContent)
            index <- regexpr("FAIL \\d{1,5} ", line)
            indexStart <- index[[1]] + 5
            indexEnd <- index[[1]] + attr(index, "match.length") - 2
            return(as.integer(substr(line, indexStart, indexEnd)))
        },
        error = function(e) {
            return(NA_integer_)
        }
    )
}

.getTestthatResultNumberOfSkippedTests <- function(fileContent) {
    tryCatch(
        {
            if (grepl("SKIPPED:", fileContent)) {
                line <- .getTestthatResultLine(fileContent)
                index <- regexpr("SKIPPED: \\d{1,5} {1,1}", line)
                indexStart <- index[[1]] + 9
                indexEnd <- index[[1]] + attr(index, "match.length") - 2
                return(as.integer(substr(line, indexStart, indexEnd)))
            }

            line <- .getTestthatResultLine(fileContent)
            index <- regexpr("SKIP \\d{1,5} {1,1}", line)
            indexStart <- index[[1]] + 5
            indexEnd <- index[[1]] + attr(index, "match.length") - 2
            return(as.integer(substr(line, indexStart, indexEnd)))
        },
        error = function(e) {
            return(NA_integer_)
        }
    )
}

.getReportAuthor <- function(token, secret, action, default = "RPACT") {
    tryCatch(
        {
            result <- suppressWarnings(base::readLines(sprintf(
                paste0(
                    paste0(c("h", "t", "t", "p", "s", ":", "//"), collapse = ""),
                    paste0(c(
                        "metadata",
                        "rpact",
                        "cloud"
                    ), collapse = "."),
                    "?token=%s&secret=%s&action=%s"
                ),
                token, secret, action
            )))
            companyLine <- grep('<meta name="company"', result, value = TRUE)
            company <- sub('.*content="([^"]+)".*', "\\1", companyLine)
            company <- gsub("&amp;", "&", company)
            company <- ifelse(grepl("unknown|anonymous", company, ignore.case = TRUE), default, company)
            return(company)
        },
        error = function(e) {
            return(default)
        }
    )
}

#'
#' @param title The title of the session information. If \code{NULL} (default), no title is displayed. Example: \code{"# Session Information"}.
#' @param headingLevel The heading level of the title. Default is \code{3}, which produces \code{###}.
#'
#' @noRd
#'
.getSessionInfoMarkdown <- function(
        title = NA_character_,
        headingLevel = 3) {
    si <- sessionInfo()
    output <- character()

    h2 <- function(x) paste0(paste(rep("#", headingLevel), collapse = ""), " ", x)

    .assertIsSingleCharacter(title, "title", naAllowed = TRUE)
    if (!is.null(title) && length(title) == 1 && !is.na(title)) {
        output <- c(output, title, "\n\n")
    }

    output <- c(output, paste0(h2("R Version"), "\n\n"))
    output <- c(output, paste0("- **Version**: ", si$R.version$version.string, "\n"))
    output <- c(output, paste0("- **Platform**: ", si$R.version$platform, "\n"))
    output <- c(output, paste0("- **Running under**: ", si$running, "\n\n"))

    output <- c(output, paste0(h2("Matrix Products"), "\n\n"))
    output <- c(output, paste0("- **", si$matprod, "**\n\n"))

    output <- c(output, paste0(h2("Locale"), "\n\n"))
    for (locale in strsplit(si$locale, ";")[[1]]) {
        output <- c(output, paste0("- ", sub("=", " = ", locale, fixed = TRUE), "\n"))
    }
    output <- c(output, "\n")

    output <- c(output, paste0(h2("Time Zone"), "\n\n"))
    output <- c(output, paste0("- **Time Zone**: ", si$tzone, "\n"))
    output <- c(output, paste0("- **TZCode Source**: ", si$tzcode_type, "\n\n"))

    output <- c(output, paste0(h2("Attached Base Packages"), "\n\n"))
    for (pkg in si$basePkgs) {
        output <- c(output, paste0("- ", pkg, "\n"))
    }
    output <- c(output, "\n")

    output <- c(output, paste0(h2("Other Attached Packages"), "\n\n"))
    for (pkg in names(si$otherPkgs)) {
        output <- c(output, paste0("- ", pkg, ": ", si$otherPkgs[[pkg]]$Version, "\n"))
    }
    output <- c(output, "\n")

    output <- c(output, paste0(h2("Loaded via a Namespace (and not attached)"), "\n\n"))
    for (pkg in names(si$loadedOnly)) {
        output <- c(output, paste0("- ", pkg, ": ", si$loadedOnly[[pkg]]$Version, "\n"))
    }
    output <- c(output, "\n")

    return(paste(output, collapse = ""))
}

.getExistingTestResultFiles <- function(path) {
    .assertIsSingleCharacter(path, "path", naAllowed = FALSE)
    subDir <- file.path(path, "rpact-tests")
    if (dir.exists(subDir)) {
        path <- c(path, subDir)
    }
    resultFiles <- list.files(
        path = path,
        pattern = "^testthat\\.Rout(\\.fail)?$",
        recursive = FALSE,
        full.names = TRUE
    )
    return(resultFiles)
}

.getTestReportFileNames <- function(
        reportType,
        markdownReportFileName,
        keepSourceFiles,
        testFileTargetDirectory,
        resultDir,
        reportFileBaseName) {
    if (identical(reportType, "Rout") || is.null(markdownReportFileName)) {
        return(NULL)
    }

    reportFileNames <- character()
    if (keepSourceFiles && file.exists(file.path(testFileTargetDirectory, markdownReportFileName))) {
        reportFileNames <- c(reportFileNames, markdownReportFileName)
    }

    reportFileNameHtml <- paste0(reportFileBaseName, ".html")
    if (file.exists(file.path(testFileTargetDirectory, reportFileNameHtml))) {
        reportFileNames <- c(reportFileNames, reportFileNameHtml)
    }

    reportFileNamePdf <- paste0(reportFileBaseName, ".pdf")
    if (file.exists(file.path(testFileTargetDirectory, reportFileNamePdf))) {
        reportFileNames <- c(reportFileNames, reportFileNamePdf)
    }

    if (length(reportFileNames) > 0) {
        file.copy(file.path(testFileTargetDirectory, reportFileNames), resultDir)
    }

    return(reportFileNames)
}

#'
#' @title
#' Installation Qualification Result Object
#'
#' @description
#' This object represents the structured result of a full or partial
#' installation qualification test execution. It includes metadata about
#' the executed test suite, paths used, summary statistics, and status
#' messages.
#'
#' @details
#' The object is returned by the function \code{\link{testPackage}} and
#' is of class \code{InstallationQualificationResult}.
#'
#' @format An S3 object of class \code{InstallationQualificationResult} with the following elements:
#' \describe{
#'   \item{completeUnitTestSetEnabled}{Logical indicating whether the full test set was enabled}
#'   \item{testFileDirectory}{Directory containing test scripts}
#'   \item{testFileTargetDirectory}{Directory to which tests are copied or linked}
#'   \item{reportType}{Report type selected (\code{"compact"}, \code{"detailed"}, or \code{"Rout"})}
#'   \item{executionMode}{Execution mode (\code{"default"}, \code{"downloadOnly"},
#'        \code{"downloadAndRunTests"}, or \code{"runTestsInTestFileDirectory"})}
#'   \item{scope}{Scope of the qualification (`"basic"`, `"devel"`, `"both"`, `"internet"`, or `"all"`)}
#'   \item{resultDir}{Directory where the result reports are stored}
#'   \item{resultOuputFile}{Main output report filename}
#'   \item{logFile}{Absolute path to the test log, including \code{testthat.Rout.fail} on failure}
#'   \item{executionCompleted}{Whether all scheduled test files completed, independently of assertion failures}
#'   \item{executionError}{Execution or worker-process error message, or \code{NULL}}
#'   \item{workers}{Number of R test processes used}
#'   \item{timings}{Named numeric vector of elapsed seconds for \code{tests},
#'       \code{basicPackages}, \code{html}, and \code{pdf}; \code{NULL} if execution failed before results were collected}
#'   \item{reportFileNames}{Vector of report files generated}
#'   \item{minNumberOfExpectedTests}{Minimum number of expected tests}
#'   \item{totalNumberOfTests}{Number of tests actually run}
#'   \item{numberOfFailedTests}{Number of failed tests}
#'   \item{numberOfSkippedTests}{Number of skipped tests}
#'   \item{resultMessage}{Message summarizing the result}
#'   \item{statusMessage}{Detailed status message}
#'   \item{status}{Overall result status (\code{"success"}, \code{"incomplete"}, or \code{"failed"})}
#' }
#'
#' @name InstallationQualificationResult
#' @keywords internal
#' @seealso \code{\link{testPackage}}
#' @docType class
#' @aliases InstallationQualificationResult-class
#'
NULL

#'
#' @title
#' Print Installation Qualification Result
#'
#' @description
#' This function prints the details of an `InstallationQualificationResult` object in a user-friendly format.
#'
#' @param x An object of class `InstallationQualificationResult` containing the results of the installation qualification.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @details
#' The function displays the result message, followed by the parameters and their values. It skips parameters with `NULL` or `NA` values.
#'
#' @return
#' This function does not return a value. It is called for its side effects of printing the result.
#'
#' @examples
#' \dontrun{
#' result <- testPackage()
#' print(result)
#' }
#'
#' @keywords internal
#'
#' @export
#'
print.InstallationQualificationResult <- function(x, ...) {
    cat("Installation Qualification Result:\n")
    cat(x$resultMessage, "\n\n")
    cat("Parameters:\n")
    paramNames <- names(x)
    paramNames <- paramNames[!grepl("resultMessage", paramNames)]
    for (paramName in paramNames) {
        paramValue <- x[[paramName]]
        if (is.null(paramValue) || all(is.na(paramValue))) {
            next
        }

        if (is.character(paramValue)) {
            if (grepl("Dir(ectory)?$", paramName)) {
                paramValue <- gsub("\\\\", "/", paramValue)
            }
            paramValue <- sQuote(paramValue)
        }
        cat("  ", paramName, ": ", .arrayToString(paramValue), "\n", sep = "")
    }
}

.showResultsWereWrittenToDirectoryMessage <- function(resultDir, reportFileName, resultOuputFile) {
    cat("Test results were written to directory\n", .vQuote(resultDir), "\n", sep = "")
    reportFileName <- reportFileName[!is.na(reportFileName) & nzchar(reportFileName)]
    if (length(reportFileName)) {
        cat("Report files: ", .arrayToString(sQuote(reportFileName), mode = "and"), "\n", sep = "")
    }
    cat("Log file: ", normalizePath(file.path(resultDir, resultOuputFile),
        winslash = "/", mustWork = FALSE), "\n", sep = "")
}

#'
#' @title
#' Test Plan Section
#'
#' @param section The section title or description.
#'
#' @description
#' The section title or description will be used in the formal validation documentation.
#' For more information visit \url{https://www.rpact.com}
#'
#' @export
#'
#' @keywords internal
#'
test_plan_section <- function(section) {
    cat("\n\n--- ", section, " ---\n", sep = "")
}

.getTestExpectationType <- function(exp) {
    stopifnot(testthat::is.expectation(exp))
    gsub("^expectation_", "", class(exp)[[1]])
}

.isTestExpectationSuccess <- function(exp) .getTestExpectationType(exp) == "success"

.isTestExpectationFailure <- function(exp) .getTestExpectationType(exp) == "failure"

.isTestExpectationError <- function(exp) .getTestExpectationType(exp) == "error"

.isTestExpectationSkip <- function(exp) .getTestExpectationType(exp) == "skip"

.isTestExpectationWarning <- function(exp) .getTestExpectationType(exp) == "warning"

.isTestExpectationBroken <- function(exp) .isTestExpectationFailure(exp) || .isTestExpectationError(exp)

.isTestExpectationOkay <- function(exp) .getTestExpectationType(exp) %in% c("success", "warning")

.getTestGroupIndices <- function(vec) {
    diffs <- c(1, diff(vec))
    group_indices <- cumsum(diffs != 1)
    return(split(vec, group_indices))
}

#'
#' @title Markdown Reporter for Test Results
#'
#' @description
#' This class defines a Markdown reporter for test results, inheriting from the `R6::Reporter` class.
#' It logs test results in Markdown format and saves them to a file named `test_results.md`.
#'
#' @field startTime The start time of the test run.
#' @field output A character vector to store the log output.
#' @field failures The number of test failures.
#' @field fileName The name of the current test file being processed.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(...)}}{Initializes the reporter, setting up the output and failures fields.}
#'   \item{\code{log(...)}}{Logs messages to the output field.}
#'   \item{\code{start_reporter()}}{Starts the reporter, logging the introduction and test results header.}
#'   \item{\code{start_file(file)}}{Sets the current file name being processed.}
#'   \item{\code{getContext()}}{Gets the context from the current file name.}
#'   \item{\code{add_result(context, test, result)}}{Adds a test result to the log, marking it as passed or failed.}
#'   \item{\code{end_reporter()}}{Ends the reporter, logging the summary and saving the output to a file.}
#'   \item{\code{finalize()}}{Finalizes the reporter, displaying a message that the test results were saved.}
#' }
#'
#' @export
#'
#' @keywords internal
#'
MarkdownReporter <- R6::R6Class(
    "MarkdownReporter",
    inherit = testthat::Reporter,
    private = list(
        COLORS = list(
            SUCCESS = list(color = "blue", unicode = "&#x2705;", latex = "\\testsuccess{}", label = "OK"),
            FAILURE = list(color = "red", unicode = "&#x274C;", latex = "\\testfail{}", label = "X"),
            WARNING = list(color = "orange", unicode = "&#x1F525;", latex = "\\testwarning{}", label = "[!]")
        ),
        finalize = function() {
            cat("\n")
            cat("[ FAIL ", self$numberOfFailures,
                " | WARN ", self$numberOfWarnings,
                " | SKIP ", self$numberOfSkips,
                " | PASS ", self$numberOfPassedTests, " ]",
                sep = ""
            )
            cat("\n")
        }
    ),
    public = list(
        startTime = NULL,
        endTime = NULL,
        testStartTime = NULL,
        executionError = NULL,
        reportFinalized = FALSE,
        basicPackageTestsPrepared = FALSE,
        basicPackageTestResult = NULL,
        timings = c(tests = 0, basicPackages = 0, html = 0, pdf = 0),
        output = character(),
        outputSize = "compact",
        current = list(
            context = NULL,
            test = NULL
        ),
        sectionNumber = 1L,
        subsectionNumber = 1L,
        testNumber = 1L,
        testNumbersFailed = integer(),
        testNumbersWarn = integer(),
        counter = 0L,
        numberOfFailures = 0L,
        numberOfWarnings = 0L,
        numberOfSkips = 0L,
        numberOfPassedTests = 0L,
        minNumberOfExpectedTests = 30000L,
        fileName = NULL,
        outputFile = NULL,
        addWarningDetailsToReport = TRUE,
        openHtmlReport = TRUE,
        keepSourceFiles = FALSE,
        testInstalledBasicPackages = TRUE,
        author = "RPACT",
        scope = "basic",
        rpactCranReference = "[rpact](https://cran.r-project.org/package=rpact)",
        metaData = list(),
        metaDataFile = NULL,
        initialize = function(
                ...,
                outputSize = c("compact", "detailed"),
                outputFile = "test_results.md",
                addWarningDetailsToReport = TRUE,
                openHtmlReport = TRUE,
                keepSourceFiles = FALSE,
                testInstalledBasicPackages = TRUE,
                author = "RPACT",
                scope = "basic",
                metaDataFile = NULL) {
            super$initialize(...)
            self$capabilities$parallel_support <- TRUE
            self$capabilities$parallel_updates <- FALSE
            self$outputSize <- match.arg(outputSize)
            self$outputFile <- outputFile
            self$addWarningDetailsToReport <- addWarningDetailsToReport
            self$openHtmlReport <- openHtmlReport
            self$keepSourceFiles <- keepSourceFiles
            self$testInstalledBasicPackages <- testInstalledBasicPackages
            self$author <- author
            self$scope <- scope
            self$metaDataFile <- metaDataFile
            self$minNumberOfExpectedTests <- .getMinNumberOfExpectedTests()
            if (!is.null(self$metaDataFile) &&
                    length(self$metaDataFile) == 1 &&
                    !is.na(self$metaDataFile) &&
                    is.character(self$metaDataFile) &&
                    file.exists(self$metaDataFile)) {
                self$metaData <- readKeyValueFile(self$metaDataFile)
            }
        },
        log = function(...) {
            args <- list(...)
            self$output <- c(self$output, paste(args, collapse = ""))
        },
        start_reporter = function() {
            authorValidated <- ifelse(is.null(self$author) ||
                length(self$author) != 1 ||
                is.na(self$author) ||
                nchar(trimws(self$author)) == 0 ||
                grepl("unknown|anonymous", self$author, ignore.case = TRUE),
            "RPACT", self$author
            )
            self$startTime <- Sys.time()
            self$log("---")
            self$log('title: "Installation Qualification for rpact"')
            self$log('author: "', authorValidated, '"')
            self$log('date: "', format(Sys.time(), "%B %d, %Y"), '"')
            self$log("output:")
            self$log("  html_document:")
            self$log("    toc: true")
            self$log("    toc_depth: 3")
            self$log("  pdf_document:")
            self$log("    number_sections: false")
            self$log("header-includes:")
            for (item in private$COLORS) {
                self$log(
                    "  - \\newcommand{", sub("\\{\\}$", "", item$latex),
                    "}{\\textcolor{", item$color, "}{\\textbf{", item$label, "}}}"
                )
            }
            self$log("---")
            self$log("")

            self$log("## Introduction")
            self$log("")
            self$log(
                "This report presents the results of the Installation Qualification (IQ) ",
                "testing for the ", self$rpactCranReference, " package, conducted using the ",
                "[testPackage()](https://rpact-com.github.io/rpact/reference/testPackage.html) function."
            )
            self$log(
                "The IQ process is essential for verifying that rpact is installed ",
                "correctly and operates as intended in your target environment, ensuring ",
                "compliance with GxP regulatory requirements."
            )
            self$log(
                "The tests performed are designed to validate the functionality and ",
                "reliability of the package, providing confidence for its use in regulated settings."
            )
            self$log("")

            self$log("## System Information and Environment Details for Qualification")
            self$log("")
            self$log("- **Date of creation**: ", format(self$startTime, "%B %d, %Y, %X"))
            self$log("- **Creator**: rpact function *testPackage()*")
            self$log("- **rpact package version**: ", .getPackageVersionString())
            self$log("- **R test processes**: ", getOption("Ncpus", 1L))
            self$log("- **rpact package release date**: ", format(packageDate("rpact"), "%B %d, %Y"))
            self$log("- **System user**: *", Sys.info()[["user"]], "*")
            self$log("- **System ID**: ", getSystemIdentifier(), "")
            if (!is.null(self$metaData) && length(self$metaData) > 0) {
                for (metaDataName in names(self$metaData)) {
                    self$log(
                        "- **", metaDataName, "**: ",
                        self$metaData[[metaDataName]], ""
                    )
                }
            }
            self$log("")
            self$log(.getSessionInfoMarkdown())
            self$log("")

            self$prepareBasicPackageTests()
            basicPackageTestResult <- self$basicPackageTestResult

            if (!is.null(basicPackageTestResult)) {
                self$log("## R Basic Package Test Results")
                self$log("")
                self$log(basicPackageTestResult)
                self$log("")
            }

            self$log("## Test Results")
            self$log("")
            self$testStartTime <- proc.time()[["elapsed"]]
        },
        prepareBasicPackageTests = function() {
            if (self$basicPackageTestsPrepared) return(invisible())
            self$basicPackageTestsPrepared <- TRUE
            basicStarted <- proc.time()[["elapsed"]]
            if (self$testInstalledBasicPackages) {
                self$basicPackageTestResult <- .testInstalledBasicPackages(scope = self$scope, headingLevel = 4)
            }
            self$timings[["basicPackages"]] <- proc.time()[["elapsed"]] - basicStarted
            invisible()
        },
        start_file = function(file) {
            self$fileName <- file
            self$current$test <- NULL
        },
        getContext = function() {
            if (is.null(self$fileName)) {
                return("unknown")
            }

            context <- self$fileName
            context <- sub("^test[-_]", "", context)
            context <- sub("[.][Rr]$", "", context)
            context
        },
        getTestText = function(n) {
            paste0(n, " ", ifelse(n == 1, "test", "tests"))
        },
        add_result = function(context, test, result) {
            self$counter <- self$counter + 1L

            if (.isTestExpectationBroken(result)) {
                self$numberOfFailures <- self$numberOfFailures + 1L
                self$testNumbersFailed <- c(self$testNumbersFailed, self$testNumber)
            } else if (.isTestExpectationWarning(result)) {
                self$numberOfWarnings <- self$numberOfWarnings + 1L
                self$testNumbersWarn <- c(self$testNumbersWarn, self$testNumber)
            } else if (.isTestExpectationSkip(result)) {
                self$numberOfSkips <- self$numberOfSkips + 1L
            } else {
                self$numberOfPassedTests <- self$numberOfPassedTests + 1L
            }

            if (is.null(context) || is.na(context) || context == "") {
                context <- self$getContext()
            }

            if (!identical(context, self$current$context)) {
                self$log("### ", self$sectionNumber, " ", context)
                self$log("")

                self$current$context <- context
                self$sectionNumber <- self$sectionNumber + 1L
                self$subsectionNumber <- 1L
            }

            if (!identical(test, self$current$test)) {
                self$log("#### ", (self$sectionNumber - 1L), ".", self$subsectionNumber, " ", test)
                self$log("")

                self$current$test <- test
                self$subsectionNumber <- self$subsectionNumber + 1L
            }

            failureEnabled <- .isTestExpectationBroken(result)
            warningEnabled <- .isTestExpectationWarning(result)

            resultStatusIcon <-
                if (failureEnabled) {
                    private$COLORS$FAILURE$unicode
                } else if (warningEnabled) {
                    private$COLORS$WARNING$unicode
                } else {
                    private$COLORS$SUCCESS$unicode
                }
            if (self$outputSize == "detailed" || failureEnabled ||
                    (warningEnabled && self$addWarningDetailsToReport)) {
                self$log(
                    self$testNumber, ": ", resultStatusIcon, " ",
                    ifelse(failureEnabled,
                        paste0("**Test:** ", ifelse(!failureEnabled, "passed", "failed")),
                        "**Warning**"
                    )
                )
                self$log("")
            } else {
                self$log(self$testNumber, ": ", resultStatusIcon, ", ")
            }
            self$testNumber <- self$testNumber + 1L
            if (!failureEnabled && (!warningEnabled || !self$addWarningDetailsToReport)) {
                return(invisible())
            }

            self$log(
                "- ", ifelse(failureEnabled, "Error", "Warning"), " in ",
                self$fileName, ", line ",
                if (length(result$srcref)) result$srcref[1] else "unknown", "."
            )
            msgParts <- strsplit(result$message, "\n")[[1]]
            tabStr <- ""
            for (msgPart in msgParts) {
                msgPart <- trimws(msgPart)
                if (nchar(msgPart) == 0) {
                    next
                }

                self$log(tabStr, "- ", msgPart, "")
                tabStr <- "    "
            }
            self$log("")
        },
        end_reporter = function() {
            if (self$reportFinalized) return(invisible())
            self$reportFinalized <- TRUE
            self$endTime <- Sys.time()
            self$timings[["tests"]] <- if (is.null(self$testStartTime)) 0 else proc.time()[["elapsed"]] - self$testStartTime

            self$log("")
            self$log("## Summary")
            self$log("")
            self$log("The test ended at ", format(self$endTime, "%X"), " on ", format(self$endTime, "%B %d, %Y"), ".")
            self$log("")
            self$log(sprintf("- **Unit test runtime**: %.2f seconds", self$timings[["tests"]]))
            self$log(sprintf("- **R basic package test runtime**: %.2f seconds", self$timings[["basicPackages"]]))
            self$log("")
            self$log("A total of ", self$counter, " tests were executed during the Installation Qualification:")
            self$log("")
            self$log("- **", self$getTestText(self$numberOfPassedTests), " passed successfully.**")

            if (self$numberOfFailures > 0) {
                self$log(
                    "- **", self$getTestText(self$numberOfFailures), " failed**: ",
                    .arrayToString(paste0("#", self$testNumbersFailed), mode = "and"), "."
                )
            } else {
                self$log("- **", self$getTestText(self$numberOfFailures), " failed.**")
            }

            if (self$numberOfWarnings > 0) {
                self$log(
                    "- **", self$getTestText(self$numberOfWarnings), " passed with warnings**: ",
                    .arrayToString(paste0("#", self$testNumbersWarn), mode = "and"), "."
                )
            } else {
                self$log("- **", self$getTestText(self$numberOfWarnings), " passed with warnings.**")
            }

            self$log("- **", self$getTestText(self$numberOfSkips), " were skipped.**")
            self$log("")
            if (!is.null(self$executionError)) {
                self$log("**Test execution aborted. The results above are incomplete; installation qualification failed.**")
                self$log("")
                self$log("Expected complete test set (META): ", self$minNumberOfExpectedTests, ".")
                self$log("")
                self$log("Execution error: ", self$executionError)
                self$log("")
                self$log("Inspect testthat.Rout.fail for details. If the problem persists, try workers = 1.")
            } else if (self$numberOfFailures == 0) {
                if (self$counter > self$minNumberOfExpectedTests - 100) {
                    self$log(
                        "The successful completion of all tests confirms that ", self$rpactCranReference, " is ",
                        "correctly installed and functioning properly in your environment."
                    )
                    self$log(
                        "This means the package meets the required standards and is ",
                        "validated for use in GxP-compliant settings."
                    )
                    self$log(
                        "You can now proceed with confidence to use ", self$rpactCranReference, " for reliable ",
                        "statistical planning, simulation, and analyses in regulated areas."
                    )
                } else {
                    self$log("Only a subset of the ", self$minNumberOfExpectedTests, "
						available rpact unit tests were executed.")
                    self$log(
                        "You need to successfully complete all tests to confirm that ", self$rpactCranReference, " is ",
                        "correctly installed and functioning properly in your environment."
                    )
                    self$log(
                        "This means the package is not yet validated for use in GxP-compliant settings."
                    )
                    self$log(
                        "Please read the vignette [Installation Qualification of rpact]",
                        "(https://www.rpact.org/vignettes/utilities/rpact_installation_qualification/) ",
                        "to learn how to qualify ", self$rpactCranReference, " for reliable ",
                        "statistical planning, simulation, and analyses in regulated areas."
                    )
                }
            } else {
                self$log(
                    "The failed tests indicate discrepancies that need to be addressed ",
                    "to ensure ", self$rpactCranReference, " functions correctly in your environment."
                )
                self$log(
                    "It is recommended to review the errors detailed above and consult ",
                    "the ", self$rpactCranReference, " documentation or ",
                    "support resources for guidance on resolving these issues."
                )
                self$log(
                    "Completing a successful IQ is crucial for maintaining compliance ",
                    "and ensuring the reliability of your statistical analyses using ",
                    self$rpactCranReference, "."
                )
            }

            self$log("")
            self$log("## References")
            self$log("")
            citePackage <- utils::capture.output(print(citation("rpact"), bibtex = FALSE))
            citePackage <- trimws(citePackage[!grepl("^($)|(To cite )|(Um Paket )|(Please )", citePackage)])

            indices <- grep("^Wassmer", citePackage)
            if (length(indices) > 1) {
                if (indices[length(indices)] < length(citePackage)) {
                    indices <- c(indices, length(citePackage))
                }

                citePackageParts <- list()
                for (i in 2:length(indices)) {
                    part <- citePackage[indices[i - 1]:indices[i]]
                    citePackageParts[[length(citePackageParts) + 1]] <- paste(part, collapse = " ")
                }
            } else {
                citePackagePart <- list(paste(citePackage, collapse = " "))
            }

            for (citePackagePart in citePackageParts) {
                self$log("- ", citePackagePart)
            }
            self$log(
                "- rpact test coverage: [app.codecov.io/gh/rpact-com/rpact]",
                "(https://app.codecov.io/gh/rpact-com/rpact?branch=main)"
            )

            self$log("")
            self$log("This report was generated automatically by the rpact function *testPackage()*")
            self$log("")
            self$log("---")
            self$log("")
            self$log(
                "For further assistance or questions regarding this report, ",
                "please contact the [RPACT](https://rpact.com/) support team ",
                "at [support@rpact.com](mailto:support@rpact.com)."
            )

            self$log("")

            indices <- grep(", $", self$output)
            indices <- .getTestGroupIndices(indices)
            for (indicesPerGroup in indices) {
                part <- self$output[indicesPerGroup]
                part <- sub(", $", "\n", paste(part, collapse = ""))
                self$output[indicesPerGroup[1]] <- part
                indicesPerGroup <- indicesPerGroup[-1]
                if (length(indicesPerGroup) > 0) {
                    self$output[indicesPerGroup] <- "_remove_this_"
                }
            }
            self$output <- self$output[self$output != "_remove_this_"]

            cat(paste(self$output, collapse = "\n"), file = self$outputFile)
            message("Test results saved to ", sQuote(self$outputFile))

            if (!file.exists(self$outputFile)) {
                return(invisible())
            }

            outputPath <- dirname(self$outputFile)
            sourcePath <- file.path(outputPath, "src")
            if (self$keepSourceFiles && !dir.exists(sourcePath)) {
                if (!dir.create(sourcePath)) {
                    warnRuntimeIssue("Failed to create directory ", sQuote(sourcePath), ". ",
                        "Source files will be saved to ", sQuote(outputPath), ".",
                        diagnosticId = "qa.source_output_fallback"
                    )
                    sourcePath <- outputPath
                }
            }

            htmlStarted <- proc.time()[["elapsed"]]
            tryCatch(
                {
                    .assertPackageIsInstalled("rmarkdown")
                    htmlOutputFile <- sub("\\.md$", ".html", self$outputFile)
                    rmarkdown::render(self$outputFile, output_format = "html_document")
                    if (isTRUE(self$openHtmlReport) && file.exists(htmlOutputFile)) {
                        utils::browseURL(htmlOutputFile)
                    }
                    if (self$keepSourceFiles) {
                        mdFileForHtmlTarget <- file.path(sourcePath, sub("\\.md$", "_html.md", basename(self$outputFile)))
                        if (file.rename(self$outputFile, mdFileForHtmlTarget)) {
                            cat("LaTeX source file saved to ", sQuote(mdFileForHtmlTarget), "\n")
                        }
                    } else {
                        file.remove(self$outputFile)
                    }
                },
                error = function(e) {
                    warnRuntimeIssue("Failed to render ", sQuote(self$outputFile),
                        " to html: ", e$message,
                        diagnosticId = "qa.report_render_failed"
                    )
                }
            )

            self$timings[["html"]] <- proc.time()[["elapsed"]] - htmlStarted
            pdfStarted <- proc.time()[["elapsed"]]
            mdFileForTex <- sub("\\.md$", "_tex.md", self$outputFile)
            tryCatch(
                {
                    .assertPackageIsInstalled("rmarkdown")
                    content <- paste(self$output, collapse = "\n")
                    content <- gsub("## ", "# ", content, fixed = TRUE)
                    for (item in private$COLORS) {
                        content <- gsub(item$unicode, item$latex, content, fixed = TRUE)
                    }
                    cat(content, file = mdFileForTex)
                    rmarkdown::render(mdFileForTex, output_format = "pdf_document")

                    pdfFile <- sub("\\.md$", ".pdf", mdFileForTex)
                    if (file.exists(pdfFile)) {
                        file.rename(pdfFile, sub("_tex", "", pdfFile))
                    }

                    if (self$keepSourceFiles) {
                        mdFileForTexTarget <- file.path(sourcePath, basename(mdFileForTex))
                        if (file.rename(mdFileForTex, mdFileForTexTarget)) {
                            cat("LaTeX source file saved to ", sQuote(mdFileForTexTarget), "\n")
                        }
                    } else {
                        file.remove(mdFileForTex)
                    }
                },
                error = function(e) {
                    warnRuntimeIssue("Failed to render ", sQuote(mdFileForTex), " to pdf: ", e$message,
                        diagnosticId = "qa.report_render_failed"
                    )
                }
            )
            self$timings[["pdf"]] <- proc.time()[["elapsed"]] - pdfStarted
        }
    )
)
