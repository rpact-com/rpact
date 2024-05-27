## |
## |  *Quality Assurance*
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
## |  File version: $Revision: 7920 $
## |  Last changed: $Date: 2024-05-23 13:56:24 +0200 (Do, 23 Mai 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_logger.R
NULL

# See testthat::skip_on_cran()
.skipTestIfDisabled <- function() {
    if (!isTRUE(.isCompleteUnitTestSetEnabled()) &&
            base::requireNamespace("testthat", quietly = TRUE)) {
        testthat::skip("Test is disabled")
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
    if (!.isMachine64Bit() && !.isMinimumRVersion4() && base::requireNamespace("testthat", quietly = TRUE)) {
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

.getTestthatResultLine <- function(fileContent) {
    if (grepl("\\[ OK:", fileContent)) {
        indexStart <- regexpr("\\[ OK: \\d", fileContent)[[1]]
        indexEnd <- regexpr("FAILED: \\d{1,5} \\]", fileContent)
        indexEnd <- indexEnd[[1]] + attr(indexEnd, "match.length") - 1
        resultPart <- substr(fileContent, indexStart, indexEnd)
        return(resultPart)
    }

    indexStart <- regexpr("\\[ FAIL \\d", fileContent)[[1]]
    if (indexStart == -1) {
        return("[ FAIL 0 | WARN 0 | SKIP 0 | PASS 14868 ]")
    }

    indexEnd <- regexpr("PASS \\d{1,5} \\]", fileContent)
    indexEnd <- indexEnd[[1]] + attr(indexEnd, "match.length") - 1
    resultPart <- substr(fileContent, indexStart, indexEnd)
    return(resultPart)
}

.getTestthatResultNumberOfFailures <- function(fileContent) {
    if (grepl("FAILED:", fileContent)) {
        line <- .getTestthatResultLine(fileContent)
        index <- regexpr("FAILED: \\d{1,5} \\]", line)
        indexStart <- index[[1]] + 8
        indexEnd <- index[[1]] + attr(index, "match.length") - 3
        return(substr(line, indexStart, indexEnd))
    }

    line <- .getTestthatResultLine(fileContent)
    index <- regexpr("FAIL \\d{1,5} ", line)
    indexStart <- index[[1]] + 5
    indexEnd <- index[[1]] + attr(index, "match.length") - 2
    return(substr(line, indexStart, indexEnd))
}

.getTestthatResultNumberOfSkippedTests <- function(fileContent) {
    if (grepl("SKIPPED:", fileContent)) {
        line <- .getTestthatResultLine(fileContent)
        index <- regexpr("SKIPPED: \\d{1,5} {1,1}", line)
        indexStart <- index[[1]] + 9
        indexEnd <- index[[1]] + attr(index, "match.length") - 2
        return(substr(line, indexStart, indexEnd))
    }

    line <- .getTestthatResultLine(fileContent)
    index <- regexpr("SKIP \\d{1,5} {1,1}", line)
    indexStart <- index[[1]] + 5
    indexEnd <- index[[1]] + attr(index, "match.length") - 2
    return(substr(line, indexStart, indexEnd))
}

# testFileTargetDirectory <- "D:/R/_temp/test_debug"
.downloadUnitTests <- function(testFileTargetDirectory, ..., token, secret,
        method = "auto", mode = "wb", cacheOK = TRUE, extra = getOption("download.file.extra"),
        cleanOldFiles = TRUE, connectionType = c("http", "ftp", "pkg")) {
    .assertIsSingleCharacter(testFileTargetDirectory, "testFileTargetDirectory")
    .assertIsSingleCharacter(token, "token")
    .assertIsSingleCharacter(secret, "secret")
    connectionType <- match.arg(connectionType)

    if (grepl("testthat(/|\\\\)?$", testFileTargetDirectory)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'testFileTargetDirectory' (", testFileTargetDirectory, ") must not end with 'testthat'"
        )
    }

    if (cleanOldFiles) {
        unlink(testFileTargetDirectory, recursive = TRUE)
    }
    dir.create(testFileTargetDirectory, recursive = TRUE)

    testthatSubDirectory <- file.path(testFileTargetDirectory, "testthat")
    if (!dir.exists(testthatSubDirectory)) {
        dir.create(testthatSubDirectory, recursive = TRUE)
    }

    if (connectionType == "ftp") {
        suppressWarnings(.downloadUnitTestsViaFtp(
            testFileTargetDirectory = testFileTargetDirectory,
            testthatSubDirectory = testthatSubDirectory,
            token = token, secret = secret, method = method, mode = mode,
            cacheOK = cacheOK, extra = extra
        ))
    } else if (connectionType == "http") {
        suppressWarnings(.downloadUnitTestsViaHttp(
            testFileTargetDirectory = testFileTargetDirectory,
            testthatSubDirectory = testthatSubDirectory,
            token = token, secret = secret
        ))
    } else if (connectionType == "pkg") {
        .prepareUnitTestFiles(extra, testFileTargetDirectory, token, secret)
    }
}

.prepareUnitTestFiles <- function(packageSource, testFileTargetDirectory, token, secret) {
    if (is.null(packageSource)) {
        return(invisible())
    }

    .assertIsValidCipher("token", token)
    .assertIsValidCipher("secret", secret)

    .assertIsSingleCharacter(packageSource, "packageSource")
    if (!file.exists(packageSource)) {
        warning(sQuote("packageSource"), " (", packageSource, ") does not exist")
    }

    if (!grepl("\\.tar\\.gz$", packageSource)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "file ", sQuote(packageSource), " must have a .tar.gz extension")
    }

    unlinkFile <- FALSE
    if (grepl("^http", packageSource)) {
        tempFile <- tempfile(fileext = ".tar.gz")
        if (utils::download.file(packageSource, tempFile) != 0) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote(packageSource), " seems to be an invalid URL")
        }
        packageSource <- tempFile
        unlinkFile <- TRUE
    }

    testthatTempDirectory <- NULL
    tryCatch(
        {
            contentLines <- utils::untar(packageSource, list = TRUE)
            if (!("rpact/DESCRIPTION" %in% contentLines) || !("rpact/tests/testthat/" %in% contentLines)) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "file ", sQuote(packageSource), " is not an rpact package source file")
            }

            testthatTempDirectory <- file.path(testFileTargetDirectory, "temp")
            utils::untar(packageSource, files = c(
                "rpact/tests/testthat.R",
                "rpact/tests/testthat/"
            ), exdir = testthatTempDirectory)
            testthatTempSubDirectory <- file.path(testthatTempDirectory, "rpact", "tests")
            testFiles <- list.files(testthatTempSubDirectory, pattern = "\\.R$", recursive = TRUE)
            for (testFile in testFiles) {
                file.copy(file.path(testthatTempSubDirectory, testFile), file.path(testFileTargetDirectory, testFile))
            }
            message(
                length(testFiles), " extracted from ", sQuote(packageSource),
                " and copied to ", sQuote(testFileTargetDirectory)
            )
        },
        finally = {
            if (!is.null(testthatTempDirectory)) {
                unlink(testthatTempDirectory, recursive = TRUE)
            }
            if (unlinkFile) {
                unlink(packageSource)
            }
        }
    )
}

.downloadUnitTestsViaHttp <- function(testFileTargetDirectory, ..., testthatSubDirectory, token, secret) {
    indexFile <- file.path(testFileTargetDirectory, "index.html")
    currentFile <- NA_character_
    tryCatch(
        {
            version <- utils::packageVersion("rpact")
            baseUrl <- paste0("https://", token, ":", secret, "@unit.tests.rpact.com/", version, "/tests/")

            if (!dir.exists(testFileTargetDirectory)) {
                dir.create(testFileTargetDirectory)
            }
            if (!dir.exists(testthatSubDirectory)) {
                dir.create(testthatSubDirectory)
            }

            testthatBaseFile <- system.file("tests", "testthat.R", package = "rpact")
            if (file.exists(testthatBaseFile)) {
                file.copy(testthatBaseFile, file.path(testFileTargetDirectory, "testthat.R"))
            } else {
                currentFile <- "testthat.R"
                result <- utils::download.file(
                    url = paste0(baseUrl, "testthat.R"),
                    destfile = file.path(testFileTargetDirectory, "testthat.R"),
                    method = "auto", mode = "wb"
                )
                if (result != 0) {
                    warning("'testthat.R' download result in ", result)
                }
            }

            currentFile <- "index.txt"
            result <- utils::download.file(
                url = paste0(baseUrl, "testthat/index.txt"),
                destfile = indexFile, quiet = TRUE,
                method = "auto", mode = "wb"
            )
            if (result != 0) {
                warning("Unit test index file download result in ", result)
            }

            lines <- .readLinesFromFile(indexFile)
            lines <- lines[grepl("\\.R", lines)]
            testFiles <- gsub("\\.R<.*", ".R", lines)
            testFiles <- gsub(".*>", "", testFiles)
            testFiles <- gsub(" *$", "", testFiles)
            if (length(testFiles) == 0) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "online source does not contain any unit test files"
                )
            }

            startTime <- Sys.time()
            message("Start to download ", length(testFiles), " unit test files (http). Please wait...")
            for (testFile in testFiles) {
                currentFile <- testFile
                result <- utils::download.file(
                    url = paste0(baseUrl, "testthat/", testFile),
                    destfile = file.path(testthatSubDirectory, testFile), quiet = TRUE,
                    method = "auto", mode = "wb"
                )
            }
            message(
                length(testFiles), " unit test files downloaded successfully (needed ",
                .getRuntimeString(startTime, runtimeUnits = "secs"), ")"
            )
        },
        warning = function(w) {
            if (grepl("404 Not Found", w$message)) {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                    "failed to download unit test files (http): file ", sQuote(currentFile), " not found"
                )
            }
        },
        error = function(e) {
            if (grepl(C_EXCEPTION_TYPE_RUNTIME_ISSUE, e$message)) {
                stop(e$message)
            }
            .logDebug(e$message)
            stop(
                C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                "failed to download unit test files (http): ",
                "illegal 'token' / 'secret' or rpact version ", version, " unknown"
            )
        },
        finally = {
            if (file.exists(indexFile)) {
                tryCatch(
                    {
                        file.remove(indexFile)
                    },
                    error = function(e) {
                        warning("Failed to remove unit test index file: ", e$message, call. = FALSE)
                    }
                )
            }
        }
    )
}

.downloadUnitTestsViaFtp <- function(testFileTargetDirectory, ..., testthatSubDirectory, token, secret,
        method = "auto", mode = "wb", cacheOK = TRUE, extra = getOption("download.file.extra")) {
    indexFile <- file.path(testFileTargetDirectory, "index.html")
    tryCatch(
        {
            version <- utils::packageVersion("rpact")
            baseUrl <- paste0("ftp://", token, ":", secret, "@ftp.rpact.com/", version, "/tests/")

            testthatBaseFile <- system.file("tests", "testthat.R", package = "rpact")
            if (file.exists(testthatBaseFile)) {
                file.copy(testthatBaseFile, file.path(testFileTargetDirectory, "testthat.R"))
            } else {
                result <- download.file(
                    url = paste0(baseUrl, "testthat.R"),
                    destfile = file.path(testFileTargetDirectory, "testthat.R"),
                    method = method, quiet = TRUE, mode = mode,
                    cacheOK = cacheOK, extra = extra, headers = NULL
                )
                if (result != 0) {
                    warning("'testthat.R' download result in ", result)
                }
            }

            result <- download.file(
                url = paste0(baseUrl, "testthat/"),
                destfile = indexFile,
                method = method, quiet = TRUE, mode = mode,
                cacheOK = cacheOK, extra = extra, headers = NULL
            )
            if (result != 0) {
                warning("Unit test index file download result in ", result)
            }

            lines <- .readLinesFromFile(indexFile)
            lines <- lines[grepl("\\.R", lines)]
            testFiles <- gsub("\\.R<.*", ".R", lines)
            testFiles <- gsub(".*>", "", testFiles)
            if (length(testFiles) == 0) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "online source does not contain any unit test files"
                )
            }

            startTime <- Sys.time()
            message("Start to download ", length(testFiles), " unit test files (ftp). Please wait...")
            for (testFile in testFiles) {
                result <- download.file(
                    url = paste0(baseUrl, "testthat/", testFile),
                    destfile = file.path(testthatSubDirectory, testFile),
                    method = method, quiet = TRUE, mode = mode,
                    cacheOK = cacheOK,
                    extra = extra,
                    headers = NULL
                )
            }
            message(
                length(testFiles), " unit test files downloaded successfully (needed ",
                .getRuntimeString(startTime, runtimeUnits = "secs"), ")"
            )
        },
        error = function(e) {
            .logDebug(e$message)
            stop(
                C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                "failed to download unit test files (ftp): ",
                "illegal 'token' / 'secret' or rpact version ", version, " unknown"
            )
        },
        finally = {
            if (file.exists(indexFile)) {
                tryCatch(
                    {
                        file.remove(indexFile)
                    },
                    error = function(e) {
                        warning("Failed to remove unit test index file: ", e$message, call. = FALSE)
                    }
                )
            }
        }
    )
}

.getConnectionArgument <- function(connection, name = c(
            "token", "secret", "method",
            "mode", "cacheEnabled", "extra", "cleanOldFiles", "connectionType"
        )) {
    if (is.null(connection) || !is.list(connection)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'connection' must be a list (is ", .getClassName(connection), ")"
        )
    }

    name <- match.arg(name)
    defaultValues <- list(
        "token" = NULL,
        "secret" = NULL,
        "method" = "auto",
        "mode" = "wb",
        "cacheEnabled" = TRUE,
        "extra" = getOption("download.file.extra"),
        "cleanOldFiles" = TRUE,
        "connectionType" = "http"
    )

    value <- connection[[name]]
    if (is.null(value)) {
        return(defaultValues[[name]])
    }

    return(value)
}

#' @title
#' Test Package
#
#' @description
#' This function allows the installed package \code{rpact} to be tested.
#'
#' @param outDir The output directory where all test results shall be saved.
#'     By default the current working directory is used.
#' @param completeUnitTestSetEnabled If \code{TRUE} (default) all existing unit tests will
#'     be executed; a subset of all unit tests will be used otherwise.
#' @param types The type(s) of tests to be done. Can be one or more of
#'     \code{c("tests", "examples", "vignettes")}, default is "tests" only.
#' @param connection A \code{list} where owners of the rpact validation documentation
#'     can enter a \code{token} and a \code{secret} to get full access to all unit tests, e.g.,
#'     to fulfill regulatory requirements (see \href{https://www.rpact.com}{www.rpact.com} for more information).
#' @inheritParams param_three_dots
#'
#' @details
#' This function creates the subdirectory \code{rpact-tests} in the specified output directory
#' and copies all unit test files of the package to this newly created directory.
#' Then the function runs all tests (or a subset of all tests if
#' \code{completeUnitTestSetEnabled} is \code{FALSE}) using
#' \code{\link[tools]{testInstalledPackage}}.
#' The test results will be saved to the text file \code{testthat.Rout} that can be found
#' in the subdirectory \code{rpact-tests}.
#'
#' @return The value of \code{completeUnitTestSetEnabled} will be returned invisible.
#'
#' @examples
#' \dontrun{
#' testPackage()
#' }
#'
#' @export
#'
testPackage <- function(outDir = ".", ...,
        completeUnitTestSetEnabled = TRUE,
        types = "tests",
        connection = list(token = NULL, secret = NULL)) {
    .assertTestthatIsInstalled()

    .assertIsSingleCharacter(outDir, "outDir", naAllowed = FALSE)
    if (!dir.exists(outDir)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "test output directory '", outDir, "' does not exist"
        )
    }

    startTime <- Sys.time()

    Sys.setenv("LANGUAGE" = "EN")
    on.exit(Sys.unsetenv("LANGUAGE"))

    temp <- .isCompleteUnitTestSetEnabled()
    on.exit(Sys.setenv("RPACT_COMPLETE_UNIT_TEST_SET_ENABLED" = temp), add = TRUE)
    Sys.setenv("RPACT_COMPLETE_UNIT_TEST_SET_ENABLED" = completeUnitTestSetEnabled)

    debug <- .getOptionalArgument("debug", ...)
    if (!is.null(debug) && length(debug) == 1 && isTRUE(as.logical(debug))) {
        setLogLevel(C_LOG_LEVEL_DEBUG)
    } else {
        setLogLevel(C_LOG_LEVEL_DISABLED)
    }
    on.exit(resetLogLevel(), add = TRUE)

    token <- .getConnectionArgument(connection, "token")
    secret <- .getConnectionArgument(connection, "secret")
    fullTestEnabled <- (!is.null(token) && !is.null(secret) &&
        length(token) == 1 && length(secret) == 1 &&
        !is.na(token) && !is.na(secret))

    if (completeUnitTestSetEnabled && fullTestEnabled) {
        cat("Run all tests. Please wait...\n")
        cat("Have a break - it takes about 20 minutes.\n")
        cat("Exceution of all available unit tests startet at ",
            format(startTime, "%H:%M (%d-%B-%Y)"), "\n",
            sep = ""
        )
    } else if (!fullTestEnabled) {
        cat("Run a small subset of all tests. Please wait...\n")
        cat("This is just a quick test (see comments below).\n")
        cat("The entire test will take only some seconds.\n")
    } else {
        cat("Run a subset of all tests. Please wait...\n")
        cat("This is just a quick test, i.e., all time consuming tests will be skipped.\n")
        cat("The entire test will take about a minute.\n")
    }

    if (outDir == ".") {
        outDir <- getwd()
    }

    oldResultFiles <- c(
        file.path(outDir, "rpact-tests", "testthat.Rout"),
        file.path(outDir, "rpact-tests", "testthat.Rout.fail")
    )
    for (oldResultFile in oldResultFiles) {
        if (file.exists(oldResultFile)) {
            file.remove(oldResultFile)
        }
    }

    pkgName <- "rpact"
    if (!fullTestEnabled) {
        tools::testInstalledPackage(pkg = pkgName, outDir = outDir, types = types)
    } else {
        testFileTargetDirectory <- file.path(outDir, paste0(pkgName, "-tests"))
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
        .testInstalledPackage(
            testFileDirectory = testFileTargetDirectory,
            pkgName = pkgName, outDir = testFileTargetDirectory, Ropts = ""
        )
    }

    outDir <- file.path(outDir, paste0(pkgName, "-tests"))

    endTime <- Sys.time()

    if (completeUnitTestSetEnabled) {
        cat("Test exceution ended at ",
            format(endTime, "%H:%M (%d-%B-%Y)"), "\n",
            sep = ""
        )
    }

    cat("Total runtime for testing: ", .getRuntimeString(startTime,
        endTime = endTime, runtimeUnits = "auto"
    ), ".\n", sep = "")

    inputFileName <- file.path(outDir, "testthat.Rout")
    if (file.exists(inputFileName)) {
        fileContent <- base::readChar(inputFileName, file.info(inputFileName)$size)
        if (completeUnitTestSetEnabled && fullTestEnabled) {
            cat("All unit tests were completed successfully, i.e., the installation \n",
                "qualification was successful.\n",
                sep = ""
            )
        } else {
            cat("Unit tests were completed successfully.\n", sep = "")
        }
        cat("Results:\n")
        cat(.getTestthatResultLine(fileContent), "\n")
        cat("\n")
        cat("Test results were written to directory \n",
            "'", outDir, "' (see file 'testthat.Rout')\n",
            sep = ""
        )
        skipped <- .getTestthatResultNumberOfSkippedTests(fileContent)
        if (skipped > 0) {
            cat("-------------------------------------------------------------------------\n")
            cat("Note that ", skipped, " tests were skipped; ",
                "a possible reason may be that expected \n",
                "error messages could not be tested ",
                "because of local translation.\n",
                sep = ""
            )
        }
        cat("-------------------------------------------------------------------------\n")
        cat("Please visit www.rpact.com to learn how to use rpact on FDA/GxP-compliant \n",
            "validated corporate computer systems and how to get a copy of the formal \n",
            "validation documentation that is customized and licensed for exclusive use \n",
            "by your company/organization, e.g., to fulfill regulatory requirements.\n",
            sep = ""
        )
    } else {
        inputFileName <- file.path(outDir, "testthat.Rout.fail")
        if (file.exists(inputFileName)) {
            fileContent <- base::readChar(inputFileName, file.info(inputFileName)$size)
            if (completeUnitTestSetEnabled) {
                cat(.getTestthatResultNumberOfFailures(fileContent),
                    " unit tests failed, i.e., the installation qualification was not successful.\n",
                    sep = ""
                )
            } else {
                cat(.getTestthatResultNumberOfFailures(fileContent), " unit tests failed :(\n", sep = "")
            }
            cat("Results:\n")
            cat(.getTestthatResultLine(fileContent), "\n")
            cat("Test results were written to directory '", outDir, "' (see file 'testthat.Rout.fail')\n", sep = "")
        }
    }
    if (!fullTestEnabled) {
        cat("-------------------------------------------------------------------------\n")
        cat("Note that only a small subset of all available unit tests were executed.\n")
        cat("You need a personal 'token' and 'secret' to perform all unit tests.\n")
        cat("You can find these data in the appendix of the validation documentation \n")
        cat("licensed for your company/organization.\n")
    } else if (!completeUnitTestSetEnabled) {
        cat("Note that only a small subset of all available unit tests were executed.\n")
        cat("Use testPackage(completeUnitTestSetEnabled = TRUE) to perform all unit tests.\n")
    }

    invisible(.isCompleteUnitTestSetEnabled())
}

.testInstalledPackage <- function(testFileDirectory, ..., pkgName = "rpact", outDir = ".", Ropts = "") {
    .assertIsSingleCharacter(testFileDirectory, "testFileDirectory", naAllowed = FALSE)
    if (!dir.exists(testFileDirectory)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'testFileDirectory' (", testFileDirectory, ") does not exist")
    }

    workingDirectoryBefore <- setwd(outDir)
    on.exit(setwd(workingDirectoryBefore))

    setwd(testFileDirectory)
    message(gettextf("Running specific tests for package %s", sQuote(pkgName)), domain = NA)
    testFiles <- dir(".", pattern = "\\.R$")
    for (testFile in testFiles) {
        message(gettextf("  Running %s", sQuote(testFile)), domain = NA)
        outfile <- paste0(testFile, "out")
        cmd <- paste(
            shQuote(file.path(R.home("bin"), "R")),
            "CMD BATCH --vanilla --no-timing", Ropts,
            shQuote(testFile), shQuote(outfile)
        )
        cmd <- if (.Platform$OS.type == "windows") paste(cmd, "LANGUAGE=C") else paste("LANGUAGE=C", cmd)
        res <- system(cmd)
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
    setwd(workingDirectoryBefore)

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
