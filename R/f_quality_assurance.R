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

#' @include f_logger.R
NULL

# Installation qualification, startup messages, and test-file provisioning.

# testFileTargetDirectory <- "D:/R/_temp/test_debug"
.downloadUnitTests <- function(
        testFileTargetDirectory,
        ...,
        token,
        secret,
        method = "auto",
        mode = "wb",
        cacheOK = TRUE,
        extra = getOption("download.file.extra"),
        cleanOldFiles = TRUE,
        connectionType = c("http", "ftp", "pkg")) {
    .assertIsSingleCharacter(testFileTargetDirectory, "testFileTargetDirectory")
    .assertIsSingleCharacter(token, "token")
    .assertIsSingleCharacter(secret, "secret")
    connectionType <- match.arg(connectionType)

    if (grepl("testthat(/|\\\\)?$", testFileTargetDirectory)) {
        stopIllegalArgument("'testFileTargetDirectory' (", testFileTargetDirectory, ") must not end with 'testthat'",
            functionName = ".downloadUnitTests",
            parameter = "testFileTargetDirectory", value = testFileTargetDirectory,
            relatedParameter = "testthat"
        )
    }

    if (cleanOldFiles) {
        unlink(testFileTargetDirectory, recursive = TRUE)
    }

    if (!dir.exists(testFileTargetDirectory)) {
        dir.create(testFileTargetDirectory, recursive = TRUE)
    }

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
        warnRuntimeIssue(sQuote("packageSource"), " (", packageSource, ") does not exist",
            parameter = "packageSource",
            value = packageSource,
            diagnosticId = "qa.package_source_missing"
        )
    }

    if (!grepl("\\.tar\\.gz$", packageSource)) {
        stopIllegalArgument("file ", sQuote(packageSource), " must have a .tar.gz extension",
            functionName = ".prepareUnitTestFiles",
            parameter = packageSource
        )
    }

    unlinkFile <- FALSE
    if (grepl("^http", packageSource)) {
        tempFile <- tempfile(fileext = ".tar.gz")
        if (utils::download.file(packageSource, tempFile) != 0) {
            stopIllegalArgument(sQuote(packageSource), " seems to be an invalid URL",
                functionName = ".prepareUnitTestFiles",
                parameter = packageSource
            )
        }
        packageSource <- tempFile
        unlinkFile <- TRUE
    }

    testthatTempDirectory <- NULL
    tryCatch(
        {
            contentLines <- utils::untar(packageSource, list = TRUE)
            if (!("rpact/DESCRIPTION" %in% contentLines) || !("rpact/tests/testthat/" %in% contentLines)) {
                stopIllegalArgument("file ", sQuote(packageSource), " is not an rpact package source file",
                    functionName = ".prepareUnitTestFiles",
                    parameter = packageSource
                )
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
    indexFile <- file.path(testFileTargetDirectory, "index.txt")
    currentFile <- NA_character_
    tryCatch(
        {
            version <- .getPackageVersionString()
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
                    warnRuntimeIssue("'testthat.R' download result in ", result,
                        parameter = "testthat.R",
                        diagnosticId = "qa.test_download_failed"
                    )
                }
            }

            currentFile <- "index.txt"
            result <- utils::download.file(
                url = paste0(baseUrl, "testthat/index.txt"),
                destfile = indexFile, quiet = TRUE,
                method = "auto", mode = "wb"
            )
            if (result != 0) {
                warnRuntimeIssue("Unit test index file download result in ", result,
                    diagnosticId = "qa.test_download_failed"
                )
            }

            lines <- .readLinesFromFile(indexFile)
            lines <- lines[grepl("\\.R", lines)]
            testFiles <- gsub("\\.R<.*", ".R", lines)
            testFiles <- gsub(".*>", "", testFiles)
            testFiles <- gsub(" *$", "", testFiles)
            if (length(testFiles) == 0) {
                stopIllegalArgument("online source does not contain any unit test files",
                    functionName = ".downloadUnitTestsViaHttp"
                )
            }

            startTime <- Sys.time()
            message("Start to download ", length(testFiles), " unit test files (http). Please wait...")
            counter <- 0
            for (testFile in testFiles) {
                currentFile <- testFile
                result <- utils::download.file(
                    url = paste0(baseUrl, "testthat/", testFile),
                    destfile = file.path(testthatSubDirectory, testFile), quiet = TRUE,
                    method = "auto", mode = "wb"
                )
                if (identical(result, 0) || identical(result, 0L)) {
                    counter <- counter + 1
                }
            }
            if (counter < length(testFiles)) {
                warnRuntimeIssue(
                    "Only ", counter, " of ", length(testFiles),
                    " unit test files were downloaded successfully (needed ",
                    .getRuntimeString(startTime, runtimeUnits = "secs"), ")",
                    diagnosticId = "qa.test_download_failed"
                )
            } else {
                message(
                    counter, " unit test files downloaded successfully (needed ",
                    .getRuntimeString(startTime, runtimeUnits = "secs"), ")"
                )
            }
        },
        warning = function(w) {
            if (grepl("404 Not Found", w$message)) {
                stopRuntimeIssue("failed to download unit test files (http): file ", sQuote(currentFile), " not found",
                    functionName = ".downloadUnitTestsViaHttp",
                    parameter = currentFile
                )
            }
        },
        error = function(e) {
            if (grepl(C_EXCEPTION_TYPE_RUNTIME_ISSUE, e$message)) {
                stopRuntimeIssue(e$message,
                    functionName = ".downloadUnitTestsViaHttp"
                )
            }
            .logDebug(e$message)
            stopRuntimeIssue("failed to download unit test files (http): ", "illegal 'token' / 'secret' or rpact version ",
                version, " unknown",
                functionName = ".downloadUnitTestsViaHttp",
                parameter = "token",
                relatedParameter = "secret"
            )
        },
        finally = {
            if (file.exists(indexFile)) {
                tryCatch(
                    {
                        file.remove(indexFile)
                    },
                    error = function(e) {
                        warnRuntimeIssue("Failed to remove unit test index file: ", e$message,
                            diagnosticId = "qa.temporary_index_cleanup_failed"
                        )
                    }
                )
            }
        }
    )
}

.downloadUnitTestsViaFtp <- function(
        testFileTargetDirectory,
        ...,
        testthatSubDirectory,
        token,
        secret,
        method = "auto",
        mode = "wb",
        cacheOK = TRUE,
        extra = getOption("download.file.extra")) {
    indexFile <- file.path(testFileTargetDirectory, "index.html")
    tryCatch(
        {
            version <- .getPackageVersionString()
            baseUrl <- paste0("ftp://", token, ":", secret, "@ftp.rpact.com/", version, "/tests/")

            testthatBaseFile <- system.file("tests", "testthat.R", package = "rpact")
            if (file.exists(testthatBaseFile)) {
                file.copy(testthatBaseFile, file.path(testFileTargetDirectory, "testthat.R"))
            } else {
                result <- utils::download.file(
                    url = paste0(baseUrl, "testthat.R"),
                    destfile = file.path(testFileTargetDirectory, "testthat.R"),
                    method = method, quiet = TRUE, mode = mode,
                    cacheOK = cacheOK, extra = extra, headers = NULL
                )
                if (result != 0) {
                    warnRuntimeIssue("'testthat.R' download result in ", result,
                        parameter = "testthat.R",
                        diagnosticId = "qa.test_download_failed"
                    )
                }
            }

            result <- utils::download.file(
                url = paste0(baseUrl, "testthat/"),
                destfile = indexFile,
                method = method, quiet = TRUE, mode = mode,
                cacheOK = cacheOK, extra = extra, headers = NULL
            )
            if (result != 0) {
                warnRuntimeIssue("Unit test index file download result in ", result,
                    diagnosticId = "qa.test_download_failed"
                )
            }

            lines <- .readLinesFromFile(indexFile)
            lines <- lines[grepl("\\.R", lines)]
            testFiles <- gsub("\\.R<.*", ".R", lines)
            testFiles <- gsub(".*>", "", testFiles)
            if (length(testFiles) == 0) {
                stopIllegalArgument("online source does not contain any unit test files",
                    functionName = ".downloadUnitTestsViaFtp"
                )
            }

            startTime <- Sys.time()
            message("Start to download ", length(testFiles), " unit test files (ftp). Please wait...")
            counter <- 0
            for (testFile in testFiles) {
                result <- utils::download.file(
                    url = paste0(baseUrl, "testthat/", testFile),
                    destfile = file.path(testthatSubDirectory, testFile),
                    method = method, quiet = TRUE, mode = mode,
                    cacheOK = cacheOK,
                    extra = extra,
                    headers = NULL
                )
                if (identical(result, 0) || identical(result, 0L)) {
                    counter <- counter + 1
                }
            }
            if (counter < length(testFiles)) {
                warnRuntimeIssue(
                    "Only ", counter, " of ", length(testFiles),
                    " unit test files were downloaded successfully (needed ",
                    .getRuntimeString(startTime, runtimeUnits = "secs"), ")",
                    diagnosticId = "qa.test_download_failed"
                )
            } else {
                message(
                    counter, " unit test files downloaded successfully (needed ",
                    .getRuntimeString(startTime, runtimeUnits = "secs"), ")"
                )
            }
        },
        error = function(e) {
            .logDebug(e$message)
            stopRuntimeIssue("failed to download unit test files (ftp): ", "illegal 'token' / 'secret' or rpact version ",
                version, " unknown",
                functionName = ".downloadUnitTestsViaFtp",
                parameter = "token",
                relatedParameter = "secret"
            )
        },
        finally = {
            if (file.exists(indexFile)) {
                tryCatch(
                    {
                        file.remove(indexFile)
                    },
                    error = function(e) {
                        warnRuntimeIssue("Failed to remove unit test index file: ", e$message,
                            diagnosticId = "qa.temporary_index_cleanup_failed"
                        )
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
        stopIllegalArgument("'connection' must be a list (is ", .getClassName(connection), ")",
            functionName = ".getConnectionArgument",
            parameter = "connection", value = connection
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

.getHashValue <- function(x) {
    .assertIsSingleCharacter(x, "x")
    tempFile <- tempfile()
    on.exit(unlink(tempFile))
    cat(x, file = tempFile)
    hashValue <- tools::md5sum(tempFile)
    names(hashValue) <- NULL
    return(hashValue)
}

.getPackageVersionString <- function() {
    return(paste(unlist(utils::packageVersion("rpact")), collapse = "."))
}

.installationQualificationDone <- function() {
    return(identical(
        .getEnvironmentVariable("RPACT_SYSTEM_IDENTIFIER",
            "rpact.system.identifier",
            type = "character"
        ),
        getSystemIdentifier(.getEnvironmentVariable("RPACT_SYSTEM_TEST_DATE",
            "rpact.system.test.date",
            type = "character"
        ))
    ))
}

.setSystemIdentifier <- function() {
    date <- .getEnvironmentVariable("RPACT_SYSTEM_TEST_DATE", "rpact.system.test.date")
    if (is.null(date)) {
        date <- Sys.Date()
        base::options("rpact.system.test.date" = date)
    }
    base::options("rpact.system.identifier" = getSystemIdentifier(date))
    saveOptions()
}


#' Check if Startup Messaging is Enabled
#'
#' @description
#' This function checks whether the startup messages for the `rpact` package are enabled.
#' It also ensures that the messages are displayed only once within a 72-hour period.
#'
#' @details
#' The function retrieves the `rpact.startup.message.enabled` option to determine if startup
#' messages are enabled. If enabled, it checks the last timestamp when the message was shown
#' and ensures that the message is displayed again only if more than 72 hours have passed.
#' The timestamp is updated after the message is shown.
#'
#' @return
#' Returns \code{TRUE} if startup messaging is enabled and should be displayed, otherwise \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' if (.isStartupMessagingEnabled()) {
#'     message("Welcome to rpact!")
#' }
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
.isStartupMessagingEnabled <- function() {
    if (isFALSE(.getEnvironmentVariable(
            "RPACT_STARTUP_MESSAGE_ENABLED",
            "rpact.startup.message.enabled",
            default = TRUE,
            type = "logical"
        ))) {
        return(FALSE)
    }

    cfg <- system.file("STARTUP_MESSAGE", package = "rpact")
    if (file.exists(cfg)) {
        cfgContent <- readLines(cfg, warn = FALSE)
        if (any(grepl("disabled", cfgContent, ignore.case = TRUE))) {
            return(FALSE)
        }
    }

    lastShownTime <- .getEnvironmentVariable(
        "RPACT_STARTUP_MESSAGE_TIMESTAMP",
        "rpact.startup.message.timestamp",
        type = "character"
    )
    currentTime <- Sys.time()
    currentTime <- as.POSIXct(currentTime, tz = "UTC")
    if (is.null(lastShownTime) || !grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", lastShownTime)) {
        base::options("rpact.startup.message.timestamp" = currentTime)
        return(TRUE)
    }

    lastShownTime <- as.POSIXct(lastShownTime, tz = "UTC")
    timeDiff <- as.numeric(difftime(currentTime, lastShownTime, units = "mins"))

    # If the time difference is greater than a random value
    # between 72 and 144 hours (3 to 6 days), show the message again
    if (timeDiff > floor(runif(1, 72, 144)) * 60) {
        base::options("rpact.startup.message.timestamp" = currentTime)
        saveOptions()
        return(TRUE)
    }

    return(FALSE)
}

#' Disable Startup Messages
#'
#' @description
#' This function disables the startup messages for the `rpact` package by setting
#' the `rpact.startup.message.enabled` option to `FALSE`.
#'
#' @details
#' Once this function is called, the startup messages will remain disabled until
#' explicitly re-enabled using the `enableStartupMessages()` function. The current
#' state is saved using the `saveOptions()` function.
#'
#' @return
#' This function does not return a value. It is called for its side effects.
#'
#' @examples
#' \dontrun{
#' disableStartupMessages()
#' }
#'
#' @export
#'
#' @keywords internal
#'
disableStartupMessages <- function() {
    if (isFALSE(.getEnvironmentVariable(
            "RPACT_STARTUP_MESSAGE_ENABLED",
            "rpact.startup.message.enabled",
            default = TRUE,
            type = "logical"
        ))) {
        return(invisible())
    }

    base::options("rpact.startup.message.enabled" = FALSE)
    saveOptions()
}

#' Enable Startup Messages
#'
#' @description
#' This function enables the startup messages for the `rpact` package by setting
#' the `rpact.startup.message.enabled` option to `TRUE`.
#'
#' @details
#' Once this function is called, the startup messages will remain enabled until
#' explicitly disabled using the `disableStartupMessages()` function. The current
#' state is saved using the `saveOptions()` function.
#'
#' @return
#' This function does not return a value. It is called for its side effects.
#'
#' @examples
#' \dontrun{
#' enableStartupMessages()
#' }
#'
#' @export
#'
#' @keywords internal
#'
enableStartupMessages <- function() {
    if (isTRUE(.getEnvironmentVariable(
            "RPACT_STARTUP_MESSAGE_ENABLED",
            "rpact.startup.message.enabled",
            default = TRUE,
            type = "logical"
        ))) {
        return(invisible())
    }

    base::options("rpact.startup.message.enabled" = TRUE)
    saveOptions()
}

.getInstallationQualificationMissingText <- function() {
    return(c(
        paste0(
            "Installation qualification for rpact ", .getPackageVersionString(),
            " could not be verified on this system."
        ),
        paste0(
            "Before using the package in a GxP-relevant environment, please ensure that installation ",
            "qualification has been successfully performed."
        ),
        "If required, run testPackage()."
    ))
}

#'
#' @title
#' Check Installation Qualification Status
#'
#' @description
#' This function checks whether the installation qualification for the `rpact` package
#' has been completed. If not, it provides a message prompting the user to run the
#' `testPackage()` function to perform the qualification.
#'
#' @param showMessage A logical value indicating whether to display a message if the
#' installation qualification has not been completed. Default is \code{TRUE}.
#'
#' @details
#' The installation qualification is a critical step in ensuring that the `rpact` package
#' is correctly installed and validated for use in GxP-relevant environments. This function
#' verifies the qualification status and informs the user if further action is required.
#'
#' @return
#' Invisibly returns \code{TRUE} if the installation qualification has been completed,
#' otherwise returns \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' checkInstallationQualificationStatus()
#' }
#'
#' @export
#'
#' @keywords internal
#'
checkInstallationQualificationStatus <- function(showMessage = TRUE) {
    if (isTRUE(.installationQualificationDone())) {
        return(invisible(TRUE))
    }

    if (!isTRUE(showMessage)) {
        return(invisible(FALSE))
    }

    message(paste0(.getInstallationQualificationMissingText(), collapse = " "))
    return(invisible(FALSE))
}

#'
#' @title
#' Get System Identifier
#'
#' @param date A character string or \code{Date} representing the date. Default is \code{Sys.Date()}.
#'
#' @description
#' This function generates a unique system identifier based on the platform, R version, and rpact package version.
#'
#' @return
#' A character string representing the unique system identifier.
#'
#' @examples
#' \dontrun{
#' getSystemIdentifier()
#' }
#'
#' @export
#'
#' @keywords internal
#'
getSystemIdentifier <- function(date = NULL) {
    if (is.null(date) || length(date) != 1 || is.na(date) || identical(date, "")) {
        date <- Sys.Date()
    }
    if (methods::is(date, "Date")) {
        date <- format(date, "%Y-%m-%d")
    }
    return(.getHashValue(paste0(
        "System_", R.version$platform,
        "_R", R.version$minor,
        "_rpact", .getPackageVersionString(),
        "_date", date
    )))
}

#'
#' @title
#' Setup Package Tests
#'
#' @description
#' This function sets up the package tests by downloading the test files and copying them to the rpact installation directory.
#'
#' @param token A character string representing the token for authentication.
#' @param secret A character string representing the secret for authentication.
#'
#' @details
#' The function first checks if the `rpact` package directory and its `tests` and `testthat` subdirectories exist.
#' If they do not exist, it stops with an error. It then downloads the test files to a temporary directory and copies them
#' to the `tests` directory of the `rpact` package. If all test files are copied successfully, it removes the default test file.
#'
#' @return
#' The function returns \code{TRUE} if all test files are downloaded and copied successfully to the rpact installation directory; otherwise, it returns \code{FALSE}.
#'
#' @references For more information, please visit: <https://www.rpact.org/vignettes/utilities/rpact_installation_qualification/>
#'
#' @export
#'
#' @keywords internal
#'
setupPackageTests <- function(token, secret) {
    .assertIsSingleCharacter(token, "token")
    .assertIsSingleCharacter(secret, "secret")
    installPath <- find.package("rpact")
    if (!dir.exists(installPath)) {
        stopRuntimeIssue("the rpact package directory was not found",
            functionName = "setupPackageTests"
        )
    }

    if (!dir.exists(file.path(installPath, "tests"))) {
        stopRuntimeIssue("the rpact package directory does not contain a test directory",
            functionName = "setupPackageTests"
        )
    }

    if (!dir.exists(file.path(installPath, "tests", "testthat"))) {
        stopRuntimeIssue("the rpact package directory does not contain a testthat directory",
            functionName = "setupPackageTests"
        )
    }

    tmpDir <- tempdir()
    testPackage(
        outDir = tmpDir,
        connection = list(token = token, secret = secret),
        downloadTestsOnly = TRUE
    )
    tmpTestsDir <- file.path(tmpDir, "rpact-tests")
    if (!dir.exists(tmpTestsDir)) {
        stopRuntimeIssue("The test package directory was not created",
            parameter = "tmpTestsDir", value = tmpTestsDir,
            constraint = "existing test package directory",
            functionName = "setupPackageTests"
        )
    }

    testFiles <- list.files(tmpTestsDir, full.names = FALSE, recursive = TRUE)
    counter <- 0
    for (testFile in testFiles) {
        suppressWarnings(result <- file.copy(
            from = file.path(tmpTestsDir, testFile),
            to = file.path(installPath, "tests", testFile),
            overwrite = TRUE
        ))
        counter <- counter + result
    }
    if (length(testFiles) == counter) {
        defaultTestFile <- file.path(installPath, "tests", "testthat", "test-rpact.R")
        if (file.exists(defaultTestFile)) {
            file.remove(defaultTestFile)
        }
    }
    message(counter, " of ", length(testFiles), " test files copied to the rpact package directory")
    return(invisible(length(testFiles) == counter))
}
