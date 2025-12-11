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
                    "failed to download unit test files (http): file ", sQuote(currentFile), " not found",
                    call. = FALSE
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
                "illegal 'token' / 'secret' or rpact version ", version, " unknown",
                call. = FALSE
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
            version <- .getPackageVersionString()
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
                "illegal 'token' / 'secret' or rpact version ", version, " unknown",
                call. = FALSE
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

.getReportAuthor <- function(token, secret, action) {
    tryCatch(
        {
            result <- base::readLines(sprintf(
                paste(c(
                    "https://rpact.shinyapps.io",
                    "metadata",
                    "?token=%s&secret=%s&action=%s"
                ), collapse = "/"),
                token, secret, action
            ))
            companyLine <- grep('<meta name="company"', result, value = TRUE)
            company <- sub('.*content="([^"]+)".*', "\\1", companyLine)
            company <- gsub("&amp;", "&", company)
            return(company)
        },
        error = function(e) {
            return("RPACT")
        }
    )
}

#'
#' @param title The title of the session information. If \code{NULL} (default), no title is displayed. Example: \code{"# Session Information"}.
#' @param headingLevel The heading level of the title. Default is \code{3}, which produces \code{###}.
#'
#' @noRd
#'
.getSessionInfoMarkdown <- function(title = NA_character_,
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
        getOption("rpact.system.identifier"),
        getSystemIdentifier(getOption("rpact.system.test.date"))
    ))
}

.setSystemIdentifier <- function() {
    date <- getOption("rpact.system.test.date")
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
    if (isFALSE(as.logical(base::getOption("rpact.startup.message.enabled")))) {
        return(FALSE)
    }

    lastShownTime <- base::getOption("rpact.startup.message.timestamp")
    currentTime <- Sys.time()
    if (is.null(lastShownTime) || !grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", lastShownTime)) {
        base::options("rpact.startup.message.timestamp" = currentTime)
        return(TRUE)
    }

    lastShownTime <- as.POSIXct(lastShownTime, tz = "UTC")
    timeDiff <- as.numeric(difftime(currentTime, lastShownTime, units = "mins"))

    # If the time difference is greater than 72 hours, show the message again
    if (timeDiff > 72 * 60) {
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
    if (isFALSE(as.logical(base::getOption("rpact.startup.message.enabled")))) {
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
    if (isTRUE(as.logical(base::getOption("rpact.startup.message.enabled")))) {
        return(invisible())
    }

    base::options("rpact.startup.message.enabled" = TRUE)
    saveOptions()
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

    message(
        "Installation qualification for rpact ", .getPackageVersionString(),
        " has not yet been performed. Please run testPackage() ",
        "before using the package in GxP relevant environments."
    )
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
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE,
            "the rpact package directory was not found",
            call. = FALSE
        )
    }

    if (!dir.exists(file.path(installPath, "tests"))) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE,
            "the rpact package directory does not contain a test directory",
            call. = FALSE
        )
    }

    if (!dir.exists(file.path(installPath, "tests", "testthat"))) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE,
            "the rpact package directory does not contain a testthat directory",
            call. = FALSE
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
        stop("The test package directory was not created", call. = FALSE)
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

.testInstalledBasicPackages <- function(scope = c("basic", "devel", "both", "internet", "all"), ..., headingLevel = 4) {
    scope <- match.arg(scope)
    tryCatch(
        {
            result <- utils::capture.output(
                tools::testInstalledBasic(scope = scope, outDir = file.path(R.home(), "tests")),
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
#' @inheritParams param_three_dots
#'
#' @details
#' This function is integral to the installation qualification (IQ) process of the \code{rpact} package,
#' ensuring it meets quality standards and functions as expected. A directory named \code{rpact-tests}
#' is created within the specified output directory, where all test files are downloaded from a secure
#' resource and executed. Results are saved in the file \code{testthat.Rout}, located in the
#' \code{rpact-tests} directory.
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
        keepSourceFiles = FALSE) {
    .assertTestthatIsInstalled()
    .assertIsSingleCharacter(outDir, "outDir", naAllowed = FALSE)
    .assertIsSingleLogical(completeUnitTestSetEnabled, "completeUnitTestSetEnabled", naAllowed = FALSE)
    .assertIsSingleCharacter(testFileDirectory, "testFileDirectory", naAllowed = TRUE)
    .assertIsSingleLogical(downloadTestsOnly, "downloadTestsOnly", naAllowed = FALSE)
    .assertIsSingleLogical(addWarningDetailsToReport, "addWarningDetailsToReport", naAllowed = FALSE)
    .assertIsSingleLogical(testInstalledBasicPackages, "testInstalledBasicPackages", naAllowed = FALSE)
    .assertIsSingleLogical(openHtmlReport, "openHtmlReport", naAllowed = FALSE)
    .assertIsSingleLogical(keepSourceFiles, "keepSourceFiles", naAllowed = FALSE)
    reportType <- match.arg(reportType)
    scope <- match.arg(scope)
    if (!dir.exists(outDir)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "test output directory '", outDir, "' does not exist"
        )
    }

    if (outDir != "." && !grepl("^([a-zA-Z]{1,1}:)?(/|\\\\)", outDir, ignore.case = TRUE)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "test output directory '", outDir, "' must be an absolute path"
        )
    }

    if (outDir == ".") {
        outDir <- getwd()
    }

    if (!is.na(testFileDirectory) && !dir.exists(testFileDirectory)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "test file directory '", testFileDirectory, "' does not exist"
        )
    }

    if (isTRUE(addWarningDetailsToReport)) {
        valueBefore <- Sys.getenv("NOT_CRAN")
        on.exit(Sys.setenv(NOT_CRAN = valueBefore), add = TRUE)
        Sys.setenv(NOT_CRAN = "true")
    }

    startTime <- Sys.time()

    if (!is.na(testFileDirectory) && !dir.exists(file.path(testFileDirectory, "testthat"))) {
        warning("'testFileDirectory' (", testFileDirectory, ") will be ignored ",
            "because it does not contain a 'testthat' subfolder",
            call. = FALSE
        )
        testFileDirectory <- NA_character_
    }

    Sys.setenv("LANGUAGE" = "EN")
    on.exit(Sys.unsetenv("LANGUAGE"), add = TRUE)

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
    credentialsAvailable <- (!is.null(token) && !is.null(secret) &&
        length(token) == 1 && length(secret) == 1 &&
        !is.na(token) && !is.na(secret))

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
    
    downloadOnlyModeEnabled <- is.na(testFileDirectory) &&
        isTRUE(downloadTestsOnly) && isTRUE(credentialsAvailable)

    if (!is.na(testFileDirectory)) {
        if (credentialsAvailable) {
            warning("The connection token and secret will be ignored ",
                "because 'testFileDirectory' is defined",
                call. = FALSE
            )
        }
        credentialsAvailable <- TRUE
    }

    if (is.na(testFileDirectory)) {
        if (credentialsAvailable) {
            if (downloadOnlyModeEnabled) {
                # download test files only
                executionMode <- "downloadOnly"
            } else {
                # download test files and run them
                executionMode <- "downloadAndRunTests"
            }
        } else {
            # run test from directory 'inst/tests'
            executionMode <- "default"
        }
    } else {
        # run test files from the user defined directory 'testFileDirectory'
        executionMode <- "runTestsInTestFileDirectory"
    }

    if (executionMode == "downloadOnly") {
        if (testInstalledBasicPackages) {
            warning("The test of installed R basic packages is not supported in 'downloadOnly' mode")
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
            cat("Have a break - it takes about 20 minutes.\n")
            cat("Exceution of all available unit tests startet at ",
                format(startTime, "%H:%M (%d-%B-%Y)"), "\n",
                sep = ""
            )
        } else {
            cat("Run a small subset of the available rpact unit tests. Please wait...\n")
            cat("This is just a quick test, i.e., all time consuming tests will be skipped.\n")
            cat("The test will take about a minute.\n")
        }
    }

    oldResultFiles <- .getExistingTestResultFiles(outDir)
    for (oldResultFile in oldResultFiles) {
        if (file.exists(oldResultFile)) {
            file.remove(oldResultFile)
        }
    }

    markdownReportFileName <- NULL
    pkgName <- "rpact"

    if (executionMode == "default") {
        testFileTargetDirectory <- file.path(find.package("rpact"), "tests")
    } else if (executionMode == "runTestsInTestFileDirectory") {
        testFileTargetDirectory <- testFileDirectory
    } else if (executionMode %in% c("downloadAndRunTests", "downloadOnly")) {
        testFileTargetDirectory <- file.path(outDir, paste0(pkgName, "-tests"))
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

    if (reportType %in% c("compact", "detailed")) {
        testthatMainFile <- file.path(testFileTargetDirectory, "testthat.R")
        if (file.exists(testthatMainFile)) {
            testthatMainFileBak <- paste0(testthatMainFile, ".bak")
            file.rename(testthatMainFile, testthatMainFileBak)
            on.exit(file.rename(testthatMainFileBak, testthatMainFile))
        }

        markdownReportFileName <- "rpact_test_result_report.md"
        testFileTargetDirectory <- gsub("\\\\", "/", testFileTargetDirectory)
        testthatCommands <- paste0(
            "\n",
            "library(testthat)\n",
            "library(rpact)\n\n",
            'test_check("rpact", ',
            "reporter = MarkdownReporter$new(",
            'outputFile = file.path("', testFileTargetDirectory, '", "', markdownReportFileName, '"), ',
            'outputSize = "', reportType, '", ',
            "openHtmlReport = ", openHtmlReport, ", ",
            "keepSourceFiles = ", keepSourceFiles, ", ",
            "testInstalledBasicPackages = ", testInstalledBasicPackages, ", ",
            'author = "', author, '", ',
            'scope = "', scope, '"',
            ")",
            ")\n\n"
        )
        cat(testthatCommands, file = testthatMainFile)
    }

    .testInstalledPackage(
        testFileDirectory = testFileTargetDirectory,
        pkgName = pkgName
    )

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
        cat("Test exceution ended at ",
            format(endTime, "%H:%M (%d-%B-%Y)"), "\n",
            sep = ""
        )
    }

    cat("Total runtime for testing: ", .getRuntimeString(startTime,
        endTime = endTime, runtimeUnits = "auto"
    ), ".\n", sep = "")

    reportFileNames <- NULL
    if (reportType != "Rout" && !is.null(markdownReportFileName)) {
        reportFileNames <- character()

        if (keepSourceFiles && file.exists(file.path(testFileTargetDirectory, markdownReportFileName))) {
            reportFileNames <- c(reportFileNames, markdownReportFileName)
        }

        reportFileNameHtml <- sub("\\.md$", ".html", markdownReportFileName)
        if (file.exists(file.path(testFileTargetDirectory, reportFileNameHtml))) {
            reportFileNames <- c(reportFileNames, reportFileNameHtml)
        }

        reportFileNamePdf <- sub("\\.md$", ".pdf", markdownReportFileName)
        if (file.exists(file.path(testFileTargetDirectory, reportFileNamePdf))) {
            reportFileNames <- c(reportFileNames, reportFileNamePdf)
        }

        if (length(reportFileNames) > 0) {
            file.copy(file.path(testFileTargetDirectory, reportFileNames), resultDir)
        }
    }

    minNumberOfExpectedTests <- .getMinNumberOfExpectedTests()
    totalNumberOfTests <- NA_integer_
    numberOfFailedTests <- 0
    numberOfSkippedTests <- 0
    resultOuputFile <- "testthat.Rout"
    inputFileName <- file.path(resultDir, resultOuputFile)
    resultMessage <- NA_character_
    statusMessage <- NA_character_
    status <- "incomplete"
    if (file.exists(inputFileName)) {
        fileContent <- base::readChar(inputFileName, file.info(inputFileName)$size)
        totalNumberOfTests <- .getTestthatResultNumberOfTests(fileContent)
        if (completeUnitTestSetEnabled && minNumberOfExpectedTests <= totalNumberOfTests) {
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
        resultMessage <- .getTestthatResultLine(fileContent)
        cat(resultMessage, "\n")
        cat("\n")
        .showResultsWereWrittenToDirectoryMessage(resultDir, reportFileNames, resultOuputFile)
        numberOfSkippedTests <- .getTestthatResultNumberOfSkippedTests(fileContent)
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
        cat("Please visit www.rpact.com to learn how to use rpact on FDA/GxP-compliant \n",
            "validated corporate computer systems and how to get a copy of the formal \n",
            "validation documentation that is customized and licensed for exclusive use \n",
            "by your company/organization, e.g., to fulfill regulatory requirements.\n",
            sep = ""
        )
    } else {
        inputFileName <- file.path(resultDir, "testthat.Rout.fail")
        status <- "failed"
        if (file.exists(inputFileName)) {
            fileContent <- base::readChar(inputFileName, file.info(inputFileName)$size)
            numberOfFailedTests <- .getTestthatResultNumberOfFailures(fileContent)
            if (completeUnitTestSetEnabled) {
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
            } else {
                cat(numberOfFailedTests, " unit test",
                    ifelse(numberOfFailedTests == 1, "", "s"), " failed :(\n",
                    sep = ""
                )
                statusMessage <- paste0("Installation qualification was not successful (subset with ", numberOfFailedTests, " failures)")
            }
            cat("Results:\n")
            resultMessage <- .getTestthatResultLine(fileContent)
            cat(resultMessage, "\n")
            .showResultsWereWrittenToDirectoryMessage(resultDir, reportFileNames, resultOuputFile)
        } else {
            cat("No test results found in directory '", resultDir, "'\n", sep = "")
            statusMessage <- "Installation qualification was not successful (no test results found)"
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
    cat("Test results were written to directory \n")
    if (!is.null(reportFileName)) {
        cat("'", resultDir, "' (see file", ifelse(length(reportFileName) == 1, "", "s"), " ",
            .arrayToString(sQuote(reportFileName), mode = "and"), ")\n",
            sep = ""
        )
    } else {
        cat("'", resultDir, "' (see file ", sQuote(resultOuputFile), ")\n", sep = "")
    }
}

.testInstalledPackage <- function(testFileDirectory, ..., pkgName = "rpact", Ropts = character()) {
    .assertIsSingleCharacter(testFileDirectory, "testFileDirectory", naAllowed = FALSE)
    if (!dir.exists(testFileDirectory)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'testFileDirectory' (", testFileDirectory, ") does not exist")
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
            warning("Failed to get the minimum number of expected tests: ", e$message)
        }
    )
    return(minNumberOfExpectedTestsDefault)
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
        initialize = function(...,
                outputSize = c("compact", "detailed"),
                outputFile = "test_results.md",
                addWarningDetailsToReport = TRUE,
                openHtmlReport = TRUE,
                keepSourceFiles = FALSE,
                testInstalledBasicPackages = TRUE,
                author = "RPACT",
                scope = "basic") {
            # self$capabilities$parallel_support <- TRUE
            super$initialize(...)
            self$outputSize <- match.arg(outputSize)
            self$outputFile <- outputFile
            self$addWarningDetailsToReport <- addWarningDetailsToReport
            self$openHtmlReport <- openHtmlReport
            self$keepSourceFiles <- keepSourceFiles
            self$testInstalledBasicPackages <- testInstalledBasicPackages
            self$author <- author
            self$scope <- scope
            self$minNumberOfExpectedTests <- .getMinNumberOfExpectedTests()
        },
        log = function(...) {
            args <- list(...)
            self$output <- c(self$output, paste(args, collapse = ""))
        },
        start_reporter = function() {
            self$startTime <- Sys.time()
            self$log("---")
            self$log('title: "Installation Qualification for rpact"')
            self$log('author: "', self$author, '"')
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
            self$log("- **rpact package release date**: ", format(packageDate("rpact"), "%B %d, %Y"))
            self$log("- **System user**: *", Sys.info()[["user"]], "*")
            self$log("- **System ID**: ", getSystemIdentifier(), "")
            self$log("")
            self$log(.getSessionInfoMarkdown())
            self$log("")

            basicPackageTestResult <- NULL
            if (self$testInstalledBasicPackages) {
                basicPackageTestResult <- .testInstalledBasicPackages(scope = self$scope, headingLevel = 4)
            }

            if (!is.null(basicPackageTestResult)) {
                self$log("## R Basic Package Test Results")
                self$log("")
                self$log(basicPackageTestResult)
                self$log("")
            }

            self$log("## Test Results")
            self$log("")
        },
        start_file = function(file) {
            self$fileName <- file
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
                self$fileName, ", line ", result$srcref[1], "."
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
            self$endTime <- Sys.time()

            self$log("")
            self$log("## Summary")
            self$log("")
            self$log("The test ended at ", format(self$endTime, "%X"), " on ", format(self$endTime, "%B %d, %Y"), ".")
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
            if (self$numberOfFailures == 0) {
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
            citePackage <- trimws(citePackage[!grepl("(^$)|(^To cite package)", citePackage)])
            citePackage <- paste(citePackage, collapse = " ")
            self$log("- ", citePackage)
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
                    warning("Failed to create directory ", sQuote(sourcePath), ". ",
                        "Source files will be saved to ", sQuote(outputPath), ".",
                        call. = FALSE
                    )
                    sourcePath <- outputPath
                }
            }

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
                    warning("Failed to render ", sQuote(self$outputFile),
                        " to html: ", e$message,
                        call. = FALSE
                    )
                }
            )

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
                    warning("Failed to render ", sQuote(mdFileForTex), " to pdf: ", e$message, call. = FALSE)
                }
            )
        }
    )
)
