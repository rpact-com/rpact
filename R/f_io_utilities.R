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


.writeLinesToFile <- function(lines, fileName) {
    if (is.null(lines) || length(lines) == 0 || !is.character(lines)) {
        warning("Empty lines. Stop to write '", fileName, "'")
        return(invisible(fileName))
    }

    fileConn <- base::file(fileName)
    tryCatch(
        {
            base::writeLines(lines, fileConn)
        },
        finally = {
            base::close(fileConn)
        }
    )
    invisible(fileName)
}

#'
#' Windows: CR (Carriage Return \r) and LF (LineFeed \n) pair
#'
#' OSX, Linux: LF (LineFeed \n)
#'
#' @noRd
#'
.readLinesFromFile <- function(inputFileName) {
    content <- .readContentFromFile(inputFileName)
    return(strsplit(content, split = "(\r?\n)|(\r\n?)")[[1]])
}

.readContentFromFile <- function(inputFileName) {
    return(readChar(inputFileName, file.info(inputFileName)$size))
}

#' 
#' @title 
#' Write a key-value file (KEY=VALUE) from a named list
#'
#' @description 
#' Writes a human-editable text file in a widely used key-value format
#' (INI/.env-like): one entry per line in the form \code{KEY=VALUE}.
#' Blank lines and comment lines (starting with \code{#} or \code{;})
#' are allowed. Values are written as a single line; special characters
#' are escaped so that the file remains one key per line.
#'
#' @details 
#' Keys are restricted to \code{[A-Za-z0-9_.-]} to keep the file portable
#' and easy to edit.
#'
#' UTF-8 handling: Values are normalized to UTF-8 on write. The file is written
#' with UTF-8 encoding when supported by the platform. On Windows, UTF-8 is
#' enforced via \code{fileEncoding = "UTF-8"}. On other platforms, UTF-8 is
#' typically the native encoding; we still normalize strings to UTF-8.
#'
#' @param keyValueList A named list of scalar (length-1) atomic values.
#'   Supported value types: character, logical, integer, numeric.
#'   \code{NA} is supported and written as \code{NA}.
#' @param filePath Path to the output file (e.g. \code{"inst/tests/META.env"}).
#' @param writeHeader Logical; if \code{TRUE}, writes a short header comment.
#' @param sortKeys Logical; if \code{TRUE}, keys are written in alphabetical order.
#' @param overwrite Logical; if \code{FALSE} and the file exists, an error is raised.
#' @param ... Currently unused.
#'
#' @return Invisibly returns \code{filePath}.
#' 
#' @examples 
#' \dontrun{
#' keyValueList <- list(
#'     STUDY_NAME = "Trial A",
#'     MAX_PATIENTS = 150L,
#'     THRESHOLD = 0.075,
#'     NOTES = "First phase\nSecond phase"
#' )
#' filePath <- tempfile(fileext = ".txt")
#' writeKeyValueFile(
#'     keyValueList = keyValueList,
#'     filePath = filePath,
#'     writeHeader = TRUE,
#'     sortKeys = TRUE,
#'     overwrite = TRUE
#' )
#' }
#' 
#' @keywords internal
#' @export 
#' 
writeKeyValueFile <- function(
        keyValueList,
        filePath,
        ...,
        writeHeader = TRUE,
        sortKeys = FALSE,
        overwrite = TRUE
        ) {
    if (!is.list(keyValueList)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'keyValueList' must be a list()", 
            call. = FALSE)
    }

    keyNames <- names(keyValueList)
    if (is.null(keyNames) || any(!nzchar(keyNames))) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'keyValueList' must be a named list()", 
            call. = FALSE)
    }

    isInvalidValue <- vapply(
        keyValueList,
        FUN = function(value) {
            is.recursive(value) || length(value) != 1L || is.null(value)
        },
        FUN.VALUE = logical(1)
    )
    if (any(isInvalidValue)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "all values must be length-1 atomic (no lists/vectors). Problem keys: ",
            paste(names(keyValueList)[isInvalidValue], collapse = ", "),
            call. = FALSE
        )
    }

    if (file.exists(filePath) && !overwrite) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "file exists and overwrite = FALSE: ", sQuote(filePath), 
            call. = FALSE)
    }

    outputDirectory <- dirname(filePath)
    dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)

    if (sortKeys) {
        ordering <- order(keyNames)
        keyValueList <- keyValueList[ordering]
        keyNames <- keyNames[ordering]
    }

    normalizeToUtf8 <- function(text) {
        if (is.na(text)) {
            return(text)
        }
        enc2utf8(as.character(text))
    }

    escapeValue <- function(valueString) {
        valueString <- normalizeToUtf8(valueString)
        valueString <- gsub("\\\\", "\\\\\\\\", valueString) # \ -> \\
        valueString <- gsub("\n", "\\\\n", valueString, fixed = TRUE)
        valueString <- gsub("\r", "\\\\r", valueString, fixed = TRUE)
        valueString <- gsub("\t", "\\\\t", valueString, fixed = TRUE)
        valueString <- gsub("\"", "\\\\\"", valueString, fixed = TRUE)
        valueString
    }

    convertValueToString <- function(value) {
        if (length(value) != 1L) {
            stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
                "value must be length 1", call. = FALSE)
        }

        if (is.na(value)) {
            return("NA")
        }

        if (is.logical(value)) {
            if (isTRUE(value)) {
                return("true")
            } else {
                return("false")
            }
        }

        if (is.integer(value)) {
            return(format(value, scientific = FALSE, trim = TRUE))
        }

        if (is.numeric(value)) {
            return(format(value, scientific = FALSE, digits = 15, trim = TRUE))
        }

        escapeValue(value)
    }

    shouldQuoteValue <- function(valueString) {
        if (!nzchar(valueString)) {
            return(TRUE)
        }
        grepl("[[:space:]]|[=#;]", valueString)
    }

    outputLines <- character(0)

    if (writeHeader) {
        outputLines <- c(
            paste0(
                "# Generated by rpact::writeKeyValueFile() on ",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
            ),
            "# Format: KEY=VALUE (comments start with # or ;)",
            ""
        )
        outputLines <- enc2utf8(outputLines)
    }

    for (index in seq_along(keyNames)) {
        key <- keyNames[index]

        if (!grepl("^[A-Za-z0-9_.-]+$", key)) {
            stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
                "invalid key name '", key, "'. Allowed: A-Za-z0-9_.-",
                call. = FALSE
            )
        }

        valueString <- convertValueToString(keyValueList[[index]])

        if (shouldQuoteValue(valueString)) {
            valueString <- paste0("\"", valueString, "\"")
        }

        outputLines <- c(outputLines, paste0(key, "=", valueString))
    }

    # Write with UTF-8 file encoding (particularly important on Windows).
    # On Unix-alikes, this usually matches the native encoding; fileEncoding is
    # still accepted and helps ensure consistent behavior.
    con <- file(filePath, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(enc2utf8(outputLines), con = con, useBytes = TRUE)

    invisible(filePath)
}


#' 
#' @title 
#' Read a key-value file (KEY=VALUE) into a named list
#'
#' @description 
#' Reads a human-editable key-value file in a widely used format
#' (INI/.env-like): \code{KEY=VALUE}. Blank lines are ignored.
#' Full-line comments starting with \code{#} or \code{;} are ignored.
#' Inline comments are supported for unquoted values when preceded by whitespace,
#' e.g., \code{KEY=123 # comment}.
#'
#' @details 
#' Values can be quoted with double quotes. Escape sequences \code{\\n},
#' \code{\\r}, \code{\\t}, \code{\\\\}, and \code{\"} are supported.
#'
#' UTF-8 handling: The file is read as UTF-8 and all character values are
#' normalized to UTF-8 via \code{enc2utf8()}.
#'
#' @param filePath Path to the key-value file.
#' @param inferTypes Logical; if \code{TRUE}, attempts to convert values to
#'   logical/integer/numeric and \code{NA}. If \code{FALSE}, returns character values.
#' @param duplicateKeys How to handle duplicate keys: \code{"error"} (default),
#'   \code{"last"} (keep last occurrence), or \code{"first"} (keep first occurrence).
#' @param ... Currently unused.
#'
#' @return A named list with parsed values.
#' 
#' @examples 
#' \dontrun{
#' keyValueList <- list(
#'     STUDY_NAME = "Trial A",
#'     MAX_PATIENTS = 150L,
#'     THRESHOLD = 0.075,
#'     NOTES = "First phase\nSecond phase"
#' )
#' filePath <- tempfile(fileext = ".txt")
#' writeKeyValueFile(
#'     keyValueList = keyValueList,
#'     filePath = filePath,
#'     writeHeader = TRUE,
#'     sortKeys = TRUE,
#'     overwrite = TRUE
#' )
#' readKeyValueFile(filePath)
#' }
#' 
#' @keywords internal
#' @export 
#' 
readKeyValueFile <- function(
        filePath,
        ...,
        inferTypes = TRUE,
        duplicateKeys = c("error", "last", "first")
        ) {
    duplicateKeys <- match.arg(duplicateKeys)

    if (!file.exists(filePath)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "file not found: ", sQuote(filePath), call. = FALSE)
    }

    con <- file(filePath, open = "r", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    inputLines <- readLines(con, warn = FALSE)

    trimWhitespace <- function(text) {
        sub("^\\s+|\\s+$", "", text)
    }

    normalizeToUtf8 <- function(text) {
        if (is.na(text)) {
            return(text)
        }
        enc2utf8(as.character(text))
    }

    unescapeValue <- function(valueString) {
        valueString <- gsub("\\\\n", "\n", valueString, fixed = TRUE)
        valueString <- gsub("\\\\r", "\r", valueString, fixed = TRUE)
        valueString <- gsub("\\\\t", "\t", valueString, fixed = TRUE)
        valueString <- gsub("\\\\\"", "\"", valueString, fixed = TRUE)
        valueString <- gsub("\\\\\\\\", "\\\\", valueString)
        normalizeToUtf8(valueString)
    }

    parseValue <- function(valueString) {
        valueString <- trimWhitespace(valueString)

        isQuoted <- nchar(valueString) >= 2L &&
            substr(valueString, 1L, 1L) == "\"" &&
            substr(valueString, nchar(valueString), nchar(valueString)) == "\""

        if (isQuoted) {
            unquoted <- substr(valueString, 2L, nchar(valueString) - 1L)
            return(unescapeValue(unquoted))
        }

        # Remove inline comments for unquoted values: whitespace then # or ;
        withoutInlineComment <- sub("\\s+[;#].*$", "", valueString)
        withoutInlineComment <- trimWhitespace(withoutInlineComment)
        withoutInlineComment <- unescapeValue(withoutInlineComment)

        if (!inferTypes) {
            return(withoutInlineComment)
        }

        if (identical(withoutInlineComment, "NA")) {
            return(NA_character_)
        }

        lowerCase <- tolower(withoutInlineComment)
        if (lowerCase %in% c("true", "false")) {
            return(lowerCase == "true")
        }

        if (grepl("^[+-]?[0-9]+$", withoutInlineComment)) {
            return(as.integer(withoutInlineComment))
        }

        if (grepl("^[+-]?([0-9]+\\.[0-9]*|\\.[0-9]+|[0-9]+)([eE][+-]?[0-9]+)?$", withoutInlineComment)) {
            return(as.numeric(withoutInlineComment))
        }

        withoutInlineComment
    }

    resultList <- list()
    seenKeys <- character(0)

    for (line in inputLines) {
        trimmedLine <- trimWhitespace(line)

        if (!nzchar(trimmedLine)) {
            next
        }

        if (startsWith(trimmedLine, "#") || startsWith(trimmedLine, ";")) {
            next
        }

        equalsPosition <- regexpr("=", trimmedLine, fixed = TRUE)
        if (equalsPosition[1] == -1L) {
            next
        }

        key <- trimWhitespace(substr(trimmedLine, 1L, equalsPosition[1] - 1L))
        valuePart <- substr(trimmedLine, equalsPosition[1] + 1L, nchar(trimmedLine))

        if (!nzchar(key)) {
            next
        }

        if (!grepl("^[A-Za-z0-9_.-]+$", key)) {
            stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
                "invalid key name in file: ", sQuote(key), ". Allowed: A-Za-z0-9_.-", call. = FALSE)
        }

        if (key %in% seenKeys) {
            if (duplicateKeys == "error") {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
                    "duplicate key in file: ", sQuote(key), call. = FALSE)
            }
            if (duplicateKeys == "first") {
                next
            }
        } else {
            seenKeys <- c(seenKeys, key)
        }

        resultList[[key]] <- parseValue(valuePart)
    }

    resultList
}
