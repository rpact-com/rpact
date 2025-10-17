## |
## |  *Logger*
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

.logBase <- function(s, ..., logLevel) {
    .assertIsSingleCharacter(s, "s")
    if (length(list(...)) > 0) {
        cat(paste0("[", logLevel, "]"), sprintf(s, ...), "\n")
    } else {
        cat(paste0("[", logLevel, "]"), s, "\n")
    }
}

.logTrace <- function(s, ...) {
    if (getLogLevel() == C_LOG_LEVEL_TRACE) {
        .logBase(s, ..., logLevel = C_LOG_LEVEL_TRACE)
    }
}

.logDebug <- function(s, ...) {
    if (getLogLevel() %in% c(C_LOG_LEVEL_TRACE, C_LOG_LEVEL_DEBUG)) {
        .logBase(s, ..., logLevel = C_LOG_LEVEL_DEBUG)
    }
}

.logInfo <- function(s, ...) {
    if (getLogLevel() %in% c(
            C_LOG_LEVEL_TRACE,
            C_LOG_LEVEL_DEBUG, C_LOG_LEVEL_INFO
        )) {
        .logBase(s, ..., logLevel = C_LOG_LEVEL_INFO)
    }
}

.logWarn <- function(s, ...) {
    if (getLogLevel() %in% c(
            C_LOG_LEVEL_TRACE, C_LOG_LEVEL_DEBUG,
            C_LOG_LEVEL_INFO, C_LOG_LEVEL_WARN
        )) {
        .logBase(s, ..., logLevel = C_LOG_LEVEL_WARN)
    }
}

.logError <- function(s, ...) {
    if (getLogLevel() %in% c(
            C_LOG_LEVEL_TRACE, C_LOG_LEVEL_DEBUG,
            C_LOG_LEVEL_INFO, C_LOG_LEVEL_WARN, C_LOG_LEVEL_ERROR
        )) {
        .logBase(s, ..., logLevel = C_LOG_LEVEL_ERROR)
    }
}

.getRuntimeString <- function(startTime, ..., endTime = Sys.time(), runtimeUnits = c("secs", "auto"), addBrackets = FALSE) {
    runtimeUnits <- match.arg(runtimeUnits)
    if (runtimeUnits == "secs") {
        time <- as.numeric(difftime(endTime, startTime, units = "secs"))
        time <- round(time, ifelse(time < 1, 4, 2))
        timeStr <- paste0(time, " secs")
    } else {
        timeStr <- format(difftime(endTime, startTime))
    }
    if (addBrackets) {
        timeStr <- paste0("[", timeStr, "]")
    }
    return(timeStr)
}

.logProgress <- function(s, ..., startTime, runtimeUnits = c("secs", "auto"), enforceLogging = FALSE) {
    if (isFALSE(enforceLogging) && !interactive()) {
        return(invisible())
    }
    
    if (!(getLogLevel() %in% c(
            C_LOG_LEVEL_TRACE, C_LOG_LEVEL_DEBUG,
            C_LOG_LEVEL_INFO, C_LOG_LEVEL_WARN,
            C_LOG_LEVEL_ERROR, C_LOG_LEVEL_PROGRESS
        ))) {
        return(invisible())
    }
    
    if (.isMarkdownEnabled()) {
        return(invisible())
    }

    runtimeUnits <- match.arg(runtimeUnits)
    timeStr <- .getRuntimeString(startTime, runtimeUnits = runtimeUnits, addBrackets = TRUE)
    if (length(list(...)) > 0) {
        cat(paste0("[", C_LOG_LEVEL_PROGRESS, "]"), sprintf(s, ...), timeStr, "\n")
    } else {
        cat(paste0("[", C_LOG_LEVEL_PROGRESS, "]"), s, timeStr, "\n")
    }
}

#'
#' @title
#' Set Log Level
#'
#' @description
#' Sets the \code{rpact} log level.
#'
#' @param logLevel The new log level to set. Can be one of
#'        "PROGRESS", "ERROR", "WARN", "INFO", "DEBUG", "TRACE", "DISABLED".
#'        Default is "PROGRESS".
#'
#' @details
#' This function sets the log level of the \code{rpact} internal log message system.
#' By default only calculation progress messages will be shown on the output console,
#' particularly \code{\link[=getAnalysisResults]{getAnalysisResults()}} shows this kind of messages.
#' The output of these messages can be disabled by setting the log level to \code{"DISABLED"}.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=getLogLevel]{getLogLevel()}} for getting the current log level,
#'   \item \code{\link[=resetLogLevel]{resetLogLevel()}} for resetting the log level to default.
#' }
#'
#' @examples
#' \dontrun{
#' # show debug messages
#' setLogLevel("DEBUG")
#'
#' # disable all log messages
#' setLogLevel("DISABLED")
#' }
#'
#' @keywords internal
#'
#' @export
#'
setLogLevel <- function(logLevel = c(
            "PROGRESS", "ERROR", "WARN",
            "INFO", "DEBUG", "TRACE", "DISABLED"
        )) {
    logLevel <- match.arg(logLevel)

    if (!is.character(logLevel) || !(logLevel %in% c(
            C_LOG_LEVEL_TRACE,
            C_LOG_LEVEL_DEBUG,
            C_LOG_LEVEL_INFO,
            C_LOG_LEVEL_WARN,
            C_LOG_LEVEL_ERROR,
            C_LOG_LEVEL_PROGRESS,
            C_LOG_LEVEL_DISABLED
        ))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'logLevel' must be one of ",
            "c(", paste(paste0("'", c(
                C_LOG_LEVEL_TRACE,
                C_LOG_LEVEL_DEBUG,
                C_LOG_LEVEL_INFO,
                C_LOG_LEVEL_WARN,
                C_LOG_LEVEL_ERROR,
                C_LOG_LEVEL_PROGRESS,
                C_LOG_LEVEL_DISABLED
            ), "'"), collapse = ", "), ")"
        )
    }

    Sys.setenv("RPACT_LOG_LEVEL" = logLevel)
}

#'
#' @title
#' Get Log Level
#'
#' @description
#' Returns the current \code{rpact} log level.
#'
#' @details
#' This function gets the log level of the \code{rpact} internal log message system.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=setLogLevel]{setLogLevel()}} for setting the log level,
#'   \item \code{\link[=resetLogLevel]{resetLogLevel()}} for resetting the log level to default.
#' }
#'
#' @return Returns a \code{\link[base]{character}} of length 1 specifying the current log level.
#'
#' @examples
#' # show current log level
#' getLogLevel()
#'
#' @keywords internal
#'
#' @export
#'
getLogLevel <- function() {
    logLevel <- Sys.getenv("RPACT_LOG_LEVEL")
    if (logLevel == "") {
        logLevel <- C_LOG_LEVEL_PROGRESS
        Sys.setenv("RPACT_LOG_LEVEL" = logLevel)
    }
    return(logLevel)
}

#'
#' @title
#' Reset Log Level
#'
#' @description
#' Resets the \code{rpact} log level.
#'
#' @details
#' This function resets the log level of the \code{rpact} internal log message
#' system to the default value \code{"PROGRESS"}.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=getLogLevel]{getLogLevel()}} for getting the current log level,
#'   \item \code{\link[=setLogLevel]{setLogLevel()}} for setting the log level.
#' }
#'
#' @examples
#' \dontrun{
#' # reset log level to default value
#' resetLogLevel()
#' }
#'
#' @keywords internal
#'
#' @export
#'
resetLogLevel <- function() {
    setLogLevel(C_LOG_LEVEL_PROGRESS)
}
