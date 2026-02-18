## |
## |  *Dictionary class*
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

createDictionary <- function(name, keyValuePairList = NULL) {
    result <- new.env(parent = emptyenv())
    class(result) <- "Dictionary"
    attr(result, "name") <- name
    if (!is.null(keyValuePairList)) {
        initDictionary(result, keyValuePairList)
    }
    return(result)
}

.assertIsDictionary <- function(x) {
    if (is.null(x)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'x' must be a valid 'Dictionary' (is NULL)",
            call. = FALSE
        )
    }
    if (!inherits(x, "Dictionary")) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'x' must be an instance of class 'Dictionary' (is ", .getClassName(x), ")",
            call. = FALSE
        )
    }
}

#'
#' @examples
#' \dontrun{
#' x <- createDictionary("x", list(a = 1, b = 2, c = 3))
#' getDictionaryKeyByValue(x, 2)
#' }
#'
#' @noRd
#'
getDictionaryKeyByValue <- function(x, value) {
    .assertIsDictionary(x)
    keys <- names(x)
    values <- as.vector(x)
    return(unique(keys[values == value]))
}

#'
#' @examples
#' \dontrun{
#' x <- createDictionary("x", list(a = 1, b = 2, c = 3))
#' getDictionarySubset(x, c("b", "c"))
#' }
#'
#' @noRd
#'
getDictionarySubset <- function(x, subsetNames) {
    .assertIsDictionary(x)
    result <- createDictionary(attr(x, "name"))
    for (objName in base::ls(envir = x)) {
        if (objName %in% subsetNames) {
            base::assign(objName, x[[objName]], envir = result)
        }
    }
    return(result)
}

cloneDictionary <- function(x) {
    .assertIsDictionary(x)
    result <- createDictionary(attr(x, "name"))
    for (objName in base::ls(envir = x)) {
        base::assign(objName, x[[objName]], envir = result)
    }
    return(result)
}

#'
#' @export
#' @noRd
#'
as.vector.Dictionary <- function(x, ...) {
    .assertIsDictionary(x)
    vec <- character()
    for (key in names(x)) {
        vec <- c(vec, base::get(key, envir = x))
    }
    return(vec)
}

#'
#' @export
#' @noRd
#'
as.list.Dictionary <- function(x, ...) {
    .assertIsDictionary(x)
    result <- list()
    for (key in names(x)) {
        result[[key]] <- base::get(key, envir = x)
    }
    return(result)
}

#'
#' @export
#' @noRd
#'
print.Dictionary <- function(x, ...) {
    .assertIsDictionary(x)
    cat(attr(x, "name"), "\n")
    for (key in names(x)) {
        cat("$", key, "\n", sep = "")
        print(base::get(key, envir = x))
        cat("\n")
    }
}

initDictionary <- function(x, keyValuePairList) {
    .assertIsDictionary(x)
    if (is.null(keyValuePairList) || length(keyValuePairList) == 0 || !is.list(keyValuePairList)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'keyValuePairList' must be a valid list", call. = FALSE)
    }

    if (any(names(keyValuePairList) == "")) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'keyValuePairList' must be a named list", call. = FALSE)
    }

    for (key in names(keyValuePairList)) {
        value <- keyValuePairList[[key]]
        addValueToDictionary(x, key, value)
    }
}

addValueToDictionary <- function(x, key, value) {
    .assertIsDictionary(x)
    if (base::exists(key, envir = x)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "dictionary ", base::sQuote(base::attr(x, "name")), " already contains key ", base::sQuote(key)
        )
    }
    setValueToDictionary(x, key, value)
}

setValueToDictionary <- function(x, key, value) {
    .assertIsDictionary(x)
    base::assign(key, value, envir = x)
}

getValueFromDictionary <- function(x, key) {
    .assertIsDictionary(x)
    if (!base::exists(key, envir = x)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "dictionary ", base::sQuote(base::attr(x, "name")), " does not contain key ", base::sQuote(key)
        )
    }

    return(base::get(key, envir = x))
}
