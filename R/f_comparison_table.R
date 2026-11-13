## |
## |  rpact: Confirmatory Adaptive Clinical Trial Design and Analysis
## |  This file is part of the rpact R package.
## |
## |  RPACT - Statistical Tools for Drug Development
## |
## |  Authors: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  SPDX-FileCopyrightText: RPACT GmbH, Germany <info@rpact.com>
## |  SPDX-License-Identifier: LGPL-3.0-only
## |
## |  Licensed under the GNU Lesser General Public License, version 3.
## |  License text: https://www.gnu.org/licenses/lgpl-3.0.html
## |
## |  Package documentation: https://www.rpact.org
## |  Resources and customer portal: https://connect.rpact.app
## |  Consulting and services: https://www.rpact.com
## |  Contact: info@rpact.com
## |

#'
#' @title
#' Render a Trial Comparison Table in knitr
#'
#' @description
#' Renders data frames prepared by [as.data.frame.TrialComparison()] with
#' `output = "auto"`, `"html"`, or `"pdf"`. HTML tables scroll horizontally.
#' LaTeX tables use longtable and split wide results into column blocks,
#' repeating the identifier columns in each block.
#'
#' @param x A `TrialComparisonTable` returned by [as.data.frame.TrialComparison()].
#' @param ... Further arguments passed to the ordinary knitr data-frame renderer
#'   when the target is neither HTML nor LaTeX.
#'
#' @return A `knitr::asis_output` object for HTML or LaTeX. Other targets use
#'   the ordinary knitr data-frame representation. Empty tables emit no output.
#'
#' @details
#' PDF mode emits a LaTeX fragment for the surrounding document. It requires
#' the suggested rmarkdown package and declares the longtable dependency via
#' knitr metadata. No PDF file is created directly by this method.
#'
#' @export
#'
knit_print.TrialComparisonTable <- function(x, ...) {
    output <- attr(x, "comparisonTableOutput")
    identifiers <- intersect(attr(x, "comparisonTableIdentifiers"), names(x))
    caption <- attr(x, "comparisonTableCaption")
    class(x) <- "data.frame"
    if (!nrow(x) || !ncol(x)) {
        return(knitr::asis_output(""))
    }
    if (identical(output, "auto")) {
        output <- if (knitr::is_latex_output()) {
            "pdf"
        } else if (knitr::is_html_output()) {
            "html"
        } else {
            "data.frame"
        }
    }
    if (identical(output, "data.frame")) {
        return(knitr::knit_print(x, ...))
    }
    if (identical(output, "html")) {
        table <- knitr::kable(x, format = "html", row.names = FALSE, digits = Inf,
            caption = caption, escape = TRUE)
        return(knitr::asis_output(paste0(
            '<div style="overflow-x:auto;max-width:100%;margin-bottom:1em;">\n',
            paste(table, collapse = "\n"), "\n</div>\n"
        )))
    }
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        stopRuntimeIssue("LaTeX comparison tables require the suggested package 'rmarkdown'",
            functionName = "knit_print.TrialComparisonTable",
            diagnosticId = "comparison.table_rmarkdown_missing",
            context = list(output = output, requiredPackage = "rmarkdown"))
    }
    blocks <- .splitTrialComparisonTable(x, identifiers)
    tables <- vapply(seq_along(blocks), function(i) {
        data <- x[blocks[[i]]]
        width <- format(0.95 / ncol(data), scientific = FALSE, decimal.mark = ".", trim = TRUE)
        align <- rep(paste0(">{\\raggedright\\arraybackslash}p{\\dimexpr ", width,
            "\\linewidth-2\\tabcolsep-2\\arrayrulewidth\\relax}"), ncol(data))
        title <- caption
        if (length(blocks) > 1L) {
            title <- paste0(title, " (", i, "/", length(blocks), ")")
        }
        table <- paste(knitr::kable(data, format = "latex", longtable = TRUE, digits = Inf,
            row.names = FALSE, align = align, caption = title, label = NA_character_,
            booktabs = FALSE, escape = TRUE), collapse = "\n")
        tableLines <- strsplit(table, "\n", fixed = TRUE)[[1L]]
        separators <- which(trimws(tableLines) == "\\hline")
        headerEnd <- separators[2L]
        repeatedHeader <- tableLines[seq.int(separators[1L], headerEnd)]
        tableLines <- c(tableLines[seq_len(headerEnd)], "\\endfirsthead",
            repeatedHeader, "\\endhead", tableLines[seq.int(headerEnd + 1L, length(tableLines))])
        return(.breakTrialComparisonLatexTokens(paste(tableLines, collapse = "\n")))
    }, character(1))
    return(knitr::asis_output(paste(tables, collapse = "\n\n"),
        meta = list(rmarkdown::latex_dependency("longtable"), rmarkdown::latex_dependency("array"))))
}

.breakTrialComparisonLatexTokens <- function(text) {
    # Break long cell tokens without touching escaped LaTeX commands or omitting text.
    positions <- gregexpr("(?<!\\\\)\\b[[:alnum:]]{13,}\\b", text, perl = TRUE)
    tokens <- regmatches(text, positions)
    replacements <- lapply(tokens, function(items) {
        return(vapply(items, function(token) {
            starts <- seq.int(1L, nchar(token), by = 6L)
            return(paste(substring(token, starts, pmin(starts + 5L, nchar(token))),
                collapse = "\\allowbreak{}"))
        }, character(1)))
    })
    regmatches(text, positions) <- replacements
    return(text)
}

#'
#' @title
#' Subset a Trial Comparison Table
#'
#' @description
#' Subsets a reporting data frame while preserving its rendering attributes.
#'
#' @param x A `TrialComparisonTable`.
#' @param i,j Row and column indices, as for a data frame.
#' @param ... Further arguments passed to the data-frame subsetting method.
#' @param drop Whether to simplify the result, as for a data frame.
#'
#' @return A subset retaining its reporting metadata when the result is a data frame.
#'
#' @export
#'
`[.TrialComparisonTable` <- function(x, i, j, ..., drop = FALSE) {
    result <- NextMethod("[")
    if (is.data.frame(result)) {
        class(result) <- c("TrialComparisonTable", "data.frame")
        for (name in c("comparisonTableOutput", "comparisonTableIdentifiers", "comparisonTableCaption")) {
            attr(result, name) <- attr(x, name)
        }
        attr(result, "comparisonTableIdentifiers") <-
            intersect(attr(result, "comparisonTableIdentifiers"), names(result))
    }
    return(result)
}

.splitTrialComparisonTable <- function(data, identifiers, widthBudget = 85L) {
    columnWidth <- function(name) {
        return(max(8L, min(30L, max(nchar(c(name, as.character(data[[name]]))), na.rm = TRUE))))
    }
    keyWidth <- sum(vapply(identifiers, columnWidth, numeric(1)))
    current <- identifiers
    cost <- keyWidth
    blocks <- list()
    for (name in setdiff(names(data), identifiers)) {
        width <- columnWidth(name)
        if (cost + width > widthBudget && length(setdiff(current, identifiers)) > 0L) {
            blocks[[length(blocks) + 1L]] <- current
            current <- identifiers
            cost <- keyWidth
        }
        current <- c(current, name)
        cost <- cost + width
    }
    blocks[[length(blocks) + 1L]] <- current
    return(lapply(blocks, function(columns) {
        return(names(data)[names(data) %in% columns])
    }))
}
