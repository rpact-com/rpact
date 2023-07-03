## |
## |  *Unit tests helper functions*
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
## |  File version: $Revision: 7139 $
## |  Last changed: $Date: 2023-06-28 08:15:31 +0200 (Mi, 28 Jun 2023) $
## |  Last changed by: $Author: pahlke $
## |


.assertIsValidOutputFormatOptionValue <- function(optionKey, optionValue) {
    if (is.null(optionValue) || length(optionValue) == 0 || nchar(trimws(optionValue)) == 0) {
        return(invisible())
    }

    C_OUTPUT_FORMAT_ARGUMENTS <- rpact:::C_OUTPUT_FORMAT_ARGUMENTS
    C_ROUND_FUNCTIONS <- rpact:::C_ROUND_FUNCTIONS

    parts <- base::strsplit(optionValue, " *, *", fixed = FALSE)[[1]]

    if (length(parts) == 0) {
        stop(
            "the value (", optionValue, ") of output format option '", optionKey, "' is invalid"
        )
    }

    for (part in parts) {
        if (!grepl(" *= *", part)) {
            stop(
                "'", optionKey, "' (", part,
                ") must contain a valid argument-value-pair: \"argument = value\""
            )
        }

        keyValuePair <- base::strsplit(part, " *= *", fixed = FALSE)[[1]]

        if (length(keyValuePair) != 2) {
            stop(
                "'", optionKey,
                "' contains an invalid argument-value-pair: ", part
            )
        }

        key <- trimws(keyValuePair[1])
        if (nchar(key) == 0) {
            stop(
                "'", optionKey, "' contains an invalid argument"
            )
        }

        if (!(key %in% C_OUTPUT_FORMAT_ARGUMENTS)) {
            stop(
                "'", optionKey, "' contains an invalid argument: '", key, "'"
            )
        }

        value <- trimws(keyValuePair[2])
        if (nchar(value) == 0) {
            stop(
                "'", optionKey, "' contains an invalid value"
            )
        }

        if (key %in% c("digits", "nsmall")) {
            if (grepl("\\D", value)) {
                stop(
                    "the value (", value, ") of '", optionKey, "' must be an integer value"
                )
            }
        } else if (key %in% c("roundFunction")) {
            if (!(value %in% C_ROUND_FUNCTIONS)) {
                stop(
                    "the value (", value, ") of '", optionKey, "' must be one of these character values: ",
                    .arrayToString(C_ROUND_FUNCTIONS, encapsulate = TRUE)
                )
            }
        } else if (key %in% c("trimSingleZeros", "futilityProbabilityEnabled")) {
            if (!grepl("TRUE|FALSE", toupper(value))) {
                stop(
                    "the value (", value, ") of '", optionKey, "' must be a logical value"
                )
            }
        }
    }
}
