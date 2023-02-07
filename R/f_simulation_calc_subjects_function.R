## |
## |  *User defined calc subjects function for simulation*
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
## |  File version: $Revision: 6695 $
## |  Last changed: $Date: 2022-11-18 09:15:48 +0100 (Fri, 18 Nov 2022) $
## |  Last changed by: $Author: pahlke $
## |

C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_MEANS_ARGUMENTS <- list(
    stage = "int",
    meanRatio = "bool", 
    thetaH0 = "double", 
    groups = "int", 
    plannedSubjects = "NumericVector",
    allocationRatioPlanned = "NumericVector",
    minNumberOfSubjectsPerStage = "NumericVector",
    maxNumberOfSubjectsPerStage = "NumericVector",
    sampleSizesPerStage = "NumericVector",
    thetaH1 = "double",
    stDevH1 = "double",
    conditionalPower = "double",
    conditionalCriticalValue = "double"
)

C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_MEANS_CPP_CODE <- c(
    "#include <Rcpp.h>",
    "#include <cmath>",
    "using namespace Rcpp;",
    "typedef double (*calcSubjectsFunctionMeansPtrTemp)(",
    "    int stage, ",
    "    bool meanRatio, ",
    "    double thetaH0, ",
    "    int groups, ",
    "    NumericVector plannedSubjects, ",
    "    NumericVector allocationRatioPlanned, ",
    "    NumericVector minNumberOfSubjectsPerStage, ",
    "    NumericVector maxNumberOfSubjectsPerStage, ",
    "    NumericVector sampleSizesPerStage, ",
    "    double thetaH1, ",
    "    double stDevH1, ",
    "    double conditionalPower, ",
    "    double conditionalCriticalValue); ",
    "double getSimulationMeansStageSubjectsTemp( ",
    "        int stage, ",
    "        bool meanRatio, ",
    "        double thetaH0, ",
    "        int groups, ",
    "        NumericVector plannedSubjects, ",
    "        NumericVector allocationRatioPlanned, ",
    "        NumericVector minNumberOfSubjectsPerStage, ",
    "        NumericVector maxNumberOfSubjectsPerStage, ",
    "        NumericVector sampleSizesPerStage, ",
    "        double thetaH1, ",
    "        double stDevH1, ",
    "        double conditionalPower, ",
    "        double conditionalCriticalValue) { ",
    "    {USER_CODE}",
    "} ",
    "// [[Rcpp::export]] ",
    "Rcpp::XPtr<calcSubjectsFunctionMeansPtrTemp> calcSubjectsFunctionCppTemp() { ",
    "    return Rcpp::XPtr<calcSubjectsFunctionMeansPtrTemp>(",
    "         new calcSubjectsFunctionMeansPtrTemp(&getSimulationMeansStageSubjectsTemp));",
    "}"
)

C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_RATES_ARGUMENTS <- list(
    stage = "int",
    riskRatio = "bool", 
    thetaH0 = "double", 
    groups = "int", 
    plannedSubjects = "NumericVector",
    directionUpper = "bool", 
    allocationRatioPlanned = "NumericVector",
    minNumberOfSubjectsPerStage = "NumericVector",
    maxNumberOfSubjectsPerStage = "NumericVector",
    sampleSizesPerStage = "NumericVector",
    conditionalPower = "NumericVector",
    overallRate = "NumericVector",
    conditionalCriticalValue = "double",
    farringtonManningValue1 = "double",
    farringtonManningValue2 = "double"
)

C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_RATES_CPP_CODE <- c(
    "#include <Rcpp.h>",
    "#include <cmath>",
    "using namespace Rcpp;",
    "typedef double (*calcSubjectsFunctionRatesPtrTemp)(",
    "    int stage, ",
    "    bool riskRatio, ",
    "    double thetaH0, ",
    "    int groups, ",
    "    NumericVector plannedSubjects, ",
    "    bool directionUpper, ",
    "    NumericVector allocationRatioPlanned, ",
    "    NumericVector minNumberOfSubjectsPerStage, ",
    "    NumericVector maxNumberOfSubjectsPerStage, ",
    "    NumericVector sampleSizesPerStage, ",
    "    NumericVector conditionalPower, ",
    "    NumericVector overallRate, ",
    "    double conditionalCriticalValue, ",
    "    double farringtonManningValue1, ",
    "    double farringtonManningValue2); ",
    "double getSimulationRatesStageSubjectsTemp(",
    "        int stage, ",
    "        bool riskRatio, ",
    "        double thetaH0, ",
    "        int groups, ",
    "        NumericVector plannedSubjects, ",
    "        bool directionUpper, ",
    "        NumericVector allocationRatioPlanned, ",
    "        NumericVector minNumberOfSubjectsPerStage, ",
    "        NumericVector maxNumberOfSubjectsPerStage, ",
    "        NumericVector sampleSizesPerStage, ",
    "        NumericVector conditionalPower, ",
    "        NumericVector overallRate, ",
    "        double conditionalCriticalValue, ",
    "        double farringtonManningValue1, ",
    "        double farringtonManningValue2) { ",
    "    {USER_CODE}",
    "} ",
    "// [[Rcpp::export]] ",
    "Rcpp::XPtr<calcSubjectsFunctionRatesPtrTemp> calcSubjectsFunctionCppTemp() { ",
    "    return Rcpp::XPtr<calcSubjectsFunctionRatesPtrTemp>(",
    "         new calcSubjectsFunctionRatesPtrTemp(&getSimulationRatesStageSubjectsTemp));",
    "}"
)

C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_SURVIVAL_ARGUMENTS <- list(
    stage = "int",
    conditionalPower = "double", 
    thetaH0 = "double", 
    estimatedTheta = "double", 
    plannedEvents = "NumericVector",
    eventsOverStages = "NumericVector", 
    minNumberOfEventsPerStage = "NumericVector",
    maxNumberOfEventsPerStage = "NumericVector",
    allocationRatioPlanned = "double",
    conditionalCriticalValue = "double"
)

C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_SURVIVAL_CPP_CODE <- c(
    "#include <Rcpp.h>",
    "#include <cmath>",
    "using namespace Rcpp;",
    "typedef double (*calcEventsFunctionSurvivalPtrTemp)(",
    "    int stage, ",
    "    double conditionalPower, ",
    "    double thetaH0, ",
    "    double estimatedTheta, ",
    "    NumericVector plannedEvents, ",
    "    NumericVector eventsOverStages, ",
    "    NumericVector minNumberOfEventsPerStage, ",
    "    NumericVector maxNumberOfEventsPerStage, ",
    "    double allocationRatioPlanned, ",
    "    double conditionalCriticalValue); ",
    "double getSimulationSurvivalStageEventsTemp(",
    "        int stage, ",
    "        double conditionalPower, ",
    "        double thetaH0, ",
    "        double estimatedTheta, ",
    "        NumericVector plannedEvents, ",
    "        NumericVector eventsOverStages, ",
    "        NumericVector minNumberOfEventsPerStage, ",
    "        NumericVector maxNumberOfEventsPerStage, ",
    "        double allocationRatioPlanned, ",
    "        double conditionalCriticalValue) { ",
    "    {USER_CODE}",
    "} ",
    "// [[Rcpp::export]] ",
    "Rcpp::XPtr<calcEventsFunctionSurvivalPtrTemp> calcEventsFunctionCppTemp() { ",
    "    return Rcpp::XPtr<calcEventsFunctionSurvivalPtrTemp>(",
    "         new calcEventsFunctionSurvivalPtrTemp(&getSimulationSurvivalStageEventsTemp));",
    "}"
)

C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_MEANS <- "base_means"
C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_RATES <- "base_rates"
C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_SURVIVAL <- "base_survival"

C_SIMULATION_CALC_SUBJECTS_FUNCTION_CPP_CODE <- list()
C_SIMULATION_CALC_SUBJECTS_FUNCTION_CPP_CODE[[C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_MEANS]] <- 
    C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_MEANS_CPP_CODE
C_SIMULATION_CALC_SUBJECTS_FUNCTION_CPP_CODE[[C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_RATES]] <- 
    C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_RATES_CPP_CODE
C_SIMULATION_CALC_SUBJECTS_FUNCTION_CPP_CODE[[C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_SURVIVAL]] <- 
    C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_SURVIVAL_CPP_CODE

C_SIMULATION_CALC_SUBJECTS_FUNCTION_ARGUMENTS <- list()
C_SIMULATION_CALC_SUBJECTS_FUNCTION_ARGUMENTS[[C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_MEANS]] <- 
    C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_MEANS_ARGUMENTS
C_SIMULATION_CALC_SUBJECTS_FUNCTION_ARGUMENTS[[C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_RATES]] <- 
    C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_RATES_ARGUMENTS
C_SIMULATION_CALC_SUBJECTS_FUNCTION_ARGUMENTS[[C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_SURVIVAL]] <- 
    C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_SURVIVAL_ARGUMENTS

.regexprCalcSubjectsFunction <- function(pattern, cmd, ..., language = c("cpp", "R")) {
    language <- match.arg(language)
    x1 <- regexpr(pattern, cmd)
    if (x1 == -1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            ifelse(language == "cpp",
                "the function definition must match 'double myFunctionName(myArgs) { myCode; }'",
                "the function definition must match 'myFunctionName <- (myArgs) { myCode }'"
            )
        )
    }
    len <- attr(x1, "match.length")
    return(list(value = x1, len = len))
}

.isCppCode <- function(code) {
    if (is.null(code) || length(code) == 0 || all(is.na(code)) || !is.character(code)) {
        return(FALSE)
    }

    return(any(grepl("(int|bool|double|NumericVector) +", code)))
}

.getCalcSubjectsFunctionRCode <- function(cmd, cppCodeBodyType) {
    .assertIsCharacter(cmd, "cmd")
    .assertIsCharacter(cppCodeBodyType, "cppCodeBodyType")
    
    cmd <- paste0(cmd, collapse = "\n")
    cmd <- trimws(cmd)
    
    validArgsList <- C_SIMULATION_CALC_SUBJECTS_FUNCTION_ARGUMENTS[[cppCodeBodyType]]
    validArgs <- names(validArgsList)

    fun <- eval(parse(text = cmd))
    args <- methods::formalArgs(fun)
    args <- args[args != "..."]
    if (!all(args %in% validArgs)) {
        invalidArgs <- args[!(args %in% validArgs)]
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "the argument", ifelse(length(invalidArgs) == 1, "", "s"),
            " ", .arrayToString(invalidArgs, encapsulate = TRUE), " ",
            ifelse(length(invalidArgs) == 1, "is", "are"), " invalid (the ", length(validArgs),
            " valid arguments can be found in the reference manual)"
        )
    }

    bodyStartIndex <- .regexprCalcSubjectsFunction("\\{", cmd, language = "R")$value
    functionBody <- substring(cmd, bodyStartIndex, nchar(cmd))
    functionCmd <- paste0("function(..., ", paste0(validArgs, collapse = ", "), ") ", functionBody)
    return(eval(parse(text = functionCmd)))
}

.getCalcSubjectsFunctionCppCode <- function(cmd, cppCodeBodyType) {
    .assertIsCharacter(cmd, "cmd")
    .assertIsCharacter(cppCodeBodyType, "cppCodeBodyType")

    cppCodeBody <- ""
    if (length(cppCodeBodyType) == 1 && cppCodeBodyType %in% names(C_SIMULATION_CALC_SUBJECTS_FUNCTION_CPP_CODE)) {
        cppCodeBody <- C_SIMULATION_CALC_SUBJECTS_FUNCTION_CPP_CODE[[cppCodeBodyType]]
    }

    cppCodeBody <- paste0(cppCodeBody, collapse = "\n")
    if (!grepl("#include <Rcpp.h>", cppCodeBody)) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'cppCodeBody' must contain '#include <Rcpp.h>'")
    }

    cmd <- paste0(cmd, collapse = "\n")
    cmd <- trimws(cmd)
    validArgsList <- C_SIMULATION_CALC_SUBJECTS_FUNCTION_ARGUMENTS[[cppCodeBodyType]]
    validArgs <- paste(validArgsList, names(validArgsList))

    len <- .regexprCalcSubjectsFunction("double +[a-zA-Z_0-9]{1,100}\\(", cmd)$len
    args <- trimws(substring(cmd, len + 1, nchar(cmd)))

    pos <- .regexprCalcSubjectsFunction("\\) *\\{.*", args)$value
    args <- substring(args, 1, pos - 1)
    if (grepl(",", args)) {
        args <- strsplit(args, "[ \r\n\t]*,[ \r\n\t]*")[[1]]
    }

    if (!all(args %in% validArgs)) {
        invalidArgs <- args[!(args %in% validArgs)]
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "the argument", ifelse(length(invalidArgs) == 1, "", "s"),
            " ", .arrayToString(invalidArgs, encapsulate = TRUE), " ",
            ifelse(length(invalidArgs) == 1, "is", "are"), " invalid (the ", length(validArgs),
            " valid arguments can be found in the reference manual)"
        )
    }

    pattern <- paste0(
        "double +[a-zA-Z_0-9]{1,100}\\([ \r\n\t]*",
        "(", paste0(paste0("(", args, ")"), collapse = "|"), "|([ \r\n\t]*,[ \r\n\t]*))*",
        "\\)[ \r\n\t]*\\{"
    )
    len <- .regexprCalcSubjectsFunction(pattern, cmd)$len
    code <- trimws(substring(cmd, len + 1, nchar(cmd)))
    code <- trimws(sub("}[ \\r\\n]*$", "", code))

    calcSubjectsFunctionCode <- sub("\\{USER_CODE\\}", code, cppCodeBody)

    return(calcSubjectsFunctionCode)
}

.getCalcSubjectsFunction <- function(...,
        design,
        simulationResults,
        calcFunction,
        expectedFunction,
        cppEnabled = TRUE) {
    .assertIsTrialDesign(design)
    .assertIsSimulationResults(simulationResults)
    .assertIsSingleLogical(cppEnabled, "cppEnabled")

    cppCodeBodyType <- NA_character_
    if (inherits(simulationResults, "SimulationResultsMeans")) {
        cppCodeBodyType <- C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_MEANS
    }
    if (inherits(simulationResults, "SimulationResultsRates")) {
        cppCodeBodyType <- C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_RATES
    }
    if (inherits(simulationResults, "SimulationResultsSurvival")) {
        cppCodeBodyType <- C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_SURVIVAL
    }
    if (is.na(cppCodeBodyType)) {
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE, ".getCalcSubjectsFunction() is not implemented for object ",
            class(simulationResults)[1]
        )
    }
    
    functionFieldName <- ifelse(inherits(simulationResults, "SimulationResultsSurvival"), 
        "calcEventsFunction", "calcSubjectsFunction")

    if (design$kMax == 1) {
        if (!is.null(calcFunction)) {
            warning("'", functionFieldName, "' will be ignored for fixed sample design", call. = FALSE)
        }
        simulationResults$.setParameterType(functionFieldName, C_PARAM_NOT_APPLICABLE)
        return(list(
            calcSubjectsFunctionR = NULL,
            calcSubjectsFunctionCpp = NULL,
            calcSubjectsFunctionType = 0
        ))
    }

    if (is.null(calcFunction)) {
        simulationResults$.setParameterType(functionFieldName, C_PARAM_DEFAULT_VALUE)
        if (!cppEnabled) {
            calcFunction <- expectedFunction
            simulationResults[[functionFieldName]] <- calcFunction
        }
        return(list(
            calcSubjectsFunctionR = calcFunction,
            calcSubjectsFunctionCpp = NULL,
            calcSubjectsFunctionType = 0
        ))
    }

    simulationResults$.setParameterType(functionFieldName, C_PARAM_USER_DEFINED)

    calcSubjectsFunctionType <- 0

    calcSubjectsFunctionR <- NULL
    if (is.function(calcFunction)) {
        .assertIsValidFunction(
            fun = calcFunction,
            funArgName = functionFieldName,
            expectedFunction = expectedFunction
        )
        simulationResults[[functionFieldName]] <- calcFunction
        return(list(
            calcSubjectsFunctionR = calcFunction,
            calcSubjectsFunctionCpp = NULL,
            calcSubjectsFunctionType = 1
        ))
    }

    if (!is.character(calcFunction)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'", functionFieldName, "' must be a function or a character ",
            "string specifying a function written in R/C++/Rcpp"
        )
    }

    if (.isCppCode(calcFunction)) {
        tryCatch(
            {
                calcSubjectsFunctionCppTemp <- NULL
                calcEventsFunctionCppTemp <- NULL
                Rcpp::sourceCpp(code = .getCalcSubjectsFunctionCppCode(
                    calcFunction, cppCodeBodyType
                ))
                simulationResults[[functionFieldName]] <- calcFunction
                tempFunction <- if (inherits(simulationResults, "SimulationResultsSurvival")) 
                        calcEventsFunctionCppTemp else calcSubjectsFunctionCppTemp 
                return(list(
                    calcSubjectsFunctionR = NULL,
                    calcSubjectsFunctionCpp = tempFunction(),
                    calcSubjectsFunctionType = 2
                ))
            },
            error = function(e) {
                cat(.getCalcSubjectsFunctionCppCode(
                    calcFunction, cppCodeBodyType
                ), "\n")
                Rcpp::sourceCpp(
                    code = .getCalcSubjectsFunctionCppCode(
                        calcFunction, cppCodeBodyType
                    ),
                    verbose = FALSE, showOutput = TRUE
                )
                stop("Failed to compile '", functionFieldName, "': ", e$message)
            }
        )
    }

    tryCatch(
        {
            calcSubjectsFunctionR <- .getCalcSubjectsFunctionRCode(calcFunction, cppCodeBodyType)
            simulationResults[[functionFieldName]] <- calcSubjectsFunctionR
            return(list(
                calcSubjectsFunctionR = calcSubjectsFunctionR,
                calcSubjectsFunctionCpp = NULL,
                calcSubjectsFunctionType = 1
            ))
        },
        error = function(e) {
            stop("Failed to evaluate and parse '", functionFieldName, "': ", e$message)
        }
    )

    return(list(
        calcSubjectsFunctionR = NULL,
        calcSubjectsFunctionCpp = NULL,
        calcSubjectsFunctionType = 0
    ))
}

