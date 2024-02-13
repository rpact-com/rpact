######################################################################################
#                                                                                    #
# -- Load source files --                                                            #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD                             #
# Licensed under "GNU Lesser General Public License" version 3                       #
# License text can be found here: https://www.r-project.org/Licenses/LGPL-3          #
#                                                                                    #
# RPACT company website: https://www.rpact.com                                       #
# RPACT package website: https://www.rpact.org                                       #
#                                                                                    #
# Contact us for information about our services: info@rpact.com                      #
#                                                                                    #
# File version: $Revision: 4443 $                                                    #
# Last changed: $Date: 2021-02-22 09:13:17 +0100 (Mon, 22 Feb 2021) $                #
# Last changed by: $Author: pahlke $                                                 #
######################################################################################

#setwd("C:\\Users\\Till\\CLionProjects\\rpactsvn")
#setwd("/home/massive/eclipse-workspace/rpact.ext")

library(Rcpp)
library(tictoc)
library(testthat)
library(parallel)
library(rbenchmark)
library(doParallel)
library(foreach)
library(R6)
library(profvis)
library(rpact.as251)

tic()

Sys.setenv("RPACT_COMPILE_CPP_FILES" = FALSE)
#Sys.setenv("RPACT_DEVELOPMENT_MODE" = TRUE)
Sys.setenv("RPACT_DEVELOPMENT_MODE" = FALSE)
Sys.setenv("RPACT_COMPLETE_UNIT_TEST_SET_ENABLED" = TRUE)

#print("The following packages are not up to date:")
#pacman::p_update(FALSE)

#sessionInfo() 

# eliminate package startup messages
#suppressPackageStartupMessages() 

if (exists(".onUnload")) {
	.onUnload("")
}

# cleanup (remove all variables and functions)
rm(list=ls(all=TRUE), envir = .GlobalEnv)
# help 'environment': https://stat.ethz.ch/R-manual/R-devel/library/base/html/environment.html

#makeActiveBinding("refresh", function() { system(paste0(R.home(),"/bin/x64/R")); q("no") }, .GlobalEnv)
#paste0(R.home(),"/bin/x64/R --no-save") 

### rpact.dev::detachPackage("rpact", characterOnly = TRUE)

# debug warnings: 
#options(warn=2) # normal: options(warn=1)
# traceback()

gc()

baseDir <- file.path(sub("/R$", "", getwd()))
sourceFileDir <- file.path(baseDir, 'R')

fileNames <- c(
  "class_core_parameter_set_r6",
  "class_analysis_stage_results_r6",
  "class_analysis_results_r6",
  "class_summary_r6",
  "class_design_r6",
  "class_core_plot_settings_r6",
  "class_design_set_r6",
  "class_event_probabilities_r6",
  "class_time_r6",
  "class_design_power_and_asn_r6",
  "class_performance_score_r6",
  "f_core_constants", 
  "class_design_plan_r6",
  "f_design_utilities",
  "f_analysis_utilities",
  "class_analysis_dataset_r6",
  "class_core_parameter_set",
  "class_simulation_results_r6",
  #"class_core_plot_settings",
  "f_core_assertions",
  "f_core_utilities", 
  #"class_design",
  #"class_design_set",
  #"class_design_power_and_asn",
  #"class_time",
  "class_summary", 
  "f_logger",
  #"class_design_plan",
  #"class_analysis_dataset",
  #"class_analysis_stage_results",
  #"class_analysis_results",
  #"class_simulation_results",
  #"class_event_probabilities",
  "f_core_output_formats",
  "f_core_plot",
  "f_design_group_sequential",
  "f_design_fisher_combination_test",
  "f_design_sample_size_calculator",
  "f_analysis_base_means",
  "f_analysis_base_rates",
  "f_analysis_base_survival",
  "f_analysis_base",
  "f_analysis_enrichment",
  "f_analysis_enrichment_means",
  "f_analysis_enrichment_rates",
  "f_analysis_enrichment_survival",	
  "f_analysis_multiarm_means",
  "f_analysis_multiarm_rates",
  "f_analysis_multiarm_survival",	
  "f_analysis_multiarm",
  "f_simulation_calc_subjects_function",
  "f_simulation_base_means",
  "f_simulation_base_rates",
  "f_simulation_base_survival",
  "f_simulation_multiarm",
  "f_simulation_multiarm_means",
  "f_simulation_multiarm_rates",
  "f_simulation_multiarm_survival",
  "f_simulation_utilities",
  "f_simulation_performance_score",
  "f_parameter_set_utilities",
  "f_object_r_code"
)

# https://stackoverflow.com/questions/17635531/calling-cuda-compiled-dll-from-r

if (as.logical(Sys.getenv("RPACT_COMPILE_CPP_FILES"))) {
	# Important: .Call methods will be only added to lookup table if used in the R folder
	Rcpp::compileAttributes(verbose = TRUE)
}

# Create init file. The file must be deleted before Rcpp::compileAttributes execution!
#tools::package_native_routine_registration_skeleton(".", file.path(baseDir, "src", "rpact_init.c"), character_only = FALSE)

#pkgbuild::clean_dll()
dllFile <- file.path(baseDir, "src", "rpact.dll")
if (!file.exists(dllFile)) {
	if (is.loaded(dllFile) || !is.null(getLoadedDLLs()[["rpact"]])) {
		dyn.unload(dllFile)
	}
	# Warning: does not create all required dll's! Use rpact.dev::buildPackage instead respectively first time! 
	pkgbuild::compile_dll(force = TRUE, compile_attributes = FALSE, register_routines = FALSE)
	#pkgbuild::compile_dll(force = TRUE, compile_attributes = TRUE, register_routines = FALSE)
	#pkgbuild::compile_dll(force = TRUE, compile_attributes = TRUE, register_routines = TRUE)
}
### print(paste0("Execute dyn.load('", dllFile, "')..."))
### print(dyn.load(dllFile))

### print(getDLLRegisteredRoutines("rpact"))
# .Call .Call.numParameters
# 1             _rpact_getRandomSurvivalDistribution                   2
# 2 _rpact_getRandomPiecewiseExponentialDistribution                   3
# 3                  _rpact_getSimulationSurvivalCpp                  31
# 4                               R_getDensityValues                   6

for (fileName in fileNames) {
	sourceFile <- file.path(sourceFileDir, paste0(fileName, ".R"))
	print(paste0("Update source file '", sourceFile, "'..."))
	source(sourceFile)
}

rHome <- Sys.getenv("R_HOME")
if (grepl("Program Files", rHome)) {
	stop("R must be installed in a directory without spaces; current directory: ", rHome)
}

if (as.logical(Sys.getenv("RPACT_COMPILE_CPP_FILES")) || !exists("getSimulationSurvivalCpp")) {
    cppSourceFiles <- list.files(file.path(baseDir, "src"), pattern = "\\.cpp$")
    cppSourceFiles <- cppSourceFiles[!(cppSourceFiles %in% c("RcppExports.cpp"))]
    cppSourceFiles <- sort(cppSourceFiles, decreasing = TRUE)
    for (cppSourceFile in cppSourceFiles) {
        if(cppSourceFile != "f_as251.cpp") {
          file <- file.path(baseDir, "src", cppSourceFile)
          print(paste0("Perform sourceCpp(", file, ")..."))
          Rcpp::sourceCpp(file)  
        }
    }
}


print("Initialization completed.")

toc()
