# to run this demo, source this file and execute the "wrangle_demo" function
library(tidyverse)
library(yaml)
library(checkmate)
source("wrangle.R")
source("behav_object_model.R")
source("read_behav.R")

wrangle_demo_n = function() {
	# note the naming convention on the files
	files = paste("../inst/examples/behav_test_data/",
				  c("045_ES_Neighborhood_Behav.csv","070_Neighborhood_Behav.csv","083_CM_Neighborhood_Behav.csv", "070_Neighborhood_Eye.edf"),
				  sep="")
	wrangled_data = wrangle(files, "parser_map.yaml")

	return(wrangled_data)
}

wrangle_demo_v = function() {
	files = paste("../inst/examples/behav_test_data/",
				  c("045_ES_VendingMachine_Instr_Behav.csv",
					"045_ES_VendingMachine_Pavlov_Behav.csv",
					"045_ES_VendingMachine_Trans_Behav.csv",
					"070_SK_VendingMachine_Instr_Behav.csv",
					"070_SK_VendingMachine_Pavlov_Behav.csv",
					"070_SK_VendingMachine_Trans_Behav.csv",
					"070_VendingMachine_Eye.edf",
					"070_VanillaBaseline_Eye.edf"),
					 sep="")
	wrangled_data = wrangle(files, "parser_map.yaml")
	return(wrangled_data)
}
