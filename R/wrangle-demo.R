# to run this demo, source this file and execute the "wrangle_demo" function
library(tidyverse)
library(yaml)
source("wrangle.R")
source("behav_object_model.R")
source("read_behav.R")

wrangle_demo = function() {
	# note the naming convention on the files
	files = paste("../inst/examples/behav_test_data/", c("045_ES_Neighborhood_Behav.csv","070_Neighborhood_Behav.csv","083_CM_Neighborhood_Behav.csv"), sep="")
	wrangled_data = wrangle(files, "parser_map.yaml")

	return(wrangled_data)
}
