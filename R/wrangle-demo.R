# to run this demo, source this file and execute the "wrangle_demo" function
library(tidyverse)
library(checkmate) # needed by behav parsers
source("wrangle.R")
source("object_model.R")
# parsers
source("read_behav.R")
source("read_eye.R")
source("read_physio.R")

wrangle_demo = function() {
	files = list.files("../inst/examples/wrangle_demo_data/", full.names=TRUE)
	return(wrangle(files))
}
