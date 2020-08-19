# to run this demo, source this file and execute the "wrangle_demo" function
library(tidyverse)
library(checkmate) # needed by behav parsers
source("wrangle.R")
source("object_model.R")
# parsers
source("read_behav.R")
source("read_eye.R")
source("read_physio.R")

# currently, only a few parsers and object models have been implemented (see the
# checklist in wrangle.R), but skeleton wrappers have been implemented to
# demonstrate the capabilities of the wrangling code. This demo shows how files
# from any number of tasks, modalites and subjects can be processed, given that
# the files have been named according to the s3 template (see wrangle.R).
wrangle_demo = function() {
	files = list.files("../inst/examples/wrangle_demo_data/", full.names=TRUE)
	return(wrangle(files))
}
