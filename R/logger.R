# GOAL: provide an interface for logging results of qa and other checks that can be used across modalities
library(tidyverse)

#' @param objects A list of objects to validate in some way
#' @param validator A function that takes as input an element of 'objects' and outputs an object with a well-formatted string representation
#' @param flags A list of named elements which can be used to specify where and how output is generated
#' @description This function can be used to facillitate a unified method of reporting validation results across tasks and modalities. Custom validation functions should be written, and some general method of logging should be determined. For example, maybe each modality can be integrated into the same file, but output will differ between tasks. This function is itself thin, but supplies the framework for integration between modes.
checkAndLog = function(validator, objects, mode=NA, header=NA, footer=NA) {
	# run validator function on objects
	results = sapply(objects, validator)

	# based on flags, output results to certain files in a certain way
	if (!is.na(mode) & mode=="behav") {
		file = "behav_checks.log"
		print("writing to file")
		write(header, append=TRUE, file=file)
		write(results, append=TRUE, file=file)
		write(footer, append=TRUE, file=file)
	}
}

#' @importFrom dplyr filter
#' @description an example validator
checkRtKey = function(df) {
	checks = filter(df, is.na(rt) & (key_pressed!="None" | !is.na(key_pressed) ))

	if (length(checks[[1]]) > 0) {
		log = paste("Warning: absent rt but key was pressed in neighborhood behav data", paste(capture.output(print(checks, n=Inf)), collapse="\n"), sep="\n")
	}
	else {
		log = "check passed"
	}

	return(log)
}
