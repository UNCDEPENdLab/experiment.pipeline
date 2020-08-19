library(dplyr)

# CHECKLIST:
# [ ] parsers
#		[ ] behav
#			[X] vending machine
#			[ ] neighborhood (needs to accept multiple files per phase)
#			[ ] sorting mushrooms
#			[ ] kindgom
#			[ ] vanilla baseline
#		[ ] physio
#			[ ] vending machine
#			[ ] neighborhood
#			[ ] sorting mushrooms
#			[ ] kindgom
#			[ ] vanilla baseline
#		[ ] eye
#			[ ] vending machine
#			[ ] neighborhood
#			[ ] sorting mushrooms
#			[ ] kindgom
#			[ ] vanilla baseline
# [ ] object models
#		[X] behav
#		[ ] physio
#		[ ] eye
# [X] wrangle framework

# Low level parsing function interface
# A parser should be written for each MODALITY and each TASK. Each task and
# modality has 1 or more files per subject that must be processed. These files
# can be further divided by PHASE and file type (.mat, .csv, etc...)
# The code in this file (wrangle.R) will direct raw input files to their
# correct parser, grouped by MODALITY, TASK and SUBJECT, so that the parser has
# access to all relevant information at once.
#
# Parsers should conform to the following interface:
# INPUT: files
# 	'files' is a list whose elements are named by PHASE and are character vectors.
#		e.g. files = list(Instr = "path/to/instrumental.csv", Pavlov = c("path/to/pav.csv", "path/to/pav.log"))
# OUTPUT:
#	a list whose elements are named by PHASE and are data frames containing the
#	parsed and consolidated information from all files for that phase passed to
#	the parser
#
# RATIONALE
# The format, content and distribution of the raw data varies across tasks,
# modalites and phases, requiring specialized code to be written. However, the
# data can then be shaped into similar formats (data frames) that can be
# processed agnostically and the specialzed parsers can be applied in general
# way. As long as each parser fits into this interface, the code in this file
# will be able to wrangle the raw data into a form consistent across tasks and
# modalites.

# Using wrangle.R
# wrangle.R implements a framework for parsing raw s3 files. It uses the
# filenames of the input to direct them to the correct parsing function, and
# unites the output of all the parsers to construct object models for each
# TASK, SUBJECT and MODALITY. The name of main function is 'wrangle'
# INPUT: 'paths'
# 	'paths' is a character vector whose elements are paths to raw s3 files.
# 		additionally, the basenames of the files should conform to the s3 renaming
# 		template discussed on slite and implemented in data_automation/s3_data_org
# 		(see https://depend.slite.com/app/channels/rnpb9CPuyb/notes/dzkTTQKWKC)
# OUTPUT:
#	a list whose elements are named by TASK and SUBJECT and are themselves
#	lists whose elements are named by MODALITY and are the object models for
#	that TASK, SUBJECT and MODALITY.
#		e.g. output might look like: list(VendingMachine.70=list(Behav=behav_obj_model, Eye=eye_obj_model), Neighborhood.83=list(Physio=physio <- physio_obj_model))
# USAGE:
# see wrangle-demo.R and the 'wrangle' function in wrangle.R

#' @param files A vector of file paths
#' @description This function implements the wrangle step. It takes the raw
#' output files from session 3 experiments and parses and combines these files
#' within a modality, task and subject, into the object model.
#' @return A list of lists, where the first level is by task and subject, and
#' the second level by mode. bottom elements are object models
wrangle = function(paths) {
	return(parseFiles(getFileInfo(paths)))
}

#' @param parsedFilesDF a data frame produced by getFileInfo containing parsed
#' path information
#' @description applies the correct parsing function to each group of files.
#' @return a list where each element is named by SUBJECT and TASK and is itself
#' a list of object models, one for each modality present
parseFiles <- function(parsedFilesDF) {

	cats = c("task", "id")
	groupedFiles = split_list(parsedFilesDF, cats)

	objModels = lapply(groupedFiles, splitByMode)

	return(objModels)
}

#' @param files a character vector of raw s3 data files that have been renamed
#' to the template given on slite
#' @description to facilitate passing files to the correct parsers, this
#' function extracts relevant information from the filename
#' @return a data frame where each row corresponds to a file path, and has
#' several columns with extracted information
getFileInfo <- function(files) {
	info = as.data.frame(sapply(files, parseFilename) %>% t())
	info = mutate(info, id = as.numeric(id), path = row.names(info)) %>%
		arrange(path) %>%
		select(path, everything())
	info[ info == "" ] <- NA
	return(info)
}

#' @param df a data frame
#' @param conds a character vector of names to split df on
#' @description repeatedly applies the base 'split' function to the input data
#' frame, keeping the structure flat instead of creating a hiearchy of lists
#' @return a flattened list of data frames divided by elements in conds
split_list = function(df, conds) {
	# convert list to factor
	f = as.factor(unlist(df[[conds[[1]]]], use.names=FALSE))
	df = split(df, f)
	for (t in conds[-1]) {
		df = lapply(df, function(x, t) {
							split(x, f=as.factor(unlist(x[[t]], use.names=FALSE)))
						},
						t=t) %>%
			unlist(recursive=F)
	}
	return(df)
}

#' @param df a data frame
#' @description applies the chooseParserAndModel function to each split of df by modality
#' @return a list whose elements are named by modality and are the corresponding object model
splitByMode = function(df) {
	# automatically named by mode due to split function
	lapply(split_list(df, c("mode")), chooseParserAndModel) %>%
	return()
}

#' @param df a data frame
#' @description applies the correct parsing function to the paths of the same
#' SUBJECT, TASK and MODALITY listed in df and passes the output to the object
#' model constructor. relies on the parsing functions being named like:
#' "read_(Behav|Eye|Physio)_(VendingMachine|Neighborhood|VanillaBaseline|SortingMushrooms|Kingdom)"
#' @return an object model
chooseParserAndModel = function(df) {
	task.i = unlist(unique(df$task))
	mode.i = unlist(unique(df$mode))
	id.i  = unlist(unique(df$id))
	pathsByPhase = df$path %>% setNames(df$phase)
	data = get(paste("read", mode.i, task.i, sep="_"))(pathsByPhase)
	list(data=data, task=task.i, id=id.i, mode=mode.i) %>%
	createObjModel() %>%
	return()
}

#' @param wrapedParse a list with 3 named elements: data, task, id and mode
#' @description uses information from wrapedParse to feed object model
#' constructor for the given phase. relies on object model constructors being
#' named like: "ep_subject_task.(Behav|Eye|Physio)"
#' @return an object model
createObjModel = function(wrapedParse) {
	# looks up correct object model constructor for this modality
	objModelFunc = get(paste("ep_subject_task.", wrapedParse$mode, sep=""))

	# applies the constructor
	newObj = objModelFunc(phases=names(wrapedParse$data),
						  subject_id=wrapedParse$id, task=wrapedParse$task)

	# converts each data frame to a tibble
	for (name in names(wrapedParse$data)) {
		newObj$data[[name]] = tibble(wrapedParse$data[[name]])
	}

	return(newObj)
}

parseFilename <- function(filename) {
	# TODO: construct regex dynamically from names specified in file. shouldnt have to manually rewrite regex if names change
	# TODO: this pattern will have to be loaded from a config file somewhere
	pattern <- "(?<id>\\d{1,4})(\\_(?<initials>\\w{2}))?\\_(?<task>[A-Za-z]+)\\_((?<phase>(Pavlov|Instr|Trans))\\_)?(?<mode>(Eye|Behav|Physio))\\.(?<suf>.*)"
	fields <- re.capture(pattern, basename(filename))
	return(fields$names)
}

# apparently named regex in R is tricky
# from: https://www.r-bloggers.com/regex-named-capture-in-r/
re.capture = function(pattern, string, ...) {
  rex = list(src=string,
             result=regexpr(pattern, string, perl=TRUE, ...),
             names=list())

  for (.name in attr(rex$result, 'capture.name')) {
	if (.name == "") {
		next
	}
    rex$names[[.name]] = substr(rex$src,
                                attr(rex$result, 'capture.start')[,.name],
                                attr(rex$result, 'capture.start')[,.name]
                                + attr(rex$result, 'capture.length')[,.name]
                                - 1)
  }

  return(rex)
}
