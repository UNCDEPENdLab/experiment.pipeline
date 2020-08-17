library(dplyr)

# parse filenames to get modality and task information
# feed filenames to parsers in modality, task, subject groups
# for each task, and subject, combine modality information into one object. for now, just put each modality object model into a list

# what are the moving parts?
#TODO:
# [X] raw files: need to be named to the target template. done via data_automation/s3_data_org code. just a matter of getting the files we need here
# [ ] parser functions: need to conform to a certain interface
# [ ]   need to better define this interface. most parsers arent yet implemented so there shouldnt be many limitations
# [X] wrangle code: needs to be generalized, rn behav is still slightly hardcoded
# [ ] object models: rn, only behav is implemented, will have to implement a skeleton objmodel for other modalities

#' @param files A vector of file paths
#' @param yamlFile A path to a YAML file specifiying a hierachical mapping from
#' task and modality to parser function name
#' @description This function implements the wrangle step. It takes the raw
#' output files from session 3 experiments and parses and combines these files
#' within a modality, task and subject, into the object model.
#' @return A list of lists, where the first level is by task and subject, and
#' the second level by mode. bottom elements are ep.subject.task objects
wrangle = function(files, yamlFile) {
	return(parseFiles(getFileInfo(files), yamlFile))
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
		arrange(path) %>% select(path, everything())
	info[ info == "" ] <- NA

	print("file df")
	print(info)
	return(info)
}

#' @param l a list of objects
#' @param conds a character vector of names to split objects in l on
#' @return a flattened list of objects, each element being a list of objects
split_list = function(l, conds) {
	#print(conds)
	firstTime = TRUE
	for (t in conds) {
		#print("t is")
		#print(t)
		#print("l is: ")
		#print(l)
		if (!firstTime) {
			#print("not first time")
			l = lapply(l, function(x, t) split(x, f=as.factor(unlist(x[[t]], use.names=FALSE))), t=t) %>% unlist(recursive=F)
		}
		else {
			#print("first time")
			#print("l[[t]] is ")
			#print(l[[t]])
			f = as.factor(unlist(l[[t]], use.names=FALSE))
			l = split(l, f)# %>% unlist(recursive = F)
			firstTime = FALSE
		}
	}
	#print(l)
	return(l)
}

#' @param parsedFilesDF A list of lists. Each sublist has a 'data', 'id' and
#' 'task' element. sublists are constructed of data from files with the same
#' subject and task.
#' @param parserYaml A yaml file specifying a mapping between tasks and
#' modalities to a file parsing function
#' @description applies the correct parsing function to each group of files.
#' parsers must return a list of event data frames, where each phase has its
#' own df
#' @return A list of lists, where the first level is by task and subject, and
#' the second level by mode. bottom elements are ep.subject.task objects
parseFiles <- function(parsedFilesDF, parserYaml) {

	#TODO: do this with S3 style OO: generic class functions instead?
	parserMap = read_yaml(parserYaml)

	cats = c("task", "id")
	groupedFiles = split_list(parsedFilesDF, cats)
	print("grouped files")
	print(groupedFiles)

	# groupedFiles: list by task and id. modes are still mixed together
	# i want: list of object models by task and id, where each element is a
	# list of modality specific obj models
		# for each entry in groupedFiles
			# split by mode
			# feed paths in each above split into the correct parser
			# feed output of parser into correct obj model constructor
			# create list of each modality's obj model

	objModels = lapply(groupedFiles, splitByMode, parserMap=parserMap)

	return(objModels)
}

splitByMode = function(df, parserMap) {
	print("in split by mode")
	print(df)
	lapply(split_list(df, c("mode")), chooseParserAndModel, parserMap=parserMap) %>%
	return()
}

chooseParserAndModel = function(df, parserMap) {
	print("in chooseParserAndModel")
	print(df)
	task.i = unlist(unique(df$task))
	mode.i = unlist(unique(df$mode))
	id.i  = unlist(unique(df$id))
	pathsByPhase = df$path %>% setNames(df$phase)
	data = get(parserMap[[task.i]][[mode.i]])(pathsByPhase)
	list(data=data, task=task.i, id=id.i, mode=mode.i) %>%
	createObjModel() %>%
	return()
}

#' @param wrapedParse a list with 3 named elements: data, task, id
#' @description uses information from wrapedParse to feed object model initializer
#' @return an object model
createObjModel = function(wrapedParse) {
	print("creating obj model")
	print(str(wrapedParse))
	objModelFunc = get(paste("ep_subject_task.", wrapedParse$mode, sep=""))
	newObj = objModelFunc(phases=names(wrapedParse$data),
						  subject_id=wrapedParse$id, task=wrapedParse$task)

	for (name in names(wrapedParse$data)) {
		newObj$data[[name]] = tibble(wrapedParse$data[[name]])
	}

	return(newObj)
}

# TODO: construct regex dynamically from names specified in file. shouldnt have to manually rewrite regex if names change
parseFilename <- function(filename) {
	pattern <- "(?<id>\\d{1,4})(\\_(?<initials>\\w{2}))?\\_(?<task>[A-Za-z]+)\\_((?<phase>(Pavlov|Instr|Trans))\\_)?(?<mode>(Eye|Behav|Physio))\\.(?<suf>.*)"
	#print(regexpr(pattern, filename, perl=TRUE))
	fields <- re.capture(pattern, basename(filename))
	return(fields$names)
}

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
