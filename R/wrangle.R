library(dplyr)

#' @param files A vector of file paths
#' @param yamlFile A path to a YAML file specifiying a hierachical mapping from task and modality to parser function name
#' @description This function implements the wrangle step. It takes the raw output files from session 3 experiments and parses and combines these files within a modality, task and subject, into the object model.
#' @return A list of behav object models.
wrangle = function(files, yamlFile) {
	return(parseFiles(chunkFileInfoDf(getFileInfo(files)), yamlFile))
}

# groups files by task, subject, modality
# information obtained from filename
# the first dimension is task&subject pair. 2nd dimension is modality
getFileInfo <- function(files) {
	info = as.data.frame(sapply(files, parseFilename) %>% t())
	info = mutate(info, id = as.numeric(id), path = row.names(info)) %>% arrange(path) %>% select(path, everything())
	info[ info == "" ] <- NA

	return(info)
}

# INPUT: a data frame containing information about files (output from getFileInfo)
# OUTPUT: a list of data frames
chunkFileInfoDf <- function(infoDF) {
	subjects = list()
	for (sub in unique(infoDF$id)) {
		#subjects[[sub]] = list()
		for (tasky in unique(filter(infoDF, id==sub)$task)) {
			#NOTE: would add another loop here to segment by modality as well
			#subjects[[sub]][[tasky]] = filter(infoDF, id==sub, task==tasky) %>% select(path, phase, suf, mode)
			subjects[[length(subjects)+1]] = list(data=filter(infoDF, id==sub, task==tasky) %>% select(path, phase, suf, mode),
												  id=sub, task=tasky)
		}
	}
	# attempt at doing the above functionally --> lower code maintenance, more flexible
	#subjects = mapply(function(df, sub, tasky) {
	#					return(list(filter(df, id==sub, task==tasky) %>% select(path, phase, suf, mode), sub, tasky)) },
	#					unique(infoDF$id), unique(infoDF$task), df=infoDF)

	print(subjects)
	return(subjects)
}

#' @param groupedFiles A list of lists. Each sublist has a 'data', 'id' and 'task' element. sublists are constructed of data from files with the same subject and task.
#' @param parserYaml A yaml file specifying a mapping between tasks and modalities to a file parsing function
#' @description applies the correct parsing function to each group of files. parsers must return a list of event data frames, where each phase has its own df
#' @return A list of ep.subject.task.behav objects
parseFiles <- function(groupedFiles, parserYaml) {
	# use yaml mapping to decide which parser to use on each set of files
	# combination of different file suffixes is handled within the specific parser
	# for yaml specification
		# filter from yaml
		# get parser
		# for subject
			# apply appropriate parser
	# output list of data frames: subject-task-mode --> multi-dimensional
	parserMap = read_yaml(yamlFile)
	# choose parser and feed it all files in df
	lapply(groupedFiles, function(entry, parserMap) {
			   data = get(parserMap[[entry$task]]$Behav)(entry$data$path) 
			   return(list(data=tibble(data), task=entry$task, id=entry$id))
		  }, parserMap=parserMap) %>%
	return() #TODO: return list of object models
}

# TODO: construct regex dynamically from names specified in file. shouldnt have to manually rewrite regex if names change
parseFilename <- function(filename) {
	pattern <- "(?<id>\\d{3})(\\_(?<initials>\\w{2}))?\\_(?<task>[A-Za-z]+)\\_((?<phase>(Pavlov|Instr|Trans))\\_)?(?<mode>(Eye|Behav|Physio))\\.(?<suf>.*)"
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
