library(dplyr)

main = function(files, yamlFile) {
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
			#TODO: would add another loop here to segment by modality as well
			#subjects[[sub]][[tasky]] = filter(infoDF, id==sub, task==tasky) %>% select(path, phase, suf, mode)
			subjects[[length(subjects)+1]] = list(data=filter(infoDF, id==sub, task==tasky) %>% select(path, phase, suf, mode),
												  id=sub, task=tasky)
		}
	}
	#subjects = mapply(function(df, sub, tasky) {
	#					return(list(filter(df, id==sub, task==tasky) %>% select(path, phase, suf, mode), sub, tasky)) },
	#					unique(infoDF$id), unique(infoDF$task), df=infoDF)

	print(subjects)
	return(subjects)
}

parseFiles <- function(groupedFiles, yamlFile) {
	# use yaml mapping to decide which parser to use on each set of files
	# combination of different file suffixes is handled within the specific parser
	# for yaml specification
		# filter from yaml
		# get parser
		# for subject
			# apply appropriate parser
	# output list of data frames: subject-task-mode --> multi-dimensional
	parserMap = read_yaml(yamlFile)
	# choose parser and feed it all files in df at once
	lapply(groupedFiles, function(entry, parserMap) {
			   data = get(parserMap[[entry$task]]$Behav)(entry$data$path) 
			   print(data)
			   return(list(data=tibble(data),
						   task=entry$task, id=entry$id))
		  }, parserMap=parserMap) %>%
	return()
}

# TODO: construct regex dynamically from names specified in file. shouldnt have to manually rewrite regex if names change
parseFilename <- function(filename) {
	pattern <- "(?<id>\\d{3})(\\_(?<initials>\\w{2}))?\\_(?<task>[A-Za-z]+)\\_((?<phase>(Pavlov|Instr|Trans))\\_)?(?<mode>(Eye|Behav|Physio))\\.(?<suf>.*)"
	#print(regexpr(pattern, filename, perl=TRUE))
	fields <- re.capture(pattern, basename(filename))
	return(fields$names)
}

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
