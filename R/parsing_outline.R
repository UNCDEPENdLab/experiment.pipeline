# psuedo code for data processing scheme

rawfiles <- getAllFiles()
fileInfo <- getFileInfo(rawfiles, yamlMapping)
# above step produces a list of tuples. the first dimension is task&subject pair. 2nd dimension is modality
# the below yaml file maps file meta-data to a parser. e.g. task x, phase y, mode z maps to parser B
taskSubModeDfs <- parseFiles(fileInfo, yamlMapping) # output of wrangle step
taskSubModeDfsQA <- qualityChecks(taskSubModeDfs) # output of quality checks step
taskSubDf <- synthesize(taskSubDfsQA) # output of synthesis step

read_behav <- function(filesOfTaskSub, parsers) {
	taskSubRawDfComb <- sapply(filesOfTaskSub, getCorrectParser, parsers=parsers)
	return(taskSubRawDfComb)
}

# input: takes all files for a given task and subject (so all modalities)
# output: a data frame for each modality for that given task and subject
	# does so by parsing each type of file, and combining them into a single data frame with complete information for all phases
# parser mapping is known through the GROUP's NAME/class and via yaml file. tells which function to use for which FILE type
read_data <- function(taskSubModeGroups, parserMap) {
	taskSubBehDfs <- mapply(read_behav,
						taskSubModeGroups$behav,
						makeParserList(parserMap, taskSubModeGroups))
	# ^^ do for each modality

	# return flat vector of data frames for a given task-subject-modality
	return(c(taskSubBehDfs, ...))
}

# input: a list of all the files in the experiment
# output: a list of these files grouped by task and subject
	# i.e. a single group has all the files associated with a given task and subject
# TODO: how does it know how to group? where is this information obtained? yaml file to describe mapping?
# TODO: parse filename information? should it expect "clean" s3 filename format outlined on slite?
# TODO: should it attach information as to which parser to use on it here?
groupFilesByTaskSub <- function
