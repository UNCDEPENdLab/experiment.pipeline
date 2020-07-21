library(dplyr)



# groups files by task, subject, modality
# information obtained from filename
# the first dimension is task&subject pair. 2nd dimension is modality
getFileInfo <- function(files) {
	info = as.data.frame(sapply(files, parseFilename) %>% t())
	info = mutate(info, id = as.numeric(id), path = row.names(info)) %>% arrange(path) %>% select(path, everything())
	info[ info == "" ] <- NA

	return(info)
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
