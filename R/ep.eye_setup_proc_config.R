#' @title Read and setup processing configuration
#' @description Validates and imports processing configuration options into the environment for later use.
#' @param file Path to the .edf file to process.
#' @param config_path Path to corresponding .yml configuration file with processing instructions. Instructions on how to effectively set up a configuration file can be found [HERE]. 
#' @return Nested list with processing options pulled from .yml configuration file.
#' @details  Also generates new log file using \code{sink()}, and sets  up a new directory for preprocessed output if it does not already exist.
#' @examples
#'  \dontrun{
#'    config <- setup_proc_configs("/proj/mnhallqlab/studies/NeuroMAP/s3_data/Neighborhood_PSU/eye/002_HS_Neighborhood_Eye.edf", "/proj/mnhallqlab/studies/NeuroMAP/s3_data_ep_specs/yaml/Neighborhood_PSU.yaml")
#'  }
#' @author Nate Hall
#' 
#' importFrom stringr str_extract
#' 
#' @export

ep.eye_setup_proc_config <- function(file, config_path, header = NULL){
  stopifnot(file.exists(file))
  if (length(file) > 1L) { stop("At present, ep.eye_setup_proc_configs is designed for one file at a time") }

  config <- validate_exp_yaml(config_path)

  #### start by pulling in global options which will launch an .elog file if requested.
  config <- ep.eye_set_config_definitions(config, "global")
  if(!is.null(header)){
    log_chunk_header(header)
    cat(paste0(str_extract(header, "\\d+\\."), "1 Setup global options: SUCCESS\n------ global config options:"))
    ep.list.tree(config$definitions$eye$initialize)
    cat("------\n")
  }

  # Designated definition fields. Implement defaults if no options are supplied. 
  eye_proc_fields <- c("initialize", "msg_parse", "gaze_preproc", "pupil_preproc", "qa") 
  count <- 1
  for(f in eye_proc_fields){
    count <- count + 1
    tryCatch.ep({
      config <- ep.eye_set_config_definitions(config, f)
    }, describe_text = paste0(str_extract(header, "\\d+\\."), count, " Setup ", f, " options:"))
    
    cat(paste0("------ ",f, " config options:"))
    ep.list.tree(config$definitions$eye[[f]])
    cat("------\n")
    
  }

  

  #### Build block and event-specific message sequences.
  config <- config %>% ep.eye_build_msg_seq()

return(config)
}


#' Implement Default Definitions
#' 
#' @param config
#' 

ep.eye_set_config_definitions <- function(config, field){
  if(field == "global"){
    ################### GLOBAL
    ################### read processing options from config into environment. This is probably more complicated than it needs to be, but works fine for now.
    opt_names <- c("prefix", "gen_log", "log_dir", "preproc_out", "return_raw")
    
    if("global" %in% names(config$definitions$eye)){
      opts <- config$definitions$eye$global
    } else{ 
      # if processing options are not specified, revert to default options.
      opts <- list()
      opts[["prefix"]] <- NULL
      opts[["gen_log"]] <- TRUE
      opts[["log_dir"]] <- NULL
      opts[["save_preproc"]] <- TRUE
      opts[["preproc_out"]] <- NULL
      opts[["return_raw"]] <- FALSE
    }

    invisible(list2env(opts,  envir = environment()))
    ###################

    ### Set prefix string. If a regex string is provided, extract from file name otherwise set to the base file name. 
    if(exists("prefix")){
      if(!is.null(prefix)) {
        # prefix <- "\\d{3}_[[:upper:]]+"
        prefix <- str_extract(basename(file), prefix)  
      } else {
        prefix <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
      }
    } else {
      prefix <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
    }
    opts[["prefix"]] <- prefix

    ### Setup ep.eye log: initialize log file if requested. Otherwise will print feedback while running checks.
    ## N.B. right now this will overwrite existing files, can come back to later.
    if(exists("gen_log")) {
        if(gen_log){
          if(!exists("log_dir")) {
            log_dir <- getwd()
          } 
        init_eyelog(file, log_dir, prefix)   
      }
    }
    
    if(!exists("save_preproc")) opts[["save_preproc"]] <- TRUE


    ### Setup folder to save preprocessed data: If none provided, creates directory "preproc" in working directory.
    if(exists("preproc_out")){
      if(!is.null(preproc_out)) {
          if(!dir.exists(preproc_out)) dir.create(preproc_out, recursive = TRUE)
      } else {
      dir.create("preproc")
      preproc_out <- "preproc"
      } 
    } else {
      dir.create("preproc")
      preproc_out <- "preproc"
    }

    opts[["preproc_out"]] <- preproc_out

    ### Return raw data?
    if(!exists("return_raw")) opts[["return_raw"]] <- FALSE
    config[["definitions"]][["eye"]][["global"]] <- opts

  } else if(field == "initialize"){
    ################### INITIALIZE
    if("initialize" %in% names(config$definitions$eye)){
      opts <- config$definitions$eye$initialize
      if(!"expected_edf_fields" %in% names(opts)) {opts[["expected_edf_fields"]] <- c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file")}
      if(!"meta_check" %in% names(opts)) {opts[["meta_check"]] <- NULL}
      if(!"unify_gaze_events" %in% names(opts)) {opts[["unify_gaze_events"]] <- c("sacc", "fix", "blink")}
    } else{ 
      # if processing options are not specified, revert to default options.
      opts <- list()
      opts[["expected_edf_fields"]] <- c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file")
      opts[["meta_check"]] <- NULL
      opts[["unify_gaze_events"]] <- c("sacc", "fix", "blink")
    }
    config[["definitions"]][["eye"]][["initialize"]] <- opts
  }
 
 return(config)
}



#' Print list structure 
#' 
#' @description this is a slight ammendment to the Hmisc package: http://math.furman.edu/~dcs/courses/math47/R/library/Hmisc/html/list.tree.html
#' 
#' @importFrom Hmisc list.tree
ep.list.tree <- function(struct, depth = -1, numbers = FALSE, maxlen = 10000, maxcomp = 30, 
    attr.print = TRUE, front = "", fill = "-", name.of, size = FALSE, digits = 5) {
  # Adapted from Hmisc pacakge: http://math.furman.edu/~dcs/courses/math47/R/library/Hmisc/html/list.tree.html
    if (depth == 0) 
        return()
    opts <- options("digits")
    options(digits = digits)
    on.exit(options(opts))
    # Remove printing object name
    # if (missing(name.of)) 
    #     name.of <- deparse(substitute(struct))
    len <- length(struct)
    # cat(front, name.of, "=", storage.mode(struct), len)
    if (size) 
        cat(" (", object.size(struct), " bytes)", sep = "")
    if (is.array(struct)) 
        cat("=", if (length(dimnames(struct))) 
            "named", "array", paste(dim(struct), collapse = " X "))
    if (is.ts(struct)) 
        cat("= time series", tsp(struct))
    if (is.factor(struct)) 
        cat("= factor (", length(levels(struct)), " levels)", 
            sep = "")
    if (length(attr(struct, "class")) > 0) 
        cat("(", attr(struct, "class"), ")")
    if (is.atomic(struct) && !is.character(struct) && len > 0 && 
        maxlen > 0) {
        field <- "="
        for (i in 1:length(struct)) {
            field <- paste(field, format(as.vector(struct[i])))
            if (nchar(field) > maxlen - 6) {
                field <- paste(field, "...")
                break
            }
        }
        cat(field, "\n", sep = "")
    }
else if (is.character(struct) && len > 0 && maxlen > 0) 
        cat("=", substring(struct[1:(last <- max(1, (1:len)[cumsum(nchar(struct) + 
            1) < maxlen]))], 1, maxlen), if (last < len) 
            " ...", "\n")
    else cat("\n")
    if (mode(struct) == "list" && len > 0) {
        structnames <- names(struct)
        if (!length(structnames)) 
            structnames <- rep("", len)
        noname <- structnames == ""
        structnames[noname] <- paste("[[", (1:length(structnames))[noname], 
            "]]", sep = "")
        for (i in 1:min(length(structnames), maxcomp)) if (mode(struct[[i]]) == 
            "argument" | mode(struct[[i]]) == "unknown") 
            cat(front, "\n")#fill, ", ", structnames[i], " = ", as.character(struct[[i]])[1], "\n", sep = ",")
        else Hmisc::list.tree(struct[[i]], depth = depth - 1, numbers, 
            maxlen, maxcomp, attr.print, if (numbers) 
                paste(front, i, sep = ".")
            else paste(front, fill, sep = ""), fill, structnames[i], 
            size = FALSE)
        if (length(structnames) > maxcomp) 
            cat(front, fill, " ...   and ", length(structnames) - 
                maxcomp, " more\n", sep = "")
    }
    attribs <- attributes(struct)
    attribnames <- names(attribs)
    if (length(attribnames) > 0 && attr.print) 
        for (i in (1:length(attribnames))[attribnames != "dim" & 
            attribnames != "dimnames" & attribnames != "levels" & 
            attribnames != "class" & attribnames != "tsp" & (attribnames != 
            "names" | mode(struct) != "list")]) list.tree(attribs[[i]], 
            depth - 1, numbers, maxlen, maxcomp, attr.print, 
            if (numbers) 
                paste(front, i, sep = "A")
            else paste(front, "A ", sep = ""), fill, attribnames[i], 
            size = FALSE)
    invisible()
}
