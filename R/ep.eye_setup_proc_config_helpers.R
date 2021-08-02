
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
      if(!"meta_check" %in% names(opts)) {
        opts[["meta_check"]] <- NULL
        } else{
          ### if a specific meta_check field is missing, set to NULL
          if(!"meta_vars" %in% names(opts$meta_check)) {opts$meta_check$meta_vars <- NULL}
          if(!"meta_vals" %in% names(opts$meta_check)) {opts$meta_check$meta_vals <- NULL}
          if(!"recording_time" %in% names(opts$meta_check)) {opts$meta_check$recording_time <- NULL}
        }
      if(!"unify_gaze_events" %in% names(opts)) {opts[["unify_gaze_events"]] <- c("sacc", "fix", "blink")}
    } else{ 
      # if processing options are not specified, revert to default options.
      opts <- list()
      opts[["expected_edf_fields"]] <- c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file")
      opts[["meta_check"]] <- NULL
      opts[["unify_gaze_events"]] <- c("sacc", "fix", "blink")
    }
    config[["definitions"]][["eye"]][["initialize"]] <- opts
  } else if(field == "msg_parse"){
      ################### PARSE MESSAGES
    if("msg_parse" %in% names(config$definitions$eye)){
      opts <- config$definitions$eye$msg_parse
      if(!"inherit_btw_ev" %in% names(opts)) {
        opts$interit_btw_ev <- NULL
      } else{
        if(!"calibration_check" %in% names(opts$inherit_btw_ev)){
          opts$interit_btw_ev$cal <- NULL
          opts$interit_btw_ev$val <- NULL
        } 
      }
      # if(!"meta_check" %in% names(opts)) {
      #   opts[["meta_check"]] <- NULL
      #   } else{
      #     ### if a specific meta_check field is missing, set to NULL
      #     if(!"meta_vars" %in% names(opts$meta_check)) {opts$meta_check$meta_vars <- NULL}
      #     if(!"meta_vals" %in% names(opts$meta_check)) {opts$meta_check$meta_vals <- NULL}
      #     if(!"recording_time" %in% names(opts$meta_check)) {opts$meta_check$recording_time <- NULL}
      #   }
      # if(!"unify_gaze_events" %in% names(opts)) {opts[["unify_gaze_events"]] <- c("sacc", "fix", "blink")}
    } else{ 
      # if processing options are not specified, revert to default options.
      opts <- list()
      opts$interit_btw_ev <- NULL
    }
    config[["definitions"]][["eye"]][["msg_parse"]] <- opts
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


#' @title Build out expected message sequences within config file. 
#' 
#' @description When a message sequence check is requested, the user specifies event-general start and end message sequences, with the message sent during the event being unique to the block and event. This function attempts to combine the general and specific into the msg_seq field of msg_parse options, which gives block/event-level specificity on the exact expected sequence of messages to check.
#' @param config Named list of configuration options read in by \code{validate_exp_yaml} 
#' @param dt Descriptive text to print after running. Defaults to NULL (silent).
#' 
#' @return Nested list with configuration options. 
#' 
#' @author Nate Hall
#' 
#' @export
ep.eye_build_msg_seq <- function(config, dt = NULL){
  tryCatch.ep({
    c.e <- config[["definitions"]][["eye"]]
    event_info <- c.e[["msg_parse"]][["event_info"]]

    if("msg_seq" %in% names(event_info)){
      if("eval_middle" %in% names(event_info[["msg_seq"]])){
        for(i in names(config[["blocks"]])){
          # check first for an eye field in each event type in a block.
          for(j in names(config[["blocks"]][[i]][["events"]])){
            ev_m <- config[["blocks"]][[i]][["events"]][[j]][["eye"]]
            if(event_info[["msg_seq"]][["eval_middle"]]){
              msg_vec <- c(event_info[["msg_seq"]][["msg_start"]], ev_m[["mid_msg"]], event_info[["msg_seq"]][["msg_end"]])
              c.e[["msg_parse"]][["event_info"]][["msg_seq"]][[i]][[j]] <- msg_vec
            }
          }
        }
      }
    }
  }, describe_text = dt)
  return(c.e)
}
