############################
##### List of subsidiary functions utilized in `ep.eye_setup_proc_config()`
############################
# - ep.eye_set_config_definitions()
# - ep.eye_build_msg_seq()
# - ep.list.tree()
############################

#' Implement Default Definitions
#'
#' @param file Path to .edf file
#' @param config Named list extracted from config file
#' @param field Character of field name to set config definitions
#'
#' @return config. Named list with config file, including default values if missing from config
#'
#' @importFrom stringr str_extract
#'
ep.eye_set_config_definitions <- function(file, config, field){
  # debug:
  # -----
  # devtools::load_all()
  # file <- "~/Documents/github_repos/arl_repos/dimt_analysis/data_raw/eye/dimt/595.edf"
  # config <- validate_exp_yaml("~/Documents/github_repos/arl_repos/dimt_analysis/config/dimt_eye_config.yaml")
  # field <- "global"
  # -----

  if(field == "global"){
    ################### GLOBAL
    ################### read processing options from config into environment. This is probably more complicated than it needs to be, but works fine for now.
    opt_names <- c("base_dir", "prefix", "save_preproc", "preproc_out", "log")

    if("global" %in% names(config$definitions$eye)){
      opts <- config$definitions$eye$global
    } else{
      # if processing options are not specified, revert to default options.
      opts <- list()
      opts[["base_dir"]] <- getwd()
      opts[["prefix"]] <- "\\d{2,3}"
      opts[["save_preproc"]] <- TRUE
      opts[["preproc_out"]] <- ppo <- file.path(getwd(), "preproc_out")
      opts[["log"]] <- FALSE
      # opts[["log"]] <- TRUE
      # opts[["return_raw"]] <- FALSE
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

    ### Set subject ID string. If a regex string is provided, extract from file name otherwise set to the prefix regex expression value
    id <- str_extract(basename(file), prefix)
    opts[["id"]] <- id

    ### Setup folder to save preprocessed data: If none provided, creates directory "preproc" in working directory.
    if(exists("preproc_out")){
      if(!is.null(preproc_out)) {
        stopifnot(class(preproc_out) == "character")
          if(!dir.exists(preproc_out)) dir.create(preproc_out, recursive = TRUE)
      } else {
      dir.create("preproc")
      preproc_out <- "preproc"
      }

      opts[["preproc_out"]] <- preproc_out
    }

    ### Setup ep.eye log: initialize log file if requested. Otherwise will print feedback while running checks.
    ## N.B. right now this will overwrite existing files, can come back to later.
    if(exists("log")) {
      if(!"log_dir" %in% names(log)) {
        log$log_dir <- file.path(getwd(), "log")
      }
      if(!"error_dump" %in% names(log)) {
        log$error_dump <- file.path(getwd(), "error_dump")
      }
      if(!"silent.messages" %in% names(log)) {
        log$silent.messages <- TRUE
      }
      if(!"silent.warnings" %in% names(log)) {
        log$silent.warnings <- TRUE
      }
    # init_eyelog(file, log_dir, prefix)
    } else {
      log <- NULL
    }
    opts[["log"]] <- log

    if(!exists("save_preproc")) opts[["save_preproc"]] <- TRUE



    ### setup tryCatchLog options
    if(exists("log")){

      options(keep.source = TRUE)        # source code file name and line number tracking
      options("tryCatchLog.write.error.dump.file" = TRUE) # dump for post-mortem analysis

      slog <- file.path(config$definitions$eye$global$log$log_dir, "test.log")

      flog.appender(appender.file(slog))  # to log into a file instead of console
      flog.threshold(INFO)    # TRACE, DEBUG, INFO, WARN, ERROR, FATAL

      opts[["preproc_out"]] <- preproc_out
    }



    ### Return raw data?
    if(!exists("return_raw")) opts[["return_raw"]] <- FALSE
    config[["definitions"]][["eye"]][["global"]] <- opts

  } else if(field == "initialize"){
    ################### INITIALIZE
    if("initialize" %in% names(config$definitions$eye)){
      opts <- config$definitions$eye$initialize

      if(!"expected_edf_fields" %in% names(opts)) {opts[["expected_edf_fields"]] <- c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file")}

      if(!"unify_gaze_events" %in% names(opts)) {
        opts[["unify_gaze_events"]][["gaze_events"]] <- c("sacc", "fix", "blink")
        opts[["unify_gaze_events"]][["confirm_correspondence"]] <- FALSE
      } else{
        ### if a specific meta_check field is missing, set to default
        if(!"gaze_events" %in% names(opts$unify_gaze_events)) {opts$unify_gaze_events$gaze_events <-  c("sacc", "fix", "blink")}
        if(!"confirm_correspondence" %in% names(opts$unify_gaze_events)) {opts$unify_gaze_events$confirm_correspondence <-  FALSE}
        }

      # both meta_check and inherit_btw_ev will default to NULL and will skip in initialization procedure if not specified

    } else{
      # if processing options are not specified, revert to default options.
      opts <- list()
      opts[["expected_edf_fields"]] <- c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file")
      opts[["unify_gaze_events"]][["gaze_events"]] <- c("sacc", "fix", "blink")
      opts[["unify_gaze_events"]][["confirm_correspondence"]] <- FALSE
    }
    config[["definitions"]][["eye"]][["initialize"]] <- opts
  } else if(field == "msg_parse"){
      ################### PARSE MESSAGES
    if("msg_parse" %in% names(config$definitions$eye)){
      opts <- config$definitions$eye$msg_parse
    } else{
      # if processing options are not specified, set to NULL
      opts <- NULL
    }
    config[["definitions"]][["eye"]][["msg_parse"]] <- opts
  } else if(field == "gaze_preproc"){
    ################### Preprocess Gaze
    if("gaze_preproc" %in% names(config$definitions$eye)){
      opts <- config$definitions$eye$gaze_preproc

      ### AOIs
      if(!"aoi" %in% names(opts)) {
        opts[["aoi"]][["indicator"]] <- "!V IAREA RECTANGLE"
        opts[["aoi"]][["extraction_method"]] <- "regex"
        opts[["aoi"]][["extraction_coords"]] <- "\\d{3,4} \\d{3,4} \\d{3,4} \\d{3,4}"
        opts[["aoi"]][["extract_labs"]] <- "[a-z]+$"
        opts[["aoi"]][["split_coords"]] <- " "
        opts[["aoi"]][["tag_raw"]] <- FALSE
      } else{
        ### if a specific aoi field is missing, set to default
        if(!"indicator" %in% names(opts$aoi)) {opts[["aoi"]][["indicator"]] <- "!V IAREA RECTANGLE"}
        if(!"extraction_method" %in% names(opts$aoi)) {opts[["aoi"]][["extraction_method"]] <- "regex"}
        if(!"extraction_coords" %in% names(opts$aoi)) {opts[["aoi"]][["extraction_coords"]] <- "\\d{3,4} \\d{3,4} \\d{3,4} \\d{3,4}"}
        if(!"extract_labs" %in% names(opts$aoi)) {opts[["aoi"]][["extract_labs"]] <- "[a-z]+$"}
        if(!"split_coords" %in% names(opts$aoi)) {opts[["aoi"]][["split_coords"]] <- " "}
        if(!"tag_raw" %in% names(opts$aoi)) {opts[["aoi"]][["tag_raw"]] <- FALSE}
      }

      ### Downsampling
      if(!"downsample" %in% names(opts)) {
        opts[["downsample"]][["downsampled_freq"]] <- 50 # in Hz
        opts[["downsample"]][["method"]] <- "mean"
      } else{
        ### if a specific downsample field is missing, set to default
        if(!"downsampled_freq" %in% names(opts$downsample)) {opts[["downsample"]][["downsampled_freq"]] <- 50}
        if(!"method" %in% names(opts$downsample)) {opts[["downsample"]][["method"]] <- "mean"}
      }

    } else{
      # if processing options are not specified, set to defaults
      opts <- list()
      opts[["aoi"]][["indicator"]] <- "!V IAREA RECTANGLE"
      opts[["aoi"]][["extraction_method"]] <- "regex"
      opts[["aoi"]][["extraction_coords"]] <- "\\d{3,4} \\d{3,4} \\d{3,4} \\d{3,4}"
      opts[["aoi"]][["extract_labs"]] <- "[a-z]+$"
      opts[["aoi"]][["split_coords"]] <- " "
      opts[["aoi"]][["tag_raw"]] <- FALSE
      opts[["downsample"]][["downsampled_freq"]] <- 50
      opts[["downsample"]][["method"]] <- "mean"
    }
    config[["definitions"]][["eye"]][["gaze_preproc"]] <- opts
  } else if(field == "pupil_preproc"){
    ################### Preprocess Gaze
    if("pupil_preproc" %in% names(config$definitions$eye)){
      opts <- config$definitions$eye$pupil_preproc

      ### Blink-correction
      if(!"blink_corr" %in% names(opts)) {
        opts[["blink_corr"]][["ms_before"]] <- 150
        opts[["blink_corr"]][["ms_after"]] <- 150
      } else{
        ### if a specific blink_corr field is missing, set to default
        if(!"ms_before" %in% names(opts$blink_corr)) {opts[["blink_corr"]][["ms_before"]] <- 150}
        if(!"ms_after" %in% names(opts$blink_corr)) {opts[["blink_corr"]][["ms_after"]] <- 150}
      }

      ### filtering/smoothing
      if(!"filter" %in% names(opts)) {
        opts[["filter"]][["method"]] <- "movingavg"
        opts[["filter"]][["window_length"]] <- 20
      } else{
        ### if a specific filter field is missing, set to default
        if(!"method" %in% names(opts$filter)) {opts[["filter"]][["method"]] <- "movingavg"}
        if(!"window_length" %in% names(opts$filter)) {opts[["filter"]][["window_length"]] <- 20}
      }

      ### interpolation
      if(!"interpolate" %in% names(opts)) {
        opts[["interpolate"]][["algor"]] <- "linear"
        opts[["interpolate"]][["maxgap"]] <- 1000
      } else{
        ### if a specific interpolate field is missing, set to default
        if(!"algor" %in% names(opts$interpolate)) {opts[["interpolate"]][["algor"]] <- "linear"}
        if(!"maxgap" %in% names(opts$interpolate)) {opts[["interpolate"]][["maxgap"]] <- 1000}
      }

      ### baseline correction
      if(!"baseline_correction" %in% names(opts)) {
        opts[["baseline_correction"]][["method"]] <- "subtract"
        opts[["baseline_correction"]][["dur_ms"]] <- 100
        opts[["baseline_correction"]][["center_on"]] <- "DISPLAY_ON"
      } else{
        ### if a specific baseline_correction field is missing, set to default
        if(!"method" %in% names(opts$baseline_correction)) {opts[["baseline_correction"]][["method"]] <- "subtract"}
        if(!"dur_ms" %in% names(opts$baseline_correction)) {opts[["baseline_correction"]][["dur_ms"]] <- 100}
        if(!"center_on" %in% names(opts$baseline_correction)) {opts[["baseline_correction"]][["center_on"]] <- "DISPLAY_ON"}
      }

      ### Downsampling
      if(!"downsample" %in% names(opts)) {
        opts[["downsample"]][["downsampled_freq"]] <- 20
        opts[["downsample"]][["method"]] <- "mean"
      } else{
        ### if a specific downsample field is missing, set to default
        if(!"downsampled_freq" %in% names(opts$downsample)) {opts[["downsample"]][["downsampled_freq"]] <- 20}
        if(!"method" %in% names(opts$downsample)) {opts[["downsample"]][["method"]] <- "mean"}
      }

    } else{
      # if processing options are not specified, set to defaults
      opts <- list()
      opts[["blink_corr"]][["ms_before"]] <- 150
      opts[["blink_corr"]][["ms_after"]] <- 150
      opts[["filter"]][["method"]] <- "movingavg"
      opts[["filter"]][["window_length"]] <- 20
      opts[["interpolate"]][["algor"]] <- "linear"
      opts[["interpolate"]][["maxgap"]] <- 1000
      opts[["baseline_correction"]][["method"]] <- "subtract"
      opts[["baseline_correction"]][["dur_ms"]] <- 100
      opts[["baseline_correction"]][["center_on"]] <- "DISPLAY_ON"
      opts[["downsample"]][["downsampled_freq"]] <- 20
      opts[["downsample"]][["method"]] <- "mean"
    }
    config[["definitions"]][["eye"]][["pupil_preproc"]] <- opts
  }

 return(config)
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
    event_info <- c.e[["msg_parse"]]

    if("msg_seq" %in% names(event_info)){
      if("eval_middle" %in% names(event_info[["msg_seq"]])){
        for(i in names(config[["blocks"]])){
          # check first for an eye field in each event type in a block.
          for(j in names(config[["blocks"]][[i]][["events"]])){
            ev_m <- config[["blocks"]][[i]][["events"]][[j]][["eye"]]
            if(event_info[["msg_seq"]][["eval_middle"]]){
              msg_vec <- c(event_info[["msg_seq"]][["msg_start"]], ev_m[["mid_msg"]], event_info[["msg_seq"]][["msg_end"]])
              c.e[["msg_parse"]][["msg_seq"]][[i]][[j]] <- msg_vec
            }
          }
        }
      }
    }
  }, describe_text = dt)
  return(c.e)
}

#' Print list structure
#'
#' @description this is a very slight ammendment to \code{Hmisc::list.tree} for easy display of lists: http://math.furman.edu/~dcs/courses/math47/R/library/Hmisc/html/list.tree.html
#'
#' @importFrom Hmisc list.tree
#'
#' @export
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
