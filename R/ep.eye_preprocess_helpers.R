############################
##### List of subsidiary functions utilized in `ep.eye_preprocess_gaze()`
############################
# - ep.eye_add_aois()
# -- ep.eye_gen_aoi_ref()
# -- ep.eye_gen_aoi_look()
# - ep.eye_collapse_time()
############################

ep.eye_add_aois <- function(ep.eye, 
                            indicator,
                            extract_coords,
                            extract_labs,
                            split_coords,
                            tag_raw = FALSE
                            ){
  ### 4.1.1 pull AOI information into new columns by eventn
  aoi_ref <- ep.eye_gen_aoi_ref(ep.eye,
                         indicator,
                         extract_coords,
                         extract_labs,
                         split_coords,
                         dt = "-- 4.1.1 Generate AOI reference object  (note. currently only regex supported for rectangular AOIs):")

  ### 4.1.2 tag gaze data with AOI_look field
  ep.eye <- ep.eye_gen_aoi_look(ep.eye,
                         aoi_ref,
                         tag_raw,
                         dt = "-- 4.1.2 Generate AOI fields in data:")

  return(ep.eye)
}

ep.eye_gen_aoi_ref <- function(ep.eye,
                               indicator,
                               extract_coords,
                               extract_labs,
                               split_coords,
                               dt){
  tryCatch.ep({

    #append to this df and write to metadata for reference of aoi labels and coordinates per eventn
    aoi_ref <- data.frame()

    for(i in unique(ep.eye$raw$eventn)){
      ev <- ep.eye$raw %>% dplyr::filter(eventn == i)
      aois <- ev$et.msg[grepl(indicator ,ev$et.msg)]

      # N.B. currently aoi coords are tagged as x1, y1, x2, y2 according to the extract_coords specification. This is not very flexible and will want to revisit this for sure.
      for (a in aois) {
        coords <- as.numeric(do.call(c, str_split(str_extract(a, extract_coords), split_coords)))
        lab <- str_extract(a, extract_labs)
        aoi_ref <- rbind(aoi_ref, data.frame(eventn = i,
                                             aoi_msg = a,
                                             x1 = coords[1], y1 = coords[2], x2 = coords[3], y2 = coords[4],
                                             aoi_lab = lab))
      }
    }
  }, describe_text = dt)

  aoi_ref$aoi_lab <- as.character(aoi_ref$aoi_lab)
  return(aoi_ref)
}

ep.eye_gen_aoi_look <- function(ep.eye, aoi_ref, tag_raw = FALSE, dt = NULL){
  cat(dt, "\n")
  if(tag_raw){ ## leaving in as an option, though I think it is probably more important to gauge which AOIs were the focus during saccades (to/from) and fixations.
    dt <- "--- 4.1.2.0 Appending AOIs to raw data"
    tryCatch.ep({
      ep.eye$raw$aoi_look <- "."
      for(i in 1:nrow(ep.eye$raw)) {
        print(i)
        evn <- ep.eye$raw[[i,"eventn"]]
        ev_aois <- aoi_ref %>% dplyr::filter(eventn == evn) %>% group_by(aoi_lab) %>% mutate(aoi_look = ifelse(ep.eye$raw[[i,"xp"]] >= min(x1,x2) & ep.eye$raw[[i,"xp"]] <= max(x1,x2) &
                                                                                           ep.eye$raw[[i,"yp"]] >= min(y1,y2) & ep.eye$raw[[i,"yp"]] <= max(y1,y2), TRUE, FALSE)) %>% ungroup()

        if(any(ev_aois$aoi_look)){
          ep.eye$raw[i,"aoi_look"] <- ev_aois %>% dplyr::filter(aoi_look) %>% pull(aoi_lab) %>% paste(collapse = "/") # if gaze falls within 2 aoi rects, will collapse into single aoi_look variable, separated by /. E.g. face/eyes, aoi1/aoi2.
        }
      }
    },describe_text = dt)
  } 

  # saccades
  dt <- "--- 4.1.2.1 Appending AOIs to saccade data"
  tryCatch.ep({
    ep.eye$gaze$sacc$aoi_start <- "."
    ep.eye$gaze$sacc$aoi_end <- "."
    for (i in 1:nrow(ep.eye$gaze$sacc)) {
      # print(i)
      i <- 1

      evn <- ep.eye$gaze$sacc[[i,"eventn"]]

      ev_aois <- aoi_ref %>% dplyr::filter(eventn == evn) %>% group_by(aoi_lab) %>% mutate(aoi_start = ifelse(ep.eye$gaze$sacc[[i,"sxp"]] >= min(x1,x2) & ep.eye$gaze$sacc[[i,"sxp"]] <= max(x1,x2) &
                                                                                                                ep.eye$gaze$sacc[[i,"syp"]] >= min(y1,y2) & ep.eye$gaze$sacc[[i,"syp"]] <= max(y1,y2), TRUE, FALSE),
                                                                                            aoi_end = ifelse(ep.eye$gaze$sacc[[i,"exp"]] >= min(x1,x2) & ep.eye$gaze$sacc[[i,"exp"]] <= max(x1,x2) &
                                                                                                              ep.eye$gaze$sacc[[i,"eyp"]] >= min(y1,y2) & ep.eye$gaze$sacc[[i,"eyp"]] <= max(y1,y2), TRUE, FALSE)) %>% ungroup()

      # NAs denote missing measurements at the beginning or end of saccade. These may need to be dumped ultimately. For now, it's easiest to code them as "no aoi" and let later scripts handle this.
      if(all(is.na(ev_aois$aoi_start))) {ev_aois$aoi_start <- FALSE}
      if(all(is.na(ev_aois$aoi_end))) {ev_aois$aoi_end <- FALSE}

      if(any(ev_aois$aoi_start)){
        ep.eye$gaze$sacc[i,"aoi_start"] <- ev_aois %>% dplyr::filter(aoi_start) %>% pull(aoi_lab) %>% paste(collapse = "/") # if gaze falls within 2 aoi rects, will collapse into single aoi_look variable, separated by /. E.g. face/eyes, aoi1/aoi2.
      }

      if(any(ev_aois$aoi_end)){
        ep.eye$gaze$sacc[i,"aoi_end"] <- ev_aois %>% dplyr::filter(aoi_end) %>% pull(aoi_lab) %>% paste(collapse = "/")
      }

    }
  }, describe_text = dt)


  # fixations
  dt <- "--- 4.1.2.2 Appending AOIs to fixation data"
  tryCatch.ep({
    ep.eye$gaze$fix$aoi_look <- "."
    for (i in 1:nrow(ep.eye$gaze$fix)) {
      # print(i)
      # i <- 1

      evn <- ep.eye$gaze$fix[[i,"eventn"]]

      ev_aois <- aoi_ref %>% dplyr::filter(eventn == evn) %>% group_by(aoi_lab) %>% mutate(aoi_look = ifelse(ep.eye$gaze$fix[[i,"axp"]] >= min(x1,x2) & ep.eye$gaze$fix[[i,"axp"]] <= max(x1,x2) &
                                                                                          ep.eye$gaze$fix[[i,"ayp"]] >= min(y1,y2) & ep.eye$gaze$fix[[i,"ayp"]] <= max(y1,y2), TRUE, FALSE)) %>% ungroup()

      # NAs denote missing measurements
      if(all(is.na(ev_aois$aoi_look))) {ev_aois$aoi_look <- FALSE}


      if(any(ev_aois$aoi_look)){
        ep.eye$gaze$fix[i,"aoi_look"] <- ev_aois %>% dplyr::filter(aoi_look) %>% pull(aoi_lab) %>% paste(collapse = "/") # if gaze falls within 2 aoi rects, will collapse into single aoi_look variable, separated by /. E.g. face/eyes, aoi1/aoi2.
      }


    }}, describe_text = dt)

  ep.eye$metadata$aoi_ref <- data.table(aoi_ref)
  return(ep.eye)
}

#' collapse timing to one row per time (paste multiple messages in a single et.msg)
#'
#'
ep.eye_collapse_time <- function(ep.eye, dt){
  tryCatch.ep({
    ep.eye$raw <- ep.eye$raw %>% data.frame() %>%
      dplyr::group_by(eventn, time, xp, yp, ps, saccn, fixn, blinkn, block, block_trial, event) %>%
      dplyr::summarise(et.msg = paste(et.msg, collapse = " | "), .groups = "drop") %>% data.table()
    # browser()

    if(unique(table(ep.eye$raw$time)) != 1){
      warning("Some measurements have more than one row")
    }
  },
  describe_text = dt
  )
  return(ep.eye)
}

