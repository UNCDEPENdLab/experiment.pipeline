add_aois <- function(eye, config, use_raw = FALSE){

  ### 4.2.1 Extract aoi options
  tryCatch.ep({
    c.aoi <- tidy_eye_config(config)[["gaze"]][["aoi"]]
    stopifnot(all(c("indicator", "extraction_method", "extract_coords", "extract_labs", "split_coords") %in% names(c.aoi)))

    if("tag_raw" %in% names(c.aoi)){
      tag_raw <- c.aoi$tag_raw
    } else{
      tag_raw <- FALSE
    }

  }, describe_text = "-- 4.2.1 Extract AOI config options:")

  ### 4.2 pull AOI information into new columns by eventn
  aoi_ref <- gen_aoi_ref(eye,
                         c.aoi,
                         "-- 4.2.2 Generate AOI reference object  (note. currently only regex supported for rectangular AOIs):")

  ### 4.3 tag gaze data with AOI_look field
  eye <- gen_aoi_look(eye,
                      aoi_ref,
                      use_raw = tag_raw,
                      "-- 4.2.3 Generate AOI fields in data:")




}


gen_aoi_look <- function(eye, aoi_ref, dt = NULL, use_raw = FALSE){
  cat(dt, "\n")
  if(use_raw){ ## leaving in as an option, though I think it is probably more important to gauge which AOIs were the focus during saccades (to/from) and fixations.
    dt <- "--- 4.2.3.0 Appending AOIs to raw data"
    tryCatch.ep({
      eye$raw$aoi_look <- "."

      for(i in 1:nrow(eye$raw)) {
        print(i)
        evn <- eye$raw[[i,"eventn"]]

        ev_aois <- aoi_ref %>% dplyr::filter(eventn == evn) %>% mutate(aoi_look = ifelse(eye$raw[[i,"xp"]] >= x1 & eye$raw[[i,"xp"]] <= x2 &
                                                                                           eye$raw[[i,"yp"]] >= y1 & eye$raw[[i,"yp"]] <= y2, TRUE, FALSE))

        if(any(ev_aois$aoi_look)){
          eye$raw[i,"aoi_look"] <- ev_aois %>% dplyr::filter(aoi_look) %>% pull(aoi_lab) %>% paste(collapse = "/") # if gaze falls within 2 aoi rects, will collapse into single aoi_look variable, separated by /. E.g. face/eyes, aoi1/aoi2.
        }

      }

    },describe_text = dt)

    eye$metadata$aoi_ref <- aoi_ref
  } else{
    # saccades
    dt <- "--- 4.2.3.1 Appending AOIs to saccade data"
    tryCatch.ep({eye$gaze$sacc$aoi_start <- "."
    eye$gaze$sacc$aoi_end <- "."
    for (i in 1:nrow(eye$gaze$sacc)) {
      # print(i)
      # i <- 1

      evn <- eye$gaze$sacc[[i,"eventn"]]

      ev_aois <- aoi_ref %>% dplyr::filter(eventn == evn) %>% mutate(aoi_start = ifelse(eye$gaze$sacc[[i,"sxp"]] >= x1 & eye$gaze$sacc[[i,"sxp"]] <= x2 &
                                                                                          eye$gaze$sacc[[i,"syp"]] >= y1 & eye$gaze$sacc[[i,"syp"]] <= y2, TRUE, FALSE),
                                                                     aoi_end = ifelse(eye$gaze$sacc[[i,"exp"]] >= x1 & eye$gaze$sacc[[i,"exp"]] <= x2 &
                                                                                        eye$gaze$sacc[[i,"eyp"]] >= y1 & eye$gaze$sacc[[i,"eyp"]] <= y2, TRUE, FALSE))

      # NAs denote missing measurements at the beginning or end of saccade. These may need to be dumped ultimately. For now, it's easiest to code them as "no aoi" and let later scripts handle this.
      if(all(is.na(ev_aois$aoi_start))) {ev_aois$aoi_start <- FALSE}
      if(all(is.na(ev_aois$aoi_end))) {ev_aois$aoi_end <- FALSE}

      if(any(ev_aois$aoi_start)){
        eye$gaze$sacc[i,"aoi_start"] <- ev_aois %>% dplyr::filter(aoi_start) %>% pull(aoi_lab) %>% paste(collapse = "/") # if gaze falls within 2 aoi rects, will collapse into single aoi_look variable, separated by /. E.g. face/eyes, aoi1/aoi2.
      }

      if(any(ev_aois$aoi_end)){
        eye$gaze$sacc[i,"aoi_end"] <- ev_aois %>% dplyr::filter(aoi_end) %>% pull(aoi_lab) %>% paste(collapse = "/")
      }

    }
    }, describe_text = dt)


    # fixations
    dt <- "--- 4.2.3.2 Appending AOIs to fixation data"
    tryCatch.ep({eye$gaze$fix$aoi_look <- "."
    for (i in 1:nrow(eye$gaze$fix)) {
      # print(i)
      # i <- 1

      evn <- eye$gaze$fix[[i,"eventn"]]

      ev_aois <- aoi_ref %>% dplyr::filter(eventn == evn) %>% mutate(aoi_look = ifelse(eye$gaze$fix[[i,"axp"]] >= x1 & eye$gaze$fix[[i,"axp"]] <= x2 &
                                                                                         eye$gaze$fix[[i,"ayp"]] >= y1 & eye$gaze$fix[[i,"ayp"]] <= y2, TRUE, FALSE))

      # NAs denote missing measurements
      if(all(is.na(ev_aois$aoi_look))) {ev_aois$aoi_look <- FALSE}


      if(any(ev_aois$aoi_look)){
        eye$gaze$fix[i,"aoi_look"] <- ev_aois %>% dplyr::filter(aoi_look) %>% pull(aoi_lab) %>% paste(collapse = "/") # if gaze falls within 2 aoi rects, will collapse into single aoi_look variable, separated by /. E.g. face/eyes, aoi1/aoi2.
      }


    }}, describe_text = dt)

    eye$metadata$aoi_ref <- data.table(aoi_ref)
  }




  return(eye)
}

gen_aoi_ref <- function(eye, c.aoi, dt){
  tryCatch.ep({

    #append to this df and write to metadata for reference of aoi labels and coordinates per eventn
    aoi_ref <- data.frame()

    for(i in unique(eye$raw$eventn)){
      ev <- eye$raw %>% dplyr::filter(eventn == i)
      aois <- ev$et.msg[grepl(c.aoi$indicator ,ev$et.msg)]

      # N.B. currently aoi coords are tagged as x1, y1, x2, y2 according to the extract_coords specification. This is not very flexible and will want to revisit this for sure.
      for (a in aois) {
        coords <- as.numeric(do.call(c, str_split(str_extract(a, c.aoi$extract_coords), c.aoi$split_coords)))
        lab <- str_extract(a, c.aoi$extract_labs)
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

