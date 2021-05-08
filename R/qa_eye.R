


qa_eye <- function(eye, config, signal = c("gaze", "pupil")){

  # ensures preprocessing was completed. Functions herein may call on values that only appear in the preprocessed data
  stopifnot("ep.eye.preproc" %in% class(eye))


  ### 1.1 Extract qa configuration options
  tryCatch.ep({
    c.qa <- tidy_eye_config(config)[["qa"]]
  }, describe_text = "- 1.1 Extract QA config options:")


  ### Add QA element to eye
  qa <- list()

  ######
  ### 1 Remove events with a large number of NAs
  ######
  for(s in names(c.qa)){
    qa[[s]][["event_nas"]] <- qa_event_na(eye,
                                          check = c.qa[[s]][["na"]][["check"]],
                                          signal = s,
                                          perc = c.qa[[s]][["na"]][["perc"]],
                                          cols = c.qa[[s]][["na"]][["cols"]])
  }

  ######
  ### 2 Gaze QA: extract descriptive information on gaze events by eventn
  ######
  qa[["gaze"]][["event_gevs"]] <- gevs_per_event(eye$gaze$downsample, rm_evs = eye$qa$missing$gaze_downsample_exclude)

  ######
  ### 3 Gaze QA: fixation duration plots
  ######
  qa[["gaze"]][["fixation_plots"]] <- fix_plots(df = eye$gaze$downsample, event_na = qa$gaze$event_nas$gaze_downsample_percmissing)
  qa[["gaze"]][["heatmaps"]] <- gen_heatmaps(df = eye$gaze$downsample, event_na = qa$gaze$event_nas$gaze_downsample_percmissing)

}



gen_heatmaps <- function(df, event_na){
  require(viridis)
  heatmaps <- list()
  # browser()
  bad_trials <- event_na %>% filter(exclude == 1) %>% pull(eventn)

  #### average per block and event
  sumdf <- df %>% group_by(block, event, time_ev) %>% summarise(mx = mean(xp,na.rm = TRUE), my = mean(yp, na.rm = TRUE))

  pp_total <- ggplot(sumdf, aes(x = mx, y = my))  +
    stat_density2d(geom = "raster", aes(fill = after_stat(density)), contour = FALSE) +
    annotate("rect", xmin = aoi_info$x1, xmax = aoi_info$x2,
             ymin = aoi_info$y1, ymax = aoi_info$y2, alpha = 1, fill = NA,
             color = "white", cex = .1)+
    geom_point(color = "white", alpha = .005) + scale_fill_viridis(option = "A",direction = 1) +
    xlim(0, eye$metadata$screen.x) + ylim(0, eye$metadata$screen.y) +
    theme_classic() + theme(legend.position = "none") + facet_nested(block ~ event)
    # labs(title = paste0("Block: ", unique(event$block), ". Trial: ", unique(event$block_trial)),
    #      subtitle = paste0(unique(event$event), " (", unique(event$eventn), ")")) +



  for(ev in unique(df$eventn)){

    missperc <- event_na %>% dplyr::filter(eventn == ev) %>% pull(xp)


    aoi_info <- eye$metadata$aoi_ref %>% filter(eventn == ev)


    event <- eye$raw %>% filter(eventn == ev) %>% left_join(eye$gaze$fix, by = c("eventn", "fixn", "block", "block_trial", "event")) %>%
      mutate(aoi_look = ifelse(is.na(aoi_look), paste0("(missing/NA): ", round(missperc*100,2), "%"), aoi_look))


    pp <- ggplot(event, aes(x = xp, y = yp))  +
      stat_density2d(geom = "raster", aes(fill = after_stat(density)), contour = FALSE) +
      annotate("rect", xmin = aoi_info$x1, xmax = aoi_info$x2,
               ymin = aoi_info$y1, ymax = aoi_info$y2, alpha = 1, fill = NA,
               color = "white", cex = .1)+
      geom_point(color = "white", alpha = .005) + scale_fill_viridis(option = "A",direction = 1) +
      xlim(0, eye$metadata$screen.x) + ylim(0, eye$metadata$screen.y) +
      theme_classic() + theme(legend.position = "none") +
      labs(title = paste0("Block: ", unique(event$block), ". Trial: ", unique(event$block_trial)),
           subtitle = paste0(unique(event$event), " (", unique(event$eventn), ")"))



    if(ev %in% bad_trials){
      pp<- pp + theme(panel.background = element_rect(fill = "darkred"))
    }
    heatmaps[[ev]] <- pp
  }

  return(heatmaps)

}
fix_plots <- function(df, event_na){

  require(see)

  #### fixation data
  fix_plots <- list()
  for(ev in unique(df$eventn)){

    missperc <- event_na %>% dplyr::filter(eventn == ev) %>% pull(xp)

    event <- df %>% filter(eventn == ev) %>% left_join(eye$gaze$fix, by = c("eventn", "fixn", "block", "block_trial", "event")) %>%
      mutate(aoi_look = ifelse(is.na(aoi_look), paste0("(missing/NA): ", round(missperc*100,2), "%"), aoi_look))

    look_for <- c("DISPLAY ON", "mouse on", "ENDBUTTON", "DISPLAY OFF")

    markers <- event %>% filter(grepl(paste(look_for, collapse = "|"), et.msg)) %>% select(time_ev, et.msg)

    pp <- ggplot(event, aes(x = time_ev, y = aoi_look)) + geom_point(color = "white") + theme_abyss() + geom_vline(xintercept = markers$time_ev, color = "white") +
      geom_text(data = markers, mapping = aes(label = et.msg, y = .01), angle = 90, color = "yellow", size = 3.5, vjust = 2, hjust = -0.005) +
      labs(title = paste0("Block: ", unique(event$block), ". Trial: ", unique(event$block_trial)),
           subtitle = paste0(unique(event$event), " (", unique(event$eventn), ")"))

    bad_trials <- event_na %>% filter(exclude == 1) %>% pull(eventn)
    if(ev %in% bad_trials){
      pp<- pp + theme(panel.background = element_rect(fill = "darkred"))
    }
    fix_plots[[ev]] <- pp
  }

  ## fixation summaries
  # df_fixdat

  return(fix_plots)
}
gevs_per_event <- function(df, rm_evs){
  # install.packages("ggh4x")
  library(ggh4x)

  out <- desc <- df %>% group_by(eventn) %>% dplyr::summarise(block = unique(block),
                                                              event = unique(event),
                                                              block_event = paste(unique(block), unique(event), sep = "_"),
                                                              nblink = length(unique(blinkn)),
                                                              nsacc = length(unique(saccn)),
                                                              nfix =length(unique(fixn)),
                                                              .groups = "drop")

  ddf <- reshape2::melt(desc, id.vars = c("eventn", "block", "event", "block_event"))
  gev_ev <- ggplot(ddf, aes(x = value) ) + geom_histogram(bins =15) +
    ggtitle("Number of gaze events per event (whole task)") +
    facet_nested(variable ~ block + event, scales = "free_y") + theme_bw()


  ### n gevs per event, removing high NA trials
  df_retain <- df %>% filter(!eventn %in% rm_evs)
  n_rm <- length(rm_evs)

  desc <- df_retain %>% group_by(eventn) %>% dplyr::summarise(block = unique(block),
                                                              event = unique(event),
                                                              block_event = paste(unique(block), unique(event), sep = "_"),
                                                              nblink = length(unique(blinkn)),
                                                              nsacc = length(unique(saccn)),
                                                              nfix =length(unique(fixn)),
                                                              .groups = "drop")

  ddf <- reshape2::melt(desc, id.vars = c("eventn", "block", "event", "block_event"))
  gev_ev2 <- ggplot(ddf, aes(x = value) ) + geom_histogram(bins =15) +
    ggtitle(paste0("Number of gaze events per event (excluded trials [n=", n_rm,"] removed)")) +
    facet_nested(variable ~ block + event, scales = "free_y") + theme_bw()



  out <- out %>% mutate(exclude = ifelse(eventn %in% rm_evs, 1, 0))

  return(list(plot = cowplot::plot_grid(gev_ev, gev_ev2, nrow =  2),
              gev_per_event = out))
}
qa_event_na <- function(eye, check, signal, perc, cols){

  # convert to decimal value between 0 and 1
  if(perc >1){perc <- perc/100}

  stopifnot(any(check %in% c("raw", "preprocessed", "downsample")))

  #### grab df of interest
  missing <- list()
  for(ch in check){
    if(ch == "raw"){
      df <- eye$raw
      tag <- "raw"
    } else {
      df <- eye[[signal]][[ch]]
      tag <- paste0(signal, "_", ch)
    }


    # percentage of missing data
    df_perc <- df %>% group_by(eventn) %>%
      dplyr::summarise(rows = n(),
                       #across(cols, ~sum(is.na(.x))),
                       across(all_of(cols), ~sum(is.na(.x)))/rows,
                       .groups = "drop")
    for(co in cols){
      df_perc[,paste0(co, "_exclude")] <- ifelse(df_perc[,co] > perc, 1,0)
    }

    exclude_cols <- names(df_perc)[which(grepl("_exclude", names(df_perc)))]

    df_perc$exclude <- ifelse(rowSums(df_perc %>% select(all_of(exclude_cols))) == 0, 0, 1)


    missing[[paste0(tag, "_percmissing")]] <- df_perc
    missing[[paste0(tag, "_exclude")]] <- df_perc %>% filter_at(.vars = c(exclude_cols, "exclude"),
                                                                any_vars(. == 1)) %>% pull(eventn)
  }

  return(missing)
}







# # scratch -----------------------------------------------------------------
#
# pdf(file = "NH_local/fix_plots.pdf", width = 11, height = 8)
# qa[["gaze"]][["fixation_plots"]]
# dev.off()
#
# pdf(file = "NH_local/heatmaps.pdf", width = 11, height = 8)
# qa[["gaze"]][["heatmaps"]]
# dev.off()
