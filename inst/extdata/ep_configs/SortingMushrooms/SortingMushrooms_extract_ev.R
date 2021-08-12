gen_SortingMushrooms_eye_events <- function(ep.eye, csv_path = NULL){

  ## need block, block_trial, event, eventn, et.msg, and time variables

  tr_id_msgs <- ep.eye$raw %>% filter(grepl("TRIALID", et.msg)) %>%
                    mutate(x = sub("TRIALID ", "", et.msg)) %>%
                    separate(x, c("block",
                                #   "phase",
                                  "block_trial",
                                  "eventn",
                                  "event"), sep = "_") %>%
                    mutate(trial = cumsum(block_trial != lag(block_trial, default=""))) %>%
                    select(block, trial, block_trial, event, eventn, time, et.msg)


  if(!is.null(csv_path)) {
    write.csv(tr_id_msgs, file = csv_path, row.names = FALSE)
  }

  return(tr_id_msgs)
}

