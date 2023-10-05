Neighborhood_extract_ev <- function(ep.eye_init, csv_path = NULL){
  
  # extract required information from the eyet data message
  tr_id_msgs <- ep.eye_init$raw %>%
    filter(grepl("IQMARKER", et.msg) & !(et.msg == "IQMARKER 1") & !(et.msg == "IQMARKER 0")) %>%
    filter(grepl("IQMARKER 22", et.msg)) %>% 
    mutate(msg = sub("IQMARKER 22", "", et.msg)) %>%
    mutate(block = case_when(as.numeric(substr(msg,1,1)) == 1 ~ "goToWin",
                             as.numeric(substr(msg,1,1)) == 2 ~ "noGoToWin",
                             as.numeric(substr(msg,1,1)) == 3 ~ "goToAvoid",
                             as.numeric(substr(msg,1,1)) == 4 ~ "noGoToAvoid")) %>%
    mutate(trial = as.numeric(substr(msg,2,4))) %>%
    mutate(event = case_when(as.numeric(substr(msg,5,5)) == 1 ~ "face",
                             as.numeric(substr(msg,5,5)) == 2 ~ "fixation",
                             as.numeric(substr(msg,5,5)) == 3 ~ "feedback"))
    
  # calculate block_trial
  tr_id_msgs$block_trial <- NA
  running_counter <- c(0, 0, 0, 0)
  for (q in unique(tr_id_msgs$trial)){
    indx_counter <- as.numeric(substr(tr_id_msgs$msg[tr_id_msgs$trial == q][1], 1, 1))
    running_counter[indx_counter] <- running_counter[indx_counter] + 1
    tr_id_msgs$block_trial[tr_id_msgs$trial == q] <- running_counter[indx_counter]
  }
  
  # final trial message dataframe
  tr_id_msgs <- tr_id_msgs %>% select(block, trial, block_trial, event, eventn, time, et.msg)
  
  # saving the trial message dataframe in a csv file
  if(!is.null(csv_path)) {
    write.csv(tr_id_msgs, file = csv_path, row.names = FALSE)
  }
  
  return(tr_id_msgs)
}