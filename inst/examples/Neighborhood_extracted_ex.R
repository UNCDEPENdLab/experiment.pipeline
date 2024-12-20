
pacman::p_load(dplyr, tidyverse)
setwd("~/github_repos/experiment.pipeline/inst/examples/")

#import raw file
file <- (file.path("~/github_repos/experiment.pipeline/inst/examples/070_neighborhood_behav.csv"))

#Everything we pull from the raw neighborhood file and what it means. 
fields <- c("face", #face stimulus
            "pgo", #probability of outcome(-1 or +1 point), see next column
            "outcome", #1, 0 or -1. get point, no point, or lose point for SIS.
            "block_loop.thisN", #block number of experiment, 0-3
            #"block_loop.thisIndex", #block number in terms sample drawn from the set of blocks... basically denotes order of conditions, not super useful
            "Cavb_trials.thisN", #within-block trial number, 0-39, repeated 4 times
            "obtained", #combo of pgo and outcome. What they actually scored on the trial
            "key_resp_10.keys", #go or no-go. space=go, none=no-go
            "key_resp_10.rt", #rt of keyresp
            "participant", #participant Number
            "participant.initials" #participant initials
)
  #transformations to make data more readable
  dat <- read.csv(file, stringsAsFactors = FALSE)
  dat <- dat[,fields] #retain only columns of interest
  dat <- dat %>% mutate_if(is.character, list(~if_else(. == "", NA_character_, .))) %>% #convert "" to NA
    mutate(id=if_else(is.na(participant), NA_character_, paste(participant, participant.initials, sep="_"))) %>%
    mutate(condition=case_when(
      is.na(pgo) ~ NA_character_,
      pgo == 0.3 & outcome == 1 ~ "no_t_w",
      pgo == 0.3 & outcome == -1 ~ "no_t_a",
      pgo == 0.7 & outcome == 1 ~ "go_t_w",
      pgo == 0.7 & outcome == -1 ~ "go_t_a",
      TRUE ~ NA_character_
    )) %>%
    rename(
      block = block_loop.thisN,
      block_trial = Cavb_trials.thisN,
      key_pressed = key_resp_10.keys,
      rt = key_resp_10.rt
    ) %>%
    mutate(block_trial=block_trial + 1, block = block + 1, trial=1:n()) %>% #use 1-based indexing
    select(-participant, -participant.initials)
  write.csv(dat, "neighborhood_clean_output_ex.csv")
  
  
  
  