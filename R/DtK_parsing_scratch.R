#Load DtK example

DtK <- data.table::fread("~/github_repos/experiment.pipeline/inst/examples/070_dtk_behav.csv")

DtK_Ins <- DtK %>% dplyr::select(MinName, MinSound, block, corrAns, incorrAns, 
                         Attacker.started, image_14.started, Fix2.started, fixation.started, 
                         participant, `sequence #`,group, `participant initials`, date) %>% 
  dplyr::filter(grepl("Ins",block)) %>% dplyr::select(-c(block)) %>%
  rename(Trial_Fix = image_14.started)


DtK_Ins <- DtK_Ins%>% 
  mutate(Fix_comb = paste0(DtK_Ins$fixation.started, Fix2.started)) %>% 
  mutate(Event_time = paste0(DtK_Ins$Attacker.started, DtK_Ins$Fix_comb)) %>%
  mutate(attack_type = ifelse(Attacker.started != "NA", "Attack", "Avoid")) 
  
DtK_Ins <- DtK_Ins %>% mutate(Event_type = ifelse(Fix_comb != "NANA", "Avoid", "Attack")) %>%
  #mutate(Event_type = paste0(DtK_Ins$attack_type, DtK_Ins$fix_type)) %>%
  mutate(Event_time= sub("NA", "", Event_time)) %>% 
  mutate(Event_time= sub("NA", "", Event_time)) %>% ##happens twice on purpose
  mutate(Event_type = sub("NA", "", Event_type)) %>% 
  dplyr::select(-c(Fix_comb, attack_type, fixation.started, Fix2.started, Attacker.started)) %>% 
  mutate(Event_time = as.numeric(Event_time))

###Figuring out log for keypresses

log_clean_ins <- rbind(log_trials, log_data)
log_clean_ins$Timestamp <- as.numeric(log_clean_ins$Timestamp)
log_clean_ins <- log_clean_ins %>% arrange(Timestamp)
log_clean_ins <- log_clean_ins %>% separate(Event, into = c("Event_type","Outcome"), sep = ":", extra = "merge")
log_clean_ins <- log_clean_ins %>% dplyr::select(Timestamp, Event_type, Outcome)
Keypress <- log_clean_ins %>% dplyr::filter(grepl("Keypress",Event_type)) 
Keypress <- Keypress %>% rename(Keypress = Outcome) %>% mutate(Event_type = NULL) %>% 
  dplyr::filter(Timestamp <= max(DtK_Ins$Event_time, na.rm = TRUE)) %>% dplyr::filter(!grepl("space",Keypress))

DtK_ins_quiz <- DtK %>% 
  dplyr::select("corrAns","InsAssess.started",  
                "InsAssess.response", "slider.started", 
                "slider.response") %>% na.omit()
DtK_Pav <- DtK %>%  dplyr::select('MinName', "Color", "corRes", 
                                  "Pavlovian.thisN","image.started",
                                  "image_2.started") %>% na.omit() %>% 
  rename(shield_started = image.started) %>%
  rename(minion_started = image_2.started)


DtK_Pav_quiz <- DtK %>%  dplyr::select('MinName', "Color", "corRes", "PavAsses.thisN",
                                       "ShieldQuiz.started", # onset time of shield quiz questions
                                       "resp_2.keys", #responses to quiz
                                       "resp_2.corr", # are they right? 0 = correct
                                       "resp_2.rt") %>% na.omit() %>%
  rename(Response = resp_2.keys) %>%
  rename(Pt_correct = resp_2.corr) %>%
  rename(Response_RT =resp_2.rt)

DtK_PIT <- DtK %>% dplyr::select("PIT.thisN", "PIT_2.thisN",
                                  "PITshield.started", 'MinName', "Color") %>% 
  mutate(PIT_trial = paste0(PIT.thisN, PIT_2.thisN)) %>%
  mutate(PIT_trial= sub("NA", "", PIT_trial)) %>%
  mutate(PIT_block_1 = ifelse(PIT.thisN >= 0, "1", "2")) %>%
  mutate(PIT_block_2 = ifelse(PIT_2.thisN >= 0, "2", "1")) %>%
  mutate(PIT_block = paste0(PIT_block_1, PIT_block_2)) %>%
  mutate(PIT_block= sub("NA", "", PIT_block)) %>%
  dplyr::select(-c(PIT.thisN, PIT_2.thisN,PIT_block_1, PIT_block_2)) %>%
  na.omit()
   

# DtK <- DtK %>% dplyr::select(MinName, MinSound, `sequence #`, block, Color, participant,
#                       group, `participant initials`, date, corrAns, incorrAns, 
#                      Attacker.started,  image_14.started, fixation.started, corrAns, InsAssess.started, InsAssess.response, slider.started,
#                       slider.response, corRes,Pavlovian.thisN, image.started, image_2.started,
#                       PavAsses.thisN, ShieldQuiz.started, resp_2.keys, 
#                       resp_2.corr, resp_2.rt, PIT.thisN, PIT_2.thisN, PITshield.started, 
#                       p_port_beginexp.started,p_port_beginexp.stopped,
#                       minion_p_port.started,minion_p_port.stopped,  minion_p_port_start,minion_p_port_end,
#                      reload_p_port.started,reload_p_port.stopped,  reload_p_port_start,reload_p_port_end,  
#                      p_port.started,p_port.stopped, p_port_start,p_port_end,  p_port_2.started,p_port_2.stopped,
#                      p_port_2_start,p_port_2_end, pitshield_p_port.started,pitshield_p_port.stopped,
#                      pitshield_p_port_start,pitshield_p_port_end, p_port_3.started, p_port_3.stopped,
#                      p_port_3_start,p_port_3_end)
                             
                             
                             
                             