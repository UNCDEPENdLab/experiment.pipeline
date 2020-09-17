#Load DtK example
pacman::p_load(dplyr, tidyverse)
setwd("~/github_repos/experiment.pipeline/inst/examples/")

DtK_raw <- data.table::fread("~/github_repos/experiment.pipeline/inst/examples/070_dtk_behav.csv")
log_raw <- read.csv(file = "070_dtk_behav.log", sep = "\n", header = F)


# Log Cleaning ------------------------------------------------------------

#2.) Splice strings into usable data format using separate (Event_time, event, column for the rest of it: #last column will likely be converted anyway into more readable form)
#for whatever reason separate works most smoothly if you go step-by-step rather than using one long pipe                                               
#split off Event_time
log_temp <- log_raw %>% separate(V1, into = c("Event_time","String"), sep = " ", extra = "merge")
#Then split log type and message
log_temp <- log_temp %>% separate(String, into = c("Type","Event"), sep = " ", extra = "merge")
#1.)start of trials
log_trials <- log_temp %>% dplyr::filter(grepl("New trial", Event))


#-DATA: Keypress (DATA Keypress b or n) and attached events
#Running these in succession appears to produce desired result: would like to eliminate all spaces except quiz space: not sure about the logic for it
log_data <- log_temp %>% dplyr::filter(Type == "\tDATA") 
#remove starting instruction presses
log_data <- log_data[-c(1:11),]
#log_data <- log_data %>% dplyr::filter(!grepl("Keypress: space", Event))
log_data <- log_data %>% dplyr::filter(!grepl("Mouse:", Event))
#b<- log_temp %>% dplyr::filter(grepl("Keypress: b", Event, fixed = TRUE)) 
#n<- log_temp %>% dplyr::filter(grepl("Keypress: n", Event, fixed = TRUE))

### Metadata ---------------------------------------------------------------
metadata <- DtK_raw %>% dplyr::select(MinName, block, corrAns, incorrAns, participant, 
                                      `sequence #`,group, text_15.started, `participant initials`, date) %>%
  dplyr::rename(Switch_time = text_15.started) %>%
  fill(Switch_time, .direction = "downup") %>%
  dplyr::filter(grepl("Instr",block)) %>% dplyr::select(-c(block)) %>% unique()


### Cleaned, event-oriented data frames ---------------------------------------------------------------
DtK_Ins <- DtK_raw %>% dplyr::select(Attacker.started, Fix2.started, fixation.started, block) %>% 
  dplyr::filter(grepl("Ins",block)) #%>% dplyr::select(-c(block)) 
DtK_Ins <- DtK_Ins%>% 
  mutate(Fix_comb = paste0(DtK_Ins$fixation.started, Fix2.started)) %>% 
  mutate(Event_time = paste0(Attacker.started, Fix_comb)) %>%
  mutate(Outcome = ifelse(Attacker.started != "NA", "Attack", "Avoid"))
  
DtK_Ins <- DtK_Ins %>% mutate(Outcome = ifelse(Fix_comb != "NANA", "Avoid", "Attack")) %>%
  #mutate(Event_type = paste0(DtK_Ins$attack_type, DtK_Ins$fix_type)) %>%
  mutate(Event_time= sub("NA", "", Event_time)) %>%
  mutate(Event_time= sub("NA", "", Event_time)) %>%
  ##happens twice on purpose
  mutate(Outcome = sub("NA", "", Outcome)) %>% 
  dplyr::select(-c(Fix_comb, fixation.started, Fix2.started, Attacker.started)) %>% 
  mutate(Event_type = "Minion") %>% na.omit()

###Figuring out log for keypresses

log_clean_ins <- rbind(log_trials, log_data)
#log_clean_ins$Event_time <- as.numeric(log_clean_ins$Event_time)
log_clean_ins <- log_clean_ins %>% arrange(Event_time)
log_clean_ins <- log_clean_ins %>% separate(Event, into = c("Event_type","Outcome"), sep = ":", extra = "merge")
log_clean_ins <- log_clean_ins %>% dplyr::select(Event_time, Outcome, Event_type)
Ins_Keypress <- log_clean_ins %>% dplyr::filter(grepl("Keypress",Event_type)) %>% 
  #rename(Keypress = Event_type) %>% 
  #mutate(string = Event_type) %>% 
  dplyr::filter(Event_time <= max(DtK_Ins$Event_time, na.rm = TRUE)) %>% 
  dplyr::filter(!grepl("space",Event_type)) %>% 
  mutate(block = ifelse(Event_time >= Ins_metadata$Switch_time[1], "Instr_2", "Instr"))
Ins_Trial_Fix <- DtK_raw %>% dplyr::select(image_14.started) %>% 
  dplyr::rename(Trial_Fix = image_14.started) %>% 
  mutate(Event_time = Trial_Fix) %>% na.omit() %>% 
  mutate(Event_type = "Trial_Fix") %>% na.omit() %>%
  mutate(Outcome = "Shield") %>% na.omit() %>%mutate(Trial_Fix = NULL) %>% 
  mutate(block = ifelse(Event_time >= Ins_metadata$Switch_time[1], "Instr_2", "Instr"))

DtK_Ins <- rbind(Ins_Keypress,Ins_Trial_Fix, DtK_Ins) %>% 
  arrange(Event_time) %>% dplyr::select(block, Event_time, Event_type, Outcome)

DtK_Ins_quiz <- DtK_raw %>% 
  dplyr::select("corrAns","InsAssess.started",  
                "InsAssess.response", "slider.started", 
                "slider.response") %>% na.omit() %>% 
  dplyr::rename(b_effective = InsAssess.response) %>%
  dplyr::rename(n_effective = slider.response) %>%
  dplyr::rename(bq_timing = InsAssess.started) %>%
  dplyr::rename(nq_timing = slider.started)
  
DtK$MinName
DtK_Pav <- DtK_raw %>%  dplyr::select('MinName', "Color", "corRes", 
                                  "Pavlovian.thisN","image.started",
                                  "image_2.started") %>% na.omit() %>% 
  dplyr::rename(shield_started = image.started) %>%
  dplyr::rename(minion_started = image_2.started)


DtK_Pav_quiz <- DtK_raw %>%  dplyr::select('MinName', "Color", "corRes", "PavAsses.thisN",
                                       "ShieldQuiz.started", # onset time of shield quiz questions
                                       "resp_2.keys", #responses to quiz
                                       "resp_2.corr", # are they right? 0 = correct
                                       "resp_2.rt") %>% na.omit() %>%
  dplyr::rename(Response = resp_2.keys) %>%
  dplyr::rename(Pt_correct = resp_2.corr) %>%
  dplyr::rename(Response_RT =resp_2.rt)


DtK_PIT <- DtK_raw %>% dplyr::select("PIT.thisN", "PIT_2.thisN",
                                  "PITshield.started", 'MinName', "Color") %>% 
  mutate(PIT_trial = paste0(PIT.thisN, PIT_2.thisN)) %>%
  mutate(PIT_trial= sub("NA", "", PIT_trial)) %>%
  mutate(PIT_block_1 = ifelse(PIT.thisN >= 0, "1", "2")) %>%
  mutate(PIT_block_2 = ifelse(PIT_2.thisN >= 0, "2", "1")) %>%
  mutate(PIT_block = paste0(PIT_block_1, PIT_block_2)) %>%
  mutate(PIT_block= sub("NA", "", PIT_block)) %>%
  dplyr::select(-c(PIT.thisN, PIT_2.thisN,PIT_block_1, PIT_block_2)) %>%
  na.omit()
   

as.data.table(metadata)
as.data.table(DtK_Ins) 
as.data.table(DtK_Ins_quiz)
as.data.table(DtK_Pav)
as.data.table(DtK_Pav_quiz)
as.data.table(DtK_PIT)

DtK_proc <- list(metadata,DtK_Ins, DtK_Ins_quiz, DtK_Pav, DtK_Pav_quiz, DtK_PIT)

names_proc <- c("metadata","DtK_Ins", "DtK_Ins_quiz", "DtK_Pav", "DtK_Pav_quiz", "DtK_PIT")

names(DtK_proc) <- names_proc


# DtK_par_ports <- DtK_raw %>% 
#   dplyr::select(p_port_beginexp.started,p_port_beginexp.stopped, minion_p_port.started,minion_p_port.stopped, 
#                 minion_p_port_start,minion_p_port_end, reload_p_port.started,reload_p_port.stopped, 
#                 reload_p_port_start,reload_p_port_end, p_port.started,p_port.stopped, p_port_start,
#                 p_port_end,p_port_2.started,p_port_2.stopped, p_port_2_start,p_port_2_end, 
#                 pitshield_p_port.started, pitshield_p_port.stopped, pitshield_p_port_start, 
#                 pitshield_p_port_end, p_port_3.started,p_port_3.stopped, p_port_3_start,p_port_3_end)
# 
# DtK_par_pors <- DtK_par_ports %>% mutate(Parport_time = paste0(p_port_beginexp.started, minion_p_port.started, 
#                                                reload_p_port.started, p_port.started, p_port_2.started,
#                                                pitshield_p_port.started, p_port_3.started)) %>% 
#   mutate(Parport_num = paste0(minion_p_port_start, reload_p_port_start, p_port_start, p_port_2_start, pitshield_p_port_start, p_port_3_start))






# DtK_fields <- DtK_raw %>% dplyr::select(MinName, MinSound, `sequence #`, block, Color, participant,
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
                             
                             
                             
                             