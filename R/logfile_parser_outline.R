#Overall code for parsing long files
pacman::p_load(tidyverse, dplyr)
#code for reading log files. handy!
log_raw <- read.csv(file = "070_dtk_behav.log", sep = "\n", header = F)


#2.) Splice strings into usable data format using separate (timestamp, event, column for the rest of it: #last column will likely be converted anyway into more readable form)
 #for whatever reason separate works most smoothly if you go step-by-step rather than using one long pipe                                               
#split off timestamp
log_temp <- log_raw %>% separate(V1, into = c("Timestamp","String"), sep = " ", extra = "merge")
#Then split log type and message
log_temp <- log_temp %>% separate(String, into = c("Type","Event"), sep = " ", extra = "merge")

##Ideally pipe will:
 
# Filtering/ identify strings that contain important information ---------------------------------------------------------------
#1.)start of trials
log_trials <- log_temp %>% dplyr::filter(grepl("New trial", Event))

#-InsAsses questions: timestamps
log_Ins_qs<- log_temp %>% dplyr::filter(grepl("On a scale", Event, fixed = TRUE)) %>% dplyr::filter(grepl("DATA", Type, fixed = TRUE))

#Slider Quiz Answers
log_Ins_slider<- log_temp %>% dplyr::filter(grepl("markerPos = ", Event, fixed = TRUE))
      #take only first time value of duplicated answers
log_Ins_slider <- log_Ins_slider[!duplicated(log_Ins_slider$Event), ]

log_ins_quiz <- rbind(log_Ins_qs, log_Ins_slider)

#-DATA: Keypress (DATA Keypress b or n) and attached events
  #Running these in succession appears to produce desired result: would like to eliminate all spaces except quiz space: not sure about the logic for it
log_data <- log_temp %>% dplyr::filter(Type == "\tDATA") 
#remove starting instruction presses
log_data <- log_data[-c(1:11),]
#log_data <- log_data %>% dplyr::filter(!grepl("Keypress: space", Event))
log_data <- log_data %>% dplyr::filter(!grepl("Mouse:", Event))
  #b<- log_temp %>% dplyr::filter(grepl("Keypress: b", Event, fixed = TRUE)) 
  #n<- log_temp %>% dplyr::filter(grepl("Keypress: n", Event, fixed = TRUE))

#-Keypresses for shieldquiz
#contained in "data" above
#shields and minions for trials
log_Pavshields <- log_temp %>% dplyr::filter(grepl("image: image =", Event))
log_PavMins <- log_temp %>% dplyr::filter(grepl("image_2: image =", Event))

log_Pav <- rbind(log_PavMins, log_Pavshields)

#-PITshield
log_PITshields <- log_temp %>% dplyr::filter(grepl("PITshield: image =", Event))

#then create long log file & organize in time
log_clean <- rbind(log_data, log_trials, log_ins_quiz, log_Pav, log_PITshields)
#needs to be numeric for arrange() to work
log_clean$Timestamp <- as.numeric(log_clean$Timestamp)
log_clean <- log_clean %>% arrange(Timestamp)


#3.) Join to csv in time
#then, attempt to integrate into csv
