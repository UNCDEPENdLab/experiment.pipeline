setwd("/Users/sophie_paolizzi/github_repos/experiment.pipeline/inst/examples/")

# Basic DTK structure --------------------------------------------------
##The experiment is divided into 3 phases (Ins and PIT both have 2 blocks + a quiz) in 1 dataset
read_behav_DtK <- function(file){
  
##This top frame contains columns that are either relevant to multiple phases or metadata relevant to none (i.e., identifier variables)
fields <- c('MinName', #stimulus presented (cross-phase indicator of minion type),
            'MinSound', #sound played presented (cross-phase indicator of minion type)
            "sequence #", #when was this task presented comapred to the other 3? 001, 002, 003, 004
            "block", #which block is currently represented. Instr, Instr-2, Pav, PIT, PIT_2)
            "Color", #which colored shield comes up on a given trial
            "participant", #pt num
            "group", #GO, GT, OT. Which two minions were they trained on in the instrumental phase? 
            "participant initials", #metadata for subject
            "date" #Session date for matching
)
instr <- c("corrAns", #b or n. This key will block attack
            "incorrAns", #b or n. This key is ineffective at blocking attacks
            "Attacker.started", # timing for when a minion appeared on screen. These occur when pts are not pressing shields. shoiuld be a time in milliseconds?
            "image_14.started", #timing of resultant fixation the screen (i.e., occurs when participant pressed a key: also for forst 2 sec. of task block )
            "fixation.started", # timing of trial-period fixation (i.e., pressing during this frame ensures that image_14 is displayed. Not pressing during this frame should lead to an attack. Should be 2s between this and Attacker/image_14)
           "block" #first or second block (Instr, Instr-2)
           )
##To pull and tranform into columns from log: DATA: KEYPRESS: TIMING for keypresses

instr_quiz <- c('MinName', #stimulus presented (cross-phase indicator of minion type)
           "corrAns", #b or n. This key will blocks attack and we want to know what the correct response is
           "InsAssess.started", #when quiz questions about block 1 of intsr were asked (needed for log file matching)
           "InsAssess.response", # numeric response indicating effectiveness of each key in block 1 ('instr", obatined from slider)
           "slider.started",#when quiz questions about block 2 of instr were asked (needed for log file matching)
           "slider.response" # numeric response indicating effectiveness of each key in block 2 ('instr_2", obatined from slider)
)
##To pull and tranform into columns from log: quiz questions/timing

Pav <- c( "corRes", #b, n, space, s and k. This reflects the answer to the quizzes
          "Pavlovian.thisN", #phase trial number, should have 44 trials indexed 0-44
          "image.started", #timing for minion
          "image_2.started" #timing for fixation
          )
          
Pavquiz <- c("PavAsses.thisN", #quiz rep number, 5 of them, 0-4
             "ShieldQuiz.started", # onset time of shield quiz questions
             "resp_2.keys", #responses to quiz
             "resp_2.corr", # are they right? 0 = correct
             "resp_2.rt" #reaction time for quiz response (probably not important)
             )
             
PIT <- c("PIT.thisN", "PIT_2.thisN", # triral nubmers for 2, 24-rep blocks of the PIT phase
         "PITshield.started", #timing of shield on screen
         )

#to parse from log: DATA keypresses, 

#Given their fairly straightforward nature, I'm not going to further explain these. They will be neccessary for matching            
Parallelport_flags <- c("p_port_beginexp.started","p_port_beginexp.stopped",
             "minion_p_port.started","minion_p_port.stopped", 
             "minion_p_port_start","minion_p_port_end",
             "reload_p_port.started","reload_p_port.stopped",  
             "reload_p_port_start","reload_p_port_end",  
             "p_port.started","p_port.stopped",         
             "p_port_start","p_port_end", 
             "p_port_2.started","p_port_2.stopped",
             "p_port_2_start","p_port_2_end",
             "pitshield_p_port.started","pitshield_p_port.stopped",
             "pitshield_p_port_start","pitshield_p_port_end",
             "p_port_3.started", "p_port_3.stopped",        
             "p_port_3_start","p_port_3_end")
             
}        
             