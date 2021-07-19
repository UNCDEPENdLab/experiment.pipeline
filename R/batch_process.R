#' batch processing of edf files across tasks
#' 

library(tidyverse)
# install.packages("functional")
library(functional)

group_dir <- "/proj/mnhallqlab"
# group_dir <- "/Users/lab/ll" # over sshfs mount.

nm_data_dir <- file.path(group_dir, "studies/NeuroMAP/s3_data")
task_specs <- file.path(group_dir, "studies/NeuroMAP/s3_data_ep_specs")

source("/proj/mnhallqlab/users/nate/experiment.pipeline/NH_local/setup_envi.R")
source(file.path(group_dir, "users/nate/experiment.pipeline/R/read_process_eye.R")) # will be imported from library

task_df <- data.frame(task = list.dirs(nm_data_dir, recursive = FALSE, full.names = FALSE)) %>% filter(task != "Physio") %>%
                mutate(config_path = c(NA,
                                  NA,
                                  NA, 
                                  file.path(task_specs,"yaml/Neighborhood_PSU.yaml"),
                                  file.path(task_specs, "yaml/Sorting_Mushrooms.yaml"),
                                  file.path(task_specs, "yaml/Sorting_Mushrooms.yaml"),
                                  NA,
                                  NA,
                                  NA,
                                  NA))
     


ep_batch_process_eye <- function(task_df, mc.cores = 1, rslurm = TRUE, rslurm_config = NULL, rslurm_opts = list(time = '1:00:00')){

    for(t in task_df$task){
     t <- "Neighborhood_PSU"
     this.task <- task_df %>% filter(task == t)
     
     
     edf_raws <- list.files(file.path(nm_data_dir, t, "eye"),full.names = TRUE)
        
     if(rslurm){
         #hard-coded for now.
         rslurm_config <- list(cpus_per_node = 26,
                               node_request = ceiling(length(edf_raws)/26),
                               jobname = t
                             )
         stopifnot(!is.null(rslurm_config)) #need basic configuration of rslurm to be specified as a list.
         
         

         # should contain everything needed to analyze a single subject through read_process_eye.
         file <- edf_raws[1]
         config_path <- this.task$config_path

         slurm_apply(f = read_process_eye, 
              params = data.frame(file = edf_raws, config_path = this.task$config_path), ## single config file should be used across subjects. 
              nodes = rslurm_config$node_request,
              cpus_per_node = rslurm_config$cpus_per_node,
              jobname = rslurm_config$jobname,
              global_objects = ls(),
              submit = TRUE,
              slurm_options = sopt
              )

     }   


    }


}



# task_df <- data.frame(task = list.dirs(nm_data_dir, recursive = FALSE, full.names = FALSE)) %>% filter(task != "Physio") %>%
#                 mutate(
                    
#                     config = c(NA,
#                                   NA,
#                                   NA, 
#                                   file.path(task_specs,"yaml/Neighborhood_PSU.yaml"),
#                                   file.path(task_specs, "yaml/Sorting_Mushrooms.yaml"),
#                                   file.path(task_specs, "yaml/Sorting_Mushrooms.yaml"),
#                                   NA,
#                                   NA,
#                                   NA,
#                                   NA)
#                                   ,
#                     gen_eye_events = c(NA,
#                                            NA,
#                                            NA,
#                                            file.path(task_specs, "gen_eye_events/gen_Neighborhood_PSU_eye_events.R"),
#                                            NA,
#                                            NA,
#                                            NA,
#                                            file.path(task_specs, "gen_eye_events/gen_Vanilla_Baseline_PSU_eye_events.R"),
#                                            NA,
#                                            file.path(task_specs, "gen_eye_events/gen_Vending_Machine_PSU_eye_events.R")
#                                            ))



     #  ev_f <- source(this.task$gen_eye_events)$value     

        #  ### if a function is supplied, use it to generate event csvs.

        #  x <- ev_f(edf_raws[1])
        # #  edf_files <- edf_raws[1]

        #  y <- edf2asc(edf_files, asc_output_dir=asc_output_dir, gzip_asc=gzip_asc)
