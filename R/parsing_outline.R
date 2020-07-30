# NOTE: this is just psuedocode and is not meant to be run. see wrangle.R

# takes raw session 3 files and a task, modality mapping to a parsing function
# outputs a list of behav object models
rawfiles <- # a vector of file paths
ep_subject_task_behav_list <- wrangle(rawfiles, parserYaml)

# takes list of object models and list of qa checking functions (validators)
# output from validators is put in the object model "qa" section
qualityChecks(ep_subject_task_behav_list, validators)

# takes a list of object models that have qa info already
# combines modalities into a consistent structure
ep_subject_task_list <- synthesize(ep_subject_task_behav_list)
