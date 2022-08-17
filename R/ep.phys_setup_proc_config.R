######
## Validate config file
# - import the yaml config file
# - check if all required data exist in the config file
# - file in the default values for the variables that don't exist
# - process the config[["blocks"]] section to create config$proc_config$code_labels_df
# - Also add config$proc_config$all_mid_codes
######

#' @title Read and setup processing configuration
#' @author Nidhi Desai

ep.phys_setup_proc_config <- function(file, config_path, header = NULL) {
  
  # 1.1 read in the config file
  stopifnot(file.exists(file))
  if (length(file) > 1L) {stop("At present, ep.phys_setup_proc_configs is designed for one file at a time")}
  config <- validate_exp_yaml(config_path)
  
  # 1.2 Add default definitions to the config if not mentioned in the config file
  setup_config_fields <- c("global", "initialize", "ecg_preproc", "eda_preproc", "qa")
  
  config <- ep.phys_set_config_definitions(file, config, setup_config_fields)
  
  # 1.3 Build partport codes sequence for blocks and trials from config file
  config <- ep.phys_build_ttl_seq(config)

  return(config)
}
