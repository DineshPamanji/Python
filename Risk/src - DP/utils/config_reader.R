library(yaml)

get_data_path <- function(){
  # Loads the configuration file containing file paths
  config_file <- file.path("configs","file_paths - DP.yml")
  if(!file.exists(config_file)) stop(paste0("Config file not found at : ",config_file))
  configs <- read_yaml(config_file)
  return(configs)
}