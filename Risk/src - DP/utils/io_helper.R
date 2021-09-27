config_path <- file.path("src - DP","utils","config_reader.R")
source(config_path)



## 1. Validate & read functions -----------------------------------------------------

## 1.1 Validate & read using fread
validate_and_fread <- function(file_path){
  if(!file.exists(file_path)){
    stop(paste0("File not found at: ", file_path))
  }
  
  tryCatch(
    {
      input_data <- fread(file_path) 
    }, error = function(e){
      input_data <- fread(file_path, fill = TRUE, blank.lines.skip = TRUE)  
    }
    
  )
  
  if((is.null(dim(input_data))) || (dim(input_data)[1] == 0)) {
    stop(paste0("Data file is empty at: ", file_path))
  }
  
  return(input_data)
}


## 1.2 Validate & read .rdata file
validate_and_load_rdata <- function(file_path){
  if(!file.exists(file_path)){
    stop(paste0("File not found at: ", file_path))
  }
  
  load(file_path, envir = globalenv(), verbose = TRUE)
}



## 2. Validate & save functions ----------------------------------------------------

## 2.1 Validate & save csv
validate_and_save_csv <- function(csv_data, file_path){
  if((is.null(dim(csv_data))) || (dim(csv_data)[1] == 0)) {
    stop(paste0("Data is empty! Not writing to: ", file_path))
  }
  #if(file.exists(file_path))
  
  write.csv(csv_data, file = file_path)
  
}

validate_and_save_xlsx <- function(data, file_path){
 
  write_xlsx(data, path = file_path)
  
}

## 3. Wrapper read functions ---------------------------------------------------------

## 3.1 Read using fread from raw data
fread_raw <- function(relative_path){
  # check if file exists & load
  file_path <- file.path(get_data_path()$data$raw, relative_path)
  return(validate_and_fread(file_path))
}


## 3.2 Load .rdata files from intermediate data folder
load_rdata_intermediate <- function(relative_path){
  # check if file exists & load
  file_path <- file.path(get_data_path()$data$intermediate, relative_path)
  return(validate_and_load_rdata(file_path))
}


## 3.3 Read using fread from mapping data
fread_mapping <- function(relative_path){
  # check if file exists & load
  file_path <- file.path(get_data_path()$data$mapping, relative_path)
  return(validate_and_fread(file_path))
}


## 3.4 Load .rdata files from output data folder
load_rdata_output <- function(relative_path){
  # check if file exists & load
  file_path <- file.path(get_data_path()$data$output, relative_path)
  return(validate_and_load_rdata(file_path))
}


## 4. Wrapper save functions ------------------------------------------------------------


assert_data_non_empty <- function(data){
  # check if data is empty & if yes stop execution
  if((is.null(dim(data))) || (dim(data)[1] == 0)) {
    stop(paste0("Data is empty!"))
  }
}

save_csv_output <- function(data,relative_path){
  full_path <- file.path(get_data_path()$data$output,relative_path)
  validate_and_save_csv(data,full_path)
}

save_xlsx_output <- function(data,relative_path){
  full_path <- file.path(get_data_path()$data$output,relative_path)
  validate_and_save_xlsx(data,full_path)
}

