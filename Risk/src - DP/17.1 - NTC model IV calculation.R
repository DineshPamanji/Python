############################################################################################
################## 11 - Calculate IV  ######################################################
############################################################################################


## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)


## 1. Load data -----------------------------------------------------------------------------

## 1.1 load NTC data
load_rdata_intermediate("model_data//ntc_data.rdata")


## 2. subset of loan types  ------------------------------------------------------------------

ntc_model_data_2W_New <- ntc_model_data %>% filter((loan_type == '2W-New') & (Category %in% c('SENP','SEP','SAL')))
ntc_model_data_PV_New <- ntc_model_data %>% filter((loan_type == 'PV-New') & (Category %in% c('SENP','SEP','SAL')))
ntc_model_data_PV_Used <- ntc_model_data %>% filter((loan_type == 'PV-Used') & (Category %in% c('SENP','SEP','SAL')))

rm(ntc_model_data)



## 3. calculate IV ------------------------------------------------------------------------------

## 3.1 Define variable list for which IV has to be calculated
var_list <- c('age',
              'phone_count_3m',
              'months_since_last_update_addr',
              'phone_count_6m',
              'addr_count_6m',
              'phone_count_12m',
              'months_since_last_update_phone',
              'addr_count_3m',
              'owner',
              'agri_profile_flag')

dependent_variable <- 'bad_loan'


## 3.2 IV calculation function
calculate_IV <- function(var_list, dependent_variable, model_data, bins){
  
  iv_data <- create_infotables(data = model_data %>% dplyr::select(c(var_list,dependent_variable)),
                               y = dependent_variable,
                               bins = bins,
                               parallel = FALSE)
  
  variable_IV <- data.frame(iv_data$Summary)
  
  return(variable_IV)
  
}


## 3.3 2W new IV
IV_2W_New_combined <- calculate_IV(var_list = var_list,
                                   dependent_variable = dependent_variable, 
                                   model_data = ntc_model_data_2W_New,
                                   bins = 5)

## 3.4 PV new IV
IV_PV_New_combined <- calculate_IV(var_list = var_list,
                                   dependent_variable = dependent_variable, 
                                   model_data = ntc_model_data_PV_New,
                                   bins = 5)

## 3.5 PV used IV
IV_PV_Used_combined <- calculate_IV(var_list = var_list,
                                   dependent_variable = dependent_variable, 
                                   model_data = ntc_model_data_PV_Used,
                                   bins = 5)





## 5. Save output ---------------------------------------------------------------------------------

## 5.1 save IV outputs
save_csv_output(IV_2W_New_combined,"IV//NTC//IV_2W_New_Combined.csv")
save_csv_output(IV_PV_New_combined,"IV//NTC//IV_PV_New_Combined.csv")
save_csv_output(IV_PV_Used_combined,"IV//NTC//IV_PV_Used_Combined.csv")




assert_data_non_empty(ntc_model_data_2W_New)
save(ntc_model_data_2W_New,
     file = file.path(
       get_data_path()$data$intermediate,
       "model_data",
       "ntc_model_data_2W_New.rdata"
     )
)


assert_data_non_empty(ntc_model_data_PV_New)
save(ntc_model_data_PV_New,
     file = file.path(
       get_data_path()$data$intermediate,
       "model_data",
       "ntc_model_data_PV_New.rdata"
     )
)

assert_data_non_empty(ntc_model_data_PV_Used)
save(ntc_model_data_PV_Used,
     file = file.path(
       get_data_path()$data$intermediate,
       "model_data",
       "ntc_model_data_PV_Used.rdata"
     )
)


rm(list=ls())







