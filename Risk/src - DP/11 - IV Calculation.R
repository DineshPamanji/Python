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

## 1.1 load bureau X var 
# load_rdata_intermediate("ADS_data//X_var_bureau.rdata")
# load_rdata_intermediate("ADS_data//X_var_bureau_vcheck.rdata")
load_rdata_intermediate("ADS_data//X_var_bureau_dpd_check.rdata")

## 1.2 load application X var
load_rdata_intermediate("ADS_data//X_var_application.rdata")
all_X_var_application <- all_X_var_application %>% dplyr::select(deal_no,
                                                                 CUSTOMER_PROFILE,
                                                                 disbursal_state,
                                                                 sub_profile,
                                                                 Category,
                                                                 agri_profile_flag)

## 1.3 load default flag
# load_rdata_intermediate("ADS_data//bad_loans.rdata")
load_rdata_intermediate("ADS_data//bad_loans_v2.rdata")
bad_loans$loan_type <- NULL

## 1.4 load gated out population
# load_rdata_output("gating_rules//gated_out_2W_new.rdata")
# load_rdata_output("gating_rules//gated_out_PV_new.rdata")
# load_rdata_output("gating_rules//gated_out_PV_used.rdata")

load_rdata_output("gating_rules//gated_out_2W_new_vcheck.rdata")
load_rdata_output("gating_rules//gated_out_PV_new_vcheck.rdata")
# load_rdata_output("gating_rules//gated_out_PV_used_vcheck.rdata")
load_rdata_output("gating_rules//gated_out_PV_used_dpd_check.rdata")


## 1.5 get disbursal date for deal numbers
load_rdata_intermediate("ADS_data//base_ads.rdata")
base_ads <- distinct(base_ads %>% dplyr::select(deal_no, disbursal_date))


## 1.6 get primary applicants customer code
load_rdata_intermediate("cleaned_data//primary_app_customer_code.rdata")


## 1.6 get customer code for primary applicant
# load_rdata_intermediate("cleaned_data//sanction_data.rdata")
# sanction_data <- unique(sanction_data %>% dplyr::select(Deal_No, Customer_Code))
# colnames(sanction_data) <- c('deal_no', 'customer_code')
# sanction_data$primary_applicant <- 1
# 
# primary_applicants <- unique(sanction_data$Customer_Code)


## 2. Combine data & create modeling data across loan types --------------------------------

model_data_2W_New <- left_join(all_X_var_bureau %>% filter(loan_type == '2W-New'), all_X_var_application, by = 'deal_no')
model_data_2W_New <- model_data_2W_New %>% filter(deal_no %notin% unique(gated_out_2W_new$deal_no))
model_data_2W_New <- left_join(model_data_2W_New,base_ads, by = 'deal_no')
model_data_2W_New <- left_join(model_data_2W_New,bad_loans,by='deal_no')
model_data_2W_New <- model_data_2W_New %>% filter(customer_code %in% unique(primary_app_customer_code$customer_code))

model_data_PV_New <- left_join(all_X_var_bureau %>% filter(loan_type == 'PV-New'), all_X_var_application, by = 'deal_no')
model_data_PV_New <- model_data_PV_New %>% filter(deal_no %notin% unique(gated_out_PV_new$deal_no))
model_data_PV_New <- left_join(model_data_PV_New,base_ads, by = 'deal_no')
model_data_PV_New <- left_join(model_data_PV_New,bad_loans,by='deal_no')
model_data_PV_New <- model_data_PV_New %>% filter(customer_code %in% unique(primary_app_customer_code$customer_code))

model_data_PV_Used <- left_join(all_X_var_bureau %>% filter(loan_type == 'PV-Used'), all_X_var_application, by = 'deal_no')
model_data_PV_Used <- model_data_PV_Used %>% filter(deal_no %notin% unique(gated_out_PV_used$deal_no))
model_data_PV_Used <- left_join(model_data_PV_Used,base_ads, by = 'deal_no')
model_data_PV_Used <- left_join(model_data_PV_Used,bad_loans,by='deal_no')
model_data_PV_Used <- model_data_PV_Used %>% filter(customer_code %in% unique(primary_app_customer_code$customer_code))

rm(all_X_var_application,all_X_var_bureau,bad_loans,gated_out_2W_new,gated_out_PV_new,base_ads,primary_app_customer_code)



## 3. calculate IV ------------------------------------------------------------------------------

## 3.1 Define variable list for which IV has to be calculated
var_list <- colnames(model_data_PV_Used)
var_list <- var_list[grep("Var_",var_list)]
var_list <- c(var_list,'agri_profile_flag')

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


## 3.3 Salary profile IV - 2W New

## 3.3.1 SENP & SEP
IV_2W_New_SENP_SEP <- calculate_IV(var_list = var_list,
                               dependent_variable = dependent_variable, 
                               model_data = model_data_2W_New %>% filter(Category %in% c('SENP','SEP')),
                               bins = 5)

## 3.3.2 SAL
IV_2W_New_SAL <- calculate_IV(var_list = var_list,
                              dependent_variable = dependent_variable, 
                              model_data = model_data_2W_New %>% filter(Category == 'SAL'),
                              bins = 5)

## 3.3.3 Combined
IV_2W_New_combined <- calculate_IV(var_list = var_list,
                                   dependent_variable = dependent_variable, 
                                   model_data = model_data_2W_New %>% filter(Category %in% c('SENP','SEP','SAL')),
                                   bins = 5)


## 3.4 Salary profile IV - PV New

## 3.4.1 SENP & SEP
IV_PV_New_SENP_SEP <- calculate_IV(var_list = var_list,
                               dependent_variable = dependent_variable, 
                               model_data = model_data_PV_New %>% filter(Category %in% c('SENP','SEP')),
                               bins = 5)

## 3.4.2 SAL
IV_PV_New_SAL <- calculate_IV(var_list = var_list,
                              dependent_variable = dependent_variable, 
                              model_data = model_data_PV_New %>% filter(Category == 'SAL'),
                              bins = 5)
## PV Used
IV_PV_Used_SENP_SEP <- calculate_IV(var_list = var_list,
                                   dependent_variable = dependent_variable, 
                                   model_data = model_data_PV_Used %>% filter(Category %in% c('SENP','SEP')),
                                   bins = 5)

## 3.4.2 SAL
IV_PV_Used_SAL <- calculate_IV(var_list = var_list,
                              dependent_variable = dependent_variable, 
                              model_data = model_data_PV_Used %>% filter(Category == 'SAL'),
                              bins = 5)








IV_PV_Used_combined <- calculate_IV(var_list = var_list,
                                   dependent_variable = dependent_variable, 
                                   model_data = model_data_PV_Used %>% filter(Category %in% c('SENP','SEP','SAL')),
                                   bins = 5)

## 4. Calculate Category Default Rate -------------------------------------------------------------

summary_2W_New <- model_data_2W_New %>% group_by(Category) %>% summarise(disbursal_count = length(unique(applicant_id)),
                                                                         total_defaults = sum(bad_loan),
                                                                         default_rate = mean(bad_loan))


summary_PV_New <- model_data_PV_New %>% group_by(Category) %>% summarise(disbursal_count = length(unique(applicant_id)),
                                                                         total_defaults = sum(bad_loan),
                                                                         default_rate = mean(bad_loan))

summary_PV_Used <- model_data_PV_Used %>% group_by(Category) %>% summarise(disbursal_count = length(unique(applicant_id)),
                                                                         total_defaults = sum(bad_loan),
                                                                         default_rate = mean(bad_loan))



## 5. Save output ---------------------------------------------------------------------------------

## 5.1 Save 2W New outputs
save_csv_output(IV_2W_New_combined,"IV//IV_2W_New_Combined.csv")
save_csv_output(IV_2W_New_SENP_SEP,"IV//IV_2W_New_SENP_SEP.csv")
save_csv_output(IV_2W_New_SAL,"IV//IV_2W_New_SAL.csv")
# save_csv_output(IV_2W_New_SEP,"IV//IV_2W_New_SEP.csv")
# save_csv_output(IV_2W_New_NE,"IV//IV_2W_New_NE.csv")
save_csv_output(summary_2W_New,"IV//Summary_2W_New.csv")


## 5.2 Save PV New outputs
fwrite(IV_PV_New,"data//output//IV//IV_PV_New_Overallv2.csv")
fwrite(IV_PV_New_SENP_SEP,"data//output//IV//IV_PV_New_SENP_SEPv2.csv")
fwrite(IV_PV_New_SAL,"data//output//IV//IV_PV_New_SALv2.csv")
# save_csv_output(IV_PV_New_SEP,"IV//IV_PV_New_SEP.csv")
# save_csv_output(IV_PV_New_NE,"IV//IV_PV_New_NE.csv")
fwrite(summary_PV_New,"data//output//IV//Summary_PV_Newv2.csv")

## 5.3 Save PV Used outputs
fwrite(IV_PV_Used,"data//output//IV//IV_PV_Used_Overall.csv")
fwrite(IV_PV_Used_SENP_SEP,"data//output//IV//IV_PV_Used_SENP_SEP.csv")
fwrite(IV_PV_Used_SAL,"data//output//IV//IV_PV_Used_SAL.csv")
fwrite(summary_PV_Used,"data//output//IV//Summary_PV_Used.csv")



fwrite(IV_PV_Used_combined,"data//output//IV//Summary_PV_Used_vcheck.csv")


# assert_data_non_empty(model_data_2W_New)
# save(model_data_2W_New,
#      file = file.path(
#        get_data_path()$data$intermediate,
#        "model_data",
#        "model_data_2W_New.rdata"
#      )
# )
# 
# 
# assert_data_non_empty(model_data_PV_New)
# save(model_data_PV_New,
#      file = file.path(
#        get_data_path()$data$intermediate,
#        "model_data",
#        "model_data_PV_New.rdata"
#      )
# )
# 
# assert_data_non_empty(model_data_PV_Used)
# save(model_data_PV_Used,
#      file = file.path(
#        get_data_path()$data$intermediate,
#        "model_data",
#        "model_data_PV_Used.rdata"
#      )
# )

save(model_data_PV_Used, file = "data/intermediate/model_data/model_data_PV_Used.rdata")

rm(list=ls())




assert_data_non_empty(model_data_2W_New)
save(model_data_2W_New,
     file = file.path(
       get_data_path()$data$intermediate,
       "model_data",
       "model_data_2W_New_vcheck.rdata"
     )
)


assert_data_non_empty(model_data_PV_New)
save(model_data_PV_New,
     file = file.path(
       get_data_path()$data$intermediate,
       "model_data",
       "model_data_PV_New_vcheck.rdata"
     )
)

# assert_data_non_empty(model_data_PV_Used)
# save(model_data_PV_Used,
#      file = file.path(
#        get_data_path()$data$intermediate,
#        "model_data",
#        "model_data_PV_Used_vcheck.rdata"
#      )
# )


assert_data_non_empty(model_data_PV_Used)
save(model_data_PV_Used,
     file = file.path(
       get_data_path()$data$intermediate,
       "model_data",
       "model_data_PV_Used_dpd_check.rdata"
     )
)
