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
load_rdata_intermediate("ADS_data//X_var_bureau_2W_RF.rdata")

## 1.2 load application X var
load_rdata_intermediate("ADS_data//X_var_application_2W_RF.rdata")
all_X_var_application <- all_X_var_application %>% dplyr::select(deal_no,
                                                                 CUSTOMER_PROFILE,
                                                                 disbursal_state,
                                                                 sub_profile,
                                                                 Category,
                                                                 agri_profile_flag)

## 1.3 load default flag
load_rdata_intermediate("ADS_data//bad_loans_2W_RF.rdata")
bad_loans$loan_type <- NULL

## 1.4 load gated out population
load_rdata_output("gating_rules//gated_out_2W_RF.rdata")


## 1.5 get disbursal date for deal numbers
load_rdata_intermediate("ADS_data//base_ads.rdata")
base_ads <- distinct(base_ads %>% dplyr::select(deal_no, disbursal_date))


## 1.6 get primary applicants customer code
# load_rdata_intermediate("cleaned_data//primary_app_customer_code.rdata")


## 1.6 get customer code for primary applicant
# load_rdata_intermediate("cleaned_data//sanction_data.rdata")
# sanction_data <- unique(sanction_data %>% dplyr::select(Deal_No, Customer_Code))
# colnames(sanction_data) <- c('deal_no', 'customer_code')
# sanction_data$primary_applicant <- 1
# 
# primary_applicants <- unique(sanction_data$Customer_Code)


## 2. Combine data & create modeling data across loan types --------------------------------

model_data_2W_RF <- left_join(all_X_var_bureau %>% filter(loan_type == 'Refinance-2W'), all_X_var_application, by = 'deal_no')
model_data_2W_RF <- model_data_2W_RF %>% filter(deal_no %notin% unique(gated_out_2W_RF$deal_no))
model_data_2W_RF <- left_join(model_data_2W_RF,base_ads, by = 'deal_no')
model_data_2W_RF <- left_join(model_data_2W_RF,bad_loans,by='deal_no')
# model_data_2W_RF <- model_data_2W_RF %>% filter(customer_code %in% unique(primary_app_customer_code$customer_code))

rm(all_X_var_application,all_X_var_bureau,bad_loans,gated_out_2W_RF,base_ads)



## 3. calculate IV ------------------------------------------------------------------------------

## 3.1 Define variable list for which IV has to be calculated
var_list <- colnames(model_data_2W_RF)
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


## 3.3 Salary profile IV - 2W RF

## 3.3.1 SENP & SEP
IV_2W_RF_SENP_SEP <- calculate_IV(var_list = var_list,
                               dependent_variable = dependent_variable, 
                               model_data = model_data_2W_RF %>% filter(Category %in% c('SENP','SEP')),
                               bins = 5)

## 3.3.2 SAL
IV_2W_RF_SAL <- calculate_IV(var_list = var_list,
                              dependent_variable = dependent_variable, 
                              model_data = model_data_2W_RF %>% filter(Category == 'SAL'),
                              bins = 5)

## 3.3.3 Combined
IV_2W_RF_combined <- calculate_IV(var_list = var_list,
                                   dependent_variable = dependent_variable, 
                                   model_data = model_data_2W_RF %>% filter(Category %in% c('SENP','SEP','SAL')),
                                   bins = 5)



## 4. Calculate Category Default Rate -------------------------------------------------------------

summary_2W_RF <- model_data_2W_RF %>% group_by(Category) %>% summarise(disbursal_count = length(unique(applicant_id)),
                                                                         total_defaults = sum(bad_loan),
                                                                         default_rate = mean(bad_loan))




## 5. Save output ---------------------------------------------------------------------------------

## 5.1 Save 2W New outputs
Output_list <- list(
  "IV_2W_RF_combined" = IV_2W_RF_combined,
  "IV_2W_RF_SENP_SEP" = IV_2W_RF_SENP_SEP,
  "IV_2W_RF_SAL" = IV_2W_RF_SAL,
  "summary_2W_RF" = summary_2W_RF
)

write_xlsx(Output_list,"data//output//IV//IV_2W_RF.xlsx" )

save_csv_output(IV_2W_RF_combined,"IV//IV_2W_RF_Combined.csv")
save_csv_output(IV_2W_RF_SENP_SEP,"IV//IV_2W_RF_SENP_SEP.csv")
save_csv_output(IV_2W_RF_SAL,"IV//IV_2W_RF_SAL.csv")
save_csv_output(summary_2W_RF,"IV//Summary_2W_RF.csv")


assert_data_non_empty(model_data_2W_RF)
save(model_data_2W_RF,
     file = file.path(
       get_data_path()$data$intermediate,
       "model_data",
       "model_data_2W_RF.rdata"
     )
)



# save(model_data_2W_RF, file = "data/intermediate/model_data/model_data_2W_RF.rdata")

rm(list=ls())







# calculate_pentile_IV <- function(var_list,train_data,test_data){
#   
#   for(each_col in var_list){
#     train_data[[each_col]] <- as.numeric(train_data[[each_col]])
#     test_data[[each_col]] <- as.numeric(test_data[[each_col]])
#     
#     
#     temp_data <- data.frame(Var = each_col,
#                             NA_count = sum(is.na(train_data[[each_col]])),
#                             Zero_count = nrow(train_data[train_data[[each_col]] == 0,]),
#                             Infinite_count = nrow(train_data[is.infinite(train_data[[each_col]]),])
#     )
#   
#     temp_data$negative_count = nrow(train_data[train_data[[each_col]] < 0,])
#     temp_data$positive_count = nrow(train_data[train_data[[each_col]] > 0,])
#     
#     train_data[[each_col]][is.infinite(train_data[[each_col]])] <- NA
#     
#     quantile_vector <- as.vector(quantile(train_data[[each_col]], 
#                                           c(0.2, 0.4, 0.6, 0.8, 1),
#                                           na.rm = TRUE))
#     
#     temp_data$min_value <- min(train_data[[each_col]], na.rm = T)
#     temp_data$perc_20 <- quantile_vector[1]
#     temp_data$perc_40 <- quantile_vector[2]
#     temp_data$perc_60 <- quantile_vector[3]
#     temp_data$perc_80 <- quantile_vector[4]
#     temp_data$perc_100 <- quantile_vector[5]
#     temp_data$max_value <- max(train_data[[each_col]], na.rm = T)
#     temp_data$mean_value <- mean(train_data[[each_col]], na.rm = T)
#     temp_data$median_value <- median(train_data[[each_col]], na.rm = T)
#     
#     binned_info <- woe.binning(train_data,
#                                "bad_loan",
#                                each_col,
#                                min.perc.total = 0.15,
#                                min.perc.class = 0.1)
#     
#     
#     
#     train_data <- woe.binning.deploy(train_data,
#                                      binned_info,
#                                      add.woe.or.dum.var = "woe")
#     train_data[[paste0("woe.",each_col,".binned")]] <- NULL
#     
#     
#     test_data <- woe.binning.deploy(test_data,
#                                      binned_info,
#                                      add.woe.or.dum.var = "woe")
#     test_data[[paste0("woe.",each_col,".binned")]] <- NULL
#     
#     
#     names(test_data)[names(test_data) == paste0(each_col,".binned")] <- paste0(each_col,".binned_WOE")
#     
#     validation <- train_data[, .(length(unique(deal_no))), .(bad_loan,get(paste0(each_col,".binned")))]
#     validation$var <- each_col
#     validation
#     
#     
#     
#   }
#   
#   
# }








