#######################################################################################################
#################### Generate data for testing ########################################################
#######################################################################################################


## 0. Load helper functions & libraries ---------------------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

model_functions <- file.path("src", "utils", "model_functions.R")
source(model_functions)

options(scipen = 999)
voptions(raise = "all")


`%notin%` <- Negate(`%in%`)




## 1 - 2W New NTC -------------------------------------------------------------------------------------

## 1.1 load train + test data
load_rdata_output("model/2W_New/NTC/vcheck/train_data.rdata")
load_rdata_output("model/2W_New/NTC/vcheck/test_data.rdata")


## 1.2 load NTC cibil score data
load("data/intermediate/cleaned_data/ntc_cibil_new.rdata")


## 1.3 Append train + test data
dev_data <- rbind(train_data_final,test_data_final)
rm(train_data_final,test_data_final)


## 1.4 get NTC score
dev_data <- left_join(dev_data,cibil_ntc,by='deal_no')


## 1.5 Define columns to be extracted from data
base_cols <- c('deal_no',
               'cibil_ntc_score',
               'predictions')

uncleaned_cols <- c('Region',
                    'Category',
                    'CUSTOMER_PROFILE',
                    'phones_reported_12m',
                    'addresses_reported_12m',
                    'Marital_Status',
                    'DOB_cleaned')

model_cols <- c('region_woe',
                'category_flag_SENP_SEP',
                'agri_profile_flag',
                'phones_reported_12m_C1',
                'addresses_reported_12m_C1',
                'Marital_Status_woe2',
                'Age_woe2')


final_cols <- c(base_cols,uncleaned_cols,model_cols)


## 1.6 extract & rename data
uat_testing_data <- dev_data %>% dplyr::select(final_cols)
colnames(uat_testing_data) <- c('deal_no',
                                'cibil_ntc_score',
                                'model_predition',
                                'raw_region',
                                'raw_categroy',
                                'raw_customer_profile',
                                'raw_phones_reported_12m',
                                'raw_addresses_reported_12m',
                                'raw_marital_status',
                                'raw_DOB',
                                'model_region',
                                'model_category_flag_SENP_SEP',
                                'model_agri_profile_flag',
                                'model_phones_reported_12m',
                                'model_addresses_reported_12m',
                                'model_marital_status',
                                'model_age'
                                )



## 1.7 save data
output_data <- list("2W_New_NTC" = uat_testing_data)

write_xlsx(output_data, path = file.path("data//output//uat_testing//NTC_2W_New.xlsx"))


rm(dev_data,output_data,uat_testing_data)












## 2 - PV New NTC -----------------------------------------------------------------------------------------------

## 2.2 load train + test data
load_rdata_output("model/PV_New/NTC/vcheck/train_data.rdata")
load_rdata_output("model/PV_New/NTC/vcheck/test_data.rdata")


## 2.2 load NTC cibil score data
load("data/intermediate/cleaned_data/ntc_cibil_new.rdata")


## 2.3 load application data
load_rdata_intermediate("ADS_data//X_var_application.rdata")


## 2.4 Append train + test data
dev_data <- rbind(train_data_final,test_data_final)
rm(train_data_final,test_data_final)


## 2.5 get NTC score
dev_data <- left_join(dev_data,cibil_ntc,by='deal_no')


## 2.6 get application data variable - customer profile
dev_data <- left_join(dev_data,all_X_var_application %>% dplyr::select(deal_no,CUSTOMER_PROFILE),by='deal_no')


## 2.7 Define columns to be extracted from data
base_cols <- c('deal_no',
               'cibil_ntc_score',
               'predictions')

uncleaned_cols <- c('Region',
                    'CUSTOMER_PROFILE',
                    'phones_reported_12m',
                    'addresses_reported_12m')

model_cols <- c( 'region_woe',
                 'agri_profile_flag',
                 'phones_reported_12m_C1',
                 'addresses_reported_12m_C1')


final_cols <- c(base_cols,uncleaned_cols,model_cols)


## 2.8 extract & rename data
uat_testing_data <- dev_data %>% dplyr::select(final_cols)
colnames(uat_testing_data) <- c('deal_no',
                                'cibil_ntc_score',
                                'model_predition',
                                'raw_region',
                                'raw_customer_profile',
                                'raw_phones_reported_12m',
                                'raw_addresses_reported_12m',
                                'model_region',
                                'model_agri_profile_flag',
                                'model_phones_reported_12m',
                                'model_addresses_reported_12m'
)



## 2.9 save data
output_data <- list("PV_New_NTC" = uat_testing_data)

write_xlsx(output_data, path = file.path("data//output//uat_testing//NTC_PV_New.xlsx"))


rm(dev_data,output_data,uat_testing_data,all_X_var_application,cibil_ntc)





## 3 - PV Used NTC -----------------------------------------------------------------------------------------------


## 3.1 Load NTC data
load_rdata_intermediate("model_data//ntc_data.rdata")

## 3.2 Load state-region mapping
state_region_mapping <- fread("data/mapping/state_region_mapping.csv")


## 3.3 Get region & filter for required PV used data
ntc_model_data$Region <- NULL

dev_data <- ntc_model_data %>% filter((loan_type == 'PV-Used') &
                                        (Category %in% c('SENP','SEP','SAL')))
dev_data <- left_join(dev_data, state_region_mapping, by=c('disbursal_state'))


## 3.4 load NTC cibil score data
load("data/intermediate/cleaned_data/ntc_cibil_new.rdata")


## 3.5 load application data
load_rdata_intermediate("ADS_data//X_var_application.rdata")


## 3.6 get NTC score
dev_data <- left_join(dev_data,cibil_ntc,by='deal_no')


## 3.7 get application data variable - customer profile
dev_data <- left_join(dev_data,all_X_var_application %>% dplyr::select(deal_no,CUSTOMER_PROFILE),by='deal_no')



## 3.8 Create model variables

## 3.8.1 agri profile flag
dev_data$agri_profile_flag <- ifelse(is.na(dev_data$agri_profile_flag), 0 , dev_data$agri_profile_flag)


## 3.8.2 No. of phones reported in 12m

# fill missing values
dev_data$phones_reported_12m_C <- ifelse(is.na(dev_data$phones_reported_12m), 1 , dev_data$phones_reported_12m)

# transformation
dev_data$phones_reported_12m_C1 <- ifelse(dev_data$phones_reported_12m_C >= 4, 4, 
                                          ifelse(dev_data$phones_reported_12m_C <= 0, 0, dev_data$phones_reported_12m_C))


## 3.8.3 Region
woe1 <- -0.235776
woe2 <- -0.06171671
woe3 <- 0.2854523
woe4 <- 0.4097233
woe5 <- 0.5042479

dev_data$region_woe <- ifelse(dev_data$Region == 'South', woe1, 
                              ifelse(dev_data$Region == 'Central', woe2,
                                     ifelse(dev_data$Region == 'West', woe3,
                                            ifelse(dev_data$Region == 'North', woe4, woe5))))

## 3.8.4 No. of addresses reported in 12m

## fill missing values
dev_data$addresses_reported_12m_C <- ifelse(is.na(dev_data$addresses_reported_12m), 0 , dev_data$addresses_reported_12m)

## transformation
dev_data$addresses_reported_12m_C1 <- ifelse(dev_data$addresses_reported_12m_C >= 4, 4, 
                                             ifelse(dev_data$addresses_reported_12m_C <= 0, 0, dev_data$addresses_reported_12m_C))



## 3.9 Score PV used data using PV New model
final_model <- readRDS("output/vcheck/model_PV_New_NTC.rds")
dev_data <- get_predictions(final_model, dev_data)



## 3.10 Define columns to be extracted from data
base_cols <- c('deal_no',
               'cibil_ntc_score',
               'predictions')

uncleaned_cols <- c('Region',
                    'CUSTOMER_PROFILE',
                    'phones_reported_12m',
                    'addresses_reported_12m')

model_cols <- c( 'region_woe',
                 'agri_profile_flag',
                 'phones_reported_12m_C1',
                 'addresses_reported_12m_C1')


final_cols <- c(base_cols,uncleaned_cols,model_cols)


## 3.11 extract & rename data
uat_testing_data <- dev_data %>% dplyr::select(final_cols)
colnames(uat_testing_data) <- c('deal_no',
                                'cibil_ntc_score',
                                'model_predition',
                                'raw_region',
                                'raw_customer_profile',
                                'raw_phones_reported_12m',
                                'raw_addresses_reported_12m',
                                'model_region',
                                'model_agri_profile_flag',
                                'model_phones_reported_12m',
                                'model_addresses_reported_12m'
)



## 3.12 save data
output_data <- list("PV_Used_NTC" = uat_testing_data)

write_xlsx(output_data, path = file.path("data//output//uat_testing//NTC_PV_Used.xlsx"))


rm(all_X_var_application,cibil_ntc,dev_data,final_model,ntc_model_data,output_data,state_region_mapping,uat_testing_data)
gc()






## 4 - 2W New ETC -----------------------------------------------------------------------------------------------


## 4.1 load train + test data
load_rdata_output("model/2W_New/Combined/vcheck/train_data.rdata")
load_rdata_output("model/2W_New/Combined/vcheck/test_data.rdata")

# load_rdata_output("model/2W_New/Combined/vcheck/dpd_check/train_data.rdata")
# load_rdata_output("model/2W_New/Combined/vcheck/dpd_check/test_data.rdata")

## 4.2 load belief testing data
load_rdata_output("reason_code//2W_New//belief_testing_2W.rdata")
# load_rdata_output("reason_code//2W_New//belief_testing_2W_dpd_check.rdata")


## 4.3 Append train + test data
dev_data <- rbind(train_data_final,test_data_final)
rm(train_data_final,test_data_final)


## 4.4 join modifier rules with data
dev_data <- left_join(dev_data,belief_testing_2W %>% dplyr::select(deal_no,customer_code,applicant_id,risk_band,risk_band_new,
                                                                   Rule_CV_Demog_addresses_reported_6m_GE_2,
                                                                   Rule_CV_Demog_phones_reported_3m_GE_2,
                                                                   Rule_EN_ratio_enquiry_to_loans_12mon_GE_3,
                                                                   Rule_PO_months_6_TW_GE_1
                                                                   ),
                      by = c('deal_no','customer_code','applicant_id'))



## 4.5 Define columns to be extracted from data
base_cols <- c('deal_no',
               'customer_code',
               'applicant_id',
               'cibil_score_v3',
               'predictions',
               'risk_band',
               'risk_band_new')

uncleaned_cols <- c('Var_credit_vintage',
                    'Var_PO_closed_excl_CC',
                    'Var_sanctioned_amount_live_loans',
                    'Var_outstanding_by_sanctioned_amount_live_loans',
                    'Var_DL_HL_paid_GE_12mon',
                    'Var_DL_Gold_paid_GE_12mon',
                    'Var_EN_enquiry_count_6m_non_CC',
                    'Var_DL_all_30dpd_6mon',
                    'Category',
                    'CUSTOMER_PROFILE')

model_cols <- c('Var_credit_vintage_C',
                'Var_PO_closed_excl_CC_woe',
                'cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live',
                'Var_DL_HL_paid_GE_12mon_C',
                'Var_DL_Gold_paid_GE_12mon_woe',
                'Var_EN_enquiry_count_6m_non_CC_C',
                'Var_DL_all_30dpd_6mon_flag',
                'category_flag_SENP_SEP',
                'agri_profile_flag')


modifier_rules <- c('Rule_CV_Demog_addresses_reported_6m_GE_2',
                    'Rule_CV_Demog_phones_reported_3m_GE_2',
                    'Rule_EN_ratio_enquiry_to_loans_12mon_GE_3',
                    'Rule_PO_months_6_TW_GE_1')

final_cols <- c(base_cols,uncleaned_cols,model_cols,modifier_rules)


## 4.6 extract & rename data
uat_testing_data <- dev_data %>% dplyr::select(final_cols)
colnames(uat_testing_data) <- c('deal_no',
                                'customer_code',
                                'applicant_id',
                                'cibil_score_v3',
                                'model_predition',
                                'risk_band_pre_modifier',
                                'risk_band_post_modifier',
                                
                                'raw_credit_vintage',
                                'raw_closed_loans_excl_CC',
                                'raw_sanctioned_amount_live_loans',
                                'raw_outstanding_amount_by_sanctioned_amount_live_loans',
                                'raw_house_loan_paid_GE_12m',
                                'raw_gold_loan_paid_GE_12m',
                                'raw_enquiry_count_non_CC_6m',
                                'raw_30dpd_in_6m',
                                'raw_category',
                                'raw_customer_profile',
                                
                                'model_credit_vintage',
                                'model_closed_loans_excl_CC',
                                'model_exposure_adjusted_ammortization',
                                'model_house_loan_paid_GE_12m',
                                'model_gold_loan_paid_GE_12m',
                                'model_enquiry_count_non_CC_6m',
                                'model_30dpd_in_6m',
                                'model_category_flag_SENP_SEP',
                                'model_agri_profile_flag',
                                
                                'Rule_addresses_reported_6m_GE_2',
                                'Rule_phones_reported_3m_GE_2',
                                'Rule_ratio_enquiry_to_loans_12mon_GE_3',
                                'Rule_TW_loan_taken_6m_GE_1'
                                
                                
)


## 4.7 save data
output_data <- list("2W_New_ETC" = uat_testing_data)

# write_xlsx(output_data, path = file.path("data//output//uat_testing//ETC_2W_New.xlsx"))
write_xlsx(output_data, path = file.path("data//output//uat_testing//ETC_2W_New_dpd_check.xlsx"))


rm(dev_data,output_data,belief_testing_2W,uat_testing_data)
gc()












## 5 - PV New ETC -----------------------------------------------------------------------------------------------


## 5.1 load train + test data
load_rdata_output("model//PV_New//Combined//vcheck//Post_RI//train_data.rdata")
load_rdata_output("model//PV_New//Combined//vcheck//Post_RI//test_data.rdata")

# load_rdata_output("model//PV_New//Combined//vcheck//Post_RI//dpd_check//train_data.rdata")
# load_rdata_output("model//PV_New//Combined//vcheck//Post_RI//dpd_check//test_data.rdata")

## 5.2 load belief testing data
load_rdata_output("reason_code//PV_New//belief_testing_PV_New.rdata")
# load_rdata_output("reason_code//PV_New//belief_testing_PV_New_dpd_check.rdata")


## 5.3 Append train + test data
dev_data <- rbind(train_data_final,test_data_final)
rm(train_data_final,test_data_final)


## 5.4 join modifier rules with data
dev_data <- left_join(dev_data,belief_testing_PV_New %>% dplyr::select(UID,
                                                                   risk_band,risk_band_new,
                                                                   Rule_CV_Demog_addresses_reported_6m_GE_2,
                                                                   Rule_CV_Demog_phones_reported_3m_GE_2,
                                                                   Rule_EN_ratio_enquiry_to_loans_12mon_GE_3),
by = c('UID'))



## 5.5 Define columns to be extracted from data
base_cols <- c('deal_no',
               'application_no',
               'customer_code',
               'applicant_id',
               'UID',
               'cibil_score_v3',
               'predictions',
               'risk_band',
               'risk_band_new')

uncleaned_cols <- c('Var_credit_vintage',
                    'Var_DL_HL_paid_GE_12mon',
                    'Var_DL_Gold_paid_GE_12mon',
                    'Var_EN_enquiry_count_6m_non_CC',
                    'Var_DL_all_30dpd_6mon',
                    'Category',
                    'Var_PO_closed_sanction_amount_excl_CC',
                    'Var_outstanding_by_sanctioned_amount_live_loans')

model_cols <- c("Var_credit_vintage_C",
                "Var_DL_HL_paid_GE_12mon_flag",
                "Var_DL_Gold_paid_GE_12mon_flag_var",
                "Var_EN_enquiry_count_6m_non_CC_C",
                "Var_DL_all_30dpd_6mon_flag",
                "category_flag_SENP_SEP",
                "cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed")


modifier_rules <- c('Rule_CV_Demog_addresses_reported_6m_GE_2',
                    'Rule_CV_Demog_phones_reported_3m_GE_2',
                    'Rule_EN_ratio_enquiry_to_loans_12mon_GE_3')

final_cols <- c(base_cols,uncleaned_cols,model_cols,modifier_rules)


## 5.6 extract & rename data
uat_testing_data <- dev_data %>% dplyr::select(final_cols)
colnames(uat_testing_data) <- c('deal_no',
                                'application_no',
                                'customer_code',
                                'applicant_id',
                                'UID',
                                'cibil_score_v3',
                                'model_predition',
                                'risk_band_pre_modifier',
                                'risk_band_post_modifier',
                                
                                'raw_credit_vintage',
                                'raw_house_loan_paid_GE_12m',
                                'raw_gold_loan_paid_GE_12m',
                                'raw_enquiry_count_non_CC_6m',
                                'raw_30dpd_in_6m',
                                'raw_category',
                                'raw_sanctioned_amount_closed_loans_excl_CC',
                                'raw_outstanding_amount_by_sanctioned_amount_live_loans',
                                
                                'model_credit_vintage',
                                'model_house_loan_paid_GE_12m',
                                'model_gold_loan_paid_GE_12m',
                                'model_enquiry_count_non_CC_6m',
                                'model_30dpd_in_6m',
                                'model_category_flag_SENP_SEP',
                                'model_payment_capability_adjusted_ammortization',
                                
                                'Rule_addresses_reported_6m_GE_2',
                                'Rule_phones_reported_3m_GE_2',
                                'Rule_ratio_enquiry_to_loans_12mon_GE_3'
                                
                                
)


## 5.7 save data
output_data <- list("PV_New_ETC" = uat_testing_data)

# write_xlsx(output_data, path = file.path("data//output//uat_testing//ETC_PV_New.xlsx"))
write_xlsx(output_data, path = file.path("data//output//uat_testing//ETC_PV_New_dpd_check.xlsx"))


rm(dev_data,output_data,belief_testing_PV_New,uat_testing_data)
gc()









## 6 - PV Used ETC -----------------------------------------------------------------------------------------------


## 6.1 load train + test data
load_rdata_output("model//PV_Used//Combined//vcheck//train_data.rdata")
load_rdata_output("model//PV_Used//Combined//vcheck//test_data.rdata")


## 6.2 load belief testing data
load_rdata_output("reason_code//PV_Used//belief_testing_PV_Used.rdata")


## 6.3 Append train + test data
dev_data <- rbind(train_data_final,test_data_final)
rm(train_data_final,test_data_final)


## 6.4 join modifier rules with data
dev_data <- left_join(dev_data,belief_testing_PV_Used %>% dplyr::select(deal_no,customer_code,applicant_id,risk_band,risk_band_new,
                                                                   Rule_CV_Demog_addresses_reported_6m_GE_2,
                                                                   Rule_CV_Demog_phones_reported_3m_GE_2,
                                                                   Rule_EN_ratio_enquiry_to_loans_12mon_GE_3
),
by = c('deal_no','customer_code','applicant_id'))



## 6.5 Define columns to be extracted from data
base_cols <- c('deal_no',
               'customer_code',
               'applicant_id',
               'cibil_score_v3',
               'predictions',
               'risk_band',
               'risk_band_new')

uncleaned_cols <- c('Var_credit_vintage',
                    'Var_PO_closed_sanction_amount_excl_CC',
                    'Var_outstanding_by_sanctioned_amount_live_loans',
                    'Category',
                    'Var_EN_unsec_enquiry_count_12m_non_CC',
                    'Var_DL_all_60dpd_3mon')

model_cols <- c('Var_credit_vintage_C',
                'cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed_woe',
                'category_flag_SENP_SEP',
                'Var_EN_unsec_enquiry_count_12m_non_CC_woe',
                'Var_DL_all_60dpd_3mon_C')


modifier_rules <- c('Rule_CV_Demog_addresses_reported_6m_GE_2',
                    'Rule_CV_Demog_phones_reported_3m_GE_2',
                    'Rule_EN_ratio_enquiry_to_loans_12mon_GE_3')

final_cols <- c(base_cols,uncleaned_cols,model_cols,modifier_rules)


## 6.6 extract & rename data
uat_testing_data <- dev_data %>% dplyr::select(final_cols)
colnames(uat_testing_data) <- c('deal_no',
                                'customer_code',
                                'applicant_id',
                                'cibil_score_v3',
                                'model_predition',
                                'risk_band_pre_modifier',
                                'risk_band_post_modifier',
                                
                                'raw_credit_vintage',
                                'raw_sanctioned_amount_closed_loans_excl_CC',
                                'raw_outstanding_amount_by_sanctioned_amount_live_loans',
                                'raw_category',
                                'raw_enquiry_count_unsecured_non_CC_12m',
                                'raw_60dpd_in_3m',
                                
                                'model_credit_vintage',
                                'model_payment_capability_adjusted_ammortization',
                                'model_category_flag_SENP_SEP',
                                'model_enquiry_count_unsecured_non_CC_12m',
                                'model_60dpd_in_3m',
                                
                                'Rule_addresses_reported_6m_GE_2',
                                'Rule_phones_reported_3m_GE_2',
                                'Rule_ratio_enquiry_to_loans_12mon_GE_3'
                                
                                
)


## 6.7 save data
output_data <- list("PV_Used_ETC" = uat_testing_data)

write_xlsx(output_data, path = file.path("data//output//uat_testing//ETC_PV_Used.xlsx"))


rm(dev_data,output_data,belief_testing_PV_Used,uat_testing_data)
gc()












## 7 - 2W Refinance (Used) ETC -------------------------------------------------------------------------------------------


## 7.1 Load 2W New data
load_rdata_intermediate("model_data//model_data_2W_RF.rdata")

## 7.2 set OOT window
oot_date_start <- '2018-04-01'
oot_date_end <- '2018-06-30'

## 7.3 define OOT & DEV data for Combined
oot_data <-
  model_data_2W_RF %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                (disbursal_date <= as.Date(oot_date_end)) &
                                (Category %in% c('SAL', 'SENP', 'SEP')))

dev_data <-
  model_data_2W_RF %>% filter(deal_no %notin% unique(oot_data$deal_no) &
                                (Category %in% c('SAL', 'SENP', 'SEP')))

rm(model_data_2W_RF)


## 7.4 append dev + oot data
dev_data <- rbind(dev_data,oot_data)
rm(oot_data)


## 7.5 get modifier rules
load_rdata_intermediate("belief_testing//all_rules//belief_testing_rules_disbursals_2W_RF.rdata")


## 7.6 join modifier rules with data
dev_data <- left_join(dev_data,belief_testing_rules_disbursals %>% dplyr::select(deal_no,applicant_id,
                                                                   Rule_CV_Demog_addresses_reported_6m_GE_2,
                                                                   Rule_CV_Demog_phones_reported_3m_GE_2,
                                                                   Rule_EN_ratio_enquiry_to_loans_12mon_GE_3,
                                                                   Rule_PO_months_6_TW_GE_1
),
by = c('deal_no','applicant_id'))



## 7.7 create variables

## 7.7.1 Credit vintage
dev_data$Var_credit_vintage_C <- ifelse(dev_data$Var_credit_vintage <= 0.01, 0.01, 
                                        ifelse(dev_data$Var_credit_vintage >= 12, 12, dev_data$Var_credit_vintage))


# 7.7.2 count of closed loans excluding CC
dev_data$Var_PO_closed_excl_CC_C <- dev_data$Var_PO_closed_excl_CC
dev_data$Var_PO_closed_excl_CC_C[is.na(dev_data$Var_PO_closed_excl_CC_C)] <- 0

woe1 <- -0.1083156
woe2 <- 0.05311667
woe3 <- 0.2382278


dev_data$Var_PO_closed_excl_CC_woe <- ifelse(dev_data$Var_PO_closed_excl_CC_C <= 1, woe1, 
                                             ifelse(dev_data$Var_PO_closed_excl_CC_C == 2, woe2,woe3))

rm(woe1, woe2, woe3)


# 7.7.3 HL paid 12 month Continuous
dev_data$Var_DL_HL_paid_GE_12mon_C <- dev_data$Var_DL_HL_paid_GE_12mon
dev_data$Var_DL_HL_paid_GE_12mon_C[is.na(dev_data$Var_DL_HL_paid_GE_12mon_C)] <- 0

dev_data$Var_DL_HL_paid_GE_12mon_C <- ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C >= 3, 3, 
                                             ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C <= 0, 0,dev_data$Var_DL_HL_paid_GE_12mon_C))



# 7.7.4 Gold paid 12 month continuous
dev_data$Var_DL_Gold_paid_GE_12mon_C <- dev_data$Var_DL_Gold_paid_GE_12mon
dev_data$Var_DL_Gold_paid_GE_12mon_C[is.na(dev_data$Var_DL_Gold_paid_GE_12mon_C)] <- 0

woe1 <- -0.02766028
woe2 <- 0.1895425
woe3 <- 0.3895373

dev_data$Var_DL_Gold_paid_GE_12mon_woe <- ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C == 0, woe1, 
                                                 ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C == 1, woe2,woe3))

rm(woe1, woe2, woe3)



# 7.7.5 Enquiry in 6m non cc 
dev_data$Var_EN_enquiry_count_6m_non_CC_C <- dev_data$Var_EN_enquiry_count_6m_non_CC
dev_data$Var_EN_enquiry_count_6m_non_CC_C[is.na(dev_data$Var_EN_enquiry_count_6m_non_CC_C)] <- 0

dev_data$Var_EN_enquiry_count_6m_non_CC_C <- ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C <= 0, 0, 
                                                    ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C >= 6, 6, dev_data$Var_EN_enquiry_count_6m_non_CC_C))



# 7.7.6 30 dpd in 6 mon
dev_data$Var_DL_all_30dpd_6mon_C <- dev_data$Var_DL_all_30dpd_6mon
dev_data$Var_DL_all_30dpd_6mon_C[is.na(dev_data$Var_DL_all_30dpd_6mon_C)] <- 0

dev_data$Var_DL_all_30dpd_6mon_flag <- ifelse(dev_data$Var_DL_all_30dpd_6mon_C == 0, 0, 1)


## 7.7.7 SENP SEP Flag
dev_data$category_flag_SENP_SEP <- ifelse(dev_data$Category %in% c('SENP', 'SEP'), 1, 0)


## 7.7.8 Agri profile flag
dev_data$agri_profile_flag <- ifelse(is.na(dev_data$agri_profile_flag), 0 , dev_data$agri_profile_flag)


## 7.7.9 cross tab variable
dev_data$Var_sanctioned_amount_live_loans_C <- dev_data$Var_sanctioned_amount_live_loans
dev_data$Var_sanctioned_amount_live_loans_C[is.na(dev_data$Var_sanctioned_amount_live_loans_C)] <- 0


dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- dev_data$Var_outstanding_by_sanctioned_amount_live_loans
dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 1




dev_data$Var_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_sanctioned_amount_live_loans_C <= 175000, 'bin_LE_175k', 
                                                        ifelse(dev_data$Var_sanctioned_amount_live_loans_C > 175000 & dev_data$Var_sanctioned_amount_live_loans_C <= 1000000, 'bin_175k_to_10L', 'bin_GE_10L'))


dev_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.25 & dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.5, 'val_25_to_50_pct','val_GE_50_pct'))


subset1 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct')))
subset2 <- dev_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct'))))
subset3 <- dev_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_50_pct','val_GE_50_pct'))))
subset4 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct')))

woe1 <- -0.1334553
woe2 <- 0.1301698
woe3 <- 0.5085008
woe4 <- 0.6125813




dev_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live <- ifelse(dev_data$deal_no %in% unique(subset1$deal_no), woe1,
                                                                        ifelse(dev_data$deal_no %in% unique(subset2$deal_no), woe2,
                                                                               ifelse(dev_data$deal_no %in% unique(subset3$deal_no), woe3,
                                                                                      woe4)))


rm(subset1,subset2,subset3,subset4,woe1, woe2, woe3, woe4)



## 7.8 Make model predictions
model_2W <- readRDS(file = file.path(get_data_path()$data$model,"vcheck//model_2W_New.rds"))
dev_data <- get_predictions(model_2W, dev_data)


## 7.9 get cibil v3 score
load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
cibil_v3 <- distinct(cibil_v3 %>% dplyr::select(deal_no,customer_code,cibil_score_v3))

dev_data <- left_join(dev_data,cibil_v3,by=c('deal_no'))

## 7.9 Define columns to be extracted from data
base_cols <- c('deal_no',
               'applicant_id',
               'cibil_score_v3',
               'predictions')

uncleaned_cols <- c('Var_credit_vintage',
                    'Var_PO_closed_excl_CC',
                    'Var_sanctioned_amount_live_loans',
                    'Var_outstanding_by_sanctioned_amount_live_loans',
                    'Var_DL_HL_paid_GE_12mon',
                    'Var_DL_Gold_paid_GE_12mon',
                    'Var_EN_enquiry_count_6m_non_CC',
                    'Var_DL_all_30dpd_6mon',
                    'Category',
                    'CUSTOMER_PROFILE')

model_cols <- c('Var_credit_vintage_C',
                'Var_PO_closed_excl_CC_woe',
                'cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live',
                'Var_DL_HL_paid_GE_12mon_C',
                'Var_DL_Gold_paid_GE_12mon_woe',
                'Var_EN_enquiry_count_6m_non_CC_C',
                'Var_DL_all_30dpd_6mon_flag',
                'category_flag_SENP_SEP',
                'agri_profile_flag')


modifier_rules <- c('Rule_CV_Demog_addresses_reported_6m_GE_2',
                    'Rule_CV_Demog_phones_reported_3m_GE_2',
                    'Rule_EN_ratio_enquiry_to_loans_12mon_GE_3',
                    'Rule_PO_months_6_TW_GE_1')

final_cols <- c(base_cols,uncleaned_cols,model_cols,modifier_rules)


## 4.6 extract & rename data
uat_testing_data <- dev_data %>% dplyr::select(final_cols)
colnames(uat_testing_data) <- c('deal_no',
                                'applicant_id',
                                'cibil_score_v3',
                                'model_predition',

                                'raw_credit_vintage',
                                'raw_closed_loans_excl_CC',
                                'raw_sanctioned_amount_live_loans',
                                'raw_outstanding_amount_by_sanctioned_amount_live_loans',
                                'raw_house_loan_paid_GE_12m',
                                'raw_gold_loan_paid_GE_12m',
                                'raw_enquiry_count_non_CC_6m',
                                'raw_30dpd_in_6m',
                                'raw_category',
                                'raw_customer_profile',

                                'model_credit_vintage',
                                'model_closed_loans_excl_CC',
                                'model_exposure_adjusted_ammortization',
                                'model_house_loan_paid_GE_12m',
                                'model_gold_loan_paid_GE_12m',
                                'model_enquiry_count_non_CC_6m',
                                'model_30dpd_in_6m',
                                'model_category_flag_SENP_SEP',
                                'model_agri_profile_flag',

                                'Rule_addresses_reported_6m_GE_2',
                                'Rule_phones_reported_3m_GE_2',
                                'Rule_ratio_enquiry_to_loans_12mon_GE_3',
                                'Rule_TW_loan_taken_6m_GE_1'


)


## 4.7 save data
output_data <- list("2W_Used_ETC" = uat_testing_data)

write_xlsx(output_data, path = file.path("data//output//uat_testing//ETC_2W_Used.xlsx"))


rm(list = ls())
gc()

