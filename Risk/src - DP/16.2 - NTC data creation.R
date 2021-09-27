## 0. Load helper functions & libraries -----------------------------------------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)

## 1. Load data -----------------------------------------------------------------------------------------------------------

## 1.1 Load ADS data
load_rdata_intermediate("ADS_data//base_ads.rdata")

model_data <- distinct(base_ads %>% filter(disbursal_date >= as.Date('2018-04-01')) %>% 
                         filter(disbursal_date <= as.Date('2020-12-31')) %>% dplyr::select(deal_no,
                                                                                           loan_type,
                                                                                           disbursal_date,
                                                                                           disbursal_state))
## 1.2 Load primary customer code
load_rdata_intermediate("cleaned_data//primary_app_customer_code.rdata")
model_data <- left_join(model_data,primary_app_customer_code,by='deal_no')

rm(primary_app_customer_code)


## 1.3 load cibil v3 scores
load_rdata_intermediate( "cleaned_data//cibil_v3_disbursals.rdata")

model_data_p1 <- left_join(model_data %>% filter(loan_type %in% c('2W-New','PV-New','PV-Used')), distinct(cibil_v3 %>% dplyr::select(deal_no,customer_code,cibil_score_v3)), by = c('deal_no','customer_code'))
model_data_p2 <- left_join(model_data %>% filter(loan_type %in% c('Refinance-2W')), distinct(cibil_v3 %>% group_by(deal_no) %>% summarise(cibil_score_v3 = min(cibil_score_v3,na.rm = T))), by = c('deal_no'))

rm(cibil_v3)

model_data <- data.frame(rbind(model_data_p1,model_data_p2))
rm(model_data_p1,model_data_p2)


## 2. Identify NTC flag ----------------------------------------------------------------------------------------------------
model_data$ETC_flag <- ifelse(is.na(model_data$cibil_score_v3), "NA", ifelse(model_data$cibil_score_v3 == -1, "NTC", "ETC"))

ntc_data <- model_data %>% filter(ETC_flag == 'NTC')

ntc_data <- distinct(ntc_data)


## 3. Load state - region mapping -------------------------------------------------------------------------------------------
region_mapping <- data.frame(fread_mapping("state_region_mapping.csv"))

ntc_data <- left_join(ntc_data,region_mapping,by = 'disbursal_state')

rm(region_mapping,model_data,base_ads)



## 4. Load CV Demogs data ----------------------------------------------------------------------------------------------------
load_rdata_intermediate("cleaned_data//cv_demog.rdata")

load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
cibil_v3$appliation_id <- NULL
cibil_v3$cibil_score_v3 <- NULL

cv_demog_data <- inner_join(cibil_v3,cv_demog,by = 'ECN')
rm(cibil_v3,cv_demog)

cv_demog_data$ECN <- NULL


ntc_data_p1 <- ntc_data %>% filter(loan_type %in% c('2W-New','PV-New','PV-Used'))
ntc_data_p2 <- ntc_data %>% filter(loan_type %in% c('Refinance-2W'))
ntc_data_p2$customer_code <- NULL

ntc_data_p1 <- left_join(ntc_data_p1, cv_demog_data, by = c('deal_no','customer_code'))
ntc_data_p2 <- left_join(ntc_data_p2, cv_demog_data, by = c('deal_no'))


ntc_data_p2 <- ntc_data_p2 %>% dplyr::select(colnames(ntc_data_p1))

ntc_data <- data.frame(rbind(ntc_data_p1,ntc_data_p2))

rm(ntc_data_p1,ntc_data_p2)
rm(cv_demog_data)



## 5. Load asset class data ---------------------------------------------------------------------------------------------------------
load_rdata_intermediate("cleaned_data//asset_classification_data.rdata")
asset_classification_data <- distinct(asset_classification_data %>% dplyr::select(Deal_No,Ben_Code,Ben_Name,Segment_Code))
colnames(asset_classification_data) <- c('deal_no',
                                         'ben_code',
                                         'ben_name',
                                         'asset_tag')

ntc_data <- left_join(ntc_data,asset_classification_data,by = 'deal_no')
rm(asset_classification_data)



## 6. Load Application Variables -----------------------------------------------------------------------------------------------------
load_rdata_intermediate("ADS_data//X_var_application.rdata")
all_X_var_application <- distinct(all_X_var_application %>% dplyr::select(deal_no,Category,agri_profile_flag))

ntc_data <- left_join(ntc_data,all_X_var_application, by = 'deal_no')

rm(all_X_var_application)



## 7. Data cleaning -----------------------------------------------------------------------------------------------------------
# cleaned // application data 
#  load sanctioned data to get mapping deal and appl

load("data/intermediate/cleaned_data/application_data.rdata")
load("data/intermediate/cleaned_data/sanction_data.rdata")

application_data <- application_data %>% dplyr::select('AppNo','DOB_cleaned','Marital_Status')
appl_data <- inner_join(application_data, sanction_data, by = c('AppNo'='Application_No'))
columns_subset <- c( 'Deal_No',
                     'DOB_cleaned',
                     'Marital_Status'
)

application_data <- appl_data %>% dplyr::select(columns_subset)

req_cols <- c('non_mortage_balance',
              'missed_payment_ratio_3m','missed_payment_ratio_6m','missed_payment_ratio_12m',
              'phones_reported_3m','phones_reported_6m','phones_reported_12m',
              'addresses_reported_3m','addresses_reported_6m','addresses_reported_12m')

ntc_model_data <- left_join(ntc_data, application_data, by = c('deal_no' = 'Deal_No'))

for(temp_col in req_cols){
  ntc_model_data[,temp_col] <- as.numeric(ifelse(ntc_model_data[,temp_col] < 0, NA, ntc_model_data[,temp_col]))
}


# test <- ntc_model_data %>% group_by(loan_type,asset_tag) %>% summarise(disb_count = n(),
#                                                                        bad_loan_pct = mean(bad_loan),
#                                                                        dpd_90_pct = mean(ever_90dpd),
#                                                                        dpd_150_pct = mean(ever_150dpd)
#                                                                        )
# fwrite(test,".//Asset_tag_delinquency.csv")

assert_data_non_empty(ntc_model_data)
save(ntc_model_data,
     file = file.path(
       get_data_path()$data$intermediate,
       "model_data",
       "new_ntc_data_complete.rdata"
     )
)
# save(ntc_model_data,
#      file = file.path("data//intermediate//model_data//new_ntc_data_complete.rdata"
#      )
# )


## 7. Load Bad loan definition --------------------------------------------------------------------------------------------------------
# bad loans = validation_60dpd_9mob.rdata
# load_rdata_intermediate("ADS_data//bad_loans_v2.rdata")
load("data/intermediate/ADS_data/validation_60dpd_9mob.rdata")
bad_loans_p1 <- validation_60dpd_9mob %>% dplyr::select(deal_no,
                                            bad_loan)
                                            # ever_90dpd,
                                            # ever_150dpd)


# load_rdata_intermediate("ADS_data//bad_loans_2W_RF.rdata")
load("data/intermediate/ADS_data/validation_60dpd_9mob_2W_RF.rdata")

bad_loans_p2 <- validation_60dpd_9mob %>% dplyr::select(deal_no,
                                            bad_loan)
                                            # ever_90dpd,
                                            # ever_150dpd)


bad_loans <- data.frame(rbind(bad_loans_p1,bad_loans_p2))
rm(bad_loans_p1,bad_loans_p2)


ntc_model_data <- inner_join(ntc_model_data,bad_loans,by ='deal_no')






## 9. save data
assert_data_non_empty(ntc_model_data)
save(ntc_model_data,
     file = file.path(
       get_data_path()$data$intermediate,
       "model_data",
       "new_ntc_data.rdata"
     )
)
save(ntc_model_data,
     file = file.path("data//intermediate//model_data//new_ntc_data.rdata"
     )
)




















# load_rdata_intermediate("cleaned_data//ntc_data.rdata")
# load_rdata_intermediate("ADS_data//validation_60dpd_12mob.rdata")
# load_rdata_intermediate("ADS_data//X_var_application.rdata")
# 
# # load_rdata_intermediate("cleaned_data//asset_classification_data.rdata")
# 
# 
# ## 2. Create intermediate datasets ------------------------------------------------------------
# 
# ## 2.1 subset for required columns from ads data
# model_data <- base_ads %>% filter(loan_type %in% c('2W-New','PV-New','PV-Used')) %>% dplyr::select(deal_no,
#                                                                                                    disbursal_date,
#                                                                                                    year_mon,
#                                                                                                    DOB_cleaned,
#                                                                                                    CIBIL_SCORE)
# 
# 
# ## 2.2 subset required columns from NTC data
# ntc_features <- ntc_data %>% dplyr::select(acct_num,
#                                            type1,
#                                            ccode,
#                                            ntc_band,
#                                            dm001s,
#                                            dm201s,
#                                            dm216s,
#                                            dm202s,
#                                            dm212s,
#                                            dm203s,
#                                            dm206s,
#                                            dm211s,
#                                            dm004s)
# 
# colnames(ntc_features) <- c('acct_num',
#                             'type1',
#                             'ccode',
#                             'ntc_band',
#                             'age',
#                             'phone_count_3m',
#                             'months_since_last_update_addr',
#                             'phone_count_6m',
#                             'addr_count_6m',
#                             'phone_count_12m',
#                             'months_since_last_update_phone',
#                             'addr_count_3m',
#                             'owner'
# )
# 
# ## 2.3 subset application data variables
# all_X_var_application <- distinct(all_X_var_application %>% dplyr::select(deal_no,disbursal_state,Category,agri_profile_flag))
# 
# 
# ## 2.4 get asset classification tags & dealer ben code
# 
# 
# ## 3. Join all datasets --------------------------------------------------------------------------------------------------
# model_data <- inner_join(model_data,primary_app_customer_code,by = 'deal_no')
# model_data <- inner_join(model_data,validation_60dpd_12mob,by = 'deal_no')
# model_data <- inner_join(model_data,ntc_features,by = c('customer_code' = 'ccode'))
# model_data <- left_join(model_data,all_X_var_application,by = 'deal_no')
# model_data <- left_join(model_data,region_mapping,by = 'disbursal_state')
# 
# 
# rm(base_ads,ntc_data,ntc_features,primary_app_customer_code,validation_60dpd_12mob,all_X_var_application,region_mapping)
# 
# 
# 
# ## 4. variable cleaning ---------------------------------------------------------------------------------------------------
# 
# ntc_model_data <- data.frame(model_data)
# 
# 
# ## 4.1 replace negative values with NA
# selected_cols <- c('age',
#                    'phone_count_3m',
#                    'months_since_last_update_addr',
#                    'phone_count_6m',
#                    'addr_count_6m',
#                    'phone_count_12m',
#                    'months_since_last_update_phone',
#                    'addr_count_3m',
#                    'owner')
# 
# for(col in selected_cols){
#   ntc_model_data[,col] <- ifelse(ntc_model_data[,col] < 0, NA, ntc_model_data[,col])
# }
# 
# 
# 
# 
# 
# 
# assert_data_non_empty(ntc_model_data)
# save(ntc_model_data,
#      file = file.path(
#        get_data_path()$data$intermediate,
#        "model_data",
#        "ntc_data.rdata"
#      )
# )
# 
# 
# 
# 
# rm(list=ls())
# 
# ############################################################
# 
# ## 0. Load helper functions & libraries ----------------------------------------------------
# load_libaries <- file.path("src","utils","load_libraries.R")
# source(load_libaries)
# 
# io_helper <- file.path("src","utils","io_helper.R")
# source(io_helper)
# 
# options(scipen = 999)
# voptions(raise = "all")
# 
# `%notin%` <- Negate(`%in%`)
# 
# 
# ## 1. Load data ----------------------------------------------------------------------------
# load_rdata_intermediate("cleaned_data//ntc_data.rdata")
# load_rdata_intermediate("cleaned_data//primary_app_customer_code.rdata")
# load_rdata_intermediate("ADS_data//base_ads.rdata")
# load_rdata_intermediate("ADS_data//validation_60dpd_12mob.rdata")
# load_rdata_intermediate("ADS_data//X_var_application.rdata")
# region_mapping <- data.frame(fread_mapping("state_region_mapping.csv"))
# # load_rdata_intermediate("cleaned_data//asset_classification_data.rdata")
# 
# 
# ## 2. Create intermediate datasets ------------------------------------------------------------
# 
# ## 2.1 subset for required columns from ads data
# model_data <- base_ads %>% filter(loan_type %in% c('2W-New','PV-New','PV-Used')) %>% dplyr::select(deal_no,
#                                                                                                    disbursal_date,
#                                                                                                    year_mon,
#                                                                                                    DOB_cleaned,
#                                                                                                    CIBIL_SCORE)
# 
# 
# ## 2.2 subset required columns from NTC data
# ntc_features <- ntc_data %>% dplyr::select(acct_num,
#                                            type1,
#                                            ccode,
#                                            ntc_band,
#                                            dm001s,
#                                            dm201s,
#                                            dm216s,
#                                            dm202s,
#                                            dm212s,
#                                            dm203s,
#                                            dm206s,
#                                            dm211s,
#                                            dm004s)
# 
# colnames(ntc_features) <- c('acct_num',
#                             'type1',
#                             'ccode',
#                             'ntc_band',
#                             'age',
#                             'phone_count_3m',
#                             'months_since_last_update_addr',
#                             'phone_count_6m',
#                             'addr_count_6m',
#                             'phone_count_12m',
#                             'months_since_last_update_phone',
#                             'addr_count_3m',
#                             'owner'
# )
# 
# ## 2.3 subset application data variables
# all_X_var_application <- distinct(all_X_var_application %>% dplyr::select(deal_no,disbursal_state,Category,agri_profile_flag))
# 
# 
# ## 2.4 get asset classification tags & dealer ben code
# 
# 
# ## 3. Join all datasets --------------------------------------------------------------------------------------------------
# model_data <- inner_join(model_data,primary_app_customer_code,by = 'deal_no')
# model_data <- inner_join(model_data,validation_60dpd_12mob,by = 'deal_no')
# model_data <- inner_join(model_data,ntc_features,by = c('customer_code' = 'ccode'))
# model_data <- left_join(model_data,all_X_var_application,by = 'deal_no')
# model_data <- left_join(model_data,region_mapping,by = 'disbursal_state')
# 
# 
# rm(base_ads,ntc_data,ntc_features,primary_app_customer_code,validation_60dpd_12mob,all_X_var_application,region_mapping)
# 
# 
# 
# ## 4. variable cleaning ---------------------------------------------------------------------------------------------------
# 
# ntc_model_data <- data.frame(model_data)
# 
# 
# ## 4.1 replace negative values with NA
# selected_cols <- c('age',
#                    'phone_count_3m',
#                    'months_since_last_update_addr',
#                    'phone_count_6m',
#                    'addr_count_6m',
#                    'phone_count_12m',
#                    'months_since_last_update_phone',
#                    'addr_count_3m',
#                    'owner')
# 
# for(col in selected_cols){
#   ntc_model_data[,col] <- ifelse(ntc_model_data[,col] < 0, NA, ntc_model_data[,col])
# }
# 
# 
# 
# 
# 
# 
# assert_data_non_empty(ntc_model_data)
# save(ntc_model_data,
#      file = file.path(
#        get_data_path()$data$intermediate,
#        "model_data",
#        "ntc_data.rdata"
#      )
# )
# 
# 
# 
# 
# rm(list=ls())







# ## 2.1 filter for 2018
# date_start <- '2018-04-01'
# date_end <- '2018-12-31'
# 
# 
# ntc_data <-
#   base_ads %>% filter((disbursal_date >= as.Date(date_start)) &
#                                  (disbursal_date <= as.Date(date_end)))
# 
# ## 2.2 subset for requred columns
# ntc_data <- distinct(ntc_data %>% dplyr::select(deal_no, 
#                                                 disbursal_date,
#                                                 year_mon,
#                                                 CIBIL_SCORE, 
#                                                 cibil_score_primary_applicant,
#                                                 new_or_existing_bank,
#                                                 CUSTOMER_PROFILE,
#                                                 Finance_Amount_Chassis,
#                                                 CITY,
#                                                 disbursal_state
#                                                 ))
# 
# 
# ## 2.3 tag bad loans
# ntc_data <- inner_join(ntc_data,bad_loans,by='deal_no')
# rm(bad_loans,base_ads)
# 
# 
# 
# ## 2.4 get bureau no hit cases
# retro_as_on_data  <- data.frame(retro_as_on_data)
# bureau_hit <- retro_as_on_data %>% dplyr::select(deal_no)
# bureau_hit <- distinct(bureau_hit)
# bureau_hit$c1 <- 1
# 
# ntc_data <- left_join(ntc_data,bureau_hit,by='deal_no')
# 
# ntc_data <- ntc_data %>% filter(is.na(c1))
# ntc_data$c1 <- NULL
# 
# 
# ## 2.5 filter for cases where cibil <= 5
# ntc_data <- ntc_data %>% filter((is.na(CIBIL_SCORE)) | (CIBIL_SCORE <= 5))
# 
# rm(bureau_hit,retro_as_on_data)
# 
# 
# ## 2.6 get salary profile
# salary_profile <- data.frame(fread_mapping('profile_mapping.csv'))
# 
# ntc_data <- left_join(ntc_data,salary_profile,by=c("CUSTOMER_PROFILE" = "sub_category"))
# 
# colnames(ntc_data)[colnames(ntc_data) == "new_category"] <- "Category"
# 
# 
# ## 2.7 create agri flag
# ntc_data$agri_profile_flag <- ifelse(ntc_data$sub_profile == "Agriculture oriented", 1, 0) 
# 
# 
# ## 2.8 remove NA cases
# ntc_data <- ntc_data %>% filter(!is.na(Category))
# 
# ## 2.9 filter for non-NE cases
# ntc_data <- ntc_data %>% filter(Category %in% c('SAL','SENP','SEP'))
# 
# 
# 
# ## 2.10 save data
# 
# assert_data_non_empty(ntc_data)
# save(ntc_data,
#      file = file.path(
#        get_data_path()$data$intermediate,
#        "model_data",
#        "ntc_data.rdata"
#      )
# )
# 

