############################################################################################
################## 2 - ADS Creation ########################################################
############################################################################################


## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")


## 1. Load disbursal data & select required columns  ---------------------------------------------------------------------------

# 1.1 Load disbursal data
load_rdata_intermediate("cleaned_data//disbursal_data.rdata")

# 1.2 create disbursal year mon column
disbursal_data$year_mon <- paste0(year(disbursal_data$disbursal_date), "-", str_pad(month(disbursal_data$disbursal_date),2,"0",side = "left"))

# 1.3 select required columns
req_cols <- c('deal_no',
              'loan_type',
              'disbursal_date',
              'year_mon',
              'new_or_existing_bank',
              'CUSTOMER_PROFILE',
              'CIBIL_SCORE',
              'Finance_Amount_Chassis',
              'irr',
              'CITY',
              'disbursal_state'
              )

# 1.4 filter for required columns and disbursals between Mar 2018 - Dec 2020
start_date <- "2018-03-01"
end_date <- "2020-12-31"

base_ads <- disbursal_data %>% filter((disbursal_date >= as.Date(start_date)) &
                                                (disbursal_date <= as.Date(end_date))) %>% dplyr::select(req_cols)

rm(disbursal_data)

base_ads <- distinct(base_ads)


## 2. Load sanction data & select required columns  -----------------------------------------------------------------------------

## 2.1 load sanction data
load_rdata_intermediate("cleaned_data//sanction_data.rdata")

## 2.2 create coborrower flags
sanction_data$Primary_CoBorrower <- ifelse(sanction_data$Primary_CoBorrower == '',NA,sanction_data$Primary_CoBorrower)
sanction_data$Secondary_CoBorrower <- ifelse(sanction_data$Secondary_CoBorrower == '',NA,sanction_data$Secondary_CoBorrower)

sanction_data$primary_coborrower_flag <- ifelse(is.na(sanction_data$Primary_CoBorrower), 0, 1)
sanction_data$secondary_coborrower_flag <- ifelse(is.na(sanction_data$Secondary_CoBorrower), 0, 1)

## 2.3 creare borrower count column
sanction_data$borrower_count <- sanction_data$primary_coborrower_flag + sanction_data$secondary_coborrower_flag


## 2.4 subset for required columns
req_cols <- c("Application_No",
              "Deal_No",
              "Vehicle_Id",
              "Customer_Code",
              "Quantum_Finance_Per_Vehicle_Chassis_Amt",
              "Quantum_Finance_Per_Vehicle_Body_Amt",
              "Quantum_Finance_Per_Vehicle_Customs_Amt",
              "Quantum_Finance_Per_Vehicle_With_Insurance_Chassis_Amt",
              "Quantum_Finance_Per_Vehicle_With_Insurance_Body_Amt",
              "Quantum_Finance_Per_Vehicle_With_Insurance_Customs_Amt",
              "Quantum_Finance_Total_Amt",
              "Cost_Per_Vehicle_Chassis_Amt",
              "Cost_Per_Vehicle_Body_Amt",
              "Cost_Per_Vehicle_Customs_Amt",
              "Cost_Per_Vehicle_Total_Amt",
              "No_Of_Vehicles",
              "Base_Per",
              "Customer_IRR",
              "Margin_Per",
              "Interest_Per",
              "Period_In_Months",
              "Total_Sanctiond_Amount",
              "deal_date_cleaned",
              "DO_date_cleaned",
              "sanction_date"
)


sanction_subset <- distinct(sanction_data %>% dplyr::select(req_cols))


## 2.5 join sanction data with disbursal data
base_ads <- left_join(base_ads,sanction_subset,by = c("deal_no" = "Deal_No"))

rm(sanction_subset,sanction_data)





## 3. Load application data & select required columns -----------------------------------------------------------

## 3.1 load application data
load_rdata_intermediate("cleaned_data//application_data_subset.rdata")

## 3.2 clean evalation tyoe of primary applicant
application_data_subset$evaluation_type_primary_applicant <- ifelse(application_data_subset$evaluation_type_primary_applicant == 'data unavailable', NA,
                                                                    application_data_subset$evaluation_type_primary_applicant)

## 3.3 rename cibil score column
colnames(application_data_subset)[colnames(application_data_subset) == 'Cibil_Score'] <- 'cibil_score_primary_applicant'

## 3.4 clean primary applicant cibil score
application_data_subset$cibil_score_primary_applicant <- ifelse(application_data_subset$cibil_score_primary_applicant == '000-1', '1',application_data_subset$cibil_score_primary_applicant)
application_data_subset$cibil_score_primary_applicant <- as.integer(application_data_subset$cibil_score_primary_applicant)


## 3.4 join application data into base ads
base_ads <- left_join(base_ads,application_data_subset,by = c("Application_No" = "AppNo"))

rm(application_data_subset)



# test <- base_ads %>% filter(!is.na(Application_No))
# test <- test %>% dplyr::select(deal_no,CIBIL_SCORE, cibil_score_primary_applicant)
# 
# test$check <- ifelse(test$CIBIL_SCORE == test$cibil_score_primary_applicant, 0, 1)


# 4. Load ADE data & select required columns ---------------------------------------------------------------------------

# 4.1 load ADE data
load_rdata_intermediate("cleaned_data//ade_data.rdata")

# 4.2 select existing to CFD column
ade_data$new_or_existing_CFD <- ade_data$ADE_Dedupe_Customer_Type
ade_data <- distinct(ade_data %>% dplyr::select(AppNo,new_or_existing_CFD))

# 4.3 join ADE data to base ads data
base_ads <- left_join(base_ads, ade_data, by = c("Application_No" = "AppNo"))

rm(ade_data)



# 5. Load lead data & select required columns ---------------------------------------------------------------------------

## 5.1 load lead data
load_rdata_intermediate("cleaned_data//lead_data.rdata")

## 5.2 subset for required columns
req_cols <- c('LeadNo',
              'lead_date',
              'lead_sync_date',
              'mapped_lead_source')

lead_data_subset <- distinct(lead_data %>% dplyr::select(req_cols))

## 5.3 join with ads data
base_ads <- left_join(base_ads,lead_data_subset,by = c("LeadNo" = "LeadNo"))

rm(lead_data,lead_data_subset)

nrow(base_ads) == length(unique(base_ads$deal_no))

## 6. Load asset data & select required columns ---------------------------------------------------------------

## 6.1 Load asset data
load_rdata_intermediate("cleaned_data//asset_data.rdata")

## 6.2 select purchase purpose column
asset_data <- distinct(asset_data %>% dplyr::select(AppNo,PurchasePurpose))

## 6.3 get app no with multiple purpose
distinct_purpose_count <- asset_data %>% group_by(AppNo) %>% summarise(count=n())

count_1 <- distinct_purpose_count %>% filter(count == 1)
count_G_1 <- distinct_purpose_count %>% filter(count > 1)

## 6.4 subset asset data based on count of purpose
asset_data_1 <- asset_data %>% filter(AppNo %in% unique(count_1$AppNo))
asset_data_G_1 <- asset_data %>% filter(AppNo %in% unique(count_G_1$AppNo))

## 6.5 create priority & choose minimum priority
asset_data_G_1$priority <- ifelse(asset_data_G_1$PurchasePurpose == 'Commercial', 1,
                                  ifelse(asset_data_G_1$PurchasePurpose == 'Personal', 2, 3))

asset_data_G_1 <- asset_data_G_1 %>% group_by(AppNo) %>% summarise(priority = min(priority))
asset_data_G_1$PurchasePurpose <- ifelse(asset_data_G_1$priority == 1, 'Commercial',
                                         ifelse(asset_data_G_1$priority == 2, 'Personal', NA))

asset_data_G_1$priority <- NULL

asset_data <- rbind(asset_data_1, asset_data_G_1)
rm(asset_data_1, asset_data_G_1, count_1, count_G_1, distinct_purpose_count)


## 6.6 join with ads data
base_ads <- left_join(base_ads,asset_data,by = c("Application_No" = "AppNo"))


nrow(base_ads) == length(unique(base_ads$deal_no))

rm(asset_data)


# 7. load approval data & select required columns -------------------------------------------------------------

## 7.1 load approval data
load_rdata_intermediate("cleaned_data//approval_data.rdata")

## 7.2 get latest approval data
approval_data_agg <- approval_data %>% group_by(Application_No) %>% summarise(approval_date_cleaned = max(approval_date_cleaned, na.rm = T))

# approval_data$nchar_deviationid <- nchar(approval_data$DeviationId)
# 
# approval_data$DeviationId <- ifelse(approval_data$nchar_deviationid %in% c(0,73), NA, approval_data$DeviationId)
# 
# approval_data_agg1 <- approval_data %>% group_by(Application_No) %>% summarise(approval_date_cleaned = max(approval_date_cleaned, na.rm = T))
# 
# approval_data_agg2 <- approval_data %>% filter(!is.na(DeviationId))
# approval_data_agg2 <- approval_data_agg2 %>% group_by(Application_No) %>% summarise(deviation_count = length(unique(DeviationId)))
# 
# 
# approval_data_agg <- left_join(approval_data_agg1,approval_data_agg2,by = "Application_No")
# rm(approval_data_agg1,approval_data_agg2)
# 
# approval_data_agg$deviation_count <- ifelse(is.na(approval_data_agg$deviation_count), 0, approval_data_agg$deviation_count)
# approval_data_agg$deviation_flag <- ifelse(approval_data_agg$deviation_count > 0, 1, 0)


## 7.3 join with ads
base_ads <- left_join(base_ads,approval_data_agg,by = c("Application_No" = "Application_No"))
rm(approval_data,approval_data_agg)



# 8. load coborrower data ---------------------------------------------------------------------------------------

## 8.1 load coborrower data
load_rdata_intermediate("cleaned_data//coborrower_2W.rdata")
load_rdata_intermediate("cleaned_data//coborrower_PV.rdata")


coborrower_data_subset <- rbind(coborrower_2W,coborrower_PV)


borrower_count_data <- coborrower_data_subset %>% group_by(primary_appno) %>% summarise(borrower_count =length(unique(AppNo)))
borrower_count_data <- borrower_count_data %>% filter(borrower_count <= 2)


base_ads <- left_join(base_ads,borrower_count_data,by=c("Application_No" = "primary_appno"))
rm(borrower_count_data)


# save(base_ads,
#      file = file.path(
#              get_data_path()$data$intermediate,
#              "ADS_data",
#              "interim_base_data.rdata"
#      )
# )


# load_rdata_intermediate("ADS_data//interim_base_data.rdata")



# test <- base_ads %>% filter(loan_type == '2W-New')
# nrow(test) # 2002917
# 
# test2 <- distinct(coborrower_2W %>% dplyr::select(primary_appno,AppNo))
# 
# 
# test <- left_join(test,test2,by = c("Application_No" = "primary_appno"))
# 
# 
# test3 <- test %>% filter(!is.na(Application_No))
# 
# test4 <- test3 %>% filter(is.na(AppNo))





## 8.2 subset ads based on borrower count
base_ads_0_coborrower <- base_ads %>% filter((borrower_count == 0) | (is.na(borrower_count)))

base_ads_primary_coborrower <- base_ads %>% filter(borrower_count == 1)

base_ads_secondary_coborrower <- base_ads %>% filter(borrower_count == 2)


## 8.3 create copies of coborrower data
coborrower_g1 <- coborrower_data_subset
coborrower_g2 <- coborrower_data_subset

rm(coborrower_data_subset, coborrower_2W, coborrower_PV)

## 8.4 rename columns in coborrower data
base_cols <- colnames(coborrower_g1)

colnames_g1 <- paste0(base_cols,"_coborrower_1")
colnames_g2 <- paste0(base_cols,"_coborrower_2")

colnames(coborrower_g1) <- colnames_g1
colnames(coborrower_g2) <- colnames_g2


## 8.5 join with base ads
base_ads_primary_coborrower <- left_join(base_ads_primary_coborrower,coborrower_g1, by = c("Application_No" = "primary_appno_coborrower_1"))




temp_secondary_coborrower <- left_join(base_ads_secondary_coborrower,coborrower_g1, by = c("Application_No" = "primary_appno_coborrower_1"))




# temp_secondary_coborrower$coborrower_index <- substr(temp_secondary_coborrower$AppNo_coborrower_1, 
#                                                      nchar(temp_secondary_coborrower$AppNo_coborrower_1),
#                                                      nchar(temp_secondary_coborrower$AppNo_coborrower_1))
# 
# 
# first_coborrower <- temp_secondary_coborrower %>% group_by(deal_no) %>% summarise(min_index = min(coborrower_index))
# second_coborrower <- temp_secondary_coborrower %>% group_by(deal_no) %>% summarise(max_index = max(coborrower_index))






temp_pivot <- temp_secondary_coborrower %>% dplyr::select(deal_no,Application_No,AppNo_coborrower_1)
temp_pivot <- temp_pivot %>% group_by(deal_no,Application_No) %>% mutate(ranking = dense_rank(AppNo_coborrower_1))
temp_pivot$col <- paste0("app_no_",temp_pivot$ranking)
temp_pivot$ranking <- NULL


temp_pivot <- pivot_wider(temp_pivot, id_cols = c(deal_no,Application_No), names_from = col, values_from = AppNo_coborrower_1) 

# temp_secondary_coborrower <- temp_secondary_coborrower %>% filter(Application_No %in% unique(temp_pivot$Application_No))


temp_1 <- temp_secondary_coborrower %>% filter(AppNo_coborrower_1 %in% unique(temp_pivot$app_no_1))

temp_2 <- temp_secondary_coborrower %>% filter(AppNo_coborrower_1 %in% unique(temp_pivot$app_no_2))


temp_2 <- temp_2 %>% dplyr::select(deal_no,Application_No,
                                   AppNo_coborrower_1,
                                   LeadNo_coborrower_1,
                                   Curr_Addr_Is_Perm_Addr_coborrower_1,
                                   Resi_Cum_Off_coborrower_1,
                                   Cibil_Score_coborrower_1,
                                   InTime_cleaned_coborrower_1,
                                   App_Maker_Date_cleaned_coborrower_1,
                                   application_date_coborrower_1,
                                   DOB_cleaned_coborrower_1,
                                   customer_type_cleaned_coborrower_1,
                                   FI_Residence_Time_coborrower_1,
                                   FI_Residence_Time_syntime_coborrower_1,
                                   FI_Office_Time_coborrower_1,
                                   FI_Office_Time_syntime_coborrower_1,
                                   fi_resi_check_coborrower_1,
                                   fi_resi_perm_check_coborrower_1,
                                   fi_office_check_coborrower_1,
                                   nri_flag_coborrower_1,
                                   profile_coborrower_1,
                                   evaluation_type_coborrower_1
                                   )

colnames(temp_2) <- c('deal_no','Application_No',
                      'AppNo_coborrower_2',
                      'LeadNo_coborrower_2',
                      'Curr_Addr_Is_Perm_Addr_coborrower_2',
                      'Resi_Cum_Off_coborrower_2',
                      'Cibil_Score_coborrower_2',
                      'InTime_cleaned_coborrower_2',
                      'App_Maker_Date_cleaned_coborrower_2',
                      'application_date_coborrower_2',
                      'DOB_cleaned_coborrower_2',
                      'customer_type_cleaned_coborrower_2',
                      'FI_Residence_Time_coborrower_2',
                      'FI_Residence_Time_syntime_coborrower_2',
                      'FI_Office_Time_coborrower_2',
                      'FI_Office_Time_syntime_coborrower_2',
                      'fi_resi_check_coborrower_2',
                      'fi_resi_perm_check_coborrower_2',
                      'fi_office_check_coborrower_2',
                      'nri_flag_coborrower_2',
                      'profile_coborrower_2',
                      'evaluation_type_coborrower_2')        
        
temp_secondary_coborrower <- left_join(temp_1, temp_2, by = c("deal_no" = "deal_no",
                                                             "Application_No" = "Application_No"))   


base_ads_secondary_coborrower <- distinct(temp_secondary_coborrower)






# 
# 
# 
# 
# base_ads_secondary_coborrower$last_char <- as.integer(substr(base_ads_secondary_coborrower$AppNo_coborrower_1,
#                                                    nchar(base_ads_secondary_coborrower$AppNo_coborrower_1),
#                                                    nchar(base_ads_secondary_coborrower$AppNo_coborrower_1)))
# 
# temp1 <-base_ads_secondary_coborrower %>% filter(last_char == 1)
# temp2 <-base_ads_secondary_coborrower %>% filter(last_char != 1)
# 
# coborrower_g2 <- coborrower_g2 %>% filter(AppNo_g2 %in% unique(temp2$))
# 
# base_ads_secondary_coborrower <- left_join(base_ads_secondary_coborrower,coborrower_g2, by = c("Secondary_CoBorrower" = "AppNo_coborrower_2"))


## 8.6 append all ADS data together

updated_base_ads <- rbindlist(l=list(base_ads_secondary_coborrower,base_ads_primary_coborrower,base_ads_0_coborrower),use.names = T,fill = T)

rm(base_ads_0_coborrower,base_ads_primary_coborrower,base_ads_secondary_coborrower,coborrower_g1,coborrower_g2)
rm(temp_1,temp_2,temp_pivot,temp_secondary_coborrower)

base_ads <- updated_base_ads
rm(updated_base_ads)


## 8.7 clean borrower cibil scores
base_ads$Cibil_Score_coborrower_1 <- ifelse(base_ads$Cibil_Score_coborrower_1 == '000-1', '1',base_ads$Cibil_Score_coborrower_1)
base_ads$Cibil_Score_coborrower_1 <- as.integer(base_ads$Cibil_Score_coborrower_1)

base_ads$Cibil_Score_coborrower_2 <- ifelse(base_ads$Cibil_Score_coborrower_2 == '000-1', '1',base_ads$Cibil_Score_coborrower_2)
base_ads$Cibil_Score_coborrower_2 <- as.integer(base_ads$Cibil_Score_coborrower_2)


## filter & save
assert_data_non_empty(base_ads)
save(base_ads,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "base_ads.rdata"
     )
)




# load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
# load_rdata_intermediate("cleaned_data//primary_app_customer_code.rdata")
# 
# 
# 
# test <- distinct(base_ads %>% dplyr::select(deal_no,loan_type,year_mon))
# test <- left_join(test,primary_app_customer_code,by ='deal_no')
# 
# test <- left_join(test,cibil_v3,by='customer_code')
# 
# test$check <- ifelse(is.na(test$cibil_score_v3),0,1)
# 
# View(test %>% group_by(loan_type,year_mon) %>% summarise(n=length(unique(deal_no.x)), sum_check = sum(check)) %>% mutate(pct = sum_check/n))






# load_rdata_intermediate("ADS_data//base_ads.rdata")


# fwrite(base_ads %>% group_by(loan_type,year_mon) %>% summarise(disbursal_count = length(unique(deal_no))), 'product_wise_disb_count.csv')



# base_ads1 <- base_ads %>% filter(disbursal_date >= as.Date('2018-04-01') & disbursal_date <= as.Date('2018-12-31'))
base_ads1 <- base_ads %>% filter(year_mon %in% c('2020-10','2020-11','2020-12'))
base_ads1 <- base_ads1 %>% filter(loan_type %in% c('2W-New','PV-New','PV-Used'))


req_cols <- c('deal_no',
              'loan_type',
              'disbursal_date',
              'profile_primary_applicant',
              'disbursal_state',
              # 'CIBIL_SCORE',
              'Finance_Amount_Chassis',
              'irr'
)


base_ads1 <- base_ads1 %>% dplyr::select(req_cols)


LTV_data <- data.frame(fread_raw("LTV_data.txt"))

base_ads1 <- left_join(base_ads1,LTV_data,by = 'deal_no')


load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
cibil_v3 <- distinct(cibil_v3 %>% dplyr::select(customer_code,cibil_score_v3))


load_rdata_intermediate("cleaned_data//primary_app_customer_code.rdata")
base_ads1 <- left_join(base_ads1,primary_app_customer_code,by='deal_no')

base_ads1 <- left_join(base_ads1,cibil_v3,by = 'customer_code')

base_ads1$customer_code <- NULL

fwrite(base_ads1,"disbursals_Oct_to_Dec_2020.csv")
