############################################################################################
################## 1 - Raw data cleaning ###################################################
############################################################################################


## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")



## 1. Clean & save disbursal data -----------------------------------------------------------

## 1.1 load data

# disbursal data
disbursal_data <- data.frame(fread_raw("bcg_data.txt"))

# payment date data
payment_date <- data.frame(fread_raw("DealNo.txt"))

# disbursal state mapping
state_mapping <- data.frame(fread_mapping("disbursal_state_mapping.csv"))



## 1.2 join data
disbursal_data <- left_join(disbursal_data,payment_date,by='deal_no')
disbursal_data <- left_join(disbursal_data,state_mapping,by='STATE')

disbursal_data$disbursal_state <- ifelse(is.na(disbursal_data$disbursal_state), NA ,disbursal_data$disbursal_state)


rm(payment_date,state_mapping)



## 1.3 change format of deal_date
disbursal_data$disbursal_date <- as.Date(substr(disbursal_data$payment_date,1,10),
                                         format = "%Y-%m-%d",
                                         origin = "1970-01-01")

## 1.4 remove duplicates
disbursal_data <- disbursal_data %>% distinct(deal_no , .keep_all = T)


## 1.5 Map PV Refinance & create unique loan type column

# temp loan identifier
disbursal_data$temp_loan_type <- paste0(disbursal_data$PRODUCT, "-",disbursal_data$used)

# drop 2W - Used records
disbursal_data <- disbursal_data %>% filter(temp_loan_type != 'H-U')

# create unique loan type column
disbursal_data$loan_type <- ifelse(disbursal_data$temp_loan_type == 'C-N','PV-New',
                                   ifelse(disbursal_data$temp_loan_type == 'H-N','2W-New',
                                          ifelse(disbursal_data$temp_loan_type == 'S-U','Refinance-2W',NA)))


disbursal_data_p1 <- disbursal_data %>% filter(!is.na(loan_type))
disbursal_data_p2 <- disbursal_data %>% filter(is.na(loan_type))

# map PV used & refinance
pv_used_mapping <- data.frame(fread_raw("PV_Used_Mapping.csv"))

pv_used_disbursals <- unique((pv_used_mapping %>% filter(Type == 'U'))$deal_no)
pv_refinance_disbursals <- unique((pv_used_mapping %>% filter(Type == 'R'))$deal_no)


disbursal_PV_used <- disbursal_data_p2 %>% filter(deal_no %in% pv_used_disbursals)
disbursal_PV_refinance <- disbursal_data_p2 %>% filter(deal_no %in% pv_refinance_disbursals)

disbursal_PV_used$loan_type <- 'PV-Used'
disbursal_PV_refinance$loan_type <- 'Refinance-PV'


disbursal_data_p2 <- data.frame(rbindlist(l= list(disbursal_PV_used,disbursal_PV_refinance)))
disbursal_data <- data.frame(rbindlist(l=list(disbursal_data_p1,disbursal_data_p2)))

rm(disbursal_data_p1,
   disbursal_data_p2,
   disbursal_PV_refinance,
   disbursal_PV_used,
   pv_used_mapping,
   pv_used_disbursals,
   pv_refinance_disbursals)

disbursal_data$temp_loan_type <- NULL


## 1.6 get mapping of new or existing to bank
ntb_status <- data.frame(fread_raw("BCG_CustDtls.txt"))
ntb_status <- distinct(ntb_status %>% dplyr::select(V1,V5))

colnames(ntb_status) <- c('deal_no','new_or_existing_bank')


disbursal_data <- data.frame(left_join(disbursal_data,ntb_status,by='deal_no'))

rm(ntb_status)

## 1.6 save data
assert_data_non_empty(disbursal_data)
save(disbursal_data,
     file = file.path(
       get_data_path()$data$intermediate,
       "cleaned_data",
       "disbursal_data.rdata"
     )
)

rm(disbursal_data)






disbursal_data <- data.frame(fread_raw("disbursal_data_2021.csv"))

# disbursal state mapping
state_mapping <- data.frame(fread_mapping("disbursal_state_mapping.csv"))

## 1.2 join data
disbursal_data <- left_join(disbursal_data,state_mapping,by='STATE')

disbursal_data$disbursal_state <- ifelse(is.na(disbursal_data$disbursal_state), NA ,disbursal_data$disbursal_state)


rm(state_mapping)


## 1.3 change format of deal_date
disbursal_data$deal_date <- as.Date(substr(disbursal_data$deal_date,1,10),
                                         format = "%Y-%m-%d",
                                         origin = "1970-01-01")

## 1.4 remove duplicates
disbursal_data <- disbursal_data %>% distinct(deal_no , .keep_all = T)


# temp loan identifier
disbursal_data$temp_loan_type <- paste0(disbursal_data$PRODUCT, "-",disbursal_data$used)

# drop 2W - Used records
disbursal_data <- disbursal_data %>% filter(temp_loan_type != 'H-U')

# create unique loan type column
disbursal_data$loan_type <- ifelse(disbursal_data$temp_loan_type == 'C-N','PV-New',
                                   ifelse(disbursal_data$temp_loan_type == 'H-N','2W-New',
                                          ifelse(disbursal_data$temp_loan_type == 'C-U','PV-Used',
                                          ifelse(disbursal_data$temp_loan_type == 'S-U','Refinance-2W',NA))))


disbursal_data_2021 <- disbursal_data %>% filter(!is.na(loan_type))


assert_data_non_empty(disbursal_data_2021)
save(disbursal_data_2021,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "disbursal_data_2021.rdata"
     )
)

rm(disbursal_data,disbursal_data_2021)


## 2. Clean & save sanction data -----------------------------------------------------------

## 2.1 load data
sanction_data_v1 <- data.frame(fread_raw("SanctionData.txt"))
sanction_data_v2 <- data.frame(fread_raw("Sanction2018.txt"))

sanction_data <- data.frame(rbindlist(l=list(sanction_data_v2,sanction_data_v1), use.names = T))

rm(sanction_data_v1,sanction_data_v2)

## 2.2 create sanction date from Maker_Date
sanction_data$sanction_date <- as.Date(substr(sanction_data$Maker_Date,1,10),
                                         format = "%Y-%m-%d",
                                         origin = "1970-01-01")


sanction_data$deal_date_cleaned <- as.Date(substr(sanction_data$Deal_Date,1,10),
                                       format = "%Y-%m-%d",
                                       origin = "1970-01-01")


sanction_data$DO_date_cleaned <- as.Date(substr(sanction_data$DO_Date,1,10),
                                           format = "%Y-%m-%d",
                                           origin = "1970-01-01")



## 2.3 save data
assert_data_non_empty(sanction_data)
save(sanction_data,
     file = file.path(
       get_data_path()$data$intermediate,
       "cleaned_data",
       "sanction_data.rdata"
     )
)

rm(sanction_data)




## 3. Clean & save Lead data -----------------------------------------------------------

## 3.1 load data
lead_data_v1 <- data.frame(fread_raw("LeadData.txt"))
lead_data_v2 <- data.frame(fread_raw("Lead2018.txt"))

col_list <- colnames(lead_data_v1)
lead_data_v2 <- lead_data_v2 %>% dplyr::select(col_list)

# change data types
lead_data_v2[] <- mapply(FUN = as,
                         lead_data_v2,
                         sapply(lead_data_v1,class),SIMPLIFY = FALSE)


lead_data <- data.frame(rbindlist(l=list(lead_data_v1,lead_data_v2), use.names = T))


rm(lead_data_v1,lead_data_v2)


## 2.2 change format of Maker_Date
lead_data$lead_date <- as.Date(substr(lead_data$Date,1,10),
                                    format = "%Y-%m-%d",
                                    origin = "1970-01-01")

lead_data$lead_sync_date <- as.Date(substr(lead_data$sync_time,1,10),
                          format = "%Y-%m-%d",
                          origin = "1970-01-01")


## 2.3 map lead source
source_mapping <- fread_mapping("source_mapping.csv")
source_mapping <- distinct(source_mapping)

lead_data <- left_join(lead_data,source_mapping,by = "lead_source")



## 2.3 save data
assert_data_non_empty(lead_data)
save(lead_data,
     file = file.path(
       get_data_path()$data$intermediate,
       "cleaned_data",
       "lead_data.rdata"
     )
)

rm(lead_data,source_mapping)




## 4. Clean & save Asset data -----------------------------------------------------------

## 4.1 load data
asset_data <- fread_raw("AssetData.txt")

## 4.2 save data
assert_data_non_empty(asset_data)
save(asset_data,
     file = file.path(
       get_data_path()$data$intermediate,
       "cleaned_data",
       "asset_data.rdata"
     )
)

rm(asset_data)



## 5. Clean & save Application data -------------------------------------------------------

## 5.1 load data
application_data <- fread_raw("ApplicationDataCombined.txt")


## 5.2 change date column formats
application_data$InTime_cleaned <- as.Date(substr(application_data$InTime,1,10),
                                   format = "%Y-%m-%d",
                                   origin = "1970-01-01")


application_data$App_Maker_Date_cleaned <- as.Date(substr(application_data$App_Maker_Date,1,10),
                                        format = "%Y-%m-%d",
                                        origin = "1970-01-01")

application_data$application_date <- as.Date(substr(application_data$app_syntime,1,10),
                                        format = "%Y-%m-%d",
                                        origin = "1970-01-01")


application_data$DOB_cleaned <- as.Date(substr(application_data$DOB,1,10),
                                        format = "%Y-%m-%d",
                                        origin = "1970-01-01")


application_data$FI_Residence_Time <- as.Date(substr(application_data$Resi_Verify_Time,1,10),
                                        format = "%Y-%m-%d",
                                        origin = "1970-01-01")


application_data$FI_Residence_Time_syntime <- as.Date(substr(application_data$Resi_Verify_syntime,1,10),
                                              format = "%Y-%m-%d",
                                              origin = "1970-01-01")



application_data$FI_Office_Time <- as.Date(substr(application_data$Off_Verify_Time,1,10),
                                              format = "%Y-%m-%d",
                                              origin = "1970-01-01")


application_data$FI_Office_Time_syntime <- as.Date(substr(application_data$Off_Verify_syntime,1,10),
                                                      format = "%Y-%m-%d",
                                                      origin = "1970-01-01")




application_data$customer_type_cleaned <- ifelse(application_data$Customer_Type %in% c('individual','Individual','INDIVIDUAL'), 'individual',
                                                ifelse(application_data$Customer_Type %in% c('Non Individual','Non INDIVIDUAL','nonIndividual'), 'non-individual',
                                                       NA))


application_data$fi_resi_check <- ifelse(is.na(application_data$FI_Resi_Pincode) | nchar(application_data$FI_Resi_Pincode) < 6, 0, 1)
application_data$fi_resi_perm_check <- ifelse(is.na(application_data$FI_Resi_Perm_Pincode) | nchar(application_data$FI_Resi_Perm_Pincode) < 6, 0, 1)
application_data$fi_office_check <- ifelse(is.na(application_data$FI_Off_Pincode) | nchar(application_data$FI_Off_Pincode) < 6, 0, 1)


application_data$nri_flag <- ifelse(application_data$IsCustomer_NRI %in% c('Yes','No'), application_data$IsCustomer_NRI, NA)
application_data$profile_primary_applicant <- ifelse(application_data$Category %in% c('NE','PEN','SAL','SENP','SEP'),
                                                     application_data$Category, NA)


evaluation_type_mapping <- fread_mapping("evaluation_type_primary_applicant_mapping.csv")

application_data <- left_join(application_data,evaluation_type_mapping,by = "ci_evaluation_type")

rm(evaluation_type_mapping)

# assert_data_non_empty(application_data)
# save(application_data,
#      file = file.path(
#              get_data_path()$data$intermediate,
#              "cleaned_data",
#              "application_data.rdata"
#      )
# )



req_cols <- c('AppNo','LeadNo','Curr_Addr_Is_Perm_Addr','Resi_Cum_Off','Cibil_Score',
              'InTime_cleaned','App_Maker_Date_cleaned','application_date','DOB_cleaned',
              'customer_type_cleaned','FI_Residence_Time','FI_Residence_Time_syntime',
              'FI_Office_Time','FI_Office_Time_syntime','fi_resi_check','fi_resi_perm_check',
              'fi_office_check','nri_flag','profile_primary_applicant','evaluation_type_primary_applicant'
)

application_data_subset <- application_data %>% dplyr::select(req_cols)

## 4.2 save data
assert_data_non_empty(application_data_subset)
save(application_data_subset,
     file = file.path(
       get_data_path()$data$intermediate,
       "cleaned_data",
       "application_data_subset.rdata"
     )
)

rm(application_data,application_data_subset)




## 6. Clean & save ADE data -------------------------------------------------------

ade_data <- data.frame(fread_raw("ADE_cleaned.txt"))

ade_data$ADE_Dedupe_Customer_Type <- ifelse(ade_data$ADE_Dedupe_Customer_Type %in% c('E','N'), ade_data$ADE_Dedupe_Customer_Type, NA)

assert_data_non_empty(ade_data)
save(ade_data,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "ade_data.rdata"
     )
)

rm(ade_data)



## 7. Clean & save approval data -----------------------------------------------------------

approval_data <- data.frame(fread_raw("approval_data_cleaned.txt"))
approval_data$approval_date_cleaned <- as.Date(substr(approval_data$Approval_Date,1,10),
                                               format = "%Y-%m-%d",
                                               origin = "1970-01-01")


approval_data$approval_timestamp <- strptime(substr(approval_data$Approval_Date,1,19),"%Y-%m-%d %H:%M:%S")

approval_data_max <- approval_data %>% group_by(Application_No) %>% summarise(max_approval = max(approval_timestamp, na.rm = T))

approval_wo_max <- left_join(approval_data,approval_data_max, by = c("Application_No" = "Application_No",
                                                                     "approval_timestamp" = "max_approval"))



approval_date_final <- approval_data %>% group_by(Application_No) %>% summarise(approval_date_cleaned = max(approval_date_cleaned, na.rm = T))

assert_data_non_empty(approval_data)
assert_data_non_empty(approval_date_final)

save(approval_data,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "approval_data.rdata"
     )
)

save(approval_date_final,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "approval_date_final.rdata"
     )
)



## 8. clean co-borrower data ---------------------------------------------------------------

coborrower_data_PV <- fread_raw("coborrower_data_PV.txt")
coborrower_data_2W <- fread_raw("coborrower_data_2W.txt")


clean_coborrower_data <- function(coborrower_data){
        
        coborrower_data$InTime_cleaned <- as.Date(substr(coborrower_data$InTime,1,10),
                                                  format = "%Y-%m-%d",
                                                  origin = "1970-01-01")
        
        
        coborrower_data$App_Maker_Date_cleaned <- as.Date(substr(coborrower_data$App_Maker_Date,1,10),
                                                          format = "%Y-%m-%d",
                                                          origin = "1970-01-01")
        
        coborrower_data$application_date <- as.Date(substr(coborrower_data$app_syntime,1,10),
                                                    format = "%Y-%m-%d",
                                                    origin = "1970-01-01")
        
        
        coborrower_data$DOB_cleaned <- as.Date(substr(coborrower_data$DOB,1,10),
                                               format = "%Y-%m-%d",
                                               origin = "1970-01-01")
        
        
        coborrower_data$FI_Residence_Time <- as.Date(substr(coborrower_data$Resi_Verify_Time,1,10),
                                                     format = "%Y-%m-%d",
                                                     origin = "1970-01-01")
        
        
        coborrower_data$FI_Residence_Time_syntime <- as.Date(substr(coborrower_data$Resi_Verify_syntime,1,10),
                                                             format = "%Y-%m-%d",
                                                             origin = "1970-01-01")
        
        
        
        coborrower_data$FI_Office_Time <- as.Date(substr(coborrower_data$Off_Verify_Time,1,10),
                                                  format = "%Y-%m-%d",
                                                  origin = "1970-01-01")
        
        
        coborrower_data$FI_Office_Time_syntime <- as.Date(substr(coborrower_data$Off_Verify_syntime,1,10),
                                                          format = "%Y-%m-%d",
                                                          origin = "1970-01-01")
        
        
        
        
        coborrower_data$customer_type_cleaned <- ifelse(coborrower_data$Customer_Type %in% c('individual','Individual','INDIVIDUAL'), 'individual',
                                                        ifelse(coborrower_data$Customer_Type %in% c('Non Individual','Non INDIVIDUAL','nonIndividual'), 'non-individual',
                                                               NA))
        
        
        coborrower_data$fi_resi_check <- ifelse(is.na(coborrower_data$FI_Resi_Pincode) | nchar(coborrower_data$FI_Resi_Pincode) < 6, 0, 1)
        coborrower_data$fi_resi_perm_check <- ifelse(is.na(coborrower_data$FI_Resi_Perm_Pincode) | nchar(coborrower_data$FI_Resi_Perm_Pincode) < 6, 0, 1)
        coborrower_data$fi_office_check <- ifelse(is.na(coborrower_data$FI_Off_Pincode) | nchar(coborrower_data$FI_Off_Pincode) < 6, 0, 1)
        
        
        coborrower_data$nri_flag <- ifelse(coborrower_data$IsCustomer_NRI %in% c('Yes','No'), coborrower_data$IsCustomer_NRI, NA)
        coborrower_data$profile <- ifelse(coborrower_data$Category %in% c('NE','PEN','SAL','SENP','SEP'),
                                          coborrower_data$Category, NA)
        
        evaluation_type_mapping <- fread_mapping("evaluation_type_primary_applicant_mapping.csv")
        
        coborrower_data <- left_join(coborrower_data,evaluation_type_mapping,by = "ci_evaluation_type")
        
        coborrower_data$evaluation_type <- coborrower_data$evaluation_type_primary_applicant
        coborrower_data$evaluation_type_primary_applicant <- NULL
        
        return(coborrower_data)
        
}


cleaned_coborrower_2W <- clean_coborrower_data(coborrower_data = coborrower_data_2W)
cleaned_coborrower_PV <- clean_coborrower_data(coborrower_data = coborrower_data_PV)



# assert_data_non_empty(coborrower_data)
# save(coborrower_data,
#      file = file.path(
#              get_data_path()$data$intermediate,
#              "cleaned_data",
#              "coborrower_data.rdata"
#      )
# )


# load_rdata_intermediate("cleaned_data//coborrower_data.rdata")

cleaned_coborrower_PV$primary_appno <- substr(cleaned_coborrower_PV$AppNo,1,nchar(cleaned_coborrower_PV$AppNo) - 3)
cleaned_coborrower_2W$primary_appno <- substr(cleaned_coborrower_2W$AppNo,1,nchar(cleaned_coborrower_2W$AppNo) - 2)

# fwrite(cleaned_coborrower_PV,"./data/coborrower_data_PV.csv")
# fwrite(cleaned_coborrower_2W,"./data/coborrower_data_2W.csv")




req_cols <- c('primary_appno','AppNo','LeadNo','Curr_Addr_Is_Perm_Addr','Resi_Cum_Off','Cibil_Score',
              'InTime_cleaned','App_Maker_Date_cleaned','application_date','DOB_cleaned',
              'customer_type_cleaned','FI_Residence_Time','FI_Residence_Time_syntime',
              'FI_Office_Time','FI_Office_Time_syntime','fi_resi_check','fi_resi_perm_check',
              'fi_office_check','nri_flag','profile','evaluation_type'
)

coborrower_PV <- cleaned_coborrower_PV %>% dplyr::select(req_cols)
coborrower_2W <- cleaned_coborrower_2W %>% dplyr::select(req_cols)

## 4.2 save data
assert_data_non_empty(coborrower_PV)
save(coborrower_PV,
     file = file.path(
        get_data_path()$data$intermediate,
        "cleaned_data",
        "coborrower_PV.rdata"
     )
)

assert_data_non_empty(coborrower_2W)
save(coborrower_2W,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "coborrower_2W.rdata"
     )
)



rm(coborrower_2W,coborrower_PV,cleaned_coborrower_PV,cleaned_coborrower_2W,coborrower_data_2W,coborrower_data_PV)






##################################################################################################################################
## 9. Clean CIBIL Bureau tradeline data (at origin) ------------------------------------------------------------------------------

as_on_tradeline1 <- data.frame(fread_raw("CIBIL_Data_18032021.txt"))
as_on_tradeline2 <- data.frame(fread_raw("CIBIL_25032021.txt"))

reject_as_on_tradeline1 <- data.frame(fread_raw("CIBIL Data 10-05-2021.txt"))
reject_as_on_tradeline2 <- data.frame(fread_raw("CIBIL Loan Track Data - 13-05-2021.txt"))

reject_as_on_tradeline <- data.frame(rbindlist(l=list(reject_as_on_tradeline1,reject_as_on_tradeline2), use.names = T, fill = T))
reject_as_on_tradeline$Deal_no <- NULL

rm(reject_as_on_tradeline1,reject_as_on_tradeline2)        

as_on_tradeline <- data.frame(rbindlist(l=list(as_on_tradeline1,as_on_tradeline2)))
rm(as_on_tradeline1,as_on_tradeline2)

clean_as_on_tradeline_data <- function(as_on_tradeline){
        
        as_on_tradeline$payment_history_start_date <- as.character(as_on_tradeline$Payment_History_EndDate)
        as_on_tradeline$payment_history_start_date <- as.Date(paste0(str_sub(as_on_tradeline$payment_history_start_date,-4,-1),
                                                                     "-",
                                                                     str_sub(as_on_tradeline$payment_history_start_date,-6,-5),
                                                                     "-",
                                                                     "01"))
        
        as_on_tradeline$payment_history_end_date <- as.character(as_on_tradeline$Payment_History_StartDate)
        as_on_tradeline$payment_history_end_date <- as.Date(paste0(str_sub(as_on_tradeline$payment_history_end_date,-4,-1),
                                                                   "-",
                                                                   str_sub(as_on_tradeline$payment_history_end_date,-6,-5),
                                                                   "-",
                                                                   "01"))
        
        as_on_tradeline$account_date_opened <- as.character(as_on_tradeline$DateOpened_Or_Disbursed)
        as_on_tradeline$account_date_opened <- as.Date(paste0(str_sub(as_on_tradeline$account_date_opened,-4,-1),
                                                              "-",
                                                              str_sub(as_on_tradeline$account_date_opened,-6,-5),
                                                              "-",
                                                              str_sub(as_on_tradeline$account_date_opened,-8,-7)))
        
        
        as_on_tradeline$account_date_closed <- as.character(as_on_tradeline$Date_Closed)
        as_on_tradeline$account_date_closed <- ifelse(is.na(as_on_tradeline$account_date_closed), NA,
                                                      as.character(paste0(str_sub(as_on_tradeline$account_date_closed,-4,-1),
                                                              "-",
                                                              str_sub(as_on_tradeline$account_date_closed,-6,-5),
                                                              "-",
                                                              str_sub(as_on_tradeline$account_date_closed,-8,-7))))
        as_on_tradeline$account_date_closed <- as.Date(as_on_tradeline$account_date_closed)
        
        as_on_tradeline$cibil_reported_date <- as.character(as_on_tradeline$CIBIL_Generated_Date)
        as_on_tradeline$cibil_reported_date <- as.Date(substr(as_on_tradeline$cibil_reported_date,1,10),
                                                       format = "%Y-%m-%d",
                                                       origin = "1970-01-01")
        
        
        as_on_tradeline$payment_history <- paste0(as_on_tradeline$Payment_History1,as_on_tradeline$Payment_History2)
        
        return(as_on_tradeline)
        
}


retro_as_on_data <- clean_as_on_tradeline_data(as_on_tradeline = as_on_tradeline)
retro_as_on_data_reject <- clean_as_on_tradeline_data(as_on_tradeline = reject_as_on_tradeline)



retro_as_on_data <- retro_as_on_data %>% dplyr::select(Deal_no,
                                                      Customer_Code,
                                                      HighCredit_SanctionedAmount,
                                                      Current_Balance,
                                                      Amount_Overdue,
                                                      Account_Type,
                                                      payment_history_start_date,
                                                      payment_history_end_date,
                                                      account_date_opened,
                                                      account_date_closed,
                                                      cibil_reported_date,
                                                      payment_history
                                                      )

retro_as_on_data_reject <- retro_as_on_data_reject %>% dplyr::select(Application_No,
                                                      Customer_Code,
                                                      HighCredit_SanctionedAmount,
                                                      Current_Balance,
                                                      Amount_Overdue,
                                                      Account_Type,
                                                      payment_history_start_date,
                                                      payment_history_end_date,
                                                      account_date_opened,
                                                      account_date_closed,
                                                      cibil_reported_date,
                                                      payment_history
)


retro_as_on_data <- distinct(retro_as_on_data)
retro_as_on_data_reject <- distinct(retro_as_on_data_reject)

colnames(retro_as_on_data) <- c('deal_no',
                                'customer_code',
                                'high_credit_sanctioned_amount',
                                'current_balance',
                                'overdue_amount',
                                'account_type',
                                'payment_history_start_date',
                                'payment_history_end_date',
                                'account_date_opened',
                                'account_date_closed',
                                'cibil_reported_date',
                                'payment_history')




load_rdata_intermediate("cleaned_data//application_data_subset.rdata")
load_rdata_intermediate("cleaned_data//lead_data.rdata")

application_data_subset <- distinct(application_data_subset %>% dplyr::select(AppNo, LeadNo,application_date))
lead_data <- distinct(lead_data %>% dplyr::select(LeadNo, Product, Vehicle_Type))

app_data <- inner_join(application_data_subset, lead_data, by = c("LeadNo" = "LeadNo"))     
app_data$LeadNo <- NULL

rm(application_data_subset,lead_data)



retro_as_on_data_reject <- inner_join(retro_as_on_data_reject,app_data,by = c("Application_No" = "AppNo"))
rm(app_data)



colnames(retro_as_on_data_reject) <- c('application_no',
                                'customer_code',
                                'high_credit_sanctioned_amount',
                                'current_balance',
                                'overdue_amount',
                                'account_type',
                                'payment_history_start_date',
                                'payment_history_end_date',
                                'account_date_opened',
                                'account_date_closed',
                                'cibil_reported_date',
                                'payment_history',
                                'application_date',
                                'product',
                                'type')


## 4.2 save data
assert_data_non_empty(retro_as_on_data)
save(retro_as_on_data,
     file = file.path(
        get_data_path()$data$intermediate,
        "cleaned_data",
        "retro_as_on_data.rdata"
     )
)

assert_data_non_empty(retro_as_on_data_reject)
save(retro_as_on_data_reject,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "retro_as_on_data_reject.rdata"
     )
)

# load_rdata_intermediate("cleaned_data//retro_as_on_data.rdata")

rm(as_on_tradeline,reject_as_on_tradeline,retro_as_on_data,retro_as_on_data_reject)






#################################################################################################################
## 10. Accepts - Cibil enquiry data -------------------------------------------------------------------

enquiry_data <- fread_raw('CIBIL Deal Enquiry.txt')

enquiry_data$enquiry_date <- as.Date(paste0(str_sub(enquiry_data$Date_Of_Enquiry,-4,-1),
                                            "-",
                                            str_sub(enquiry_data$Date_Of_Enquiry,-6,-5),
                                            "-",
                                            str_sub(enquiry_data$Date_Of_Enquiry,-8,-7)))


colnames(enquiry_data)[colnames(enquiry_data) == 'Deal_No'] <- 'deal_no'



assert_data_non_empty(enquiry_data)
save(enquiry_data,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "enquiry_data.rdata"
     )
)






#################################################################################################################
## 11. Rejects - Cibil enquiry data -------------------------------------------------------------------

reject_enquiry1 <- fread_raw("CIBIL - Reject application - 13-05-2021.txt")
reject_enquiry2 <- fread_raw("CIBIL Enquiry - 13-05-2021.txt")

reject_enquiry <- data.frame(rbindlist(l=list(reject_enquiry1,reject_enquiry2)))

rm(reject_enquiry1,reject_enquiry2)

reject_enquiry$enquiry_date <- as.Date(paste0(str_sub(reject_enquiry$Date_Of_Enquiry,-4,-1),
                                            "-",
                                            str_sub(reject_enquiry$Date_Of_Enquiry,-6,-5),
                                            "-",
                                            str_sub(reject_enquiry$Date_Of_Enquiry,-8,-7)))



assert_data_non_empty(reject_enquiry)
save(reject_enquiry,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "reject_enquiry.rdata"
     )
)





#################################################################################################################
## 12. Primary applicant code for disbursals -------------------------------------------------------------------

primary_app_customer_code <- fread_raw("Customer_Data.txt")

assert_data_non_empty(primary_app_customer_code)
save(primary_app_customer_code,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "primary_app_customer_code.rdata"
     )
)





#################################################################################################################
## 13. PV Used - Retro CIBIL bureau at origin -------------------------------------------------------------------

retro_as_on_data_pv_used <- data.frame(fread_raw("INDUSINDREVTL.txt"))

colnames(retro_as_on_data_pv_used) <-  c('cust_id',
                           'customer_code',
                           'disbursal_date',
                           
                           'serial_no',
                           'cibil_reported_date',
                           'owner_indication',
                           
                           'current_balance',
                           'account_date_closed',
                           'member_name',
                           
                           "account_number",
                           "account_type",
                           "account_date_opened",
                           
                           "high_credit_sanctioned_amount",
                           'payment_history_start_date',
                           'payment_history_end_date',      
                           
                           "suit_filed",
                           "wo_settled",
                           "overdue_amount", 
                           
                           "payment_history_01",
                           "payment_history_02",
                           "payment_history_03",
                           "payment_history_04",
                           "payment_history_05",
                           "payment_history_06",
                           "payment_history_07",
                           "payment_history_08",
                           "payment_history_09",
                           "payment_history_10",
                           "payment_history_11",
                           "payment_history_12",
                           "payment_history_13",
                           "payment_history_14",
                           "payment_history_15",
                           "payment_history_16",
                           "payment_history_17",
                           "payment_history_18",
                           "payment_history_19",
                           "payment_history_20",
                           "payment_history_21",
                           "payment_history_22",
                           "payment_history_23",
                           "payment_history_24",
                           "payment_history_25",
                           "payment_history_26",
                           "payment_history_27",
                           "payment_history_28",
                           "payment_history_29",
                           "payment_history_30",
                           "payment_history_31",
                           "payment_history_32",
                           "payment_history_33",
                           "payment_history_34",
                           "payment_history_35",
                           "payment_history_36",
                           "CIBIL_SCORE" )




## select requireed columns
retro_as_on_data_pv_used$cust_id <- NULL
retro_as_on_data_pv_used$serial_no <- NULL
retro_as_on_data_pv_used$owner_indication <- NULL
retro_as_on_data_pv_used$member_name <- NULL
retro_as_on_data_pv_used$account_number <- NULL



## format date columns
retro_as_on_data_pv_used$disbursal_date <- as.Date(substr(retro_as_on_data_pv_used$disbursal_date,1,10),
                                               format = "%Y-%m-%d",
                                               origin = "1970-01-01")

retro_as_on_data_pv_used$cibil_reported_date <- as.Date(substr(retro_as_on_data_pv_used$cibil_reported_date,1,10),
                                                   format = "%Y-%m-%d",
                                                   origin = "1970-01-01")


retro_as_on_data_pv_used$account_date_closed <- ifelse(retro_as_on_data_pv_used$account_date_closed == '1900-01-01', NA, retro_as_on_data_pv_used$account_date_closed)
retro_as_on_data_pv_used$account_date_closed <- as.Date(substr(retro_as_on_data_pv_used$account_date_closed,1,10),
                                                        format = "%Y-%m-%d",
                                                        origin = "1970-01-01")

retro_as_on_data_pv_used$account_date_opened <- as.Date(substr(retro_as_on_data_pv_used$account_date_opened,1,10),
                                                        format = "%Y-%m-%d",
                                                        origin = "1970-01-01")


retro_as_on_data_pv_used$payment_history_start_date1 <- retro_as_on_data_pv_used$payment_history_end_date
retro_as_on_data_pv_used$payment_history_end_date1 <- retro_as_on_data_pv_used$payment_history_start_date


retro_as_on_data_pv_used$payment_history_start_date <- as.Date(substr(retro_as_on_data_pv_used$payment_history_start_date1,1,10),
                                                        format = "%Y-%m-%d",
                                                        origin = "1970-01-01")


retro_as_on_data_pv_used$payment_history_end_date <- as.Date(substr(retro_as_on_data_pv_used$payment_history_end_date1,1,10),
                                                               format = "%Y-%m-%d",
                                                               origin = "1970-01-01")


retro_as_on_data_pv_used$payment_history_start_date1 <- NULL
retro_as_on_data_pv_used$payment_history_end_date1 <- NULL




## format payment columns
payment_cols <- c(
        "payment_history_01",
        "payment_history_02",
        "payment_history_03",
        "payment_history_04",
        "payment_history_05",
        "payment_history_06",
        "payment_history_07",
        "payment_history_08",
        "payment_history_09",
        "payment_history_10",
        "payment_history_11",
        "payment_history_12",
        "payment_history_13",
        "payment_history_14",
        "payment_history_15",
        "payment_history_16",
        "payment_history_17",
        "payment_history_18",
        "payment_history_19",
        "payment_history_20",
        "payment_history_21",
        "payment_history_22",
        "payment_history_23",
        "payment_history_24",
        "payment_history_25",
        "payment_history_26",
        "payment_history_27",
        "payment_history_28",
        "payment_history_29",
        "payment_history_30",
        "payment_history_31",
        "payment_history_32",
        "payment_history_33",
        "payment_history_34",
        "payment_history_35",
        "payment_history_36"
)



for (col in payment_cols) {
        retro_as_on_data_pv_used[, col] <- ifelse(retro_as_on_data_pv_used[, col] %in% c(-1, -2), NA, retro_as_on_data_pv_used[, col])
        retro_as_on_data_pv_used[, col] <- str_pad(retro_as_on_data_pv_used[,col], width = 3, pad = "0", side = "left")
        retro_as_on_data_pv_used[, col] <- ifelse(is.na(retro_as_on_data_pv_used[, col]), "XXX", retro_as_on_data_pv_used[, col])

}


## create payment string columns
retro_as_on_data_pv_used$payment_history <- paste0(retro_as_on_data_pv_used$payment_history_01,
                                                   retro_as_on_data_pv_used$payment_history_02,
                                                   retro_as_on_data_pv_used$payment_history_03,
                                                   retro_as_on_data_pv_used$payment_history_04,
                                                   retro_as_on_data_pv_used$payment_history_05,
                                                   retro_as_on_data_pv_used$payment_history_06,
                                                   retro_as_on_data_pv_used$payment_history_07,
                                                   retro_as_on_data_pv_used$payment_history_08,
                                                   retro_as_on_data_pv_used$payment_history_09,
                                                   retro_as_on_data_pv_used$payment_history_10,
                                                   retro_as_on_data_pv_used$payment_history_11,
                                                   retro_as_on_data_pv_used$payment_history_12,
                                                   retro_as_on_data_pv_used$payment_history_13,
                                                   retro_as_on_data_pv_used$payment_history_14,
                                                   retro_as_on_data_pv_used$payment_history_15,
                                                   retro_as_on_data_pv_used$payment_history_16,
                                                   retro_as_on_data_pv_used$payment_history_17,
                                                   retro_as_on_data_pv_used$payment_history_18,
                                                   retro_as_on_data_pv_used$payment_history_19,
                                                   retro_as_on_data_pv_used$payment_history_20,
                                                   retro_as_on_data_pv_used$payment_history_21,
                                                   retro_as_on_data_pv_used$payment_history_22,
                                                   retro_as_on_data_pv_used$payment_history_23,
                                                   retro_as_on_data_pv_used$payment_history_24,
                                                   retro_as_on_data_pv_used$payment_history_25,
                                                   retro_as_on_data_pv_used$payment_history_26,
                                                   retro_as_on_data_pv_used$payment_history_27,
                                                   retro_as_on_data_pv_used$payment_history_28,
                                                   retro_as_on_data_pv_used$payment_history_29,
                                                   retro_as_on_data_pv_used$payment_history_30,
                                                   retro_as_on_data_pv_used$payment_history_31,
                                                   retro_as_on_data_pv_used$payment_history_32,
                                                   retro_as_on_data_pv_used$payment_history_33,
                                                   retro_as_on_data_pv_used$payment_history_34,
                                                   retro_as_on_data_pv_used$payment_history_35,
                                                   retro_as_on_data_pv_used$payment_history_36
                                                   )




## subset for required columns
retro_as_on_data_pv_used <-
        retro_as_on_data_pv_used %>% dplyr::select(
                customer_code,
                high_credit_sanctioned_amount,
                current_balance,
                overdue_amount,
                account_type,
                payment_history_start_date,
                payment_history_end_date,
                account_date_opened,
                account_date_closed,
                cibil_reported_date,
                payment_history
        )



## get deal numbers
deal_numbers <- fread_raw("PV_CUSTOMER_CODE_INFO.csv")
deal_numbers <- distinct(deal_numbers %>% dplyr::select(CUSTOMER_CODE,DEAL_NO))
colnames(deal_numbers) <- c('customer_code','deal_no')

retro_as_on_data_pv_used <- inner_join(retro_as_on_data_pv_used,deal_numbers,by = c('customer_code' = 'customer_code'))


### change columns to numeric
retro_as_on_data_pv_used$high_credit_sanctioned_amount <- as.numeric(retro_as_on_data_pv_used$high_credit_sanctioned_amount)
retro_as_on_data_pv_used$current_balance <- as.numeric(retro_as_on_data_pv_used$current_balance)
retro_as_on_data_pv_used$overdue_amount <- as.numeric(retro_as_on_data_pv_used$overdue_amount)
retro_as_on_data_pv_used$account_type <- as.numeric(retro_as_on_data_pv_used$account_type)




## get disbursal date
load_rdata_intermediate("cleaned_data//disbursal_data.rdata")

disbursal_data <- distinct(disbursal_data %>% dplyr::select(deal_no,disbursal_date))

retro_as_on_data_pv_used <- left_join(retro_as_on_data_pv_used,disbursal_data,by='deal_no')



## filter cases where cibil report date is beyond disbursal date
retro_as_on_data_pv_used <- retro_as_on_data_pv_used %>% filter(cibil_reported_date <= disbursal_date)

retro_as_on_data_pv_used$cibil_reported_date <- retro_as_on_data_pv_used$disbursal_date
retro_as_on_data_pv_used$disbursal_date <- NULL


assert_data_non_empty(retro_as_on_data_pv_used)
save(retro_as_on_data_pv_used,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "retro_as_on_data_pv_used.rdata"
     )
)


rm(retro_as_on_data_pv_used)






#################################################################################################################
## 14. PV Used - Enquiry CIBIL at origin -------------------------------------------------------------------

enquiry_data_pv_used <- data.frame(fread_raw("INDUSINDREVEQ.txt"))

colnames(enquiry_data_pv_used) <- c('cust_id',
                           'Customer_Code',
                           'serial_no',
                           'disbursal_date',
                           'member_name',
                           'ecn',
                           'enq_date',
                           'Enquiry_Purpose',
                           'Enquiry_Amount'
                            )


enquiry_data_pv_used$enquiry_date <- as.Date(substr(enquiry_data_pv_used$enq_date,1,10),
                                                        format = "%Y-%m-%d",
                                                        origin = "1970-01-01")




enquiry_data_pv_used <- inner_join(enquiry_data_pv_used,deal_numbers,by = c('Customer_Code' = 'customer_code'))

enquiry_data_pv_used <- enquiry_data_pv_used %>% dplyr::select(deal_no,Customer_Code,Enquiry_Purpose,Enquiry_Amount,enquiry_date)

enquiry_data_pv_used$Enquiry_Purpose <- as.numeric(enquiry_data_pv_used$Enquiry_Purpose)
enquiry_data_pv_used$Enquiry_Amount <- as.numeric(enquiry_data_pv_used$Enquiry_Amount)

assert_data_non_empty(enquiry_data_pv_used)
save(enquiry_data_pv_used,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "enquiry_data_pv_used.rdata"
     )
)

rm(list=ls())


#################################################################################################################
## 15. NTC data from CIBIL ---------------------------------------------------------------------------------

ntc_data <- data.frame(fread_raw("indusind_output.csv"))

assert_data_non_empty(ntc_data)
save(ntc_data,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "ntc_data.rdata"
     )
)





#################################################################################################################
## 16. CIBIL bureau scrub as of DEC 2020  ------------------------------------------------------------------

reject_tradelines <- data.frame(fread_raw("BP03115220_28042021_IBL_PV_Reject_Data_PR_Scrub_Indusind_4L_Rev_29-04-2021-01-16-25_DS_TEMPLATE_25_BKP_CIR.csv"))

load_rdata_intermediate("cleaned_data//application_data_subset.rdata")
load_rdata_intermediate("cleaned_data//lead_data.rdata")

application_data_subset <- distinct(application_data_subset %>% dplyr::select(AppNo, LeadNo,application_date))
lead_data <- distinct(lead_data %>% dplyr::select(LeadNo, Product, Vehicle_Type))

app_data <- inner_join(application_data_subset, lead_data, by = c("LeadNo" = "LeadNo"))     
app_data$LeadNo <- NULL

rm(application_data_subset,lead_data)



reject_tradelines <- inner_join(reject_tradelines,app_data,by = c("MemberReference" = "AppNo"))
rm(app_data)

req_cols <- c('MemberReference',
              'application_date',
              'Product',
              'Vehicle_Type',
              'DateOpenedDisbursed',
              'DateofLastPayment',
              'DateClosed',
              'DateReported_trades',
              'HighCreditSanctionedAmount',
              'CurrentBalance',
              'AmountOverdue',
              'Pay_Hist_Start_Date',
              'pay_hist_end_date',
              'suit_filed_status',
              'writeoff_status',
              'AccountType',
              'Paymt_hst_01',
              'Paymt_hst_02',
              'Paymt_hst_03',
              'Paymt_hst_04',
              'Paymt_hst_05',
              'Paymt_hst_06',
              'Paymt_hst_07',
              'Paymt_hst_08',
              'Paymt_hst_09',
              'Paymt_hst_10',
              'Paymt_hst_11',
              'Paymt_hst_12',
              'Paymt_hst_13',
              'Paymt_hst_14',
              'Paymt_hst_15',
              'Paymt_hst_16',
              'Paymt_hst_17',
              'Paymt_hst_18',
              'Paymt_hst_19',
              'Paymt_hst_20',
              'Paymt_hst_21',
              'Paymt_hst_22',
              'Paymt_hst_23',
              'Paymt_hst_24',
              'Paymt_hst_25',
              'Paymt_hst_26',
              'Paymt_hst_27',
              'Paymt_hst_28',
              'Paymt_hst_29',
              'Paymt_hst_30',
              'Paymt_hst_31',
              'Paymt_hst_32',
              'Paymt_hst_33',
              'Paymt_hst_34',
              'Paymt_hst_35',
              'Paymt_hst_36'
              )

reject_app_tradelines <- reject_tradelines %>% dplyr::select(req_cols)


payment_cols <- c('Paymt_hst_01',
                  'Paymt_hst_02',
                  'Paymt_hst_03',
                  'Paymt_hst_04',
                  'Paymt_hst_05',
                  'Paymt_hst_06',
                  'Paymt_hst_07',
                  'Paymt_hst_08',
                  'Paymt_hst_09',
                  'Paymt_hst_10',
                  'Paymt_hst_11',
                  'Paymt_hst_12',
                  'Paymt_hst_13',
                  'Paymt_hst_14',
                  'Paymt_hst_15',
                  'Paymt_hst_16',
                  'Paymt_hst_17',
                  'Paymt_hst_18',
                  'Paymt_hst_19',
                  'Paymt_hst_20',
                  'Paymt_hst_21',
                  'Paymt_hst_22',
                  'Paymt_hst_23',
                  'Paymt_hst_24',
                  'Paymt_hst_25',
                  'Paymt_hst_26',
                  'Paymt_hst_27',
                  'Paymt_hst_28',
                  'Paymt_hst_29',
                  'Paymt_hst_30',
                  'Paymt_hst_31',
                  'Paymt_hst_32',
                  'Paymt_hst_33',
                  'Paymt_hst_34',
                  'Paymt_hst_35',
                  'Paymt_hst_36')


for (col in payment_cols) {
        reject_app_tradelines[, col] <- ifelse(reject_app_tradelines[, col] == "", "XXX", reject_app_tradelines[, col])
}



## create payment string columns
reject_app_tradelines$payment_history <- paste0(
        reject_app_tradelines$Paymt_hst_01,
        reject_app_tradelines$Paymt_hst_02,
        reject_app_tradelines$Paymt_hst_03,
        reject_app_tradelines$Paymt_hst_04,
        reject_app_tradelines$Paymt_hst_05,
        reject_app_tradelines$Paymt_hst_06,
        reject_app_tradelines$Paymt_hst_07,
        reject_app_tradelines$Paymt_hst_08,
        reject_app_tradelines$Paymt_hst_09,
        reject_app_tradelines$Paymt_hst_10,
        reject_app_tradelines$Paymt_hst_11,
        reject_app_tradelines$Paymt_hst_12,
        reject_app_tradelines$Paymt_hst_13,
        reject_app_tradelines$Paymt_hst_14,
        reject_app_tradelines$Paymt_hst_15,
        reject_app_tradelines$Paymt_hst_16,
        reject_app_tradelines$Paymt_hst_17,
        reject_app_tradelines$Paymt_hst_18,
        reject_app_tradelines$Paymt_hst_19,
        reject_app_tradelines$Paymt_hst_20,
        reject_app_tradelines$Paymt_hst_21,
        reject_app_tradelines$Paymt_hst_22,
        reject_app_tradelines$Paymt_hst_23,
        reject_app_tradelines$Paymt_hst_24,
        reject_app_tradelines$Paymt_hst_25,
        reject_app_tradelines$Paymt_hst_26,
        reject_app_tradelines$Paymt_hst_27,
        reject_app_tradelines$Paymt_hst_28,
        reject_app_tradelines$Paymt_hst_29,
        reject_app_tradelines$Paymt_hst_30,
        reject_app_tradelines$Paymt_hst_31,
        reject_app_tradelines$Paymt_hst_32,
        reject_app_tradelines$Paymt_hst_33,
        reject_app_tradelines$Paymt_hst_34,
        reject_app_tradelines$Paymt_hst_35,
        reject_app_tradelines$Paymt_hst_36
)






## format date columns
reject_app_tradelines$account_date_opened <- as.character(reject_app_tradelines$DateOpenedDisbursed)
reject_app_tradelines$account_date_opened <- ifelse(is.na(reject_app_tradelines$account_date_opened), NA,
                                                    as.character(paste0(str_sub(reject_app_tradelines$account_date_opened,-4,-1),
                                                                        "-",
                                                                        str_sub(reject_app_tradelines$account_date_opened,-6,-5),
                                                                        "-",
                                                                        str_sub(reject_app_tradelines$account_date_opened,-8,-7))))
reject_app_tradelines$account_date_opened <- as.Date(reject_app_tradelines$account_date_opened)


reject_app_tradelines$account_date_closed <- as.character(reject_app_tradelines$DateClosed)
reject_app_tradelines$account_date_closed <- ifelse(is.na(reject_app_tradelines$account_date_closed), NA,
                                                    as.character(paste0(str_sub(reject_app_tradelines$account_date_closed,-4,-1),
                                                                        "-",
                                                                        str_sub(reject_app_tradelines$account_date_closed,-6,-5),
                                                                        "-",
                                                                        str_sub(reject_app_tradelines$account_date_closed,-8,-7))))
reject_app_tradelines$account_date_closed <- as.Date(reject_app_tradelines$account_date_closed)


reject_app_tradelines$cibil_reported_date <- as.character(reject_app_tradelines$DateReported_trades)
reject_app_tradelines$cibil_reported_date <- ifelse(is.na(reject_app_tradelines$cibil_reported_date), NA,
                                                    as.character(paste0(str_sub(reject_app_tradelines$cibil_reported_date,-4,-1),
                                                                        "-",
                                                                        str_sub(reject_app_tradelines$cibil_reported_date,-6,-5),
                                                                        "-",
                                                                        str_sub(reject_app_tradelines$cibil_reported_date,-8,-7))))
reject_app_tradelines$cibil_reported_date <- as.Date(reject_app_tradelines$cibil_reported_date)


reject_app_tradelines$payment_history_end_date <- as.character(reject_app_tradelines$Pay_Hist_Start_Date)
reject_app_tradelines$payment_history_end_date <- ifelse(is.na(reject_app_tradelines$payment_history_end_date), NA,
                                                         as.character(paste0(str_sub(reject_app_tradelines$payment_history_end_date,-4,-1),
                                                                             "-",
                                                                             str_sub(reject_app_tradelines$payment_history_end_date,-6,-5),
                                                                             "-",
                                                                             str_sub(reject_app_tradelines$payment_history_end_date,-8,-7))))
reject_app_tradelines$payment_history_end_date <- as.Date(reject_app_tradelines$payment_history_end_date)



reject_app_tradelines$payment_history_start_date <- as.character(reject_app_tradelines$pay_hist_end_date)
reject_app_tradelines$payment_history_start_date <- ifelse(is.na(reject_app_tradelines$payment_history_start_date), NA,
                                                           as.character(paste0(str_sub(reject_app_tradelines$payment_history_start_date,-4,-1),
                                                                               "-",
                                                                               str_sub(reject_app_tradelines$payment_history_start_date,-6,-5),
                                                                               "-",
                                                                               str_sub(reject_app_tradelines$payment_history_start_date,-8,-7))))
reject_app_tradelines$payment_history_start_date <- as.Date(reject_app_tradelines$payment_history_start_date)





req_cols <- c('MemberReference',
              'application_date',
              'Product',
              'Vehicle_Type',
              'account_date_opened',
              'account_date_closed',
              'cibil_reported_date',
              'HighCreditSanctionedAmount',
              'CurrentBalance',
              'AmountOverdue',
              'payment_history_start_date',
              'payment_history_end_date',
              'suit_filed_status',
              'writeoff_status',
              'AccountType',
              'payment_history'
)

reject_app_tradelines <- reject_app_tradelines %>% dplyr::select(req_cols)


colnames(reject_app_tradelines) <- c('application_no',
                                 'application_date',
                                 'product',
                                 'type',
                                 'account_date_opened',
                                 'account_date_closed',
                                 'cibil_reported_date',
                                 'high_credit_sanctioned_amount',
                                 'current_balance',
                                 'overdue_amount',
                                 'payment_history_start_date',
                                 'payment_history_end_date',
                                 'suit_filed',
                                 'wo_settled',
                                 'AccountType',
                                 'payment_history'
)

reject_app_tradelines$high_credit_sanctioned_amount <- as.numeric(reject_app_tradelines$high_credit_sanctioned_amount)
reject_app_tradelines$current_balance <- as.numeric(reject_app_tradelines$current_balance)
reject_app_tradelines$overdue_amount <- as.numeric(reject_app_tradelines$overdue_amount)
# reject_tradelines$account_type <- as.numeric(retro_as_on_data_pv_used$account_type)



assert_data_non_empty(reject_app_tradelines)
save(reject_app_tradelines,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "reject_app_tradelines.rdata"
     )
)


rm(reject_app_tradelines,reject_tradelines)







#################################################################################################################
## 17. Asset classification, BEN code, LTV data -----------------------------------------------------------------

asset_classification_data <- data.frame(fread_raw("BEN_code_asset_tag.txt"))
LTV_data <- data.frame(fread_raw("LTV_data.txt"))


assert_data_non_empty(asset_classification_data)
save(asset_classification_data,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "asset_classification_data.rdata"
     )
)

assert_data_non_empty(LTV_data)
save(LTV_data,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "LTV_data.rdata"
     )
)



##################################################################################################################################
## 18. Clean CIBIL Bureau tradeline data 2W Refinance ------------------------------------------------------------------------------

as_on_tradeline <- data.frame(fread_raw("TW_Refin_CIBIL_Loan_Track 13-05-2021.txt"))
#as_on_tradeline <- data.frame(fread("data//raw//TW_Refin_CIBIL_Loan_Track 13-05-2021.txt"))

clean_as_on_tradeline_data <- function(as_on_tradeline){
        
        as_on_tradeline$payment_history_start_date <- as.character(as_on_tradeline$Payment_History_EndDate)
        as_on_tradeline$payment_history_start_date <- as.Date(paste0(str_sub(as_on_tradeline$payment_history_start_date,-4,-1),
                                                                     "-",
                                                                     str_sub(as_on_tradeline$payment_history_start_date,-6,-5),
                                                                     "-",
                                                                     "01"))
        
        as_on_tradeline$payment_history_end_date <- as.character(as_on_tradeline$Payment_History_StartDate)
        as_on_tradeline$payment_history_end_date <- as.Date(paste0(str_sub(as_on_tradeline$payment_history_end_date,-4,-1),
                                                                   "-",
                                                                   str_sub(as_on_tradeline$payment_history_end_date,-6,-5),
                                                                   "-",
                                                                   "01"))
        
        as_on_tradeline$account_date_opened <- as.character(as_on_tradeline$DateOpened_Or_Disbursed)
        as_on_tradeline$account_date_opened <- as.Date(paste0(str_sub(as_on_tradeline$account_date_opened,-4,-1),
                                                              "-",
                                                              str_sub(as_on_tradeline$account_date_opened,-6,-5),
                                                              "-",
                                                              str_sub(as_on_tradeline$account_date_opened,-8,-7)))
        
        
        as_on_tradeline$account_date_closed <- as.character(as_on_tradeline$Date_Closed)
        as_on_tradeline$account_date_closed <- ifelse(is.na(as_on_tradeline$account_date_closed), NA,
                                                      as.character(paste0(str_sub(as_on_tradeline$account_date_closed,-4,-1),
                                                                          "-",
                                                                          str_sub(as_on_tradeline$account_date_closed,-6,-5),
                                                                          "-",
                                                                          str_sub(as_on_tradeline$account_date_closed,-8,-7))))
        as_on_tradeline$account_date_closed <- as.Date(as_on_tradeline$account_date_closed)
        
        as_on_tradeline$cibil_reported_date <- as.character(as_on_tradeline$CIBIL_Generated_Date)
        as_on_tradeline$cibil_reported_date <- as.Date(substr(as_on_tradeline$cibil_reported_date,1,10),
                                                       format = "%Y-%m-%d",
                                                       origin = "1970-01-01")
        
        
        as_on_tradeline$payment_history <- paste0(as_on_tradeline$Payment_History1,as_on_tradeline$Payment_History2)
        
        return(as_on_tradeline)
        
}


retro_as_on_data_2WR <- clean_as_on_tradeline_data(as_on_tradeline = as_on_tradeline)


retro_as_on_data_2WR <- retro_as_on_data_2WR %>% dplyr::select(Deal_No,
                                                               Appno,
                                                               HighCredit_Or_SanctionedAmount,
                                                       Current_Balance,
                                                       Amount_Overdue,
                                                       Account_Type,
                                                       payment_history_start_date,
                                                       payment_history_end_date,
                                                       account_date_opened,
                                                       account_date_closed,
                                                       cibil_reported_date,
                                                       payment_history
)




retro_as_on_data_2WR <- distinct(retro_as_on_data_2WR)

colnames(retro_as_on_data_2WR) <- c('deal_no',
                                'Appno',
                                'high_credit_sanctioned_amount',
                                'current_balance',
                                'overdue_amount',
                                'account_type',
                                'payment_history_start_date',
                                'payment_history_end_date',
                                'account_date_opened',
                                'account_date_closed',
                                'cibil_reported_date',
                                'payment_history')





## 18.2 save data
assert_data_non_empty(retro_as_on_data_2WR)
save(retro_as_on_data_2WR,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "retro_as_on_data_2W_RF.rdata"
     )
)

save(retro_as_on_data_2WR, file = file.path("data//intermediate//cleaned_data//retro_as_on_data_2W_RF.rdata"))

rm(as_on_tradeline)






#################################################################################################################
## 19. Cibil enquiry data 2W Refinance -------------------------------------------------------------------

enquiry_data_2W_RF <- fread_raw('TW_Refin_Enquiry 13-05-2021')
enquiry_data_2W_RF <- fread('data//raw//TW_Refin_Enquiry 13-05-2021.txt')


enquiry_data_2W_RF$enquiry_date <- as.Date(paste0(str_sub(enquiry_data_2W_RF$Date_Of_Enquiry,-4,-1),
                                            "-",
                                            str_sub(enquiry_data_2W_RF$Date_Of_Enquiry,-6,-5),
                                            "-",
                                            str_sub(enquiry_data_2W_RF$Date_Of_Enquiry,-8,-7)))


colnames(enquiry_data_2W_RF)[colnames(enquiry_data_2W_RF) == 'Deal_No'] <- 'deal_no'



assert_data_non_empty(enquiry_data_2W_RF)
save(enquiry_data_2W_RF,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "enquiry_data_2W_RF.rdata"
     )
)

#save(enquiry_data_2W_RF, file = file.path("data//intermediate//cleaned_data//enquiry_data_2W_RF.rdata"))

rm(list = ls())



#################################################################################################################
## 20. Cibil data Jan-Mar 21 ------------------------------------------------------------------------------------


as_on_tradeline <- data.frame(fread_raw("CIBIL LOAN TRACK JAN MAR 21.txt"))
#as_on_tradeline <- data.frame(fread("data//raw//TW_Refin_CIBIL_Loan_Track 13-05-2021.txt"))

clean_as_on_tradeline_data <- function(as_on_tradeline){
        
        as_on_tradeline$payment_history_start_date <- as.character(as_on_tradeline$Payment_History_EndDate)
        as_on_tradeline$payment_history_start_date <- as.Date(paste0(str_sub(as_on_tradeline$payment_history_start_date,-4,-1),
                                                                     "-",
                                                                     str_sub(as_on_tradeline$payment_history_start_date,-6,-5),
                                                                     "-",
                                                                     "01"))
        
        as_on_tradeline$payment_history_end_date <- as.character(as_on_tradeline$Payment_History_StartDate)
        as_on_tradeline$payment_history_end_date <- as.Date(paste0(str_sub(as_on_tradeline$payment_history_end_date,-4,-1),
                                                                   "-",
                                                                   str_sub(as_on_tradeline$payment_history_end_date,-6,-5),
                                                                   "-",
                                                                   "01"))
        
        as_on_tradeline$account_date_opened <- as.character(as_on_tradeline$DateOpened_Or_Disbursed)
        as_on_tradeline$account_date_opened <- as.Date(paste0(str_sub(as_on_tradeline$account_date_opened,-4,-1),
                                                              "-",
                                                              str_sub(as_on_tradeline$account_date_opened,-6,-5),
                                                              "-",
                                                              str_sub(as_on_tradeline$account_date_opened,-8,-7)))
        
        
        as_on_tradeline$account_date_closed <- as.character(as_on_tradeline$Date_Closed)
        as_on_tradeline$account_date_closed <- ifelse(is.na(as_on_tradeline$account_date_closed), NA,
                                                      as.character(paste0(str_sub(as_on_tradeline$account_date_closed,-4,-1),
                                                                          "-",
                                                                          str_sub(as_on_tradeline$account_date_closed,-6,-5),
                                                                          "-",
                                                                          str_sub(as_on_tradeline$account_date_closed,-8,-7))))
        as_on_tradeline$account_date_closed <- as.Date(as_on_tradeline$account_date_closed)
        
        as_on_tradeline$cibil_reported_date <- as.character(as_on_tradeline$CIBIL_Generated_Date)
        as_on_tradeline$cibil_reported_date <- as.Date(substr(as_on_tradeline$cibil_reported_date,1,10),
                                                       format = "%Y-%m-%d",
                                                       origin = "1970-01-01")
        
        
        as_on_tradeline$payment_history <- paste0(as_on_tradeline$Payment_History1,as_on_tradeline$Payment_History2)
        
        return(as_on_tradeline)
        
}


retro_as_on_data_2021 <- clean_as_on_tradeline_data(as_on_tradeline = as_on_tradeline)


retro_as_on_data_2021 <- retro_as_on_data_2021 %>% dplyr::select(Application_No,
                                                                 Customer_Code,
                                                                 Type,
                                                               HighCredit_Or_SanctionedAmount,
                                                               Current_Balance,
                                                               Amount_Overdue,
                                                               Account_Type,
                                                               payment_history_start_date,
                                                               payment_history_end_date,
                                                               account_date_opened,
                                                               account_date_closed,
                                                               cibil_reported_date,
                                                               payment_history
)




retro_as_on_data_2021 <- distinct(retro_as_on_data_2021)

colnames(retro_as_on_data_2021) <- c('application_no',
                                    'customer_code',
                                    'type',
                                    'high_credit_sanctioned_amount',
                                    'current_balance',
                                    'overdue_amount',
                                    'account_type',
                                    'payment_history_start_date',
                                    'payment_history_end_date',
                                    'account_date_opened',
                                    'account_date_closed',
                                    'cibil_reported_date',
                                    'payment_history')





## 18.2 save data
assert_data_non_empty(retro_as_on_data_2021)
save(retro_as_on_data_2021,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "retro_as_on_data_2021.rdata"
     )
)



rm(retro_as_on_data_2021,as_on_tradeline)



#################################################################################################################
## 20. Cibil data Jan-Mar 21 ------------------------------------------------------------------------------------

enquiry_data_2021 <- fread_raw('CIBIL ENQUIRY JAN MAR 21.txt')



enquiry_data_2021$enquiry_date <- as.Date(paste0(str_sub(enquiry_data_2021$Date_Of_Enquiry,-4,-1),
                                                  "-",
                                                  str_sub(enquiry_data_2021$Date_Of_Enquiry,-6,-5),
                                                  "-",
                                                  str_sub(enquiry_data_2021$Date_Of_Enquiry,-8,-7)))


enquiry_data_2021 <- distinct(enquiry_data_2021 %>% dplyr::select(Application_No,
                                                         Customer_Code,
                                                         Type,
                                                         Enquiry_Purpose,
                                                         Enquiry_Amount,
                                                         enquiry_date))


colnames(enquiry_data_2021) <- c('application_no',
                                 'customer_code',
                                 'type',
                                 'enquiry_purpose',
                                 'enquiry_amount',
                                 'enquiry_date')


assert_data_non_empty(enquiry_data_2021)
save(enquiry_data_2021,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "enquiry_data_2021.rdata"
     )
)

rm(enquiry_data_2021)



#################################################################################################################
## 22. cibil v3 score ------------------------------------------------------------------------------------

## load v3 scores for ECN
cibil_v3 <- fread_raw("cibil_v3_complete_ECN.txt")

## load disbursal ECN mapping
ecn_p1 <- fread_raw("ECN-revised.txt")
ecn_p2 <- fread_raw("Missing ECN.txt")
ecn_p3 <- fread_raw("Refinance ECN-31-05-2021.txt")

ecn_p1 <- ecn_p1 %>% dplyr::select(Deal_No,Customer_Code,EnquiryControlNumber,ApplicationId)
ecn_p2 <- ecn_p2 %>% dplyr::select(Deal_No,customer_code,EnquiryControlNumber,ApplicationId)
ecn_p3 <- ecn_p3 %>% dplyr::select(DEAL_NO,EnquiryControlNumber)

colnames(ecn_p1) <- c('deal_no','customer_code','ECN','appliation_id')
colnames(ecn_p2) <- c('deal_no','customer_code','ECN','appliation_id')
colnames(ecn_p3) <- c('deal_no','ECN')

ECN_mapping <- data.frame(rbindlist(l= list(ecn_p1,ecn_p2,ecn_p3), use.names = T, fill = T))
rm(ecn_p1,ecn_p2,ecn_p3)



cibil_v3$ECN <- as.character(cibil_v3$ECN)
ECN_mapping$ECN <- as.character(ECN_mapping$ECN)


## get v3 score for disbursals
ECN_mapping <- inner_join(ECN_mapping,cibil_v3,by = 'ECN')
cibil_v3 <- ECN_mapping %>% filter(deal_no != "")
rm(ECN_mapping)


## save v3 score for disbursals
assert_data_non_empty(cibil_v3)
save(cibil_v3,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "cibil_v3_disbursals.rdata"
     )
)



## load v3 scores for ECN
cibil_v3 <- fread_raw("cibil_v3_complete_ECN.txt")
cibil_v3$ECN <- as.character(cibil_v3$ECN)

## load application ECN mapping
application_ECN_mapping <- fread_raw("Application ECN Data.txt")

application_ECN_mapping$EnquiryControlNumber <- as.character(application_ECN_mapping$EnquiryControlNumber)

cibil_v3_applications <- inner_join(application_ECN_mapping,cibil_v3,by = c("EnquiryControlNumber" = "ECN"))

# cibil_v3_applications$EnquiryControlNumber <- as.numeric(cibil_v3_applications$EnquiryControlNumber)

## save v3 score for applications
assert_data_non_empty(cibil_v3_applications)
save(cibil_v3_applications,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "cibil_v3_applications.rdata"
     )
)




#################################################################################################################
## 23. CV Demogs -----------------------------------------------------------------------------------------------


cv_demog <- data.frame(fread_raw("CV_Demogs.txt"))

cv_demog$non_mortage_balance <- as.numeric(cv_demog$non_mortage_balance)

cv_demog$missed_payment_ratio_3m <- as.numeric(cv_demog$missed_payment_ratio_3m)
cv_demog$missed_payment_ratio_6m <- as.numeric(cv_demog$missed_payment_ratio_6m)
cv_demog$missed_payment_ratio_12m <- as.numeric(cv_demog$missed_payment_ratio_12m)


cv_demog$phones_reported_3m <- as.numeric(cv_demog$phones_reported_3m)
cv_demog$phones_reported_6m <- as.numeric(cv_demog$phones_reported_6m)
cv_demog$phones_reported_12m <- as.numeric(cv_demog$phones_reported_12m)

cv_demog$addresses_reported_3m <- as.numeric(cv_demog$addresses_reported_3m)
cv_demog$addresses_reported_6m <- as.numeric(cv_demog$addresses_reported_6m)
cv_demog$addresses_reported_12m <- as.numeric(cv_demog$addresses_reported_12m)

cv_demog$cibil_score_v3 <- NULL

cv_demog$ECN <- as.character(cv_demog$ECN)

assert_data_non_empty(cv_demog)
save(cv_demog,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "cv_demog.rdata"
     )
)




#################################################################################################################
## 24. NTC Score  -----------------------------------------------------------------------------------------------


ntc_score <- data.frame(fread_raw("Final NTC File.txt"))

ntc_score <- ntc_score %>% dplyr::select(c('cust_id','memref','ntc_risk_band'))


colnames(ntc_score) <- c('cust_id','deal_no','cibil_ntc_score')

ntc_score$deal_no <- str_sub(ntc_score$deal_no, 1,nchar(ntc_score$deal_no)-4)



ecn_p1 <- fread_raw("ECN-revised.txt")
ecn_p2 <- fread_raw("Missing ECN.txt")

ecn_p1 <- ecn_p1 %>% dplyr::select(Deal_No,Customer_Code,EnquiryControlNumber,ApplicationId)
ecn_p2 <- ecn_p2 %>% dplyr::select(Deal_No,customer_code,EnquiryControlNumber,ApplicationId)

colnames(ecn_p1) <- c('deal_no','customer_code','ECN','appliation_id')
colnames(ecn_p2) <- c('deal_no','customer_code','ECN','appliation_id')

ECN_mapping <- data.frame(rbindlist(l= list(ecn_p1,ecn_p2), use.names = T, fill = T))
rm(ecn_p1,ecn_p2)


ntc_score <- distinct(ntc_score)

assert_data_non_empty(ntc_score)
save(ntc_score,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "ntc_score.rdata"
     )
)



cibil_ntc <- fread("data//raw//CIBIL_NTC_SCORE_PV_FINAL_OUTUT.txt")
cibil_ntc <- cibil_ntc %>% filter(Ownership_Tag == 'Primary')
cibil_ntc <- cibil_ntc %>% dplyr::select(c('Deal_no','ntc_risk_band'))
colnames(cibil_ntc) <- c('deal_no', 'cibil_ntc_score')

assert_data_non_empty(cibil_ntc)
save(cibil_ntc,
     file = file.path(
             get_data_path()$data$intermediate,
             "cleaned_data",
             "ntc_cibil_new.rdata"
     )
)
