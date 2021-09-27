############################################################################################
################## 7 - Cibil Retro As on Data Preparation ##################################
############################################################################################


## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)



## 1. Load data ----------------------------------------------------------------------------

## 1.1 Load cibil retro as on data 
load_rdata_intermediate("cleaned_data//retro_as_on_data_2W_RF.rdata")

retro_as_on_data <- data.frame(retro_as_on_data_2WR)

## 1.2 Load disbursal data 
load_rdata_intermediate("cleaned_data//disbursal_data.rdata")


## 1.3 Load deal numbers for modeling
load_rdata_intermediate("ADS_data//validation_60dpd_12mob_2W_RF.rdata")
load_rdata_intermediate("ADS_data//validation_60dpd_9mob_2W_RF.rdata")
load_rdata_intermediate("ADS_data//validation_60dpd_oct_to_dec_2020_2W_RF.rdata")


## 2. Subset data & create unique loan identifier ------------------------------------------

## 2.1 subset disbursal data
disbursal_data$year_mon <- paste0(year(disbursal_data$disbursal_date), "-", str_pad(month(disbursal_data$disbursal_date),2,"0",side = "left"))

validation_deals <- c(unique(validation_60dpd_12mob$deal_no), unique(validation_60dpd_9mob$deal_no), unique(validation_60dpd_oct_to_dec_2020$deal_no))

disbursal_data <- distinct(disbursal_data %>% filter(deal_no %in% validation_deals) %>% dplyr::select(deal_no,
                                                                                                          loan_type,
                                                                                                          disbursal_date,
                                                                                                          year_mon))


## 2.2 inner join disbursal data with retro bureau data
tradeline_data <- inner_join(disbursal_data, retro_as_on_data, by = 'deal_no')

rm(disbursal_data,retro_as_on_data_2WR)

## 2.3 clean tradeline data
tradeline_subset <- tradeline_data %>% filter((!is.na(high_credit_sanctioned_amount)) &
                                                (high_credit_sanctioned_amount > 1000) &
                                                (!is.na(cibil_reported_date)) &
                                                (!is.na(account_date_opened)) &
                                                (!is.na(account_type)))


## 2.4 create loan identifier
tradeline_subset$loan_identifier <- paste(tradeline_subset$account_type,
                                          tradeline_subset$high_credit_sanctioned_amount,
                                          tradeline_subset$account_date_opened,
                                          sep = "-")


## 2.6 tag account descriptions
account_mapping <- fread_mapping("account_type_mapping.csv")

tradeline_subset <- left_join(tradeline_subset,account_mapping,by = 'account_type')
tradeline_subset$account_description <- NULL

tradeline_subset <- tradeline_subset %>% filter(!is.na(loan_description))


## 2.7 identify live loans
tradeline_subset$live_loan <- ifelse((tradeline_subset$loan_notation != 'CC' & 
                                       is.na(tradeline_subset$account_date_closed) &
                                       tradeline_subset$current_balance > 0) |
                                       (tradeline_subset$loan_notation == 'CC' & 
                                          is.na(tradeline_subset$account_date_closed) &
                                          (tradeline_subset$current_balance > 0 | tradeline_subset$current_balance == 0)
                                          ),
                                     1,0)


## 2.8 create applicant level ID
tradeline_subset$applicant_id <- paste0(tradeline_subset$deal_no, "_", tradeline_subset$Appno)



## 3. Remove duplicates ---------------------------------------------------------------------

## 3.1 get last reporting date at deal x loan id level
# last_reported_date <- tradeline_subset %>% group_by(deal_no,loan_identifier) %>% summarise(last_reported_date = max(cibil_reported_date, na.rm = T))
last_reported_date <- tradeline_subset %>% group_by(applicant_id,loan_identifier) %>% summarise(last_reported_date = max(cibil_reported_date, na.rm = T))

## 3.2 merge with tradelines data
tradeline_subset <- left_join(tradeline_subset,last_reported_date, by = c("applicant_id" = "applicant_id",
                                                                          "loan_identifier" = "loan_identifier"))


## 3.3 create unique key
tradeline_subset$key <- paste(tradeline_subset$applicant_id,
                              tradeline_subset$loan_identifier,
                              tradeline_subset$last_reported_date)

 
## 3.4 create monthly dpd values
x <- 0
for(index in 1:36){
  tradeline_subset[[paste0("month_",index,"_dpd")]] <- stri_reverse(substr(stri_reverse(tradeline_subset$payment_history),
                                                              index + x,
                                                              index + x + 2))
  x <- x+2
}

# View(head(tradeline_subset,10))
                                       

## 3.5 get duplicated keys
duplicated_keys <- tradeline_subset[duplicated(tradeline_subset$key),]
duplicated_data <- tradeline_subset %>% filter(key %in% unique(duplicated_keys$key))
dpd_cols <- names(duplicated_data)[grep("_dpd",names(duplicated_data))]


## 3.6 convert dpd values into numerical values
for(dpd_col in dpd_cols){
  duplicated_data[[dpd_col]][duplicated_data[[dpd_col]] == '901'] <- 'STD'
  duplicated_data[[dpd_col]][duplicated_data[[dpd_col]] == '902'] <- 'SUB'
  duplicated_data[[dpd_col]][duplicated_data[[dpd_col]] == '903'] <- 'DBT'
  duplicated_data[[dpd_col]][duplicated_data[[dpd_col]] == '904'] <- 'LSS'
  duplicated_data[[dpd_col]][duplicated_data[[dpd_col]] == '905'] <- 'SMA'
  
  duplicated_data[[dpd_col]][duplicated_data[[dpd_col]] == 'LSS'] <- 180
  duplicated_data[[dpd_col]][duplicated_data[[dpd_col]] == 'DBT'] <- 150
  duplicated_data[[dpd_col]][duplicated_data[[dpd_col]] == 'SUB'] <- 90
  duplicated_data[[dpd_col]][duplicated_data[[dpd_col]] == 'SMA'] <- 60
  duplicated_data[[dpd_col]][duplicated_data[[dpd_col]] == 'STD'] <- 0
  duplicated_data[[dpd_col]] <- as.numeric(as.character(duplicated_data[[dpd_col]]))
}

## 3.7 create final measure to order by & remove duplicates
duplicated_data$payment_dpd <- rowSums(duplicated_data[,c(dpd_cols)], na.rm = T)
duplicated_data$NA_in_payment_string <- apply(duplicated_data[, dpd_cols], 1, function(x) sum(!is.na(x)))
duplicated_data$current_balance <- as.numeric(duplicated_data$current_balance)

duplicated_data$final_measure <- duplicated_data$payment_dpd + duplicated_data$NA_in_payment_string + duplicated_data$current_balance


## 3.8 aggregate to remove duplicates 
not_duplicated_data <- duplicated_data %>% group_by(deal_no,
                                                    loan_type,
                                                    disbursal_date,
                                                    year_mon,
                                                    Appno,
                                                    applicant_id,
                                                    high_credit_sanctioned_amount,
                                                    overdue_amount,
                                                    account_type,
                                                    account_date_opened,
                                                    cibil_reported_date,
                                                    loan_identifier,
                                                    payment_history_start_date,
                                                    payment_history_end_date,
                                                    ) %>% arrange(desc(final_measure)) %>% summarise(payment_history = payment_history[1],
                                                                                                     current_balance = current_balance[1]) %>% ungroup() 


## 3.9 create monthly dpd columns
x <- 0
for(index in 1:36){
  not_duplicated_data[[paste0("month_",index,"_dpd")]] <- stri_reverse(substr(stri_reverse(not_duplicated_data$payment_history),
                                                                              index + x,
                                                                              index + x + 2))
  x <- x+2
}


## 3.10 get columns common to non duplicated data & overall tradelines data
common_cols <- intersect(colnames(not_duplicated_data), colnames(tradeline_subset))


## 3.11 combine data
trades_clean <- rbind(tradeline_subset %>% filter(key %notin% unique(duplicated_data$key)) %>% dplyr::select(common_cols),
                      not_duplicated_data %>% dplyr::select(common_cols))


rm(duplicated_data,duplicated_keys, last_reported_date, not_duplicated_data, tradeline_data)


## 3.12 get account mapping
trades_clean <- left_join(trades_clean,account_mapping,by = 'account_type')
trades_clean$account_description <- NULL

trades_clean <- trades_clean %>% filter(!is.na(loan_description))




## 4. Get latest closed date for cleaned as on data -----------------------------------------------------

## 4.1 create payment key
tradeline_subset$payment_key <- paste(tradeline_subset$applicant_id,
                                      tradeline_subset$loan_identifier,
                                      tradeline_subset$cibil_reported_date,
                                      tradeline_subset$payment_history,
                                      sep ="-")

trades_clean$payment_key <- paste(trades_clean$applicant_id,
                                  trades_clean$loan_identifier,
                                  trades_clean$cibil_reported_date,
                                  trades_clean$payment_history,
                                  sep ="-")

## 4.2 get closing date
closing_date_info <- distinct(tradeline_subset %>% dplyr::select(loan_identifier,account_date_closed, payment_key))

closing_date_info <- closing_date_info %>% filter(payment_key %in% unique(trades_clean$payment_key) )

## 4.3 get unique closing key
closing_date_agg <- closing_date_info %>% group_by(payment_key) %>% summarise(account_date_closed = max(account_date_closed, na.rm = T))

closing_date_agg$account_date_closed[is.infinite(closing_date_agg$account_date_closed)] <- NA

## 4.4 join to get closing date
trades_clean <- left_join(trades_clean,closing_date_agg, by= 'payment_key')

rm(closing_date_agg,closing_date_info)


## 5. Create date indexes ------------------------------------------------------------------

trades_clean$yearmon_reported_date <- as.numeric(paste0(year(trades_clean$cibil_reported_date), str_pad(month(trades_clean$cibil_reported_date),2,"0",side = "left")))
trades_clean$yearmon_account_open_date <- as.numeric(paste0(year(trades_clean$account_date_opened), str_pad(month(trades_clean$account_date_opened),2,"0",side = "left")))
trades_clean$yearmon_payment_start_date <- as.numeric(paste0(year(trades_clean$payment_history_start_date), str_pad(month(trades_clean$payment_history_start_date),2,"0",side = "left")))


trades_clean <- trades_clean %>% filter(account_date_opened >= as.Date('1990-01-01'))

assert_data_non_empty(trades_clean)
save(trades_clean,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "bureau_tradelines_validation_2W_RF.rdata"
     )
)

# save(trades_clean,
#      file = file.path(
#        "data//intermediate//ADS_data//bureau_tradelines_validation_2W_RF.rdata"
#      )
# )
# bureau_tradelines_2W_RF_validation.rdata
rm(tradeline_subset)


## 6. Melt data & create indexes --------------------------------------------------------------------

## 6.1 melt data
req_cols <- c('deal_no',
              'Appno',
              'applicant_id',
              'loan_type',
              'disbursal_date',
              'high_credit_sanctioned_amount',
              'overdue_amount',
              'account_type',
              'account_date_opened',
              'account_date_closed',
              'cibil_reported_date',
              'payment_history_start_date',
              'payment_history_end_date',
              'loan_identifier',
              'current_balance',
              'payment_history',
              'yearmon_reported_date',
              'yearmon_account_open_date',
              'yearmon_payment_start_date',
              'loan_description',
              'loan_notation',
              'unsecured_flag',
              'month_1_dpd',
              'month_2_dpd',
              'month_3_dpd',
              'month_4_dpd',
              'month_5_dpd',
              'month_6_dpd',
              'month_7_dpd',
              'month_8_dpd',
              'month_9_dpd',
              'month_10_dpd',
              'month_11_dpd',
              'month_12_dpd',
              'month_13_dpd',
              'month_14_dpd',
              'month_15_dpd',
              'month_16_dpd',
              'month_17_dpd',
              'month_18_dpd',
              'month_19_dpd',
              'month_20_dpd',
              'month_21_dpd',
              'month_22_dpd',
              'month_23_dpd',
              'month_24_dpd',
              'month_25_dpd',
              'month_26_dpd',
              'month_27_dpd',
              'month_28_dpd',
              'month_29_dpd',
              'month_30_dpd',
              'month_31_dpd',
              'month_32_dpd',
              'month_33_dpd',
              'month_34_dpd',
              'month_35_dpd',
              'month_36_dpd'
              )

trades_melt <- melt(distinct(trades_clean %>% dplyr::select(req_cols)),
                    id.vars = c('deal_no',
                                'Appno',
                                'applicant_id',
                                'loan_type',
                                'disbursal_date',
                                'high_credit_sanctioned_amount',
                                'overdue_amount',
                                'account_type',
                                'account_date_opened',
                                'account_date_closed',
                                'cibil_reported_date',
                                'payment_history_start_date',
                                'payment_history_end_date',
                                'loan_identifier',
                                'current_balance',
                                'payment_history',
                                'yearmon_reported_date',
                                'yearmon_account_open_date',
                                'yearmon_payment_start_date',
                                'loan_description',
                                'loan_notation',
                                'unsecured_flag'
                                ))

## 6.2 remove records where no DPD value is available
trades_melt <- trades_melt %>% filter(value != "")


## 6.3 load index mapping tables
month_dpd_mapping <- fread_mapping("month_dpd_index_mapping.csv")
year_mon_index <- fread_mapping("year_mon_index_mapping.csv")

# month_dpd_mapping <- fread("data//mapping//month_dpd_index_mapping.csv")
# year_mon_index <- fread("data//mapping//year_mon_index_mapping.csv")


## 6.4 create date reported index
trades_melt$index_reported_date <- year_mon_index$index[match(trades_melt$yearmon_reported_date,year_mon_index$year_mon)]

## 6.5 create account open date index
trades_melt$index_account_open_date <- year_mon_index$index[match(trades_melt$yearmon_account_open_date,year_mon_index$year_mon)]

## 6.6 create payment start date index
trades_melt$index_payment_start_date <- year_mon_index$index[match(trades_melt$yearmon_payment_start_date,year_mon_index$year_mon)]

## 6.7 create monthly dpd status index
trades_melt$index_dpd_month <- month_dpd_mapping$index[match(trades_melt$variable,month_dpd_mapping$month_dpd)]
trades_melt$index_dpd_month_adjusted <- rowSums(trades_melt[,c('index_payment_start_date','index_dpd_month')])


## 6.8 create numerical DPD column
trades_melt$dpd_num <- trades_melt$value

trades_melt$dpd_num <- ifelse(trades_melt$dpd_num == '901', 'STD',
                              ifelse(trades_melt$dpd_num == '902', 'SUB',
                                     ifelse(trades_melt$dpd_num == '903', 'DBT',
                                            ifelse(trades_melt$dpd_num == '904', 'LSS',
                                                   ifelse(trades_melt$dpd_num == '905', 'SMA',
                                                          trades_melt$dpd_num)))))

trades_melt$dpd_num <- ifelse(trades_melt$dpd_num == 'LSS', '180',
                              ifelse(trades_melt$dpd_num == 'DBT', '150',
                                     ifelse(trades_melt$dpd_num == 'SUB', '90',
                                            ifelse(trades_melt$dpd_num == 'SMA', '60',
                                                   ifelse(trades_melt$dpd_num == 'STD', '0',
                                                          trades_melt$dpd_num)))))

trades_melt$dpd_num <- as.numeric(trades_melt$dpd_num)


## 6.9 get difference in months between reported date & monthly payment date
trades_melt$diff_reported_payment <- trades_melt$index_reported_date - trades_melt$index_dpd_month_adjusted

## 6.10 save data
assert_data_non_empty(trades_melt)
save(trades_melt,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "trades_melt_validation_2W_RF.rdata"
     )
)

# save(trades_melt,
#      file = file.path("data//intermediate//ADS_data//trades_melt_validation_2W_RF.rdata"
#      )
# )

rm(trades_clean,trades_melt,year_mon_index,month_dpd_mapping)




## 7. Load, clean & save enquiry bureau data ----------------------------------------------------------

## 7.2 load enquiry data
load_rdata_intermediate("cleaned_data//enquiry_data_2W_RF.rdata")

enquiry_data <- data.frame(enquiry_data_2W_RF)
rm(enquiry_data_2W_RF)


## 7.3 subset enquiry data
# enquiry_data <- inner_join(enquiry_data, bad_loans, by = "deal_no")
enquiry_data <- enquiry_data %>% filter(deal_no %in% validation_deals)
enquiry_data$applicant_id <- paste0(enquiry_data$deal_no,"_",enquiry_data$Appno)

## 7.4 get cibil reported date for deal numbers
load_rdata_intermediate("ADS_data//bureau_tradelines_validation_2W_RF.rdata")
trades_clean <- distinct(trades_clean %>% dplyr::select(deal_no,Appno,applicant_id,disbursal_date,cibil_reported_date,loan_type))



enquiry_data <- left_join(enquiry_data,trades_clean, by = c("deal_no" = "deal_no",
                                                            "Appno" = "Appno",
                                                            "applicant_id" = "applicant_id"))

enquiry_data <- enquiry_data %>% filter(!is.na(cibil_reported_date))


## 7.5 get loan type description
enquiry_data <- left_join(enquiry_data,account_mapping, by = c("Enquiry_Purpose" = "account_type"))
enquiry_data$account_description <- NULL

## 7.6 calculate days between enquiry date & date on which cibil report was pulled
enquiry_data$days <- as.numeric(enquiry_data$cibil_reported_date - enquiry_data$enquiry_date)

enquiry_data <- enquiry_data %>% filter(days >= 0)


## 7.7 save data
assert_data_non_empty(enquiry_data)
save(enquiry_data,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "enquiry_data_validation_2W_RF.rdata"
     )
)

# save(enquiry_data,
#      file = file.path("data//intermediate//ADS_data//enquiry_data_validation_2W_RF.rdata"
#      )
# )

rm(list=ls())
