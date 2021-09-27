############################################################################################
################## Reject tagging ##################################
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
load_rdata_intermediate("cleaned_data//reject_app_tradelines.rdata")


## 2. Data manipulation-----------------------------------------------------------------------

## 2.1 filter for applications between Apr - Dec 2018
start_date <- '2018-04-01'
end_date <- '2018-12-31'

reject_tradelines <- reject_app_tradelines %>% filter((application_date >= as.Date(start_date)) & 
                                                        (application_date <= as.Date(end_date)))


## 2.2 get account type information
account_type_mapping <- fread_mapping("account_type_mapping_rejects.csv")

reject_tradelines <- left_join(reject_tradelines,account_type_mapping,by = 'AccountType')


rm(reject_app_tradelines,account_type_mapping)



## 2.3 clean data
tradeline_subset <- reject_tradelines %>% filter((!is.na(high_credit_sanctioned_amount)) &
                                                (high_credit_sanctioned_amount > 1000) &
                                                (!is.na(cibil_reported_date)) &
                                                (!is.na(account_date_opened)) &
                                                (!is.na(account_type)))


tradeline_subset <- tradeline_subset %>% filter(!is.na(loan_description))


## 2.4 create loan identifier
tradeline_subset$loan_identifier <- paste(tradeline_subset$account_type,
                                          tradeline_subset$high_credit_sanctioned_amount,
                                          tradeline_subset$account_date_opened,
                                          sep = "-")


## 2.5 filter for accounts which have opened after Jan 1990
tradeline_subset <- tradeline_subset %>% filter(account_date_opened >= as.Date('1990-01-01'))




## 3. Remove duplicates ---------------------------------------------------------------------

## 3.1 get last reporting date at deal x loan id level
last_reported_date <- tradeline_subset %>% group_by(application_no,loan_identifier) %>% summarise(last_reported_date = max(cibil_reported_date, na.rm = T))

## 3.2 merge with tradelines data
tradeline_subset <- left_join(tradeline_subset,last_reported_date, by = c("application_no" = "application_no",
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
not_duplicated_data <- duplicated_data %>% group_by(application_no,
                                                    product,
                                                    type,
                                                    application_date,
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


rm(duplicated_data,duplicated_keys, last_reported_date, not_duplicated_data)


## 3.12 get account mapping
account_type_mapping <- fread_mapping("account_type_mapping_rejects.csv")
trades_clean <- left_join(trades_clean,account_type_mapping,by = 'account_type')
trades_clean$account_description <- NULL

trades_clean <- trades_clean %>% filter(!is.na(loan_description))




## 4. Get latest closed date for cleaned as on data -----------------------------------------------------

## 4.1 create payment key
tradeline_subset$payment_key <- paste(tradeline_subset$application_no,
                                      tradeline_subset$loan_identifier,
                                      tradeline_subset$cibil_reported_date,
                                      tradeline_subset$payment_history,
                                      sep ="-")

trades_clean$payment_key <- paste(trades_clean$application_no,
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
rm(reject_tradelines,tradeline_subset)

## 5. Create date indexes ------------------------------------------------------------------

trades_clean$yearmon_reported_date <- as.numeric(paste0(year(trades_clean$cibil_reported_date), str_pad(month(trades_clean$cibil_reported_date),2,"0",side = "left")))
trades_clean$yearmon_account_open_date <- as.numeric(paste0(year(trades_clean$account_date_opened), str_pad(month(trades_clean$account_date_opened),2,"0",side = "left")))
trades_clean$yearmon_payment_start_date <- as.numeric(paste0(year(trades_clean$payment_history_start_date), str_pad(month(trades_clean$payment_history_start_date),2,"0",side = "left")))
trades_clean$yearmon_application_date <- as.numeric(paste0(year(trades_clean$application_date), str_pad(month(trades_clean$application_date),2,"0",side = "left")))


## get time difference in days between application & date opened
trades_clean$time_diff <- as.numeric(trades_clean$account_date_opened - trades_clean$application_date)


## tag live loan
trades_clean$live_loan <- ifelse((trades_clean$loan_notation != 'CC' & 
                                        is.na(trades_clean$account_date_closed) &
                                    trades_clean$current_balance > 0) |
                                       (trades_clean$loan_notation == 'CC' & 
                                          is.na(trades_clean$account_date_closed) &
                                          (trades_clean$current_balance > 0 | trades_clean$current_balance == 0)
                                       ),
                                     1,0)




# 6. Melt data & create indexes --------------------------------------------------------------------

## 6.1 melt data
req_cols <- c('application_no',
              'product',
              'type',
              'application_date',
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
              'yearmon_application_date',
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
                    id.vars = c('application_no',
                                'product',
                                'type',
                                'application_date',
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
                                'yearmon_application_date',
                                'loan_description',
                                'loan_notation',
                                'unsecured_flag'
                    ))

## 6.2 remove records where no DPD value is available
trades_melt <- trades_melt %>% filter(value != "")


## 6.3 load index mapping tables
month_dpd_mapping <- fread_mapping("month_dpd_index_mapping.csv")
year_mon_index <- fread_mapping("year_mon_index_mapping.csv")


## 6.4 create date reported index
trades_melt$index_reported_date <- year_mon_index$index[match(trades_melt$yearmon_reported_date,year_mon_index$year_mon)]

## 6.5 create account open date index
trades_melt$index_account_open_date <- year_mon_index$index[match(trades_melt$yearmon_account_open_date,year_mon_index$year_mon)]

## 6.6 create payment start date index
trades_melt$index_payment_start_date <- year_mon_index$index[match(trades_melt$yearmon_payment_start_date,year_mon_index$year_mon)]

## 6.7 create monthly dpd status index
trades_melt$index_dpd_month <- month_dpd_mapping$index[match(trades_melt$variable,month_dpd_mapping$month_dpd)]
trades_melt$index_dpd_month_adjusted <- rowSums(trades_melt[,c('index_payment_start_date','index_dpd_month')])

## 6.8 create application date index
trades_melt$index_application_date <- year_mon_index$index[match(trades_melt$yearmon_application_date,year_mon_index$year_mon)]


## 6.9 create numerical DPD column
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
trades_melt <- trades_melt %>% filter(!is.na(dpd_num))

## 6.9 get difference in months between reported date & monthly payment date
# trades_melt$mob <- trades_melt$index_application_date - trades_melt$index_dpd_month_adjusted
trades_melt$mob <- trades_melt$index_dpd_month_adjusted - trades_melt$index_application_date
trades_melt$mob_post_open <- trades_melt$index_dpd_month_adjusted - trades_melt$index_account_open_date

rm(account_type_mapping,month_dpd_mapping,year_mon_index)




assert_data_non_empty(trades_clean)
save(trades_clean,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "reject_bureau_tradelines.rdata"
     )
)


assert_data_non_empty(trades_melt)
save(trades_melt,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "reject_bureau_tradelines_melt.rdata"
     )
)

### 2W New
################################################################################################################################
#####
load_rdata_intermediate("reject_data//reject_bureau_tradelines.rdata")

load_rdata_intermediate("reject_data//reject_bureau_tradelines_melt.rdata")
trades_melt$mob_post_open <- trades_melt$index_dpd_month_adjusted - trades_melt$index_account_open_date

################################################################################################################

## Tag rejects in 2W

post_app_months_2W <- 3
window_start <- 0
window_end <- 30 * post_app_months_2W
loan_notation_2W <- 'TW'
sanction_amount_2W <- 100000

## Tag R1
rejects_2W_R1 <- trades_clean %>% filter((time_diff > window_start) & 
                                           (time_diff <= window_end) & 
                                           (loan_notation == loan_notation_2W) &
                                           (product == 'H') & (type %notin% c('R','U'))
                                         )

rejects_2W_R1 <- distinct(rejects_2W_R1 %>% dplyr::select(application_no))
rejects_2W_R1$tag <- 'R1'


## Tag R2
rejects_2W_R2 <- trades_clean %>% filter((time_diff > window_start) & 
                                           (time_diff <= window_end) & 
                                           (loan_notation != loan_notation_2W) &
                                           (product == 'H') & (type %notin% c('R','U')) &
                                           (high_credit_sanctioned_amount <= sanction_amount_2W) &
                                           (application_no %notin% unique(rejects_2W_R1$application_no)))

rejects_2W_R2 <- distinct(rejects_2W_R2 %>% dplyr::select(application_no))
rejects_2W_R2$tag <- 'R2'


## Tag R3
rejects_2W_R3 <- trades_clean %>% filter((time_diff <= window_start)  &
                                           (product == 'H') & (type %notin% c('R','U')) &
                                           (live_loan == 1) &
                                           (application_no %notin% unique(rejects_2W_R1$application_no))  &
                                           (application_no %notin% unique(rejects_2W_R2$application_no)) )

rejects_2W_R3 <- distinct(rejects_2W_R3 %>% dplyr::select(application_no))
rejects_2W_R3$tag <- 'R3'


## Tag R4
rejects_2W_R4 <- trades_clean %>% filter((time_diff <= window_start)  &
                                           (product == 'H') & (type %notin% c('R','U')) &
                                           (live_loan == 0) &
                                           (application_no %notin% unique(rejects_2W_R1$application_no))  &
                                           (application_no %notin% unique(rejects_2W_R2$application_no)) &
                                           (application_no %notin% unique(rejects_2W_R3$application_no)))

rejects_2W_R4 <- distinct(rejects_2W_R4 %>% dplyr::select(application_no))
rejects_2W_R4$tag <- 'R4'

## combine all rejects 
rejects_2W <- data.frame(rbindlist(l=list(rejects_2W_R1,
                                          rejects_2W_R2,
                                          rejects_2W_R3,
                                          rejects_2W_R4)))

# rejects_2W <- data.frame(rbindlist(l=list(rejects_2W_R1,
#                                           rejects_2W_R2)))

rm(rejects_2W_R1,
   rejects_2W_R2,
   rejects_2W_R3,
   rejects_2W_R4)


# rm(rejects_2W_R1,
#    rejects_2W_R2)

##################################################################################################

## tag bad loans
dpd_threshold <- 60
mob_threshold <- 15

## filter till Mar 2020 (remove covid period)
trades_melt_subset <- trades_melt %>% filter(index_dpd_month_adjusted <= 363)
trades_melt_subset <- trades_melt_subset %>% filter(account_date_opened >= application_date)


no_of_mob <- trades_melt_subset %>% group_by(application_no) %>% summarise(mob_count = length(unique(mob_post_open)))
app_ids_with_required_data <- unique((no_of_mob %>% filter(mob_count >= mob_threshold))$application_no)

################- R1 ###################################
## get R1 60 dpd in 15mob
bad_flag_R1 <- trades_melt_subset %>% filter((product == 'H') & (type %notin% c('R','U')) &
                                                      (loan_notation == loan_notation_2W) &
                                                      (mob_post_open >= 0) &
                                                      (mob_post_open <= mob_threshold) &
                                                      (application_no %in% app_ids_with_required_data) &
                                               (application_no %in% unique((rejects_2W %>% filter(tag=='R1'))$application_no)))


bad_flag_R1$bad_loan <- ifelse(bad_flag_R1$dpd_num >= dpd_threshold,1,0)
bad_flag_R1 <- bad_flag_R1 %>% group_by(application_no) %>% summarise(bad_loan = max(bad_loan,na.rm=T))

## get R1 90, 150 dpd
flag_90_150_dpd_R1 <- trades_melt_subset %>% filter((product == 'H') & (type %notin% c('R','U')) &
                                               (loan_notation == loan_notation_2W) &
                                               (mob_post_open >= 0) &
                                               # (mob_post_open <= mob_threshold) &
                                               (application_no %in% app_ids_with_required_data) &
                                               (application_no %in% unique((rejects_2W %>% filter(tag=='R1'))$application_no)))

flag_90_150_dpd_R1$ever_90dpd <- ifelse(flag_90_150_dpd_R1$dpd_num >= 90,1,0)
flag_90_150_dpd_R1$ever_150dpd <- ifelse(flag_90_150_dpd_R1$dpd_num >= 150,1,0)
flag_90_150_dpd_R1 <- flag_90_150_dpd_R1 %>% group_by(application_no) %>% summarise(ever_90dpd = max(ever_90dpd,na.rm=T),
                                                                                    ever_150dpd = max(ever_150dpd,na.rm=T))
## join to get 60 dpd 150 mob, 90 dpd, 150 dpd in one dataframe
bad_flag_R1 <- inner_join(bad_flag_R1,flag_90_150_dpd_R1,by='application_no')
rm(flag_90_150_dpd_R1)


## join bad tags with R1
reject_data_2W_R1 <- inner_join(rejects_2W %>% filter(tag=='R1'), bad_flag_R1, by = 'application_no')

rm(bad_flag_R1)







################- R2 ###################################

## get 60 dpd 15mob for R2
bad_flag_R2 <- trades_melt_subset %>% filter((product == 'H') & 
                                               (loan_notation != loan_notation_2W) &
                                               (mob_post_open >= 0) &
                                               (mob_post_open <= mob_threshold) &
                                               (application_no %in% app_ids_with_required_data) &
                                               (application_no %in% unique((rejects_2W %>% filter(tag=='R2'))$application_no)))


bad_flag_R2$bad_loan <- ifelse(bad_flag_R2$dpd_num >= dpd_threshold,1,0)
bad_flag_R2 <- bad_flag_R2 %>% group_by(application_no) %>% summarise(bad_loan = max(bad_loan,na.rm=T))



## get R2 90, 150 dpd
flag_90_150_dpd_R2 <- trades_melt_subset %>% filter((product == 'H') & 
                                                      (loan_notation != loan_notation_2W) &
                                                      (mob_post_open >= 0) &
                                                      # (mob_post_open <= mob_threshold) &
                                                      (application_no %in% app_ids_with_required_data) &
                                                      (application_no %in% unique((rejects_2W %>% filter(tag=='R2'))$application_no)))

flag_90_150_dpd_R2$ever_90dpd <- ifelse(flag_90_150_dpd_R2$dpd_num >= 90,1,0)
flag_90_150_dpd_R2$ever_150dpd <- ifelse(flag_90_150_dpd_R2$dpd_num >= 150,1,0)
flag_90_150_dpd_R2 <- flag_90_150_dpd_R2 %>% group_by(application_no) %>% summarise(ever_90dpd = max(ever_90dpd,na.rm=T),
                                                                                    ever_150dpd = max(ever_150dpd,na.rm=T))
## join to get 60 dpd 150 mob, 90 dpd, 150 dpd in one dataframe
bad_flag_R2 <- inner_join(bad_flag_R2,flag_90_150_dpd_R2,by='application_no')
rm(flag_90_150_dpd_R2)


## join bad tags with R2
reject_data_2W_R2 <- inner_join(rejects_2W %>% filter(tag=='R2'), bad_flag_R2, by = 'application_no')

rm(bad_flag_R2)



################- R3 ###################################

## get 60 dpd 15mob for R3
bad_flag_R3 <- trades_melt_subset %>% filter((product == 'H') & #(type %in% c('N')) &
                                                 (mob_post_open >= 0) &
                                                 (mob_post_open <= mob_threshold) &
                                                 (application_no %in% app_ids_with_required_data) &
                                               (application_no %in% unique((rejects_2W %>% filter(tag=='R3'))$application_no)))


bad_flag_R3$bad_loan <- ifelse(bad_flag_R3$dpd_num >= dpd_threshold,1,0)
bad_flag_R3 <- bad_flag_R3 %>% group_by(application_no) %>% summarise(bad_loan = max(bad_loan,na.rm=T))



## get R2 90, 150 dpd
flag_90_150_dpd_R3 <- trades_melt_subset %>% filter((product == 'H') & 
                                                      # (loan_notation != loan_notation_2W) &
                                                      (mob_post_open >= 0) &
                                                      # (mob_post_open <= mob_threshold) &
                                                      (application_no %in% app_ids_with_required_data) &
                                                      (application_no %in% unique((rejects_2W %>% filter(tag=='R3'))$application_no)))

flag_90_150_dpd_R3$ever_90dpd <- ifelse(flag_90_150_dpd_R3$dpd_num >= 90,1,0)
flag_90_150_dpd_R3$ever_150dpd <- ifelse(flag_90_150_dpd_R3$dpd_num >= 150,1,0)
flag_90_150_dpd_R3 <- flag_90_150_dpd_R3 %>% group_by(application_no) %>% summarise(ever_90dpd = max(ever_90dpd,na.rm=T),
                                                                                    ever_150dpd = max(ever_150dpd,na.rm=T))
## join to get 60 dpd 150 mob, 90 dpd, 150 dpd in one dataframe
bad_flag_R3 <- inner_join(bad_flag_R3,flag_90_150_dpd_R3,by='application_no')
rm(flag_90_150_dpd_R3)


## join bad tags with R2
reject_data_2W_R3 <- inner_join(rejects_2W %>% filter(tag=='R3'), bad_flag_R3, by = 'application_no')

rm(bad_flag_R3)








#####################################################################################################################
## get bad_flags for rejects

reject_data_2W <- rbind(reject_data_2W_R1,reject_data_2W_R2)
reject_data_2W <- rbind(reject_data_2W,reject_data_2W_R3)
reject_data_2W <- data.frame(rbindlist(l=list(reject_data_2W,rejects_2W %>% filter(tag == 'R4')), use.names = T, fill = T))


rm(reject_data_2W_R1,reject_data_2W_R2,reject_data_2W_R3,reject_data_2W_R3)


assert_data_non_empty(reject_data_2W)
save(reject_data_2W,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "reject_data_2W.rdata"
     )
)

rm(list=ls())


#######################################################################################################################################
####################### PV New #######################################################################


load_rdata_intermediate("reject_data//reject_bureau_tradelines.rdata")

load_rdata_intermediate("reject_data//reject_bureau_tradelines_melt.rdata")
trades_melt$mob_post_open <- trades_melt$index_dpd_month_adjusted - trades_melt$index_account_open_date


################################################################################################################

## Tag rejects in PV New

post_app_months_PV_New <- 6
window_start <- 0
window_end <- 30 * post_app_months_PV_New
# loan_notation_PV_New <- 'AL'
loan_notation_PV_New <- c(1)
sanction_amount_PV_New <- 1000000

## Tag R1
rejects_PV_New_R1 <- trades_clean %>% filter((time_diff > window_start) &
                                           (time_diff <= window_end) &
                                           (account_type %in% loan_notation_PV_New) &
                                           (product == 'C') & (type %in% c('N','')))

rejects_PV_New_R1 <- distinct(rejects_PV_New_R1 %>% dplyr::select(application_no))
rejects_PV_New_R1$tag <- 'R1'


# rejects_PV_New_R1 <- trades_clean %>% filter((time_diff > window_start) &
#                                                (time_diff <= window_end) &
#                                                (account_type %in% loan_notation_PV_New) &
#                                                (product == 'C') & (type %in% c('N')))
# 
# rejects_PV_New_R1 <- distinct(rejects_PV_New_R1 %>% dplyr::select(application_no))
# rejects_PV_New_R1$tag <- 'R1'


## Tag R2
rejects_PV_New_R2 <- trades_clean %>% filter((time_diff > window_start) &
                                           (time_diff <= window_end) &
                                           (account_type %notin% loan_notation_PV_New) &
                                           (product == 'C') & (type %in% c('N','')) &
                                           (high_credit_sanctioned_amount <= sanction_amount_PV_New) &
                                           (application_no %notin% unique(rejects_PV_New_R1$application_no)))

rejects_PV_New_R2 <- distinct(rejects_PV_New_R2 %>% dplyr::select(application_no))
rejects_PV_New_R2$tag <- 'R2'


# rejects_PV_New_R2 <- trades_clean %>% filter((time_diff > window_start) & 
#                                                (time_diff <= window_end) & 
#                                                (account_type %notin% loan_notation_PV_New) &
#                                                (product == 'C') & (type %in% c('N')) &
#                                                (high_credit_sanctioned_amount <= sanction_amount_PV_New) &
#                                                (application_no %notin% unique(rejects_PV_New_R1$application_no)))
# 
# rejects_PV_New_R2 <- distinct(rejects_PV_New_R2 %>% dplyr::select(application_no))
# rejects_PV_New_R2$tag <- 'R2'

## Tag R3
rejects_PV_New_R3 <- trades_clean %>% filter((time_diff <= window_start)  &
                                               (product == 'C') & (type %in% c('N','')) &
                                           (live_loan == 1) &
                                           (application_no %notin% unique(rejects_PV_New_R1$application_no))  &
                                           (application_no %notin% unique(rejects_PV_New_R2$application_no)) )

rejects_PV_New_R3 <- distinct(rejects_PV_New_R3 %>% dplyr::select(application_no))
rejects_PV_New_R3$tag <- 'R3'


## Tag R4
rejects_PV_New_R4 <- trades_clean %>% filter((time_diff <= window_start)  &
                                               (product == 'C') & (type %in% c('N','')) &
                                           (live_loan == 0) &
                                           (application_no %notin% unique(rejects_PV_New_R1$application_no))  &
                                           (application_no %notin% unique(rejects_PV_New_R2$application_no)) &
                                           (application_no %notin% unique(rejects_PV_New_R3$application_no)))

rejects_PV_New_R4 <- distinct(rejects_PV_New_R4 %>% dplyr::select(application_no))
rejects_PV_New_R4$tag <- 'R4'

## combine all rejects
rejects_PV_New <- data.frame(rbindlist(l=list(rejects_PV_New_R1,
                                          rejects_PV_New_R2,
                                          rejects_PV_New_R3,
                                          rejects_PV_New_R4)))

# rejects_PV_New <- data.frame(rbindlist(l=list(rejects_PV_New_R1,
#                                           rejects_PV_New_R2)))

rm(rejects_PV_New_R1,
   rejects_PV_New_R2,
   rejects_PV_New_R3,
   rejects_PV_New_R4)


# rm(rejects_PV_New_R1,
#    rejects_PV_New_R2)

##################################################################################################

## tag bad loans
dpd_threshold <- 60
mob_threshold <- 15

## filter till Mar 2020 (remove covid period)
trades_melt_subset <- trades_melt %>% filter(index_dpd_month_adjusted <= 363)
trades_melt_subset <- trades_melt_subset %>% filter(account_date_opened > application_date)


no_of_mob <- trades_melt_subset %>% group_by(application_no) %>% summarise(mob_count = length(unique(mob_post_open)))
app_ids_with_required_data <- unique((no_of_mob %>% filter(mob_count >= mob_threshold))$application_no)


## get R1 60 dpd in 15mob
bad_flag_R1 <- trades_melt_subset %>% filter((product == 'C') & 
                                               (type %in% c('N','')) &
                                               # (type %in% c('N')) &
                                               (account_type %in% loan_notation_PV_New) &
                                               (mob_post_open >= 0) &
                                               (mob_post_open <= mob_threshold) &
                                               (application_no %in% app_ids_with_required_data) &
                                               (application_no %in% unique((rejects_PV_New %>% filter(tag=='R1'))$application_no)))


bad_flag_R1$bad_loan <- ifelse(bad_flag_R1$dpd_num >= dpd_threshold,1,0)
bad_flag_R1 <- bad_flag_R1 %>% group_by(application_no) %>% summarise(bad_loan = max(bad_loan,na.rm=T))

## get R1 90, 150 dpd
flag_90_150_dpd_R1 <- trades_melt_subset %>% filter((product == 'C') & (type %in% c('N','')) & 
                                                      (account_type %in% loan_notation_PV_New) &
                                                      (mob_post_open >= 0) &
                                                      # (mob_post_open <= mob_threshold) &
                                                      (application_no %in% app_ids_with_required_data) &
                                                      (application_no %in% unique((rejects_PV_New %>% filter(tag=='R1'))$application_no)))

flag_90_150_dpd_R1$ever_90dpd <- ifelse(flag_90_150_dpd_R1$dpd_num >= 90,1,0)
flag_90_150_dpd_R1$ever_150dpd <- ifelse(flag_90_150_dpd_R1$dpd_num >= 150,1,0)
flag_90_150_dpd_R1 <- flag_90_150_dpd_R1 %>% group_by(application_no) %>% summarise(ever_90dpd = max(ever_90dpd,na.rm=T),
                                                                                    ever_150dpd = max(ever_150dpd,na.rm=T))
## join to get 60 dpd 150 mob, 90 dpd, 150 dpd in one dataframe
bad_flag_R1 <- inner_join(bad_flag_R1,flag_90_150_dpd_R1,by='application_no')
rm(flag_90_150_dpd_R1)


## join bad tags with R1
reject_data_PV_New_R1 <- inner_join(rejects_PV_New %>% filter(tag=='R1'), bad_flag_R1, by = 'application_no')

rm(bad_flag_R1)








## get 60 dpd 15mob for R2
bad_flag_R2 <- trades_melt_subset %>% filter((product == 'C') & (type %in% c('N','')) &
                                               (account_type %notin% loan_notation_PV_New) &
                                               (mob_post_open >= 0) &
                                               (mob_post_open <= mob_threshold) &
                                               (application_no %in% app_ids_with_required_data) &
                                               (application_no %in% unique((rejects_PV_New %>% filter(tag=='R2'))$application_no)))


bad_flag_R2$bad_loan <- ifelse(bad_flag_R2$dpd_num >= dpd_threshold,1,0)
bad_flag_R2 <- bad_flag_R2 %>% group_by(application_no) %>% summarise(bad_loan = max(bad_loan,na.rm=T))



## get R2 90, 150 dpd
flag_90_150_dpd_R2 <- trades_melt_subset %>% filter((product == 'C') & (type %in% c('N','')) &
                                                      (account_type %notin% loan_notation_PV_New) &
                                                      (mob_post_open >= 0) &
                                                      # (mob_post_open <= mob_threshold) &
                                                      (application_no %in% app_ids_with_required_data) &
                                                      (application_no %in% unique((rejects_PV_New %>% filter(tag=='R2'))$application_no)))

flag_90_150_dpd_R2$ever_90dpd <- ifelse(flag_90_150_dpd_R2$dpd_num >= 90,1,0)
flag_90_150_dpd_R2$ever_150dpd <- ifelse(flag_90_150_dpd_R2$dpd_num >= 150,1,0)
flag_90_150_dpd_R2 <- flag_90_150_dpd_R2 %>% group_by(application_no) %>% summarise(ever_90dpd = max(ever_90dpd,na.rm=T),
                                                                                    ever_150dpd = max(ever_150dpd,na.rm=T))
## join to get 60 dpd 150 mob, 90 dpd, 150 dpd in one dataframe
bad_flag_R2 <- inner_join(bad_flag_R2,flag_90_150_dpd_R2,by='application_no')
rm(flag_90_150_dpd_R2)


## join bad tags with R2
reject_data_PV_New_R2 <- inner_join(rejects_PV_New %>% filter(tag=='R2'), bad_flag_R2, by = 'application_no')

rm(bad_flag_R2)












## get 60 dpd 15mob for R3
bad_flag_R3 <- trades_melt_subset %>% filter((product == 'C') & (type %in% c('N','')) &
                                               # (account_type %notin% loan_notation_PV_New) &
                                               (mob_post_open >= 0) &
                                               (mob_post_open <= mob_threshold) &
                                               (application_no %in% app_ids_with_required_data) &
                                               (application_no %in% unique((rejects_PV_New %>% filter(tag=='R3'))$application_no)))


bad_flag_R3$bad_loan <- ifelse(bad_flag_R3$dpd_num >= dpd_threshold,1,0)
bad_flag_R3 <- bad_flag_R3 %>% group_by(application_no) %>% summarise(bad_loan = max(bad_loan,na.rm=T))



## get R2 90, 150 dpd
flag_90_150_dpd_R3 <- trades_melt_subset %>% filter((product == 'C') & (type %in% c('N','')) &
                                                      # (account_type %notin% loan_notation_PV_New) &
                                                      (mob_post_open >= 0) &
                                                      # (mob_post_open <= mob_threshold) &
                                                      (application_no %in% app_ids_with_required_data) &
                                                      (application_no %in% unique((rejects_PV_New %>% filter(tag=='R3'))$application_no)))

flag_90_150_dpd_R3$ever_90dpd <- ifelse(flag_90_150_dpd_R3$dpd_num >= 90,1,0)
flag_90_150_dpd_R3$ever_150dpd <- ifelse(flag_90_150_dpd_R3$dpd_num >= 150,1,0)
flag_90_150_dpd_R3 <- flag_90_150_dpd_R3 %>% group_by(application_no) %>% summarise(ever_90dpd = max(ever_90dpd,na.rm=T),
                                                                                    ever_150dpd = max(ever_150dpd,na.rm=T))
## join to get 60 dpd 150 mob, 90 dpd, 150 dpd in one dataframe
bad_flag_R3 <- inner_join(bad_flag_R3,flag_90_150_dpd_R3,by='application_no')
rm(flag_90_150_dpd_R3)


## join bad tags with R2
reject_data_PV_New_R3 <- inner_join(rejects_PV_New %>% filter(tag=='R3'), bad_flag_R3, by = 'application_no')

rm(bad_flag_R3)



















#####################################################################################################################
## get bad_flags for rejects

reject_data_PV_New <- rbind(reject_data_PV_New_R1,reject_data_PV_New_R2)
reject_data_PV_New <- rbind(reject_data_PV_New,reject_data_PV_New_R3)
reject_data_PV_New <- data.frame(rbindlist(l=list(reject_data_PV_New,rejects_PV_New %>% filter(tag == 'R4')), use.names = T, fill = T))

rm(reject_data_PV_New_R1,reject_data_PV_New_R2,reject_data_PV_New_R3)


assert_data_non_empty(reject_data_PV_New)
save(reject_data_PV_New,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "reject_data_PV_New.rdata"
     )
)

rm(reject_data_PV_New,rejects_PV_New,trades_melt_subset)










#######################################################################################################################################
####################### PV Used #######################################################################


load_rdata_intermediate("reject_data//reject_bureau_tradelines.rdata")

load_rdata_intermediate("reject_data//reject_bureau_tradelines_melt.rdata")
trades_melt$mob_post_open <- trades_melt$index_dpd_month_adjusted - trades_melt$index_account_open_date


################################################################################################################

## Tag rejects in PV Used

post_app_months_PV_Used <- 6
window_start <- 0
window_end <- 30 * post_app_months_PV_Used
loan_notation_PV_Used <- c(32)
sanction_amount_PV_Used <- 650000

## Tag R1
rejects_PV_Used_R1 <- trades_clean %>% filter((time_diff > window_start) & 
                                               (time_diff <= window_end) & 
                                               (account_type %in% loan_notation_PV_Used) &
                                               (product == 'C') & (type %in% c('U')))

rejects_PV_Used_R1 <- distinct(rejects_PV_Used_R1 %>% dplyr::select(application_no))
rejects_PV_Used_R1$tag <- 'R1'


## Tag R2
rejects_PV_Used_R2 <- trades_clean %>% filter((time_diff > window_start) & 
                                               (time_diff <= window_end) & 
                                               (account_type %notin% loan_notation_PV_Used) &
                                               (product == 'C') & (type %in% c('U')) &
                                               (high_credit_sanctioned_amount <= sanction_amount_PV_Used) &
                                               (application_no %notin% unique(rejects_PV_Used_R1$application_no)))

rejects_PV_Used_R2 <- distinct(rejects_PV_Used_R2 %>% dplyr::select(application_no))
rejects_PV_Used_R2$tag <- 'R2'


## Tag R3
rejects_PV_Used_R3 <- trades_clean %>% filter((time_diff <= window_start)  &
                                                (product == 'C') & (type %in% c('U')) &
                                           (live_loan == 1) &
                                           (application_no %notin% unique(rejects_PV_Used_R1$application_no))  &
                                           (application_no %notin% unique(rejects_PV_Used_R2$application_no)) )

rejects_PV_Used_R3 <- distinct(rejects_PV_Used_R3 %>% dplyr::select(application_no))
rejects_PV_Used_R3$tag <- 'R3'


## Tag R4
rejects_PV_Used_R4 <- trades_clean %>% filter((time_diff <= window_start)  &
                                                (product == 'C') & (type %in% c('U')) &
                                           (live_loan == 0) &
                                           (application_no %notin% unique(rejects_PV_Used_R1$application_no))  &
                                           (application_no %notin% unique(rejects_PV_Used_R2$application_no)) &
                                           (application_no %notin% unique(rejects_PV_Used_R3$application_no)))

rejects_PV_Used_R4 <- distinct(rejects_PV_Used_R4 %>% dplyr::select(application_no))
rejects_PV_Used_R4$tag <- 'R4'

## combine all rejects 
rejects_PV_Used <- data.frame(rbindlist(l=list(rejects_PV_Used_R1,
                                          rejects_PV_Used_R2,
                                          rejects_PV_Used_R3,
                                          rejects_PV_Used_R4)))

# rejects_PV_Used <- data.frame(rbindlist(l=list(rejects_PV_Used_R1,
#                                               rejects_PV_Used_R2)))

rm(rejects_PV_Used_R1,
   rejects_PV_Used_R2,
   rejects_PV_Used_R3,
   rejects_PV_Used_R4)


# rm(rejects_PV_Used_R1,
#    rejects_PV_Used_R2)

##################################################################################################

## tag bad loans
dpd_threshold <- 60
mob_threshold <- 15

## filter till Mar 2020 (remove covid period)
trades_melt_subset <- trades_melt %>% filter(index_dpd_month_adjusted <= 363)
trades_melt_subset <- trades_melt_subset %>% filter(account_date_opened >= application_date)


no_of_mob <- trades_melt_subset %>% group_by(application_no) %>% summarise(mob_count = length(unique(mob_post_open)))
app_ids_with_required_data <- unique((no_of_mob %>% filter(mob_count >= mob_threshold))$application_no)


## get R1 60 dpd in 15mob
bad_flag_R1 <- trades_melt_subset %>% filter((product == 'C') & (type == 'U') &
                                               (account_type %in% loan_notation_PV_Used) &
                                               (mob_post_open >= 0) &
                                               (mob_post_open <= mob_threshold) &
                                               # (application_no %in% app_ids_with_required_data) &
                                               (application_no %in% unique((rejects_PV_Used %>% filter(tag=='R1'))$application_no)))


bad_flag_R1$bad_loan <- ifelse(bad_flag_R1$dpd_num >= dpd_threshold,1,0)
bad_flag_R1 <- bad_flag_R1 %>% group_by(application_no) %>% summarise(bad_loan = max(bad_loan,na.rm=T))

## get R1 90, 150 dpd
flag_90_150_dpd_R1 <- trades_melt_subset %>% filter((product == 'C') & (type == 'U') &
                                                      (account_type %in% loan_notation_PV_Used) &
                                                      (mob_post_open >= 0) &
                                                      # (mob_post_open <= mob_threshold) &
                                                     # (application_no %in% app_ids_with_required_data) &
                                                      (application_no %in% unique((rejects_PV_Used %>% filter(tag=='R1'))$application_no)))

flag_90_150_dpd_R1$ever_90dpd <- ifelse(flag_90_150_dpd_R1$dpd_num >= 90,1,0)
flag_90_150_dpd_R1$ever_150dpd <- ifelse(flag_90_150_dpd_R1$dpd_num >= 150,1,0)
flag_90_150_dpd_R1 <- flag_90_150_dpd_R1 %>% group_by(application_no) %>% summarise(ever_90dpd = max(ever_90dpd,na.rm=T),
                                                                                    ever_150dpd = max(ever_150dpd,na.rm=T))
## join to get 60 dpd 150 mob, 90 dpd, 150 dpd in one dataframe
bad_flag_R1 <- inner_join(bad_flag_R1,flag_90_150_dpd_R1,by='application_no')
rm(flag_90_150_dpd_R1)


## join bad tags with R1
reject_data_PV_Used_R1 <- inner_join(rejects_PV_Used %>% filter(tag=='R1'), bad_flag_R1, by = 'application_no')

rm(bad_flag_R1)








## get 60 dpd 15mob for R2
bad_flag_R2 <- trades_melt_subset %>% filter((product == 'C') & (type == 'U') &
                                               (account_type %notin% loan_notation_PV_Used) &
                                               (mob_post_open >= 0) &
                                               (mob_post_open <= mob_threshold) &
                                               # (application_no %in% app_ids_with_required_data) &
                                               (application_no %in% unique((rejects_PV_Used %>% filter(tag=='R2'))$application_no)))


bad_flag_R2$bad_loan <- ifelse(bad_flag_R2$dpd_num >= dpd_threshold,1,0)
bad_flag_R2 <- bad_flag_R2 %>% group_by(application_no) %>% summarise(bad_loan = max(bad_loan,na.rm=T))



## get R2 90, 150 dpd
flag_90_150_dpd_R2 <- trades_melt_subset %>% filter((product == 'C') & (type == 'U') &
                                                      (account_type %notin% loan_notation_PV_Used) &
                                                      (mob_post_open >= 0) &
                                                      # (mob_post_open <= mob_threshold) &
                                                    #  (application_no %in% app_ids_with_required_data) &
                                                      (application_no %in% unique((rejects_PV_Used %>% filter(tag=='R2'))$application_no)))

flag_90_150_dpd_R2$ever_90dpd <- ifelse(flag_90_150_dpd_R2$dpd_num >= 90,1,0)
flag_90_150_dpd_R2$ever_150dpd <- ifelse(flag_90_150_dpd_R2$dpd_num >= 150,1,0)
flag_90_150_dpd_R2 <- flag_90_150_dpd_R2 %>% group_by(application_no) %>% summarise(ever_90dpd = max(ever_90dpd,na.rm=T),
                                                                                    ever_150dpd = max(ever_150dpd,na.rm=T))
## join to get 60 dpd 150 mob, 90 dpd, 150 dpd in one dataframe
bad_flag_R2 <- inner_join(bad_flag_R2,flag_90_150_dpd_R2,by='application_no')
rm(flag_90_150_dpd_R2)


## join bad tags with R2
reject_data_PV_Used_R2 <- inner_join(rejects_PV_Used %>% filter(tag=='R2'), bad_flag_R2, by = 'application_no')

rm(bad_flag_R2)













## get 60 dpd 15mob for R3
bad_flag_R3 <- trades_melt_subset %>% filter((product == 'C') & (type == 'U') &
                                              # (account_type %notin% loan_notation_PV_Used) &
                                               (mob_post_open >= 0) &
                                               (mob_post_open <= mob_threshold) &
                                               # (application_no %in% app_ids_with_required_data) &
                                               (application_no %in% unique((rejects_PV_Used %>% filter(tag=='R3'))$application_no)))


bad_flag_R3$bad_loan <- ifelse(bad_flag_R3$dpd_num >= dpd_threshold,1,0)
bad_flag_R3 <- bad_flag_R3 %>% group_by(application_no) %>% summarise(bad_loan = max(bad_loan,na.rm=T))



## get R2 90, 150 dpd
flag_90_150_dpd_R3 <- trades_melt_subset %>% filter((product == 'C') & (type == 'U') &
                                                     # (account_type %notin% loan_notation_PV_Used) &
                                                      (mob_post_open >= 0) &
                                                      # (mob_post_open <= mob_threshold) &
                                                      #  (application_no %in% app_ids_with_required_data) &
                                                      (application_no %in% unique((rejects_PV_Used %>% filter(tag=='R3'))$application_no)))

flag_90_150_dpd_R3$ever_90dpd <- ifelse(flag_90_150_dpd_R3$dpd_num >= 90,1,0)
flag_90_150_dpd_R3$ever_150dpd <- ifelse(flag_90_150_dpd_R3$dpd_num >= 150,1,0)
flag_90_150_dpd_R3 <- flag_90_150_dpd_R3 %>% group_by(application_no) %>% summarise(ever_90dpd = max(ever_90dpd,na.rm=T),
                                                                                    ever_150dpd = max(ever_150dpd,na.rm=T))
## join to get 60 dpd 150 mob, 90 dpd, 150 dpd in one dataframe
bad_flag_R3 <- inner_join(bad_flag_R3,flag_90_150_dpd_R3,by='application_no')
rm(flag_90_150_dpd_R3)


## join bad tags with R2
reject_data_PV_Used_R3 <- inner_join(rejects_PV_Used %>% filter(tag=='R3'), bad_flag_R3, by = 'application_no')

rm(bad_flag_R3)





#####################################################################################################################
## get bad_flags for rejects

reject_data_PV_Used <- rbind(reject_data_PV_Used_R1,reject_data_PV_Used_R2)
reject_data_PV_Used <- rbind(reject_data_PV_Used,reject_data_PV_Used_R3)
reject_data_PV_Used <- data.frame(rbindlist(l=list(reject_data_PV_Used,rejects_PV_Used %>% filter(tag == 'R4')), use.names = T, fill = T))

rm(reject_data_PV_Used_R1,reject_data_PV_Used_R2,reject_data_PV_Used_R3)


assert_data_non_empty(reject_data_PV_Used)
save(reject_data_PV_Used,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "reject_data_PV_Used.rdata"
     )
)

rm(list=ls())




### 2W New
################################################################################################################################
#####
load_rdata_intermediate("reject_data//reject_bureau_tradelines.rdata")

load_rdata_intermediate("reject_data//reject_bureau_tradelines_melt.rdata")
trades_melt$mob_post_open <- trades_melt$index_dpd_month_adjusted - trades_melt$index_account_open_date

################################################################################################################

## Tag rejects in 2W

post_app_months_2W <- 3
window_start <- 0
window_end <- 30 * post_app_months_2W
loan_notation_2W <- 'TW'
sanction_amount_2W <- 100000

## Tag R1
rejects_2W_R1 <- trades_clean %>% filter((time_diff > window_start) & 
                                           (time_diff <= window_end) & 
                                           (loan_notation == loan_notation_2W) &
                                           (product == 'H') & (type %in% c('R','U'))
)

rejects_2W_R1 <- distinct(rejects_2W_R1 %>% dplyr::select(application_no))
rejects_2W_R1$tag <- 'R1'


## Tag R2
rejects_2W_R2 <- trades_clean %>% filter((time_diff > window_start) & 
                                           (time_diff <= window_end) & 
                                           (loan_notation != loan_notation_2W) &
                                           (product == 'H') & (type %in% c('R','U')) &
                                           (high_credit_sanctioned_amount <= sanction_amount_2W) &
                                           (application_no %notin% unique(rejects_2W_R1$application_no)))

rejects_2W_R2 <- distinct(rejects_2W_R2 %>% dplyr::select(application_no))
rejects_2W_R2$tag <- 'R2'


## Tag R3
rejects_2W_R3 <- trades_clean %>% filter((time_diff <= window_start)  &
                                           (product == 'H') & (type %in% c('R','U')) &
                                           (live_loan == 1) &
                                           (application_no %notin% unique(rejects_2W_R1$application_no))  &
                                           (application_no %notin% unique(rejects_2W_R2$application_no)) )

rejects_2W_R3 <- distinct(rejects_2W_R3 %>% dplyr::select(application_no))
rejects_2W_R3$tag <- 'R3'


## Tag R4
rejects_2W_R4 <- trades_clean %>% filter((time_diff <= window_start)  &
                                           (product == 'H') & (type %in% c('R','U')) &
                                           (live_loan == 0) &
                                           (application_no %notin% unique(rejects_2W_R1$application_no))  &
                                           (application_no %notin% unique(rejects_2W_R2$application_no)) &
                                           (application_no %notin% unique(rejects_2W_R3$application_no)))

rejects_2W_R4 <- distinct(rejects_2W_R4 %>% dplyr::select(application_no))
rejects_2W_R4$tag <- 'R4'

## combine all rejects 
rejects_2W <- data.frame(rbindlist(l=list(rejects_2W_R1,
                                          rejects_2W_R2,
                                          rejects_2W_R3,
                                          rejects_2W_R4)))

# rejects_2W <- data.frame(rbindlist(l=list(rejects_2W_R1,
#                                           rejects_2W_R2)))

rm(rejects_2W_R1,
   rejects_2W_R2,
   rejects_2W_R3,
   rejects_2W_R4)


# rm(rejects_2W_R1,
#    rejects_2W_R2)

##################################################################################################

## tag bad loans
dpd_threshold <- 60
mob_threshold <- 15

## filter till Mar 2020 (remove covid period)
trades_melt_subset <- trades_melt %>% filter(index_dpd_month_adjusted <= 363)
trades_melt_subset <- trades_melt_subset %>% filter(account_date_opened >= application_date)


no_of_mob <- trades_melt_subset %>% group_by(application_no) %>% summarise(mob_count = length(unique(mob_post_open)))
app_ids_with_required_data <- unique((no_of_mob %>% filter(mob_count >= mob_threshold))$application_no)

################- R1 ###################################
## get R1 60 dpd in 15mob
bad_flag_R1 <- trades_melt_subset %>% filter((product == 'H') & (type %in% c('R','U')) &
                                               (loan_notation == loan_notation_2W) &
                                               (mob_post_open >= 0) &
                                               (mob_post_open <= mob_threshold) &
                                               # (application_no %in% app_ids_with_required_data) &
                                               (application_no %in% unique((rejects_2W %>% filter(tag=='R1'))$application_no)))


bad_flag_R1$bad_loan <- ifelse(bad_flag_R1$dpd_num >= dpd_threshold,1,0)
bad_flag_R1 <- bad_flag_R1 %>% group_by(application_no) %>% summarise(bad_loan = max(bad_loan,na.rm=T))

## get R1 90, 150 dpd
flag_90_150_dpd_R1 <- trades_melt_subset %>% filter((product == 'H') & (type %in% c('R','U')) &
                                                      (loan_notation == loan_notation_2W) &
                                                      (mob_post_open >= 0) &
                                                      # (mob_post_open <= mob_threshold) &
                                                      # (application_no %in% app_ids_with_required_data) &
                                                      (application_no %in% unique((rejects_2W %>% filter(tag=='R1'))$application_no)))

flag_90_150_dpd_R1$ever_90dpd <- ifelse(flag_90_150_dpd_R1$dpd_num >= 90,1,0)
flag_90_150_dpd_R1$ever_150dpd <- ifelse(flag_90_150_dpd_R1$dpd_num >= 150,1,0)
flag_90_150_dpd_R1 <- flag_90_150_dpd_R1 %>% group_by(application_no) %>% summarise(ever_90dpd = max(ever_90dpd,na.rm=T),
                                                                                    ever_150dpd = max(ever_150dpd,na.rm=T))
## join to get 60 dpd 150 mob, 90 dpd, 150 dpd in one dataframe
bad_flag_R1 <- inner_join(bad_flag_R1,flag_90_150_dpd_R1,by='application_no')
rm(flag_90_150_dpd_R1)


## join bad tags with R1
reject_data_2W_R1 <- inner_join(rejects_2W %>% filter(tag=='R1'), bad_flag_R1, by = 'application_no')

rm(bad_flag_R1)







################- R2 ###################################

## get 60 dpd 15mob for R2
bad_flag_R2 <- trades_melt_subset %>% filter((product == 'H') & 
                                               (loan_notation != loan_notation_2W) &
                                               (mob_post_open >= 0) &
                                               # (mob_post_open <= mob_threshold) &
                                               # (application_no %in% app_ids_with_required_data) &
                                               (application_no %in% unique((rejects_2W %>% filter(tag=='R2'))$application_no)))


bad_flag_R2$bad_loan <- ifelse(bad_flag_R2$dpd_num >= dpd_threshold,1,0)
bad_flag_R2 <- bad_flag_R2 %>% group_by(application_no) %>% summarise(bad_loan = max(bad_loan,na.rm=T))



## get R2 90, 150 dpd
flag_90_150_dpd_R2 <- trades_melt_subset %>% filter((product == 'H') & 
                                                      (loan_notation != loan_notation_2W) &
                                                      (mob_post_open >= 0) &
                                                      # (mob_post_open <= mob_threshold) &
                                                      # (application_no %in% app_ids_with_required_data) &
                                                      (application_no %in% unique((rejects_2W %>% filter(tag=='R2'))$application_no)))

flag_90_150_dpd_R2$ever_90dpd <- ifelse(flag_90_150_dpd_R2$dpd_num >= 90,1,0)
flag_90_150_dpd_R2$ever_150dpd <- ifelse(flag_90_150_dpd_R2$dpd_num >= 150,1,0)
flag_90_150_dpd_R2 <- flag_90_150_dpd_R2 %>% group_by(application_no) %>% summarise(ever_90dpd = max(ever_90dpd,na.rm=T),
                                                                                    ever_150dpd = max(ever_150dpd,na.rm=T))
## join to get 60 dpd 150 mob, 90 dpd, 150 dpd in one dataframe
bad_flag_R2 <- inner_join(bad_flag_R2,flag_90_150_dpd_R2,by='application_no')
rm(flag_90_150_dpd_R2)


## join bad tags with R2
reject_data_2W_R2 <- inner_join(rejects_2W %>% filter(tag=='R2'), bad_flag_R2, by = 'application_no')

rm(bad_flag_R2)



################- R3 ###################################

## get 60 dpd 15mob for R3
bad_flag_R3 <- trades_melt_subset %>% filter((product == 'H') & #(type %in% c('N')) &
                                               (mob_post_open >= 0) &
                                               (mob_post_open <= mob_threshold) &
                                               # (application_no %in% app_ids_with_required_data) &
                                               (application_no %in% unique((rejects_2W %>% filter(tag=='R3'))$application_no)))


bad_flag_R3$bad_loan <- ifelse(bad_flag_R3$dpd_num >= dpd_threshold,1,0)
bad_flag_R3 <- bad_flag_R3 %>% group_by(application_no) %>% summarise(bad_loan = max(bad_loan,na.rm=T))



## get R2 90, 150 dpd
flag_90_150_dpd_R3 <- trades_melt_subset %>% filter((product == 'H') & 
                                                      # (loan_notation != loan_notation_2W) &
                                                      (mob_post_open >= 0) &
                                                      # (mob_post_open <= mob_threshold) &
                                                      # (application_no %in% app_ids_with_required_data) &
                                                      (application_no %in% unique((rejects_2W %>% filter(tag=='R3'))$application_no)))

flag_90_150_dpd_R3$ever_90dpd <- ifelse(flag_90_150_dpd_R3$dpd_num >= 90,1,0)
flag_90_150_dpd_R3$ever_150dpd <- ifelse(flag_90_150_dpd_R3$dpd_num >= 150,1,0)
flag_90_150_dpd_R3 <- flag_90_150_dpd_R3 %>% group_by(application_no) %>% summarise(ever_90dpd = max(ever_90dpd,na.rm=T),
                                                                                    ever_150dpd = max(ever_150dpd,na.rm=T))
## join to get 60 dpd 150 mob, 90 dpd, 150 dpd in one dataframe
bad_flag_R3 <- inner_join(bad_flag_R3,flag_90_150_dpd_R3,by='application_no')
rm(flag_90_150_dpd_R3)


## join bad tags with R2
reject_data_2W_R3 <- inner_join(rejects_2W %>% filter(tag=='R3'), bad_flag_R3, by = 'application_no')

rm(bad_flag_R3)








#####################################################################################################################
## get bad_flags for rejects

reject_data_2W <- rbind(reject_data_2W_R1,reject_data_2W_R2)
reject_data_2W <- rbind(reject_data_2W,reject_data_2W_R3)
reject_data_2W <- data.frame(rbindlist(l=list(reject_data_2W,rejects_2W %>% filter(tag == 'R4')), use.names = T, fill = T))


rm(reject_data_2W_R1,reject_data_2W_R2,reject_data_2W_R3)


assert_data_non_empty(reject_data_2W)
save(reject_data_2W,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "reject_data_2W_RF.rdata"
     )
)
save(reject_data_2W,
     file = file.path("data//intermediate//reject_data//reject_data_2W_RF.rdata"
     )
)
rm(list=ls())









