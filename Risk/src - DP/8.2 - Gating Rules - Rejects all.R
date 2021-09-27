############################################################################################
################## 8 - Gating Rules  ######################################################
############################################################################################


## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)


## 1. Define constants -----------------------------------------------------------------------

## 1.1 dataframe for month - day mapping
month_day_mapping <- data.frame(month = c(1,3,6,12,18,24),
                                days = c(30, 90, 180, 365, 545, 730))

## 1.2 vector of all months
months_vector <- c(1,3,6,12,18,24)

## 1.3 vector of all loan products
loans_vector <- c('HL','PL','LAP','AL','BL','TW','Tractor','CV','CE','Gold')

## 1.4 vector for all DPD
dpd_vector <- c(30,60,90)


## 1.5 Enquiry buffer days
buffer <- 7

## 1.6 TAT for product
tat_2W_new <- 0
tat_PV_new <- 0
tat_PV_used <- 7


## 2. Create Product Ownership Rules --------------------------------------------------------

## 2.1 load tradeline data
load_rdata_intermediate("reject_data//pre_app_bureau_tradelines_rejects1.rdata")


final_output <- distinct(trades_clean %>% dplyr::select(application_no))


## 2.2 create live loans flag
trades_clean$live_loan <- ifelse((trades_clean$loan_notation != 'CC' &
                                    is.na(trades_clean$account_date_closed) &
                                    trades_clean$current_balance > 0) |
                                       (trades_clean$loan_notation == 'CC' & 
                                          is.na(trades_clean$account_date_closed) &
                                          (trades_clean$current_balance > 0 | trades_clean$current_balance == 0)
                                       ),
                                     1,0)

## 2.3 calculate days between date opened & date on which cibil report was pulled
trades_clean$days <- as.numeric(trades_clean$cibil_reported_date - trades_clean$account_date_opened)


## 2.4 create tradelines subset for adding rules
tradelines <- trades_clean %>% dplyr::select(application_no,
                                             applicant_id,
                                             product,
                                             type,
                                             application_date,
                                             year_mon,
                                             high_credit_sanctioned_amount,
                                             overdue_amount,
                                             account_type,
                                             account_date_opened,
                                             cibil_reported_date,
                                             loan_identifier,
                                             payment_history_start_date,
                                             payment_history_end_date,
                                             current_balance,
                                             payment_history,
                                             loan_description,
                                             loan_notation,
                                             unsecured_flag,
                                             account_date_closed,
                                             yearmon_reported_date,
                                             yearmon_account_open_date,
                                             yearmon_payment_start_date,
                                             live_loan,
                                             days)


## 2.5 create rules function - loans taken in last x months
create_POR_loans_in_x_months <- function(tradelines, months_vector){
  for(month_value in months_vector){
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    tradelines[,paste0('Rule_PO_months_',month_value)] <- ifelse(tradelines$days > 0 & 
                                                                   tradelines$days <= days_cutoff, 1, 0)
  }
  return(tradelines)
}


## 2.6 create rules function - live loans taken in last x months
create_POR_live_loans_in_x_months <- function(tradelines, months_vector){
  for(month_value in months_vector){
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    tradelines[,paste0('Rule_PO_months_',month_value, '_live')] <- ifelse(tradelines$days > 0 & 
                                                                            tradelines$days <= days_cutoff & 
                                                                            tradelines$live_loan == 1, 1, 0)
  }
  return(tradelines)
}


## 2.7 create rules function - Specific loans taken in last x months
create_POR_product_loans_in_x_months <- function(tradelines, months_vector, loans_vector){
  for(month_value in months_vector){
    for(loan_value in loans_vector){
      days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
      
      tradelines[,paste0('Rule_PO_months_',month_value, '_',loan_value)] <- ifelse(tradelines$days > 0 & 
                                                                                     tradelines$days <= days_cutoff & 
                                                                                     tradelines$loan_notation == loan_value, 1, 0)
    }
    
  }
  return(tradelines)
}


## 2.8 create rules function - Specific live loans taken in last x months
create_POR_product_live_loans_in_x_months <- function(tradelines, months_vector, loans_vector){
  for(month_value in months_vector){
    for(loan_value in loans_vector){
      days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
      
      tradelines[,paste0('Rule_PO_months_',month_value, '_',loan_value, '_live')] <- ifelse(tradelines$days > 0 & 
                                                                                              tradelines$days <= days_cutoff & 
                                                                                              tradelines$loan_notation == loan_value & 
                                                                                              tradelines$live_loan == 1, 1, 0)
    }
    
  }
  return(tradelines)
}


## 2.9 create rules function - unsecured loans taken in last x months
create_POR_unsec_loans_in_x_months <- function(tradelines, months_vector){
  for(month_value in months_vector){
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    tradelines[,paste0('Rule_PO_months_',month_value, '_unsec')] <- ifelse(tradelines$days > 0 & 
                                                                             tradelines$days <= days_cutoff & 
                                                                             tradelines$unsecured_flag == 'unsecured', 1, 0)
  }
  return(tradelines)
}


## 2.10 create rules function - unsecured live loans taken in last x months
create_POR_unsec_live_loans_in_x_months <- function(tradelines, months_vector){
  for(month_value in months_vector){
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    tradelines[,paste0('Rule_PO_months_',month_value, '_live_unsec')] <- ifelse(tradelines$days > 0 & 
                                                                            tradelines$days <= days_cutoff & 
                                                                            tradelines$live_loan == 1 & 
                                                                            tradelines$unsecured_flag == 'unsecured', 1, 0)
  }
  return(tradelines)
}


## 2.11 create rules function - unsecured loans taken in last x months excluding CC, CD
create_POR_unsec_loans_in_x_months_excl_CC_CD <- function(tradelines, months_vector){
  for(month_value in months_vector){
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    tradelines[,paste0('Rule_PO_months_',month_value, '_unsec_excl_CC_CD')] <- ifelse(tradelines$days > 0 &
                                                                                        tradelines$days <= days_cutoff & 
                                                                                        tradelines$unsecured_flag == 'unsecured' &
                                                                                        tradelines$loan_notation %notin% c('CD','CC'), 1, 0)
  }
  return(tradelines)
}




## 2.12 call all rule creation functions
tradelines <- create_POR_loans_in_x_months(tradelines, months_vector)
tradelines <- create_POR_live_loans_in_x_months(tradelines, months_vector)
tradelines <- create_POR_product_loans_in_x_months(tradelines, months_vector, loans_vector)
tradelines <- create_POR_product_live_loans_in_x_months(tradelines, months_vector, loans_vector)
tradelines <- create_POR_unsec_loans_in_x_months(tradelines, months_vector)
tradelines <- create_POR_unsec_live_loans_in_x_months(tradelines, months_vector)
tradelines <- create_POR_unsec_loans_in_x_months_excl_CC_CD(tradelines, months_vector)


## 2.13 subset for required columns
req_cols <- names(tradelines)[grep("Rule_PO",names(tradelines))]

# tradelines <- tradelines %>% dplyr::select(c('deal_no', 'customer_code', 'applicant_id','loan_type', req_cols))
tradelines <- tradelines %>% dplyr::select(c('application_no', 'applicant_id', req_cols))


## 2.14 aggregate rules at deal number level

# rules_PO <- tradelines %>% group_by(deal_no,customer_code,applicant_id,loan_type) %>% summarise(across(everything(), list(sum)))
rules_PO <- tradelines %>% group_by(application_no,applicant_id) %>% summarise(across(everything(), list(sum)))
colnames(rules_PO) <- c('application_no', 'applicant_id', req_cols)
rules_PO <- rules_PO %>% ungroup()


## 2.15 convert sum to flags
for(col in req_cols){
  value_list <- c(1,2,3,4,5)
  for(value in value_list){
    new_col <- paste0(col, "_GE_",value)
    rules_PO[,new_col] <- as.numeric(ifelse(rules_PO[,col] >= value, 1, 0))
  }
}

## 2.16 select only flag columns
new_cols <- colnames(rules_PO)
new_cols <- new_cols[new_cols %notin% req_cols]

rules_PO <- rules_PO %>% dplyr::select(new_cols)


rm(trades_clean, tradelines)


rules_PO_agg <- rules_PO
rules_PO_agg$applicant_id <- NULL

rule_names <- names(rules_PO_agg)[grep("Rule_",names(rules_PO_agg))]

rules_PO_agg <- rules_PO_agg %>% group_by(application_no) %>% summarise(across(everything(), list(max)))

colnames(rules_PO_agg) <- c('application_no', rule_names)




assert_data_non_empty(rules_PO)
save(rules_PO,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "gating_rules",
    #   "2W-New",
       "rules_PO.rdata"
     )
)


assert_data_non_empty(rules_PO_agg)
save(rules_PO_agg,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "gating_rules",
    #   "2W-New",
       "rules_PO_agg.rdata"
     )
)

rm(rules_PO, rules_PO_agg)




## 3. Create delinquency rules ---------------------------------------------------------

## 3.1 load tradeline melt data
load_rdata_intermediate("reject_data//pre_app_trades_melt_rejects.rdata")


## 3.2 create subset of data with required columns
trades_dpd <- trades_melt %>% dplyr::select(application_no,
                                            applicant_id,
                                            product,
                                            type,
                                            application_date,
                                            high_credit_sanctioned_amount,
                                            overdue_amount,
                                            account_type,
                                            account_date_opened,
                                            account_date_closed,
                                            cibil_reported_date,
                                            payment_history_start_date,
                                            payment_history_end_date,
                                            loan_identifier,
                                            current_balance,
                                            yearmon_reported_date,
                                            yearmon_account_open_date,
                                            yearmon_payment_start_date,
                                            loan_notation,
                                            unsecured_flag,
                                            variable,
                                            diff_reported_payment,
                                            dpd_num
                                            ) %>% filter(!is.na(dpd_num))

rm(trades_melt)
gc()

## 3.3 create rules function - x DPD in last y months
create_DR_x_dpd_y_mon <- function(trades_dpd, dpd_vector, months_vector){
  for(dpd_value in dpd_vector){
    for(month_value in months_vector){
      trades_dpd[, paste0('Rule_DR_all_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                           trades_dpd$diff_reported_payment <= month_value & 
                                                                                           trades_dpd$dpd_num >= dpd_value &
                                                                                           trades_dpd$current_balance >= 3000, 1, 0)
    }
  }
  return(trades_dpd)
}


## 3.4 create rules function - x DPD in last y months - non CC, CD
create_DR_x_dpd_y_mon_non_CC_CD <- function(trades_dpd, dpd_vector, months_vector){
  for(dpd_value in dpd_vector){
    for(month_value in months_vector){
      trades_dpd[, paste0('Rule_DR_non_CC_CD_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                           trades_dpd$diff_reported_payment <= month_value & 
                                                                                           trades_dpd$dpd_num >= dpd_value & 
                                                                                             trades_dpd$loan_notation %notin% c('CD','CC') &
                                                                                             trades_dpd$current_balance >= 3000 ,1, 0)
    }
  }
  return(trades_dpd)
}


## 3.5 create rules function - x DPD in last y months - Gold
create_DR_x_dpd_y_mon_specific_loans <- function(trades_dpd, dpd_vector, months_vector){
  for(dpd_value in dpd_vector){
    for(month_value in months_vector){
      # trades_dpd[, paste0('Rule_DR_Gold_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
      #                                                                                            trades_dpd$diff_reported_payment <= month_value & 
      #                                                                                            trades_dpd$dpd_num >= dpd_value & 
      #                                                                                            trades_dpd$account_type %in% c(7) &
      #                                                                                            trades_dpd$current_balance >= 3000 ,1, 0)
      # 
      # trades_dpd[, paste0('Rule_DR_Education_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
      #                                                                                       trades_dpd$diff_reported_payment <= month_value & 
      #                                                                                       trades_dpd$dpd_num >= dpd_value & 
      #                                                                                       trades_dpd$account_type %in% c(8) &
      #                                                                                       trades_dpd$current_balance >= 3000 ,1, 0)
      # 
      # trades_dpd[, paste0('Rule_DR_Agri_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
      #                                                                                       trades_dpd$diff_reported_payment <= month_value & 
      #                                                                                       trades_dpd$dpd_num >= dpd_value & 
      #                                                                                       trades_dpd$account_type %in% c(36, 53,57) &
      #                                                                                       trades_dpd$current_balance >= 3000 ,1, 0)
      # 
      # trades_dpd[, paste0('Rule_DR_Mudra_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
      #                                                                                       trades_dpd$diff_reported_payment <= month_value & 
      #                                                                                       trades_dpd$dpd_num >= dpd_value & 
      #                                                                                       trades_dpd$account_type %in% c(39) &
      #                                                                                       trades_dpd$current_balance >= 3000 ,1, 0)
      # 
      # trades_dpd[, paste0('Rule_DR_CC_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
      #                                                                                        trades_dpd$diff_reported_payment <= month_value & 
      #                                                                                        trades_dpd$dpd_num >= dpd_value & 
      #                                                                                        trades_dpd$account_type %in% c(10,31,35,36) &
      #                                                                                        trades_dpd$current_balance >= 3000 ,1, 0)
      # 
      # trades_dpd[, paste0('Rule_DR_non_Gold_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
      #                                                                                       trades_dpd$diff_reported_payment <= month_value & 
      #                                                                                       trades_dpd$dpd_num >= dpd_value & 
      #                                                                                       trades_dpd$account_type %notin% c(7) &
      #                                                                                       trades_dpd$current_balance >= 3000 ,1, 0)
      # 
      # trades_dpd[, paste0('Rule_DR_non_Education_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
      #                                                                                             trades_dpd$diff_reported_payment <= month_value & 
      #                                                                                             trades_dpd$dpd_num >= dpd_value & 
      #                                                                                             trades_dpd$account_type %notin% c(8) &
      #                                                                                             trades_dpd$current_balance >= 3000 ,1, 0)
      # 
      # trades_dpd[, paste0('Rule_DR_non_Agri_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
      #                                                                                      trades_dpd$diff_reported_payment <= month_value & 
      #                                                                                      trades_dpd$dpd_num >= dpd_value & 
      #                                                                                      trades_dpd$account_type %notin% c(36, 53,57) &
      #                                                                                      trades_dpd$current_balance >= 3000 ,1, 0)
      # 
      # trades_dpd[, paste0('Rule_DR_non_Mudra_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
      #                                                                                          trades_dpd$diff_reported_payment <= month_value & 
      #                                                                                          trades_dpd$dpd_num >= dpd_value & 
      #                                                                                          trades_dpd$account_type %notin% c(39) &
      #                                                                                          trades_dpd$current_balance >= 3000 ,1, 0)
      # 
      # trades_dpd[, paste0('Rule_DR_non_CC_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
      #                                                                                            trades_dpd$diff_reported_payment <= month_value & 
      #                                                                                            trades_dpd$dpd_num >= dpd_value & 
      #                                                                                            trades_dpd$account_type %notin% c(10,31,35,36) &
      #                                                                                            trades_dpd$current_balance >= 3000 ,1, 0)
      # 
      
      trades_dpd[, paste0('Rule_DR_non_Gold_Edu_Agri_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                              trades_dpd$diff_reported_payment <= month_value & 
                                                                                              trades_dpd$dpd_num >= dpd_value & 
                                                                                              trades_dpd$account_type %notin% c(7, 8, 36, 53, 57) &
                                                                                              trades_dpd$current_balance >= 3000 ,1, 0)
      
      trades_dpd[, paste0('Rule_DR_non_Gold_Edu_Agri_CC_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                                         trades_dpd$diff_reported_payment <= month_value & 
                                                                                                         trades_dpd$dpd_num >= dpd_value & 
                                                                                                         trades_dpd$account_type %notin% c(7, 8, 36, 53, 57, 10,31,35) &
                                                                                                         trades_dpd$current_balance >= 3000 ,1, 0)
      
      
    }
  }
  return(trades_dpd)
}


## 3.5 call all rule creation functions
trades_dpd <- create_DR_x_dpd_y_mon(trades_dpd, dpd_vector, months_vector)
trades_dpd <- create_DR_x_dpd_y_mon_non_CC_CD(trades_dpd, dpd_vector, months_vector)
trades_dpd <- create_DR_x_dpd_y_mon_specific_loans(trades_dpd, dpd_vector, months_vector)

gc()
## 3.6 subset for required columns
req_cols <- names(trades_dpd)[grep("Rule_DR",names(trades_dpd))]
trades_dpd <- trades_dpd %>% dplyr::select(c('application_no', 'applicant_id', req_cols))

gc()

rules_numbers <- c(1:length(req_cols))
blocks_temp <- split(rules_numbers, ceiling(seq_along(rules_numbers)/50))

rules_DL <- distinct(trades_dpd %>% dplyr::select(application_no,applicant_id))

for(temp_block in blocks_temp){
  temp_rules <- req_cols[min(temp_block) : max(temp_block)]
  temp_agg <- trades_dpd %>% dplyr::select(c('application_no','applicant_id',temp_rules))
  temp_agg <- temp_agg %>% group_by(application_no,applicant_id) %>% summarise(across(everything(), list(sum)))
  colnames(temp_agg) <- c('application_no','applicant_id', temp_rules)
  temp_agg <- temp_agg %>% ungroup()
  
  rules_DL <- left_join(rules_DL,temp_agg,by = c('application_no' = 'application_no',
                                                              'applicant_id' = 'applicant_id'))
  gc()
}

rm(temp_agg,temp_rules,temp_block,blocks_temp,rules_numbers)
gc()

rm(trades_dpd)
gc()




## 3.8 convert sum to flags
for(col in req_cols){
  value_list <- c(1,2,3,4,5)
  for(value in value_list){
    new_col <- paste0(col, "_GE_",value)
    rules_DL[,new_col] <- as.numeric(ifelse(rules_DL[,col] >= value, 1, 0))
  }
  
}

## 3.9 select only flag columns
new_cols <- colnames(rules_DL)
new_cols <- new_cols[new_cols %notin% req_cols]

rules_DL <- rules_DL %>% dplyr::select(new_cols)





rules_DL_agg <- rules_DL
rules_DL_agg$customer_code <- NULL
rules_DL_agg$applicant_id <- NULL


rule_names <- names(rules_DL_agg)[grep("Rule_",names(rules_DL_agg))]

rules_numbers <- c(1:length(rule_names))
blocks_temp <- split(rules_numbers, ceiling(seq_along(rules_numbers)/50))

temp_output_df <- distinct(rules_DL_agg %>% dplyr::select(application_no))

for(temp_block in blocks_temp){
  temp_rules <- rule_names[min(temp_block) : max(temp_block)]
  temp_agg <- rules_DL_agg %>% dplyr::select(c('application_no',temp_rules))
  temp_agg <- temp_agg %>% group_by(application_no) %>% summarise(across(everything(), list(max)))
  colnames(temp_agg) <- c('application_no', temp_rules)
  temp_agg <- temp_agg %>% ungroup()
  
  temp_output_df <- left_join(temp_output_df,temp_agg,by = c('application_no' = 'application_no'))
  
  gc()
  
}

rm(temp_agg,temp_rules,temp_block,blocks_temp,rules_numbers)
gc()

rules_DL_agg <- temp_output_df
# rules_DL_agg <- rules_DL_agg %>% group_by(deal_no,loan_type) %>% summarise(across(everything(), list(max)))

# colnames(rules_DL_agg) <- c('deal_no', 'loan_type', rule_names)
rm(temp_output_df)
gc()



assert_data_non_empty(rules_DL)
save(rules_DL,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "gating_rules",
  #     "2W-New",
       "rules_DL.rdata"
     )
)


assert_data_non_empty(rules_DL_agg)
save(rules_DL_agg,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "gating_rules",
  #     "2W-New",
       "rules_DL_agg.rdata"
     )
)

rm(rules_DL, rules_DL_agg)

gc()

## 4. Create enquiry variables -------------------------------------------------------------

## 4.1 load enquiry data
# load_rdata_intermediate("ADS_data//enquiry_data.rdata")

load_rdata_intermediate("reject_data//pre_app_enquiry_data_rejects.rdata")
enquiry_data <- distinct(enquiry_data)

enquiry_data <- enquiry_data %>% filter(!is.na(loan_notation))



## 4.2 create rules function 
create_ER_x_enq_y_mon <- function(enquiry_data, months_vector, buffer){
  
  # get only deal number columns
  base_output <- distinct(enquiry_data %>% dplyr::select(Application_No))
  
  # create buffer
  buffer_2W_new <- buffer + tat_2W_new
  
  # iterate over different months
  for(month_value in months_vector){
    
    # get days cutoff
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    
    enquiry_agg <- enquiry_data %>% filter((days > (0+buffer_2W_new)) & (days <= (days_cutoff+buffer_2W_new))) %>% group_by(Application_No,
                                                                                                                            loan_notation) %>% summarise(enquiry_count = length(unique(enquiry_date)))
    enquiry_agg <- enquiry_agg %>% ungroup()
    
    # change column name
    colnames(enquiry_agg)[colnames(enquiry_agg) == 'enquiry_count'] <- paste0('Rule_EN_enquiry_count_',month_value,'m')
    
    # melt data to get enquiry count column name as a separate column
    enquiry_agg <- melt(enquiry_agg, id.vars = c('Application_No','loan_notation'))
    
    # transpose rows to columns to get count across loan types
    enquiry_agg_dcast <- dcast(Application_No ~ variable+loan_notation,
                               data = enquiry_agg,
                               value.var = "value",
                               sum)
    
    
    # get columns names to calculate overall enquiry count
    enquiry_count_cols <- names(enquiry_agg_dcast)[grep("enquiry_count",names(enquiry_agg_dcast))]
    
    # get column names to calculate enquiry count excluding CC
    enquiry_count_cols_non_CC <- enquiry_count_cols[!grepl("_CC",enquiry_count_cols)]
    
    # get column names to calculate enquiry count excluding CD
    enquiry_count_cols_non_CD <- enquiry_count_cols[!grepl("_CD",enquiry_count_cols)]
    
    # get column names to calculate enquiry count excluding CC & CD
    enquiry_count_cols_non_CC_CD <- enquiry_count_cols[!grepl("_CC",enquiry_count_cols)]
    enquiry_count_cols_non_CC_CD <- enquiry_count_cols_non_CC_CD[!grepl("_CD",enquiry_count_cols_non_CC_CD)]
    
    # create overall enquiry columns
    enquiry_agg_dcast[,paste0('Rule_EN_enquiry_count_',month_value,'m')] <- rowSums(enquiry_agg_dcast[,enquiry_count_cols])
    enquiry_agg_dcast[,paste0('Rule_EN_enquiry_count_',month_value,'m_non_CC')] <- rowSums(enquiry_agg_dcast[,enquiry_count_cols_non_CC])
    enquiry_agg_dcast[,paste0('Rule_EN_enquiry_count_',month_value,'m_non_CD')] <- rowSums(enquiry_agg_dcast[,enquiry_count_cols_non_CD])
    enquiry_agg_dcast[,paste0('Rule_EN_enquiry_count_',month_value,'m_non_CC_CD')] <- rowSums(enquiry_agg_dcast[,enquiry_count_cols_non_CC_CD])
    
    # join with base output
    base_output <- left_join(base_output, enquiry_agg_dcast, by = c('Application_No' = 'Application_No'))
  }
  
  return(data.frame(base_output))
}



## 4.3 call enquiry rules creation function
rules_EN <- create_ER_x_enq_y_mon(enquiry_data, months_vector, buffer)
rules_EN[is.na(rules_EN)] <- 0 


## 4.4 create rules from enquiry variables
req_cols <- names(rules_EN)[grep("Rule_EN_enquiry_count_",names(rules_EN))]

for(col in req_cols){
  value_list <- c(1,3,5,6,7,9)
  for(value in value_list){
    new_col <- paste0(col, "_GE_",value)
    rules_EN[,new_col] <- as.numeric(ifelse(rules_EN[,col] >= value, 1, 0))
  }
}

## 4.5 subset for only rules
new_cols <- colnames(rules_EN)
new_cols <- new_cols[new_cols %notin% req_cols]

rules_EN <- rules_EN %>% dplyr::select(new_cols)

rm(enquiry_data,month_day_mapping)


rules_EN_agg <- rules_EN
rules_EN_agg$customer_code <- NULL
rules_EN_agg$applicant_id <- NULL

rule_names <- names(rules_EN_agg)[grep("Rule_",names(rules_EN_agg))]

rules_EN_agg <- rules_EN_agg %>% group_by(Application_No) %>% summarise(across(everything(), list(max)))

colnames(rules_EN_agg) <- c('application_no', rule_names)




assert_data_non_empty(rules_EN)
save(rules_EN,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "gating_rules",
       # "2W-New",
       "rules_EN.rdata"
     )
)


assert_data_non_empty(rules_EN_agg)
save(rules_EN_agg,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "gating_rules",
       # "2W-New",
       "rules_EN_agg.rdata"
     )
)

rm(rules_EN, rules_EN_agg)









## 5. Combine all rules ------------------------------------------------------------------------------

# load_rdata_intermediate("reject_data//gating_rules//2W-New//rules_PO_agg.rdata")
# load_rdata_intermediate("reject_data//gating_rules//2W-New//rules_DL_agg.rdata")
# load_rdata_intermediate("reject_data//gating_rules//2W-New//rules_EN_agg.rdata")


load_rdata_intermediate("reject_data//gating_rules//rules_PO_agg.rdata")
load_rdata_intermediate("reject_data//gating_rules//rules_DL_agg.rdata")
load_rdata_intermediate("reject_data//gating_rules//rules_EN_agg.rdata")


all_rules <- left_join(final_output,rules_PO_agg, by = c('application_no' = 'application_no'))

all_rules <- left_join(all_rules, rules_DL_agg, by = c('application_no' = 'application_no'))

all_rules <- left_join(all_rules, rules_EN_agg, by = c('application_no' = 'application_no'))

rm(final_output,rules_DL_agg,rules_EN_agg,rules_PO_agg)


all_rules[is.na(all_rules)] <- 0 


## 6. Get bad tagging for rules ----------------------------------------------------------------------
load_rdata_intermediate("reject_data//reject_data_2W.rdata")
load_rdata_intermediate("reject_data//reject_data_PV_New.rdata")
load_rdata_intermediate("reject_data//reject_data_PV_Used.rdata")

reject_data_all <- data.frame(rbindlist(l=list(reject_data_2W,reject_data_PV_New,reject_data_PV_Used)))

# bad_loans$loan_type <- NULL

output <- inner_join(reject_data_all, all_rules, by = 'application_no')


# gating_rules_deal_no_level_reject_2W <- output
# assert_data_non_empty(gating_rules_deal_no_level_reject_2W)

gating_rules_deal_no_level_reject <- output
assert_data_non_empty(gating_rules_deal_no_level_reject)

save(gating_rules_deal_no_level_reject,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "gating_rules",
     #  "2W-New",
       "gating_rules_deal_no_level_reject.rdata"
     )
)

rm(list=ls())
