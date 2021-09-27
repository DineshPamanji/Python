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
tat_2W_RF <- 7


## 2. Create Product Ownership Rules --------------------------------------------------------

## 2.1 load tradeline data

load_rdata_intermediate("ADS_data//bureau_tradelines_2W_RF_modeling.rdata")  # bureau_tradelines_2W_RF_modeling.rdata

# final_output <- distinct(trades_clean %>% dplyr::select(deal_no, Appno, applicant_id, loan_type))
final_output <- distinct(trades_clean %>% dplyr::select(deal_no, loan_type))

# count_custid <- trades_clean %>% group_by(deal_no) %>% summarise(count = length(unique(Appno)))

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
tradelines <- trades_clean %>% dplyr::select(deal_no,
                                             Appno,
                                             applicant_id,
                                             loan_type,
                                             disbursal_date,
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

tradelines <- tradelines %>% dplyr::select(c('deal_no', 'Appno', 'applicant_id','loan_type', req_cols))


## 2.14 aggregate rules at deal number level
rules_PO <- tradelines %>% group_by(deal_no,Appno,applicant_id,loan_type) %>% summarise(across(everything(), list(sum)))
colnames(rules_PO) <- c('deal_no', 'Appno', 'applicant_id', 'loan_type', req_cols)
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
rules_PO_agg$Appno <- NULL
rules_PO_agg$applicant_id <- NULL

rule_names <- names(rules_PO_agg)[grep("Rule_",names(rules_PO_agg))]

rules_PO_agg <- rules_PO_agg %>% group_by(deal_no,loan_type) %>% summarise(across(everything(), list(max)))

colnames(rules_PO_agg) <- c('deal_no', 'loan_type', rule_names)




assert_data_non_empty(rules_PO)
save(rules_PO,
     file = file.path(
       get_data_path()$data$output,
       "gating_rules",
       "rules_PO_2W_RF.rdata"
     )
)

# save(rules_PO_agg,
#      file = file.path("data//output//gating_rules//rules_PO_agg_2W_RF.rdata"
#      )
# )


assert_data_non_empty(rules_PO_agg)
save(rules_PO_agg,
     file = file.path(
       get_data_path()$data$output,
       "gating_rules",
       "rules_PO_agg_2W_RF.rdata"
     )
)

rm(rules_PO, rules_PO_agg)




## 3. Create delinquency rules ---------------------------------------------------------

## 3.1 load tradeline melt data

load_rdata_intermediate("ADS_data//trades_melt_2W_RF_modeling.rdata")


## 3.2 create subset of data with required columns
trades_dpd <- trades_melt %>% dplyr::select(deal_no,
                                            Appno,
                                            applicant_id,
                                            loan_type,
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
      trades_dpd[, paste0('Rule_DR_Gold_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                                 trades_dpd$diff_reported_payment <= month_value & 
                                                                                                 trades_dpd$dpd_num >= dpd_value & 
                                                                                                 trades_dpd$account_type %in% c(7) &
                                                                                                 trades_dpd$current_balance >= 3000 ,1, 0)
      
      trades_dpd[, paste0('Rule_DR_Education_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                            trades_dpd$diff_reported_payment <= month_value & 
                                                                                            trades_dpd$dpd_num >= dpd_value & 
                                                                                            trades_dpd$account_type %in% c(8) &
                                                                                            trades_dpd$current_balance >= 3000 ,1, 0)
      
      trades_dpd[, paste0('Rule_DR_Agri_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                            trades_dpd$diff_reported_payment <= month_value & 
                                                                                            trades_dpd$dpd_num >= dpd_value & 
                                                                                            trades_dpd$account_type %in% c(36, 53,57) &
                                                                                            trades_dpd$current_balance >= 3000 ,1, 0)
      
      trades_dpd[, paste0('Rule_DR_Mudra_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                            trades_dpd$diff_reported_payment <= month_value & 
                                                                                            trades_dpd$dpd_num >= dpd_value & 
                                                                                            trades_dpd$account_type %in% c(39) &
                                                                                            trades_dpd$current_balance >= 3000 ,1, 0)
      
      trades_dpd[, paste0('Rule_DR_CC_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                             trades_dpd$diff_reported_payment <= month_value & 
                                                                                             trades_dpd$dpd_num >= dpd_value & 
                                                                                             trades_dpd$account_type %in% c(10,31,35,36) &
                                                                                             trades_dpd$current_balance >= 3000 ,1, 0)
      
      trades_dpd[, paste0('Rule_DR_non_Gold_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                            trades_dpd$diff_reported_payment <= month_value & 
                                                                                            trades_dpd$dpd_num >= dpd_value & 
                                                                                            trades_dpd$account_type %notin% c(7) &
                                                                                            trades_dpd$current_balance >= 3000 ,1, 0)
      
      trades_dpd[, paste0('Rule_DR_non_Education_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                                  trades_dpd$diff_reported_payment <= month_value & 
                                                                                                  trades_dpd$dpd_num >= dpd_value & 
                                                                                                  trades_dpd$account_type %notin% c(8) &
                                                                                                  trades_dpd$current_balance >= 3000 ,1, 0)
      
      trades_dpd[, paste0('Rule_DR_non_Agri_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                           trades_dpd$diff_reported_payment <= month_value & 
                                                                                           trades_dpd$dpd_num >= dpd_value & 
                                                                                           trades_dpd$account_type %notin% c(36, 53,57) &
                                                                                           trades_dpd$current_balance >= 3000 ,1, 0)
      
      trades_dpd[, paste0('Rule_DR_non_Mudra_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                               trades_dpd$diff_reported_payment <= month_value & 
                                                                                               trades_dpd$dpd_num >= dpd_value & 
                                                                                               trades_dpd$account_type %notin% c(39) &
                                                                                               trades_dpd$current_balance >= 3000 ,1, 0)
      
      trades_dpd[, paste0('Rule_DR_non_CC_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                                 trades_dpd$diff_reported_payment <= month_value & 
                                                                                                 trades_dpd$dpd_num >= dpd_value & 
                                                                                                 trades_dpd$account_type %notin% c(10,31,35,36) &
                                                                                                 trades_dpd$current_balance >= 3000 ,1, 0)
      
      
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
trades_dpd <- trades_dpd %>% dplyr::select(c('deal_no', 'Appno', 'applicant_id', 'loan_type', req_cols))

gc()

rules_numbers <- c(1:length(req_cols))
blocks_temp <- split(rules_numbers, ceiling(seq_along(rules_numbers)/50))

rules_DL <- distinct(trades_dpd %>% dplyr::select(deal_no,Appno,applicant_id,loan_type))

for(temp_block in blocks_temp){
  temp_rules <- req_cols[min(temp_block) : max(temp_block)]
  temp_agg <- trades_dpd %>% dplyr::select(c('deal_no','Appno','applicant_id', 'loan_type',temp_rules))
  temp_agg <- temp_agg %>% group_by(deal_no,Appno,applicant_id,loan_type) %>% summarise(across(everything(), list(sum)))
  colnames(temp_agg) <- c('deal_no', 'Appno','applicant_id','loan_type', temp_rules)
  temp_agg <- temp_agg %>% ungroup()
  
  rules_DL <- left_join(rules_DL,temp_agg,by = c('deal_no' = 'deal_no',
                                                             'Appno' = 'Appno',
                                                              'applicant_id' = 'applicant_id',
                                                             'loan_type' = 'loan_type'))
  gc()
}

rm(temp_agg,temp_rules,temp_block,blocks_temp,rules_numbers)
gc()

rm(trades_dpd)
gc()



# req_cols_p1 <- req_cols[1:81]
# req_cols_p2 <- req_cols[82:162]
# 
# trades_dpd_p1 <- trades_dpd %>% dplyr::select(c('deal_no', 'Appno', 'applicant_id', 'loan_type', req_cols_p1))
# trades_dpd_p2 <- trades_dpd %>% dplyr::select(c('deal_no', 'Appno', 'applicant_id', 'loan_type', req_cols_p2))
# 
# rm(trades_dpd)
# ## 3.7 aggregate rules at deal number level
# rules_DL_p1 <- trades_dpd_p1 %>% group_by(deal_no,Appno,applicant_id,loan_type) %>% summarise(across(everything(), list(sum)))
# rm(trades_dpd_p1)
# 
# 
# 
# rules_DL_p2 <- trades_dpd_p2 %>% group_by(deal_no,Appno,applicant_id,loan_type) %>% summarise(across(everything(), list(sum)))
# rm(trades_dpd_p2)
# 
# colnames(rules_DL_p1) <- c('deal_no', 'Appno', 'applicant_id', 'loan_type', req_cols_p1)
# rules_DL_p1 <- rules_DL_p1 %>% ungroup()
# 
# colnames(rules_DL_p2) <- c('deal_no', 'Appno', 'applicant_id', 'loan_type', req_cols_p2)
# rules_DL_p2 <- rules_DL_p2 %>% ungroup()
# 
# rules_DL <- left_join(rules_DL_p1,rules_DL_p2, by = c('deal_no' = 'deal_no',
#                                                       'Appno' = 'Appno',
#                                                       'applicant_id' = 'applicant_id',
#                                                       'loan_type' = 'loan_type'
#                                                       ))
# 
# rm(rules_DL_p1,rules_DL_p2)
# # colnames(rules_DL) <- c('deal_no', 'Appno', 'applicant_id', 'loan_type', req_cols)
# rules_DL <- rules_DL %>% ungroup()
# 
# gc()

## 3.8 convert sum to flags
for(col in req_cols){
  # value_list <- c(1,2,3,4)
  # for(value in value_list){
  #   new_col <- paste0(col, "_Equal_To_",value)
  #   rules_DL[,new_col] <- as.numeric(ifelse(rules_DL[,col] == value, 1, 0))
  # }
  # 
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
rules_DL_agg$Appno <- NULL
rules_DL_agg$applicant_id <- NULL


rule_names <- names(rules_DL_agg)[grep("Rule_",names(rules_DL_agg))]

rules_numbers <- c(1:length(rule_names))
blocks_temp <- split(rules_numbers, ceiling(seq_along(rules_numbers)/50))

temp_output_df <- distinct(rules_DL_agg %>% dplyr::select(deal_no, loan_type))

for(temp_block in blocks_temp){
  temp_rules <- rule_names[min(temp_block) : max(temp_block)]
  temp_agg <- rules_DL_agg %>% dplyr::select(c('deal_no', 'loan_type',temp_rules))
  temp_agg <- temp_agg %>% group_by(deal_no,loan_type) %>% summarise(across(everything(), list(max)))
  colnames(temp_agg) <- c('deal_no', 'loan_type', temp_rules)
  temp_agg <- temp_agg %>% ungroup()
  
  temp_output_df <- left_join(temp_output_df,temp_agg,by = c('deal_no' = 'deal_no',
                                                             'loan_type' = 'loan_type'))
  
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
       get_data_path()$data$output,
       "gating_rules",
       "rules_DL_2W_RF.rdata"
     )
)

# save(rules_DL_agg,
#      file = file.path("data//output//gating_rules//rules_DL_agg_2W_RF.rdata"
#      )
# )


assert_data_non_empty(rules_DL_agg)
save(rules_DL_agg,
     file = file.path(
       get_data_path()$data$output,
       "gating_rules",
       "rules_DL_agg_2W_RF.rdata"
     )
)

rm(rules_DL, rules_DL_agg)

gc()

## 4. Create enquiry variables -------------------------------------------------------------

## 4.1 load enquiry data
# load_rdata_intermediate("ADS_data//enquiry_data.rdata")

load_rdata_intermediate("ADS_data//enquiry_data_2W_RF_modeling.rdata")
enquiry_data <- distinct(enquiry_data)

enquiry_data <- enquiry_data %>% filter(!is.na(loan_notation))




## 4.2 create rules function 
create_ER_x_enq_y_mon <- function(enquiry_data, months_vector, buffer){
  
  # get only deal number columns
  base_output <- distinct(enquiry_data %>% dplyr::select(deal_no, Appno, applicant_id))
  
  # create buffer
  buffer_2W_RF <- buffer + tat_2W_RF
  
  # iterate over different months
  for(month_value in months_vector){
    
    # get days cutoff
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    # subset for loan types
    enquiry_agg <- enquiry_data %>% filter(loan_type == 'Refinance-2W')
    
    ### get count of enquiries at deal no x customer code x loan notation level
    
    # 2W
    enquiry_agg <- enquiry_agg %>% filter((days > (0+buffer_2W_RF)) & (days <= (days_cutoff+buffer_2W_RF))) %>%
      group_by(deal_no,
               Appno,
               applicant_id,
               loan_notation) %>%
      summarise(enquiry_count = length(unique(enquiry_date)))
    enquiry_agg <- enquiry_agg %>% ungroup()
    
    # change column name
    colnames(enquiry_agg)[colnames(enquiry_agg) == 'enquiry_count'] <- paste0('Rule_EN_enquiry_count_',month_value,'m')
    
    # melt data to get enquiry count column name as a separate column
    enquiry_agg <- melt(enquiry_agg, id.vars = c('deal_no','Appno','applicant_id','loan_notation'))
    
    # transpose rows to columns to get count across loan types
    enquiry_agg_dcast <- dcast(deal_no+Appno+applicant_id ~ variable+loan_notation,
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
    base_output <- left_join(base_output, enquiry_agg_dcast, by = c('deal_no' = 'deal_no',
                                                                    'Appno' = 'Appno',
                                                                    'applicant_id' = 'applicant_id'))
  }
  
  return(data.frame(base_output))
}



## 4.3 call enquiry rules creation function
rules_EN <- create_ER_x_enq_y_mon(enquiry_data, months_vector, buffer)
rules_EN[is.na(rules_EN)] <- 0 


## 4.4 create rules from enquiry variables
req_cols <- names(rules_EN)[grep("Rule_EN_enquiry_count_",names(rules_EN))]

for(col in req_cols){
  value_list <- c(1,3,5,7,9)
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

colnames(rules_EN)[colnames(rules_EN) == 'Appno'] <- 'Appno'



rules_EN_agg <- rules_EN
rules_EN_agg$Appno <- NULL
rules_EN_agg$applicant_id <- NULL

rule_names <- names(rules_EN_agg)[grep("Rule_",names(rules_EN_agg))]

rules_EN_agg <- rules_EN_agg %>% group_by(deal_no) %>% summarise(across(everything(), list(max)))

colnames(rules_EN_agg) <- c('deal_no', rule_names)




assert_data_non_empty(rules_EN)
save(rules_EN,
     file = file.path(
       get_data_path()$data$output,
       "gating_rules",
       "rules_EN_2W_RF.rdata"
     )
)

# save(rules_EN,
#      file = file.path(
#        "data//output//gating_rules//rules_EN_2W_RF.rdata"
#      )
# )


assert_data_non_empty(rules_EN_agg)
save(rules_EN_agg,
     file = file.path(
       get_data_path()$data$output,
       "gating_rules",
       "rules_EN_agg_2W_RF.rdata"
     )
)

save(rules_EN_agg,
     file = file.path(
       "data//output//gating_rules//rules_EN_agg_2W_RF.rdata"
     )
)

rm(rules_EN, rules_EN_agg)









## 5. Combine all rules ------------------------------------------------------------------------------

load_rdata_output("gating_rules//rules_PO_agg_2W_RF.rdata")
load_rdata_output("gating_rules//rules_DL_agg_2W_RF.rdata")
load_rdata_output("gating_rules//rules_EN_agg_2W_RF.rdata")



rules_PO_agg$loan_type <- NULL
rules_DL_agg$loan_type <- NULL

all_rules <- left_join(final_output,rules_PO_agg, by = c('deal_no' = 'deal_no'))

all_rules <- left_join(all_rules, rules_DL_agg, by = c('deal_no' = 'deal_no'))

all_rules <- left_join(all_rules, rules_EN_agg, by = c('deal_no' = 'deal_no'))

rm(final_output,rules_DL_agg,rules_EN_agg,rules_PO_agg)

# all_rules <- left_join(final_output,rules_PO, by = c('deal_no' = 'deal_no',
#                                                      'Appno' = 'Appno',
#                                                      'applicant_id' = 'applicant_id'))
# 
# all_rules <- left_join(all_rules, rules_DL, by = c('deal_no' = 'deal_no',
#                                                    'Appno' = 'Appno',
#                                                    'applicant_id' = 'applicant_id'))
# 
# all_rules <- left_join(all_rules, rules_EN, by = c('deal_no' = 'deal_no',
#                                                    'Appno' = 'Appno',
#                                                    'applicant_id' = 'applicant_id'))


all_rules[is.na(all_rules)] <- 0 

# rm(rules_PO, rules_DL, rules_EN, final_output)

# all_rules_backup <- all_rules


# all_rules$Appno <- NULL
# all_rules$applicant_id <- NULL
# 
# rule_names <- names(all_rules)[grep("Rule_",names(all_rules))]
# 
# all_rules <- all_rules %>% group_by(deal_no,loan_type) %>% summarise(across(everything(), list(max)))
# 
# colnames(all_rules) <- c('deal_no', 'loan_type', rule_names)

## 6. Get bad tagging for rules ----------------------------------------------------------------------
load_rdata_intermediate("ADS_data//bad_loans_2W_RF.rdata")

bad_loans$loan_type <- NULL

output <- inner_join(bad_loans, all_rules, by = 'deal_no')


# gating_rules_all <- output
# assert_data_non_empty(gating_rules_all)
# save(gating_rules_all,
#      file = file.path(
#        get_data_path()$data$output,
#        "gating_rules",
#        "gating_rules.rdata"
#      )
# )


# rm(gating_rules_all)

gating_rules_deal_no_level <- output
assert_data_non_empty(gating_rules_deal_no_level)
save(gating_rules_deal_no_level,
     file = file.path(
       get_data_path()$data$output,
       "gating_rules",
       "gating_rules_deal_no_level_2W_RF.rdata"
     )
)

save(gating_rules_deal_no_level,
     file = file.path("data//output//gating_rules//gating_rules_deal_no_level_2W_RF.rdata"
     )
)

rm(bad_loans,all_rules,gating_rules_deal_no_level,month_day_mapping)

## 7. save output for product types -------------------------------------------------------------------

## 7.1 function to calculate rule statistics
# calculate_rule_statistics <- function(gating_rules_data, loan_type_temp){
#   
#   ## subset for required loan type & select columns
#   gating_rules_subset <- gating_rules_data %>% filter(loan_type == loan_type_temp)
#   rule_names <- colnames(gating_rules_subset)
#   rule_names <- rule_names[rule_names %notin% c('deal_no', 'Appno','applicant_id','loan_type','bad_loan','ever_90dpd','ever_150dpd')]
#   
#   ## create output list
#   rules_output <- list()
#   i <- 0
#   
#   ## iterate over rules & calculate summary numbers
#   for(rule in rule_names){
#     temp_data <- gating_rules_subset %>% dplyr::select(c('deal_no','Appno','applicant_id','bad_loan','ever_90dpd','ever_150dpd', rule))
#     colnames(temp_data) <- c('deal_no','Appno','applicant_id', 'bad_loan','ever_90dpd','ever_150dpd', 'rule')
#     temp_data_subset <- temp_data %>% filter(rule == 1)
#     
#     temp_output_df <- data.frame(rule = rule,
#                                  tagged_population_pct = nrow(temp_data_subset)/nrow(temp_data),
#                                  tagged_population_count = nrow(temp_data_subset),
#                                  rule_default_rate = mean(temp_data_subset$bad_loan),
#                                  rule_ever_90dpd = mean(temp_data_subset$ever_90dpd),
#                                  rule_ever_150dpd = mean(temp_data_subset$ever_150dpd),
#                                  count_ever_150dpd = sum(temp_data_subset$ever_150dpd),
#                                  overall_default_rate = mean(temp_data$bad_loan),
#                                  overall_ever_90dpd = mean(temp_data$ever_90dpd),
#                                  overall_ever_150dpd = mean(temp_data$ever_150dpd)
#     )
#     
#     i <- i+1
#     rules_output[[i]] <- temp_output_df
#   }
#   
#   
#   ## rbind all rule dataframes into 1 output dataframe
#   rules_output_df <- do.call(rbind,rules_output)
#   
#   ## replace NAN with NA in data
#   is.nan.data.frame <- function(x)
#   do.call(cbind, lapply(x, is.nan))
#   
#   rules_output_df[is.nan(rules_output_df)] <- NA
#   
#   ## return output
#   return(rules_output_df)
# }


calculate_rule_statistics <- function(gating_rules_data, loan_type_temp){
  
  ## subset for required loan type & select columns
  gating_rules_subset <- gating_rules_data %>% filter(loan_type == loan_type_temp)
  rule_names <- colnames(gating_rules_subset)
  rule_names <- rule_names[rule_names %notin% c('deal_no','loan_type','bad_loan','ever_90dpd','ever_150dpd')]
  
  ## create output list
  rules_output <- list()
  i <- 0
  
  ## iterate over rules & calculate summary numbers
  for(rule in rule_names){
    temp_data <- gating_rules_subset %>% dplyr::select(c('deal_no','bad_loan','ever_90dpd','ever_150dpd', rule))
    colnames(temp_data) <- c('deal_no', 'bad_loan','ever_90dpd','ever_150dpd', 'rule')
    temp_data_subset <- temp_data %>% filter(rule == 1)
    
    temp_output_df <- data.frame(rule = rule,
                                 tagged_population_pct = nrow(temp_data_subset)/nrow(temp_data),
                                 tagged_population_count = nrow(temp_data_subset),
                                 rule_default_rate = mean(temp_data_subset$bad_loan),
                                 rule_ever_90dpd = mean(temp_data_subset$ever_90dpd),
                                 rule_ever_150dpd = mean(temp_data_subset$ever_150dpd),
                                 count_ever_150dpd = sum(temp_data_subset$ever_150dpd),
                                 overall_default_rate = mean(temp_data$bad_loan),
                                 overall_ever_90dpd = mean(temp_data$ever_90dpd),
                                 overall_ever_150dpd = mean(temp_data$ever_150dpd)
    )
    
    i <- i+1
    rules_output[[i]] <- temp_output_df
  }
  
  
  ## rbind all rule dataframes into 1 output dataframe
  rules_output_df <- do.call(rbind,rules_output)
  
  ## replace NAN with NA in data
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  
  rules_output_df[is.nan(rules_output_df)] <- NA
  
  ## return output
  return(rules_output_df)
}



## 7.2 get rule statistics for 2W New
rules_agg_2W_RF <- calculate_rule_statistics(gating_rules_data = output, loan_type_temp = 'Refinance-2W')

save_csv_output(rules_agg_2W_RF, paste0("\\gating_rules\\gating_rules_2W-RF_rules.csv"))

rm(list=ls())
gc()


