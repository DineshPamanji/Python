############################################################################################
################## 9 - X Bureau Variable Creation  #########################################
############################################################################################


## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src - DP","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src - DP","utils","io_helper.R")
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
load_rdata_intermediate("ADS_data//bureau_tradelines_2W_RF_modeling.rdata")

final_output <- distinct(trades_clean %>% dplyr::select(deal_no, Appno, applicant_id, loan_type))


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
                                             #loan_type,
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


## 2.5 variable creation function - loans taken in last x months
create_POV_loans_in_x_months <- function(tradelines, months_vector){
  for(month_value in months_vector){
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    tradelines[,paste0('Var_PO_months_',month_value)] <- ifelse(tradelines$days > 0 & 
                                                                  tradelines$days <= days_cutoff, 1, 0)
  }
  return(tradelines)
}


## 2.6 variable creation function - live loans taken in last x months
create_POV_live_loans_in_x_months <- function(tradelines, months_vector){
  for(month_value in months_vector){
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    tradelines[,paste0('Var_PO_months_',month_value, '_live')] <- ifelse(tradelines$days > 0 & 
                                                                           tradelines$days <= days_cutoff & 
                                                                           tradelines$live_loan == 1, 1, 0)
  }
  return(tradelines)
}


## 2.7 variable creation function - Specific loans taken in last x months
create_POV_product_loans_in_x_months <- function(tradelines, months_vector, loans_vector){
  for(month_value in months_vector){
    for(loan_value in loans_vector){
      days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
      
      tradelines[,paste0('Var_PO_months_',month_value, '_',loan_value)] <- ifelse(tradelines$days > 0 & 
                                                                                    tradelines$days <= days_cutoff & 
                                                                                    tradelines$loan_notation == loan_value, 1, 0)
    }
    
  }
  return(tradelines)
}


## 2.8 variable creation function - Specific live loans taken in last x months
create_POV_product_live_loans_in_x_months <- function(tradelines, months_vector, loans_vector){
  for(month_value in months_vector){
    for(loan_value in loans_vector){
      days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
      
      tradelines[,paste0('Var_PO_months_',month_value, '_',loan_value, '_live')] <- ifelse(tradelines$days > 0 & 
                                                                                             tradelines$days <= days_cutoff & 
                                                                                             tradelines$loan_notation == loan_value & 
                                                                                             tradelines$live_loan == 1, 1, 0)
    }
    
  }
  return(tradelines)
}


## 2.9 variable creation function - unsecured loans taken in last x months
create_POV_unsec_loans_in_x_months <- function(tradelines, months_vector){
  for(month_value in months_vector){
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    tradelines[,paste0('Var_PO_months_',month_value, '_unsec')] <- ifelse(tradelines$days > 0 & 
                                                                            tradelines$days <= days_cutoff & 
                                                                            tradelines$unsecured_flag == 'unsecured', 1, 0)
  }
  return(tradelines)
}


## 2.10 variable creation function - unsecured live loans taken in last x months
create_POV_unsec_live_loans_in_x_months <- function(tradelines, months_vector){
  for(month_value in months_vector){
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    tradelines[,paste0('Var_PO_months_',month_value, '_live_unsec')] <- ifelse(tradelines$days > 0 & 
                                                                                 tradelines$days <= days_cutoff & 
                                                                                 tradelines$live_loan == 1 & 
                                                                                 tradelines$unsecured_flag == 'unsecured', 1, 0)
  }
  return(tradelines)
}


## 2.11 variable creation function - unsecured loans taken in last x months excluding CC, CD
create_POV_unsec_loans_in_x_months_excl_CC_CD <- function(tradelines, months_vector){
  for(month_value in months_vector){
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    tradelines[,paste0('Var_PO_months_',month_value, '_unsec_excl_CC_CD')] <- ifelse(tradelines$days > 0 &
                                                                                       tradelines$days <= days_cutoff & 
                                                                                       tradelines$unsecured_flag == 'unsecured' &
                                                                                       tradelines$loan_notation %notin% c('CD','CC'), 1, 0)
  }
  return(tradelines)
}


## 2.12 variable creation - live loan count at the time of application excluding CC
create_POV_live_loans_excl_CC <- function(tradelines){
  tradelines[,paste0('Var_PO_live_excl_CC')] <- ifelse(tradelines$days > 0 & 
                                                         tradelines$live_loan == 1 & 
                                                         tradelines$loan_notation %notin% c('CC'), 1, 0)
  
  return(tradelines)
}


## 2.13 variable creation - closed loan count at the time of application excluding CC
create_POV_closed_loans_excl_CC <- function(tradelines){
  tradelines[,paste0('Var_PO_closed_excl_CC')] <- ifelse(tradelines$days > 0 & 
                                                           tradelines$live_loan == 0 & 
                                                           tradelines$loan_notation %notin% c('CC'), 1, 0)
  
  return(tradelines)
}

## 2.14 variable creation - total sanctioned amount of all closed loans at time of application excluding CC
create_POV_closed_sanction_amount_excl_CC <- function(tradelines){
  tradelines[,paste0('Var_PO_closed_sanction_amount_excl_CC')] <- ifelse(tradelines$days > 0 & 
                                                                           tradelines$live_loan == 0 & 
                                                                           tradelines$loan_notation %notin% c('CC'), 
                                                                         tradelines$high_credit_sanctioned_amount, 0)
  
  return(tradelines)
}





## 2.15 call all variable creation functions
tradelines <- create_POV_loans_in_x_months(tradelines, months_vector)
tradelines <- create_POV_live_loans_in_x_months(tradelines, months_vector)
tradelines <- create_POV_product_loans_in_x_months(tradelines, months_vector, loans_vector)
tradelines <- create_POV_product_live_loans_in_x_months(tradelines, months_vector, loans_vector)
tradelines <- create_POV_unsec_loans_in_x_months(tradelines, months_vector)
tradelines <- create_POV_unsec_live_loans_in_x_months(tradelines, months_vector)
tradelines <- create_POV_unsec_loans_in_x_months_excl_CC_CD(tradelines, months_vector)
tradelines <- create_POV_live_loans_excl_CC(tradelines)
tradelines <- create_POV_closed_loans_excl_CC(tradelines)
tradelines <- create_POV_closed_sanction_amount_excl_CC(tradelines)


## 2.16 create vintage variable
get_credit_vintage_years <- function(tradelines) {
  temp_data <- tradelines %>%
    group_by(deal_no,Appno,applicant_id) %>%
    summarise(
      Var_credit_vintage = max(days, na.rm = T)/365
    )
  return(temp_data)
}
credit_vintage_df <- get_credit_vintage_years(tradelines)

## 2.17 exposure variables
get_exposure_variables <- function(tradelines){
  exposure_var <- tradelines %>% filter(live_loan == 1) %>% group_by(deal_no,Appno,applicant_id) %>% summarise(
    Var_sanctioned_amount_live_loans = sum(high_credit_sanctioned_amount,na.rm = T),
    Var_outstanding_amount_live_loans = sum(current_balance, na.rm = T)
  )
  
  exposure_var$Var_sanctioned_by_outstanding_amount_live_loans <- exposure_var$Var_sanctioned_amount_live_loans / exposure_var$Var_outstanding_amount_live_loans
  exposure_var$Var_outstanding_by_sanctioned_amount_live_loans <- exposure_var$Var_outstanding_amount_live_loans / exposure_var$Var_sanctioned_amount_live_loans
  return(exposure_var)
}
exposure_df <- get_exposure_variables(tradelines = tradelines)
exposure_df$Var_sanctioned_by_outstanding_amount_live_loans[is.infinite(exposure_df$Var_sanctioned_by_outstanding_amount_live_loans)] <- NA
exposure_df$Var_outstanding_by_sanctioned_amount_live_loans[is.infinite(exposure_df$Var_outstanding_by_sanctioned_amount_live_loans)] <- NA


## 2.18 subset for required columns
req_cols <- names(tradelines)[grep("Var_PO",names(tradelines))]
tradelines <- tradelines %>% dplyr::select(c('deal_no', 'Appno','applicant_id', req_cols))


## 2.19 aggregate variables at deal number level
variables_PO <- tradelines %>% group_by(deal_no,Appno,applicant_id) %>% summarise(across(everything(), list(sum)))
colnames(variables_PO) <- c('deal_no', 'Appno','applicant_id', req_cols)
variables_PO <- variables_PO %>% ungroup()


## 2.20 join credit vintage variable
final_output <- left_join(final_output, credit_vintage_df, by=c('deal_no' = 'deal_no',
                                                                'Appno' = 'Appno',
                                                                'applicant_id' = 'applicant_id'))

## 2.21 join exposure variables
final_output <- left_join(final_output, exposure_df, by=c('deal_no' = 'deal_no',
                                                          'Appno' = 'Appno',
                                                          'applicant_id' = 'applicant_id'))

## 2.22 join all PO variables
final_output <- left_join(final_output,variables_PO,by=c('deal_no' = 'deal_no',
                                                         'Appno' = 'Appno',
                                                         'applicant_id' = 'applicant_id'))



rm(trades_clean, tradelines, credit_vintage_df, variables_PO, exposure_df)

gc()


## 3. Create delinquency rules ---------------------------------------------------------

## 3.1 load tradeline melt data
load_rdata_intermediate("ADS_data//trades_melt_2W_RF_modeling.rdata")


## 3.2 create subset of data with required columns
trades_dpd <- trades_melt %>% dplyr::select(deal_no,
                                            Appno,
                                            applicant_id,
                                            # loan_type,
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


## 3.3 variable creation function - x DPD in last y months
create_DV_x_dpd_y_mon <- function(trades_dpd, dpd_vector, months_vector){
  for(dpd_value in dpd_vector){
    for(month_value in months_vector){
      trades_dpd[, paste0('Var_DL_all_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                          trades_dpd$diff_reported_payment <= month_value & 
                                                                                          trades_dpd$dpd_num >= dpd_value &
                                                                                          trades_dpd$current_balance >= 3000, 1, 0)
    }
  }
  return(trades_dpd)
}


## 3.4 variable creation function - x DPD in last y months - non CC, CD
create_DV_x_dpd_y_mon_non_CC_CD <- function(trades_dpd, dpd_vector, months_vector){
  for(dpd_value in dpd_vector){
    for(month_value in months_vector){
      trades_dpd[, paste0('Var_DL_non_CC_CD_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                                trades_dpd$diff_reported_payment <= month_value & 
                                                                                                trades_dpd$dpd_num >= dpd_value & 
                                                                                                trades_dpd$loan_notation %notin% c('CD','CC') &
                                                                                                trades_dpd$current_balance >= 3000 ,1, 0)
    }
  }
  return(trades_dpd)
}


## 3.5 variable creation function - x DPD in last y months - unsecured
create_DV_x_dpd_y_mon_unsec <- function(trades_dpd, dpd_vector, months_vector){
  for(dpd_value in dpd_vector){
    for(month_value in months_vector){
      trades_dpd[, paste0('Var_DL_unsec_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 & 
                                                                                            trades_dpd$diff_reported_payment <= month_value & 
                                                                                            trades_dpd$dpd_num >= dpd_value & 
                                                                                            trades_dpd$unsecured_flag == 'unsecured' &
                                                                                            trades_dpd$current_balance >= 3000 ,1, 0)
    }
  }
  return(trades_dpd)
}


## 3.6 variable creation function - x DPD in last y months - secured
create_DV_x_dpd_y_mon_secured <- function(trades_dpd, dpd_vector, months_vector){
  for(dpd_value in dpd_vector){
    for(month_value in months_vector){
      trades_dpd[, paste0('Var_DL_secured_',dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 &
                                                                                            trades_dpd$diff_reported_payment <= month_value &
                                                                                            trades_dpd$dpd_num >= dpd_value &
                                                                                            trades_dpd$unsecured_flag == 'secured' &
                                                                                            trades_dpd$current_balance >= 3000 ,1, 0)
    }
  }
  return(trades_dpd)
}


## 3.6 variable creation function - minimum payment months for specific loan
create_DV_min_x_payments <- function(trades_dpd, account_type_num, min_months, loan_notation_temp){
  
  trades_temp <- trades_dpd %>% filter((account_type %in% account_type_num) & (dpd_num < 90)) %>% group_by(deal_no,
                                                                                                           Appno,
                                                                                                           applicant_id,
                                                                                                           loan_identifier) %>% summarise(n_months = length(unique(variable)))
  
  trades_temp$check <- ifelse(trades_temp$n_months >= min_months, 1, 0)
  
  trades_temp <- trades_temp %>% group_by(deal_no,Appno,applicant_id) %>% summarise(count = sum(check))
  trades_temp$flag <- ifelse(trades_temp$count > 0, 1, 0)
  
  
  temp_variable_name <- paste0("Var_DL_", loan_notation_temp, "_paid_GE_", min_months,"mon")
  temp_variable_name_flag <- paste0(temp_variable_name,"_flag")
  
  colnames(trades_temp) <- c('deal_no', 'Appno','applicant_id', temp_variable_name, temp_variable_name_flag)
  
  return(trades_temp)
}





## 3.7 call all rule creation functions
trades_dpd <- create_DV_x_dpd_y_mon(trades_dpd, dpd_vector, months_vector)
trades_dpd <- create_DV_x_dpd_y_mon_non_CC_CD(trades_dpd, dpd_vector, months_vector)
trades_dpd <- create_DV_x_dpd_y_mon_unsec(trades_dpd, dpd_vector, months_vector)
trades_dpd <- create_DV_x_dpd_y_mon_secured(trades_dpd, dpd_vector, months_vector)
temp_HL_paid_12mon <- create_DV_min_x_payments(trades_dpd, c(2), 12,'HL')
temp_gold_paid_12mon <- create_DV_min_x_payments(trades_dpd, c(7), 12,'Gold')
temp_gold_paid_6mon <- create_DV_min_x_payments(trades_dpd, c(7), 6,'Gold')
temp_education_paid_12mon <- create_DV_min_x_payments(trades_dpd, c(8), 12,'Education')
temp_education_paid_6mon <- create_DV_min_x_payments(trades_dpd, c(8), 6,'Education')



## 3.8 subset for required columns
req_cols <- names(trades_dpd)[grep("Var_DL",names(trades_dpd))]
trades_dpd <- trades_dpd %>% dplyr::select(c('deal_no', 'Appno','applicant_id', req_cols))


## 3.9 aggregate rules at deal number level
variables_DL <- trades_dpd %>% group_by(deal_no,Appno,applicant_id) %>% summarise(across(everything(), list(sum)))

colnames(variables_DL) <- c('deal_no', 'Appno','applicant_id', req_cols)
variables_DL <- variables_DL %>% ungroup()


## 3.10 join all DL vairables with final output
final_output <- left_join(final_output, variables_DL, by=c('deal_no' = 'deal_no',
                                                           'Appno' = 'Appno',
                                                           'applicant_id' = 'applicant_id'))

final_output <- left_join(final_output,temp_HL_paid_12mon,by=c('deal_no' = 'deal_no',
                                                               'Appno' = 'Appno',
                                                               'applicant_id' = 'applicant_id'))


final_output <- left_join(final_output,temp_gold_paid_12mon,by=c('deal_no' = 'deal_no',
                                                                 'Appno' = 'Appno',
                                                                 'applicant_id' = 'applicant_id'))

final_output <- left_join(final_output,temp_gold_paid_6mon,by=c('deal_no' = 'deal_no',
                                                                'Appno' = 'Appno',
                                                                'applicant_id' = 'applicant_id'))

final_output <- left_join(final_output,temp_education_paid_12mon,by=c('deal_no' = 'deal_no',
                                                                      'Appno' = 'Appno',
                                                                      'applicant_id' = 'applicant_id'))

final_output <- left_join(final_output,temp_education_paid_6mon,by=c('deal_no' = 'deal_no',
                                                                     'Appno' = 'Appno',
                                                                     'applicant_id' = 'applicant_id'))




# variables_DL <- left_join(variables_DL,temp_HL_paid_12mon,by = 'deal_no')

rm(trades_dpd, trades_melt, temp_HL_paid_12mon, variables_DL,temp_gold_paid_12mon,temp_gold_paid_6mon,temp_education_paid_12mon,temp_education_paid_6mon)




## 4. Create enquiry variables -------------------------------------------------------------

## 4.1 load enquiry data
load_rdata_intermediate("ADS_data//enquiry_data_2W_RF_modeling.rdata")
enquiry_data <- distinct(enquiry_data)

enquiry_data <- enquiry_data %>% filter(!is.na(loan_notation))

colnames(enquiry_data)[colnames(enquiry_data) == 'Appno'] <- 'Appno'

## 4.2 variable creation function - x enquiries in y months
create_EV_x_enq_y_mon <- function(enquiry_data, months_vector, buffer){
  
  # get only deal number columns
  base_output <- distinct(enquiry_data %>% dplyr::select(deal_no, Appno, applicant_id))
  
  # create buffer
  buffer_2W_RF <- buffer + tat_2W_RF
  
  # iterate over different months
  for(month_value in months_vector){
    
    # get days cutoff
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    # get count of enquiries at deal no x customer code x loan notation level
    # subset for loan types
    enquiry_agg <- enquiry_data %>% filter(loan_type == 'Refinance-2W')
    
    ### get count of enquiries at deal no x customer code x loan notation level
    
    # 2W RF
    enquiry_agg <- enquiry_agg %>% filter((days > (0+buffer_2W_RF)) & (days <= (days_cutoff+buffer_2W_RF))) %>% 
      group_by(deal_no,
               Appno,
               applicant_id,
               loan_notation) %>%
      summarise(enquiry_count = length(unique(enquiry_date)))
    enquiry_agg <- enquiry_agg %>% ungroup()
    enquiry_agg <- data.frame(enquiry_agg)
    
    # # get max count of enquiries across deal no x loan notation level
    # enquiry_agg <- enquiry_agg %>% group_by(deal_no,loan_notation) %>% summarise(enquiry_count = max(enquiry_count,na.rm = T))
    # enquiry_agg <- enquiry_agg %>% ungroup()
    # 
    
    # change column name
    colnames(enquiry_agg)[colnames(enquiry_agg) == 'enquiry_count'] <- paste0('Var_EN_enquiry_count_',month_value,'m')
    
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
    enquiry_agg_dcast[,paste0('Var_EN_enquiry_count_',month_value,'m')] <- rowSums(enquiry_agg_dcast[,enquiry_count_cols], na.rm = T)
    enquiry_agg_dcast[,paste0('Var_EN_enquiry_count_',month_value,'m_non_CC')] <- rowSums(enquiry_agg_dcast[,enquiry_count_cols_non_CC], na.rm = T)
    enquiry_agg_dcast[,paste0('Var_EN_enquiry_count_',month_value,'m_non_CD')] <- rowSums(enquiry_agg_dcast[,enquiry_count_cols_non_CD], na.rm = T)
    enquiry_agg_dcast[,paste0('Var_EN_enquiry_count_',month_value,'m_non_CC_CD')] <- rowSums(enquiry_agg_dcast[,enquiry_count_cols_non_CC_CD], na.rm = T)
    
    # join with base output
    base_output <- left_join(base_output, enquiry_agg_dcast,by = c('deal_no' = 'deal_no',
                                                                   'Appno' = 'Appno',
                                                                   'applicant_id' = 'applicant_id'))
  }
  return(data.frame(base_output))
}


## 4.3 variable creation function - unsecured x enquiries in y months
create_EV_unsec_x_enq_y_mon <- function(enquiry_data, months_vector,buffer){
  
  # filter to get unsecured loan enquiries
  enquiry_data_unsec <- enquiry_data %>% filter(unsecured_flag == "unsecured")
  
  # get only deal number columns
  base_output <- distinct(enquiry_data_unsec %>% dplyr::select(deal_no, Appno, applicant_id))
  
  # create buffer
  buffer_2W_RF <- buffer + tat_2W_RF
  
  # iterate over different months
  for(month_value in months_vector){
    
    # get days cutoff
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    # get count of enquiries at deal no x customer code x loan notation level
    # subset for loan types
    enquiry_agg <- enquiry_data %>% filter(loan_type == 'Refinance-2W')
    
    ### get count of enquiries at deal no x customer code x loan notation level
    
    # 2W
    enquiry_agg <- enquiry_agg %>% filter((days > (0+buffer_2W_RF)) & (days <= (days_cutoff+buffer_2W_RF))) %>%
      group_by(deal_no,
               Appno,
               applicant_id,
               loan_notation) %>% summarise(enquiry_count = length(unique(enquiry_date)))
    enquiry_agg <- enquiry_agg %>% ungroup()
    
    enquiry_agg <- data.frame(enquiry_agg)
    
    
    # get max count of enquiries across deal no x loan notation level
    # enquiry_agg <- enquiry_agg %>% group_by(deal_no,loan_notation) %>% summarise(enquiry_count = max(enquiry_count,na.rm = T))
    # enquiry_agg <- enquiry_agg %>% ungroup()
    
    
    # change column name
    colnames(enquiry_agg)[colnames(enquiry_agg) == 'enquiry_count'] <- paste0('Var_EN_unsec_enquiry_count_',month_value,'m')
    
    # melt data to get enquiry count column name as a separate column
    enquiry_agg <- melt(enquiry_agg, id.vars = c('deal_no','Appno','applicant_id','loan_notation'))
    
    # transpose rows to columns to get count across loan types
    enquiry_agg_dcast <- dcast(deal_no+Appno+applicant_id ~ variable+loan_notation,
                               data = enquiry_agg,
                               value.var = "value",
                               sum)
    
    
    # get columns names to calculate overall enquiry count
    enquiry_count_cols <- names(enquiry_agg_dcast)[grep("unsec_enquiry_count",names(enquiry_agg_dcast))]
    
    # get column names to calculate enquiry count excluding CC
    enquiry_count_cols_non_CC <- enquiry_count_cols[!grepl("_CC",enquiry_count_cols)]
    
    # get column names to calculate enquiry count excluding CD
    enquiry_count_cols_non_CD <- enquiry_count_cols[!grepl("_CD",enquiry_count_cols)]
    
    # get column names to calculate enquiry count excluding CC & CD
    enquiry_count_cols_non_CC_CD <- enquiry_count_cols[!grepl("_CC",enquiry_count_cols)]
    enquiry_count_cols_non_CC_CD <- enquiry_count_cols_non_CC_CD[!grepl("_CD",enquiry_count_cols_non_CC_CD)]
    
    # create overall enquiry columns
    enquiry_agg_dcast[,paste0('Var_EN_unsec_enquiry_count_',month_value,'m')] <- rowSums(enquiry_agg_dcast[,enquiry_count_cols], na.rm = T)
    enquiry_agg_dcast[,paste0('Var_EN_unsec_enquiry_count_',month_value,'m_non_CC')] <- rowSums(enquiry_agg_dcast[,enquiry_count_cols_non_CC], na.rm = T)
    enquiry_agg_dcast[,paste0('Var_EN_unsec_enquiry_count_',month_value,'m_non_CD')] <- rowSums(enquiry_agg_dcast[,enquiry_count_cols_non_CD], na.rm = T)
    enquiry_agg_dcast[,paste0('Var_EN_unsec_enquiry_count_',month_value,'m_non_CC_CD')] <- rowSums(enquiry_agg_dcast[,enquiry_count_cols_non_CC_CD], na.rm = T)
    
    # join with base output
    base_output <- left_join(base_output, enquiry_agg_dcast, by = c('deal_no' = 'deal_no',
                                                                    'Appno' = 'Appno',
                                                                    'applicant_id' = 'applicant_id'))
  }
  
  return(data.frame(base_output))
}


## 4.4 call enquiry variable creation function
variables_EN <- create_EV_x_enq_y_mon(enquiry_data, months_vector, buffer)
variables_EN_unsec <- create_EV_unsec_x_enq_y_mon(enquiry_data, months_vector, buffer)


## 4.5 join enquiry variables with final variable dataset
final_output <- left_join(final_output, variables_EN, by=c('deal_no' = 'deal_no',
                                                           'Appno' = 'Appno',
                                                           'applicant_id' = 'applicant_id'))


final_output <- left_join(final_output, variables_EN_unsec, by=c('deal_no' = 'deal_no',
                                                                 'Appno' = 'Appno',
                                                                 'applicant_id' = 'applicant_id'))



# variables_EN <- left_join(variables_EN,variables_EN_unsec,by = c('deal_no' = 'deal_no',
#                                                                  'Appno' = 'Appno',
#                                                                  'applicant_id' = 'applicant_id'))
# 
# variables_EN[is.na(variables_EN)] <- 0 

rm(enquiry_data,month_day_mapping, variables_EN_unsec,variables_EN)





## 5. Combine all variables ------------------------------------------------------------------------------

# all_X_var <- left_join(variables_PO, variables_DL, by = c("deal_no" = "deal_no",
#                                                   "loan_type" = "loan_type"))
# 
# all_X_var <- left_join(all_X_var, variables_EN, by = c("deal_no" = "deal_no"))


# all_rules[is.na(all_rules)] <- 0 

# rm(variables_PO, variables_DL, variables_EN)

all_X_var_bureau <- final_output
assert_data_non_empty(all_X_var_bureau)
save(all_X_var_bureau,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "X_var_bureau_2W_RF.rdata"
     )
)

# save(all_X_var_bureau,
#      file = file.path("data//intermediate//ADS_data//X_var_bureau_2W_RF.rdata"
#      )
# )
rm(final_output, all_X_var_bureau)


# load_rdata_intermediate("ADS_data//X_var_bureau.rdata")

## 6. Create application variables ------------------------------------------------------------------------

## 6.1 load ads data data
load_rdata_intermediate("ADS_data//base_ads.rdata")

## 6.2 subset for required loan types & columns
req_loan_types <- c('Refinance-2W')

base_ads <- distinct(base_ads %>% filter(loan_type %in% req_loan_types) %>% dplyr::select(deal_no,
                                                                                          loan_type,
                                                                                          CUSTOMER_PROFILE,
                                                                                          disbursal_state))


## 6.3 load salary category mapping
salary_profile <- data.frame(fread_mapping('profile_mapping.csv'))


## 6.4 join with ads data
base_ads <- left_join(base_ads,salary_profile,by=c("CUSTOMER_PROFILE" = "sub_category"))

colnames(base_ads)[colnames(base_ads) == "new_category"] <- "Category"


## 6.5 create agri flag
base_ads$agri_profile_flag <- ifelse(base_ads$sub_profile == "Agriculture oriented", 1, 0) 


## 6.6 save data
all_X_var_application <- base_ads
assert_data_non_empty(all_X_var_application)
save(all_X_var_application,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "X_var_application_2W_RF.rdata"
     )
)

# save(all_X_var_application,
#      file = file.path("data//intermediate//ADS_data//X_var_application_2W_RF.rdata"
#      )
# )









rm(list=ls())

