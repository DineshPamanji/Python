#########################################################################################################
#################### Belief testing data creation - Disbursals ##########################################
#########################################################################################################


## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)


## 1 Load bureau tradelines data -------------------------------------------------------------------

## 1.1 dataframe for month - day mapping
month_day_mapping <- data.frame(month = c(1,3,6,12),
                                days = c(30, 90, 180, 365))

## 1.2 vector of all months
months_vector <- c(3,6,12)

## 1.3 vector of all loan products
loan_notation_list <- c('HL','PL','AL','TW','Gold')


## 1.4 vector for all DPD
dpd_vector <- c(30,60)



## 2. Create delinquency variables  ----------------------------------------------------------------------

## 2.1 load melted bureau data
# load_rdata_intermediate("ADS_data//trades_melt_vf.rdata")
load_rdata_intermediate("ADS_data//trades_melt_2W_RF_modeling.rdata")
load("data/intermediate/cleaned_data/cibil_v3_disbursals.rdata")
cibil_v3 <- cibil_v3 %>% dplyr::select('deal_no','customer_code','cibil_score_v3')
trades_melt <- left_join(trades_melt, cibil_v3 )

## 2.2 keep selected columns
trades_dpd <- trades_melt %>% dplyr::select(deal_no,
                                            customer_code,
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


## 2.3 Function - x DPD in last y months
create_DV_x_dpd_y_mon_loan <- function(trades_dpd, dpd_vector, months_vector, loan_notation_list){
  for(dpd_value in dpd_vector){
    for(month_value in months_vector){
      for (loan_notation in loan_notation_list) {
        
        
        trades_dpd[, paste0('Rule_DL_',loan_notation,"_",dpd_value,'dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 &
                                                                                                           trades_dpd$diff_reported_payment <= month_value &
                                                                                                           trades_dpd$dpd_num >= dpd_value &
                                                                                                           trades_dpd$loan_notation == loan_notation &
                                                                                                           trades_dpd$current_balance >= 3000 ,1, 0)
      }
    }
  }
  return(trades_dpd)
}

## 2.4 Function - x DPD but never 90dpd last y months
create_DV_x_dpd_y_mon_never_90_loan <- function(trades_dpd, dpd_vector, months_vector, loan_notation_list){
  for(dpd_value in dpd_vector){
    for(month_value in months_vector){
      for (loan_notation in loan_notation_list) {
        
        
        trades_dpd[, paste0('Rule_DL_',loan_notation,"_",dpd_value,'dpd_never_90dpd_',month_value,'mon')] <-  ifelse(trades_dpd$diff_reported_payment >= 1 &
                                                                                                                       trades_dpd$diff_reported_payment <= month_value &
                                                                                                                       trades_dpd$dpd_num >= dpd_value &
                                                                                                                       trades_dpd$dpd_num < 90 &
                                                                                                                       trades_dpd$loan_notation == loan_notation &
                                                                                                                       trades_dpd$current_balance >= 3000,1, 0)
      }
    }
  }
  return(trades_dpd)
}


## 2.5 Function - x DPD but never 90dpd last y months for unsecured loans
create_DV_x_dpd_y_mon_never_90_unsecured <- function(trades_dpd, dpd_vector, months_vector, unsecured_flag){
  for(dpd_value in dpd_vector){
    for(month_value in months_vector){
      
      trades_dpd[, paste0('Rule_DL_',dpd_value,'dpd_never_90dpd_',month_value,'mon_',unsecured_flag)] <-  ifelse(trades_dpd$diff_reported_payment >= 1 &
                                                                                                                   trades_dpd$diff_reported_payment <= month_value &
                                                                                                                   trades_dpd$dpd_num >= dpd_value &
                                                                                                                   trades_dpd$dpd_num < 90 &
                                                                                                                   trades_dpd$unsecured_flag == unsecured_flag &
                                                                                                                   trades_dpd$current_balance >= 3000,1, 0)
      
    }
  }
  return(trades_dpd)
}



## 2.6 Run functions
# trades_dpd <- create_DV_x_dpd_y_mon_loan(trades_dpd, dpd_vector, months_vector, loan_notation_list)
trades_dpd <- create_DV_x_dpd_y_mon_never_90_loan(trades_dpd, dpd_vector, months_vector, loan_notation_list)
# trades_dpd <- create_DV_x_dpd_y_mon_never_90_unsecured(trades_dpd, dpd_vector, months_vector, "secured")
# trades_dpd <- create_DV_x_dpd_y_mon_never_90_unsecured(trades_dpd, dpd_vector, months_vector, "unsecured")


## 2.7 subset for required columns
req_cols <- names(trades_dpd)[grep("Rule_DL",names(trades_dpd))]
trades_dpd <- trades_dpd %>% dplyr::select(c('deal_no', 'customer_code','applicant_id', 'loan_type',req_cols))

rm(trades_melt)
gc()

## 2.8 aggregate rules at an applicant level
rules_DL <- trades_dpd %>% group_by(deal_no,customer_code,applicant_id,loan_type) %>% summarise(across(everything(), list(sum)))

colnames(rules_DL) <- c('deal_no', 'customer_code','applicant_id','loan_type', req_cols)
rules_DL <- rules_DL %>% ungroup()

rm(trades_melt, trades_dpd)


## 2.9 convert sum to flags
for(col in req_cols){
  value_list <- c(1)
  for(value in value_list){
    new_col <- paste0(col, "_GE_",value)
    rules_DL[,new_col] <- as.numeric(ifelse(rules_DL[,col] >= value, 1, 0))
  }
}

## 2.10 select only flag columns
new_cols <- colnames(rules_DL)
new_cols <- new_cols[new_cols %notin% req_cols]

rules_DL <- rules_DL %>% dplyr::select(new_cols)





## 3. Create product ownership rules --------------------------------------------------------------------------

## 3.1 load bureau data
# load_rdata_intermediate("ADS_data//bureau_tradelines_vf.rdata")
load_rdata_intermediate("ADS_data//bureau_tradelines_2W_RF_modeling.rdata")
trades_clean <- left_join(trades_clean,cibil_v3)

final_output <- distinct(trades_clean %>% dplyr::select(deal_no,customer_code,applicant_id))

## 3.2 create live loans flag
trades_clean$live_loan <- ifelse((trades_clean$loan_notation != 'CC' &
                                    is.na(trades_clean$account_date_closed) &
                                    trades_clean$current_balance > 0) |
                                   (trades_clean$loan_notation == 'CC' & 
                                      is.na(trades_clean$account_date_closed) &
                                      (trades_clean$current_balance > 0 | trades_clean$current_balance == 0)
                                   ),
                                 1,0)

## 3.3 calculate days between date opened & date on which cibil report was pulled
trades_clean$days <- as.numeric(trades_clean$cibil_reported_date - trades_clean$account_date_opened)


## 3.4 create tradelines subset for adding rules
tradelines <- trades_clean %>% dplyr::select(deal_no,
                                             customer_code,
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



## 3.5 Function - Sanctioned amount in x loan in y months
create_PO_loans_sanction_amount_in_mon <- function(tradelines, months_vector, loan_notation_list){
  for(month_value in months_vector){
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    for (loan_notation in loan_notation_list) {
      tradelines[, paste0('Rule_PO_',loan_notation,"_sanctioned_amount_",month_value,'mon')] <-  
        ifelse(tradelines$days >= 1 &
                 tradelines$days <= days_cutoff &
                 tradelines$high_credit_sanctioned_amount >= 3000 &
                 tradelines$loan_notation == loan_notation,
               tradelines$high_credit_sanctioned_amount, 0)
    }
  }
  return(tradelines)
}

## 3.6 Function - Sanctioned amount in x live loan in y months
create_PO_live_loans_sanction_amount_in_mon <- function(tradelines, months_vector, loan_notation_list){
  for(month_value in months_vector){
    days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
    
    for (loan_notation in loan_notation_list) {
      tradelines[, paste0('Rule_PO_',loan_notation,"_live_sanctioned_amount_",month_value,'mon')] <-  
        ifelse(tradelines$days >= 1 &
                 tradelines$days <= days_cutoff &
                 tradelines$loan_notation == loan_notation &
                 tradelines$high_credit_sanctioned_amount >= 3000 &
                 tradelines$live_loan == 1,
               tradelines$high_credit_sanctioned_amount, 0)
    }
  }
  return(tradelines)
}


# ## 3.7 Function - Secured loans in y months
# create_POV_sec_loans_in_x_months <- function(tradelines, months_vector){
#   for(month_value in months_vector){
#     days_cutoff <- unique((month_day_mapping %>% filter(month == month_value))$days)
#     
#     tradelines[,paste0('Rule_PO_months_',month_value, '_secured')] <- ifelse(tradelines$days > 0 &
#                                                                               tradelines$days <= days_cutoff & 
#                                                                               tradelines$unsecured_flag == 'secured', 1, 0)
#   }
#   tradelines[,paste0('Rule_PO_secured')] <- ifelse(tradelines$days > 0 & tradelines$unsecured_flag == 'secured', 1, 0)
#   
#   return(tradelines)
# }


## 3.8 Run functions
# tradelines <- create_POV_sec_loans_in_x_months(tradelines, months_vector)
tradelines <- create_PO_loans_sanction_amount_in_mon(tradelines, months_vector, loan_notation_list)
tradelines <- create_PO_live_loans_sanction_amount_in_mon(tradelines, months_vector, loan_notation_list)


## 3.9 Create rules based on overall time period
tradelines$Rule_PO_months_live <- ifelse(tradelines$live_loan == 1, 1, 0)
tradelines$Rule_PO_months_closed <- ifelse(tradelines$live_loan == 0, 1, 0)
tradelines$Rule_PO_months_all <- 1


## 3.10 subset for required columns
req_cols <- names(tradelines)[grep("Rule_PO",names(tradelines))]
rules_PO <- tradelines %>% dplyr::select(c('deal_no', 'customer_code','applicant_id', 'loan_type',req_cols))

## 3.11 Aggregate to applicant level
rules_PO <- rules_PO %>% group_by(deal_no,customer_code,applicant_id,loan_type) %>% summarise(across(everything(), list(sum)))
colnames(rules_PO) <- c('deal_no', 'customer_code', 'applicant_id', 'loan_type', req_cols)
rules_PO <- rules_PO %>% ungroup()



## 3.12 Get CC utilization rules
get_exposure_variables <- function(tradelines){
  exposure_var <- tradelines %>% filter(current_balance > 0 & high_credit_sanctioned_amount > 0)
  exposure_var <- exposure_var %>% filter(loan_notation == 'CC') %>% group_by(deal_no,customer_code,applicant_id,loan_type) %>% summarise(
    Var_sanctioned_amount_CC = sum(high_credit_sanctioned_amount,na.rm = T),
    Var_outstanding_amount_CC = sum(current_balance, na.rm = T)
  )
  
  exposure_var$Rule_PO_sanctioned_by_outstanding_amount_CC_util <- exposure_var$Var_sanctioned_amount_CC / exposure_var$Var_outstanding_amount_CC
  exposure_var$Rule_PO_outstanding_by_sanctioned_amount_CC_util <- exposure_var$Var_outstanding_amount_CC / exposure_var$Var_sanctioned_amount_CC
  return(exposure_var)
}

exposure_df <- get_exposure_variables(tradelines = tradelines)
exposure_df$Rule_PO_sanctioned_by_outstanding_amount_CC_util[is.infinite(exposure_df$Rule_PO_sanctioned_by_outstanding_amount_CC_util)] <- NA
exposure_df$Rule_PO_outstanding_by_sanctioned_amount_CC_util[is.infinite(exposure_df$Rule_PO_outstanding_by_sanctioned_amount_CC_util)] <- NA

exposure_df$Var_sanctioned_amount_CC <- NULL
exposure_df$Var_outstanding_amount_CC <- NULL

rm(trades_clean)


## 4. Rules from existing bureau variables -------------------------------------------------------------------

## 4.1 load data
# load_rdata_intermediate("ADS_data//X_var_bureau.rdata")
load_rdata_intermediate("ADS_data//X_var_bureau_2W_RF.rdata")
all_X_var_bureau <- left_join(all_X_var_bureau, cibil_v3)

## 4.2 select required columns
columns_list <- c('deal_no',
                  'customer_code',
                  'applicant_id',
                  'loan_type',
                  
                  'Var_PO_months_3_Gold',
                  'Var_PO_months_6_Gold',
                  'Var_PO_months_12_Gold',
                  
                  'Var_PO_months_3_Gold_live',
                  'Var_PO_months_6_Gold_live',
                  'Var_PO_months_12_Gold_live',
                  
                  'Var_PO_months_3_AL',
                  'Var_PO_months_6_AL',
                  'Var_PO_months_12_AL',
                  
                  'Var_PO_months_3_AL_live',
                  'Var_PO_months_6_AL_live',
                  'Var_PO_months_12_AL_live',
                  
                  'Var_PO_months_3_TW',
                  'Var_PO_months_6_TW',
                  'Var_PO_months_12_TW',
                  
                  'Var_PO_months_3_TW_live',
                  'Var_PO_months_6_TW_live',
                  'Var_PO_months_12_TW_live',
                  
                  'Var_PO_months_3_unsec',
                  'Var_PO_months_6_unsec',
                  'Var_PO_months_12_unsec',
                  
                  'Var_PO_months_3_live',
                  'Var_PO_months_6_live',
                  'Var_PO_months_12_live',
                  
                  'Var_PO_months_3',
                  'Var_PO_months_6',
                  'Var_PO_months_12',
                  
                  'Var_EN_enquiry_count_3m',
                  'Var_EN_enquiry_count_6m',
                  'Var_EN_enquiry_count_12m'
)


all_X_var_bureau <- all_X_var_bureau %>% dplyr::select(columns_list)

column_name_changed <- c(
  'deal_no',
  'customer_code',
  'applicant_id',
  'loan_type',
  
  'Rule_PO_months_3_Gold',
  'Rule_PO_months_6_Gold',
  'Rule_PO_months_12_Gold',
  
  'Rule_PO_months_3_Gold_live',
  'Rule_PO_months_6_Gold_live',
  'Rule_PO_months_12_Gold_live',
  
  'Rule_PO_months_3_AL',
  'Rule_PO_months_6_AL',
  'Rule_PO_months_12_AL',
  
  'Rule_PO_months_3_AL_live',
  'Rule_PO_months_6_AL_live',
  'Rule_PO_months_12_AL_live',
  
  'Rule_PO_months_3_TW',
  'Rule_PO_months_6_TW',
  'Rule_PO_months_12_TW',
  
  'Rule_PO_months_3_TW_live',
  'Rule_PO_months_6_TW_live',
  'Rule_PO_months_12_TW_live',
  
  'Rule_PO_months_3_unsec',
  'Rule_PO_months_6_unsec',
  'Rule_PO_months_12_unsec',
  
  'Rule_PO_months_3_live',
  'Rule_PO_months_6_live',
  'Rule_PO_months_12_live',
  
  'Rule_PO_months_3',
  'Rule_PO_months_6',
  'Rule_PO_months_12',
  
  'Rule_EN_enquiry_count_3m',
  'Rule_EN_enquiry_count_6m',
  'Rule_EN_enquiry_count_12m'
)


colnames(all_X_var_bureau) <- column_name_changed

rm(column_name_changed,columns_list)


## 4.3 create enquiry to loans taken variable
all_X_var_bureau$Rule_EN_ratio_enquiry_to_loans_3mon <- ifelse(all_X_var_bureau$Rule_EN_enquiry_count_3m > 6,
                                                               all_X_var_bureau$Rule_EN_enquiry_count_3m/all_X_var_bureau$Rule_PO_months_3, NA)
all_X_var_bureau$Rule_EN_ratio_enquiry_to_loans_6mon <- ifelse(all_X_var_bureau$Rule_EN_enquiry_count_6m > 6,
                                                               all_X_var_bureau$Rule_EN_enquiry_count_6m/all_X_var_bureau$Rule_PO_months_6, NA)
all_X_var_bureau$Rule_EN_ratio_enquiry_to_loans_12mon <- ifelse(all_X_var_bureau$Rule_EN_enquiry_count_12m > 6,
                                                                all_X_var_bureau$Rule_EN_enquiry_count_12m/all_X_var_bureau$Rule_PO_months_12, NA)

all_X_var_bureau$Rule_EN_ratio_enquiry_to_loans_3mon[is.infinite(all_X_var_bureau$Rule_EN_ratio_enquiry_to_loans_3mon)] <- NA
all_X_var_bureau$Rule_EN_ratio_enquiry_to_loans_6mon[is.infinite(all_X_var_bureau$Rule_EN_ratio_enquiry_to_loans_6mon)] <- NA
all_X_var_bureau$Rule_EN_ratio_enquiry_to_loans_12mon[is.infinite(all_X_var_bureau$Rule_EN_ratio_enquiry_to_loans_12mon)] <- NA





## 5. Combine  ------------------------------------------------------------------------------------------------

## 5.1 combine with rules PO
rules_PO <- left_join(rules_PO, all_X_var_bureau,by = c('deal_no','customer_code','applicant_id','loan_type'))
rules_PO <- left_join(rules_PO, exposure_df,by = c('deal_no','customer_code','applicant_id','loan_type'))
rm(all_X_var_bureau,exposure_df)


original_cols <- names(rules_PO)[grep("Rule_",names(rules_PO))]

rm(month_day_mapping)



## 6. Create rules ---------------------------------------------------------------------------------------------

## 6.1 PO rules - x loans taken in y months
req_cols <- c(
  "Rule_PO_months_12",
  "Rule_PO_months_12_AL",
  "Rule_PO_months_12_AL_live",
  "Rule_PO_months_12_Gold",
  "Rule_PO_months_12_Gold_live",
  "Rule_PO_months_12_live",
  # "Rule_PO_months_12_secured",
  "Rule_PO_months_12_TW",
  "Rule_PO_months_12_TW_live",
  "Rule_PO_months_12_unsec",
  "Rule_PO_months_3",
  "Rule_PO_months_3_AL",
  "Rule_PO_months_3_AL_live",
  "Rule_PO_months_3_Gold",
  "Rule_PO_months_3_Gold_live",
  "Rule_PO_months_3_live",
  # "Rule_PO_months_3_secured",
  "Rule_PO_months_3_TW",
  "Rule_PO_months_3_TW_live",
  "Rule_PO_months_3_unsec",
  "Rule_PO_months_6",
  "Rule_PO_months_6_AL",
  "Rule_PO_months_6_AL_live",
  "Rule_PO_months_6_Gold",
  "Rule_PO_months_6_Gold_live",
  "Rule_PO_months_6_live",
  # "Rule_PO_months_6_secured",
  "Rule_PO_months_6_TW",
  "Rule_PO_months_6_TW_live",
  "Rule_PO_months_6_unsec",
  "Rule_PO_months_all",
  "Rule_PO_months_closed",
  "Rule_PO_months_live"
  
)

for(col in req_cols){
  value_list <- c(1,2,3,5)
  for(value in value_list){
    new_col <- paste0(col,"_GE_",value)
    rules_PO[,new_col] <- as.numeric(ifelse(rules_PO[,col] >= value, 1, 0))
  }
}




## 6.2 Enquiry to loan ratio rules
req_cols <- c(
  "Rule_EN_ratio_enquiry_to_loans_12mon",
  "Rule_EN_ratio_enquiry_to_loans_3mon",
  "Rule_EN_ratio_enquiry_to_loans_6mon"
)

for(col in req_cols){
  
  value_list <- c(3,4,5,6,7)
  for(value in value_list){
    new_col <- paste0(col,"_GE_",value)
    rules_PO[,new_col] <- as.numeric(ifelse(rules_PO[,col] >= value, 1, 0))
  }
  
  # new_col <- paste0(col,"_LT1")
  # rules_PO[,new_col] <- as.numeric(ifelse(rules_PO[,col] < 1, 1,0))
  # 
  # new_col <- paste0(col,"_1to3")
  # rules_PO[,new_col] <- as.numeric(ifelse(rules_PO[,col] >=1 & rules_PO[,col] <= 3, 1,0))
  # 
  # new_col <- paste0(col,"_4to7")
  # rules_PO[,new_col] <- as.numeric(ifelse(rules_PO[,col] > 3 & rules_PO[,col] <= 7, 1,0))
  # 
  # new_col <- paste0(col,"_GT7")
  # rules_PO[,new_col] <- as.numeric(ifelse(rules_PO[,col] > 7, 1,0))
  
}



## 6.3 Credit card utilization
req_cols <- c(
  'Rule_PO_outstanding_by_sanctioned_amount_CC_util'
)

for(col in req_cols){
  
  new_col <- paste0(col,"_LE25pct")
  rules_PO[,new_col] <- as.numeric(ifelse(rules_PO[,col] <= 0.25, 1,0))
  
  new_col <- paste0(col,"_LE50pct")
  rules_PO[,new_col] <- as.numeric(ifelse(rules_PO[,col] <= 0.5, 1,0))
  
  new_col <- paste0(col,"_GE75pct")
  rules_PO[,new_col] <- as.numeric(ifelse(rules_PO[,col] >= 0.75, 1,0))
  
  new_col <- paste0(col,"_GE90pct")
  rules_PO[,new_col] <- as.numeric(ifelse(rules_PO[,col] >= 0.9, 1,0))
  
}







# ## 6.4 Sanctioned amount based rules
# 
# ## 6.4.1 subset data based on loan types
# rules_PO_2W <- rules_PO %>% filter(loan_type == '2W-New')
# rules_PO_PV <- rules_PO %>% filter(loan_type %in% c('PV-New','PV-Used'))
# 
# 
# 
# ## 6.4.2 Function - create sanctioned amount rules
# create_sanctioned_amount_rules <- function(req_cols_temp, rules_PO_temp, percentile_25, percentile_75){
# 
#   for(col in req_cols_temp){
# 
#     new_col <- paste0(col,"_Low")
#     rules_PO_temp[,new_col] <- as.numeric(ifelse(rules_PO_temp[,col] >= 3000 & rules_PO_temp[,col] <= percentile_25, 1,0))
# 
#     new_col <- paste0(col,"_Medium")
#     rules_PO_temp[,new_col] <- as.numeric(ifelse(rules_PO_temp[,col] > percentile_25 & rules_PO_temp[,col] < percentile_75, 1,0))
# 
#     new_col <- paste0(col,"_High")
#     rules_PO_temp[,new_col] <- as.numeric(ifelse(rules_PO_temp[,col] >= percentile_75, 1,0))
# 
#   }
# 
#   return(rules_PO_temp)
# 
# }
# 
# 
# 
# 
# req_cols_HL <- c(
#   "Rule_PO_HL_live_sanctioned_amount_12mon",
#   "Rule_PO_HL_live_sanctioned_amount_3mon",
#   "Rule_PO_HL_live_sanctioned_amount_6mon",
#   "Rule_PO_HL_sanctioned_amount_12mon",
#   "Rule_PO_HL_sanctioned_amount_3mon",
#   "Rule_PO_HL_sanctioned_amount_6mon"
# )
# 
# 
# req_cols_PL <- c(
#   "Rule_PO_PL_live_sanctioned_amount_12mon",
#   "Rule_PO_PL_live_sanctioned_amount_3mon",
#   "Rule_PO_PL_live_sanctioned_amount_6mon",
#   "Rule_PO_PL_sanctioned_amount_12mon",
#   "Rule_PO_PL_sanctioned_amount_3mon",
#   "Rule_PO_PL_sanctioned_amount_6mon"
# )
# 
# 
# req_cols_AL <- c(
#   "Rule_PO_AL_live_sanctioned_amount_12mon",
#   "Rule_PO_AL_live_sanctioned_amount_3mon",
#   "Rule_PO_AL_live_sanctioned_amount_6mon",
#   "Rule_PO_AL_sanctioned_amount_12mon",
#   "Rule_PO_AL_sanctioned_amount_3mon",
#   "Rule_PO_AL_sanctioned_amount_6mon"
# )
# 
# 
# req_cols_TW <- c(
#   "Rule_PO_TW_live_sanctioned_amount_12mon",
#   "Rule_PO_TW_live_sanctioned_amount_3mon",
#   "Rule_PO_TW_live_sanctioned_amount_6mon",
#   "Rule_PO_TW_sanctioned_amount_12mon",
#   "Rule_PO_TW_sanctioned_amount_3mon",
#   "Rule_PO_TW_sanctioned_amount_6mon"
# )
# 
# 
# req_cols_Gold <- c(
#   "Rule_PO_Gold_live_sanctioned_amount_12mon",
#   "Rule_PO_Gold_live_sanctioned_amount_3mon",
#   "Rule_PO_Gold_live_sanctioned_amount_6mon",
#   "Rule_PO_Gold_sanctioned_amount_12mon",
#   "Rule_PO_Gold_sanctioned_amount_3mon",
#   "Rule_PO_Gold_sanctioned_amount_6mon"
# )
# 
# 
# 
# 
# 
# ## 6.4.3 Distribution of Sanctioned amount for AL, GL, HL, PL, TW for 2W New
# for (loan_notation_temp in loan_notation_list) {
#   temp_subset <- tradelines %>% filter(loan_notation == loan_notation_temp)
#   temp_subset <- temp_subset %>% filter(applicant_id %in% unique(rules_PO_2W$applicant_id))
#   temp_subset <- temp_subset %>% filter(high_credit_sanctioned_amount > 3000)
#   print(paste0("-----------------------",loan_notation_temp,"------------------------"))
#   print(quantile(temp_subset$high_credit_sanctioned_amount, c(0.25,0.5,0.75)))
# }
# 
# 
# 
# ## 6.4.3.1 Function - Create rules for HL Sanctioned amount
# percentile_25_temp <- 300000
# percentile_75_temp <- 1300000
# 
# rules_PO_2W <- create_sanctioned_amount_rules(req_cols_temp = req_cols_HL,
#                                               rules_PO_temp = rules_PO_2W,
#                                               percentile_25 = percentile_25_temp,
#                                               percentile_75 = percentile_75_temp)
# 
# 
# 
# ## 6.4.3.2 Function - Create rules for PL Sanctioned amount
# percentile_25_temp <- 60000
# percentile_75_temp <- 250000
# 
# rules_PO_2W <- create_sanctioned_amount_rules(req_cols_temp = req_cols_PL,
#                                               rules_PO_temp = rules_PO_2W,
#                                               percentile_25 = percentile_25_temp,
#                                               percentile_75 = percentile_75_temp)
# 
# 
# 
# ## 6.4.3.3 Function - Create rules for AL Sanctioned amount
# percentile_25_temp <- 200000
# percentile_75_temp <- 500000
# 
# rules_PO_2W <- create_sanctioned_amount_rules(req_cols_temp = req_cols_AL,
#                                               rules_PO_temp = rules_PO_2W,
#                                               percentile_25 = percentile_25_temp,
#                                               percentile_75 = percentile_75_temp)
# 
# 
# 
# ## 6.4.3.4 Function - Create rules for TW Sanctioned amount
# percentile_25_temp <- 30000
# percentile_75_temp <- 50000
# 
# rules_PO_2W <- create_sanctioned_amount_rules(req_cols_temp = req_cols_TW,
#                                               rules_PO_temp = rules_PO_2W,
#                                               percentile_25 = percentile_25_temp,
#                                               percentile_75 = percentile_75_temp)
# 
# 
# 
# ## 6.4.3.5 Function - Create rules for Gold Sanctioned amount
# percentile_25_temp <- 20000
# percentile_75_temp <- 80000
# 
# rules_PO_2W <- create_sanctioned_amount_rules(req_cols_temp = req_cols_Gold,
#                                               rules_PO_temp = rules_PO_2W,
#                                               percentile_25 = percentile_25_temp,
#                                               percentile_75 = percentile_75_temp)
# 
# 
# 
# 
# 
# 
# 
# 
# ## 6.4.4 Distribution of Sanctioned amount for AL, GL, HL, PL, TW for PV New
# for (loan_notation_temp in loan_notation_list) {
#   temp_subset <- tradelines %>% filter(loan_notation == loan_notation_temp)
#   temp_subset <- temp_subset %>% filter(applicant_id %in% unique(rules_PO_PV$applicant_id))
#   temp_subset <- temp_subset %>% filter(high_credit_sanctioned_amount > 3000)
#   print(paste0("-----------------------",loan_notation_temp,"------------------------"))
#   print(quantile(temp_subset$high_credit_sanctioned_amount, c(0.25,0.5,0.75)))
# }
# 
# 
# ## 6.4.4.1 Function - Create rules for HL Sanctioned amount
# percentile_25_temp <- 450000
# percentile_75_temp <- 1850000
# 
# rules_PO_PV <- create_sanctioned_amount_rules(req_cols_temp = req_cols_HL,
#                                               rules_PO_temp = rules_PO_PV,
#                                               percentile_25 = percentile_25_temp,
#                                               percentile_75 = percentile_75_temp)
# 
# 
# ## 6.4.4.2 Function - Create rules for PL Sanctioned amount
# percentile_25_temp <- 70000
# percentile_75_temp <- 300000
# 
# rules_PO_PV <- create_sanctioned_amount_rules(req_cols_temp = req_cols_PL,
#                                               rules_PO_temp = rules_PO_PV,
#                                               percentile_25 = percentile_25_temp,
#                                               percentile_75 = percentile_75_temp)
# 
# 
# ## 6.4.4.3 Function - Create rules for AL Sanctioned amount
# percentile_25_temp <- 250000
# percentile_75_temp <- 550000
# 
# rules_PO_PV <- create_sanctioned_amount_rules(req_cols_temp = req_cols_AL,
#                                               rules_PO_temp = rules_PO_PV,
#                                               percentile_25 = percentile_25_temp,
#                                               percentile_75 = percentile_75_temp)
# 
# ## 6.4.4.4 Function - Create rules for TW Sanctioned amount
# percentile_25_temp <- 35000
# percentile_75_temp <- 50000
# 
# 
# rules_PO_PV <- create_sanctioned_amount_rules(req_cols_temp = req_cols_TW,
#                                               rules_PO_temp = rules_PO_PV,
#                                               percentile_25 = percentile_25_temp,
#                                               percentile_75 = percentile_75_temp)
# 
# ## 6.4.4.5 Function - Create rules for Gold Sanctioned amount
# percentile_25_temp <- 35000
# percentile_75_temp <- 120000
# 
# rules_PO_PV <- create_sanctioned_amount_rules(req_cols_temp = req_cols_Gold,
#                                               rules_PO_temp = rules_PO_PV,
#                                               percentile_25 = percentile_25_temp,
#                                               percentile_75 = percentile_75_temp)
# 
# 
# 
# rules_PO <- rbind(rules_PO_2W,rules_PO_PV)
# rm(rules_PO_2W,rules_PO_PV)
# rm(temp_subset)
# rm(tradelines)




## 6.5 Keep only rules
new_cols <- colnames(rules_PO)
new_cols <- new_cols[new_cols %notin% original_cols]

rules_PO <- rules_PO %>% dplyr::select(new_cols)




## 7. CV Demog Rules ---------------------------------------------------------------------------------------------

## 7.1 Load CV demog variables
load_rdata_intermediate("cleaned_data//cv_demog.rdata")

## 7.2 Load ECN mapping
load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
cibil_v3$appliation_id <- NULL
cibil_v3$cibil_score_v3 <- NULL


## 7.3 join data
cv_demog_data <- inner_join(cibil_v3,cv_demog,by = 'ECN')
rm(cibil_v3,cv_demog)

cv_demog_data <- cv_demog_data %>% filter(deal_no %in% unique(final_output$deal_no))


## 7.4 select required columns
cv_demog_data <- cv_demog_data %>% dplyr::select(deal_no,
                                                 customer_code,
                                                 phones_reported_3m,
                                                 phones_reported_6m,
                                                 phones_reported_12m,
                                                 addresses_reported_3m,
                                                 addresses_reported_6m,
                                                 addresses_reported_12m)

## 7.5 create rules
req_cols <- c(
  "phones_reported_3m",
  "phones_reported_6m",
  "phones_reported_12m",
  "addresses_reported_3m",
  "addresses_reported_6m",
  "addresses_reported_12m"
)

for(col in req_cols){
  value_list <- c(1,2,3,4,5)
  for(value in value_list){
    new_col <- paste0("Rule_CV_Demog_",col,"_GE_",value)
    cv_demog_data[,new_col] <- as.numeric(ifelse(cv_demog_data[,col] >= value, 1, 0))
  }
}


## 7.6 Keep only rules
new_cols <- colnames(cv_demog_data)
new_cols <- new_cols[new_cols %notin% req_cols]

rules_CV_Demog <- cv_demog_data %>% dplyr::select(new_cols)
rm(cv_demog_data)




## 8. Combine all rules ---------------------------------------------------------------------------------------------

rules_DL$loan_type <- NULL

all_rules <- left_join(final_output,rules_PO, by = c('deal_no','customer_code','applicant_id'))

all_rules <- left_join(all_rules, rules_DL, by = c('deal_no','customer_code','applicant_id'))

all_rules <- left_join(all_rules, rules_CV_Demog, by = c('deal_no','customer_code'))

rm(final_output,rules_DL,rules_PO,rules_CV_Demog)

all_rules[is.na(all_rules)] <- 0 



belief_testing_rules_disbursals <- all_rules
assert_data_non_empty(belief_testing_rules_disbursals)
save(belief_testing_rules_disbursals,
     file = file.path(
       get_data_path()$data$intermediate,
       "belief_testing",
       "all_rules",
       "belief_testing_rules_disbursals_2W_RF.rdata"
     )
)
# save(belief_testing_rules_disbursals,
#      file = file.path("data//intermediate//belief_testing//all_rules//belief_testing_rules_disbursals_2W_RF.rdata"
#      )
# )



# rm(list =ls())








