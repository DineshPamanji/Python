############################################################################################
################## 12 - Bivariate Analysis  ################################################
############################################################################################


## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src", "utils", "load_libraries.R")
source(load_libaries)

io_helper <- file.path("src", "utils", "io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)


## 1. Load data & define functions ----------------------------------------------------------



load_rdata_intermediate("reject_data//variable_data//model_data_PV_New_A_R.rdata")
oot_date_start <- '2018-04-01'
oot_date_end <- '2018-06-30'


## 1.4 define bivariate calculation function
get_bivariates <-
  function(model_data,
           independent_variables,
           dependent_variable) {
    output <- list()
    i <- 0
    
    for (feature in independent_variables) {
      temp_data <-
        model_data %>% dplyr::select(c(feature, dependent_variable))
      temp_data$var <- as.character(feature)
      temp_data$decile <- ntile(temp_data[[feature]], 10)
      
      colnames(temp_data) <-
        c('feature', 'dependent_variable', 'var', 'decile')
      decile_df <- temp_data %>% group_by(var, decile) %>%
        summarise(
          count_deals = length(var),
          avg_val = mean(feature , na.rm = T),
          median_val = median(feature , na.rm = T),
          min_val = min(feature , na.rm = T),
          max_val = max(feature , na.rm = T),
          bad_sum_val = sum(dependent_variable)
        )
      decile_df$good_sum_val <-
        decile_df$count_deals - decile_df$bad_sum_val
      decile_df$odds_ratio <-
        decile_df$bad_sum_val / decile_df$good_sum_val
      decile_df$default_rate <-
        decile_df$bad_sum_val / decile_df$count_deals
      
      i <- i + 1
      output[[i]] <- decile_df
    }
    
    output <- rbindlist(l = output)
    
    return(output)
  }




## 4. Bivariates for PV New - SAL, SENP & SEP Combined ----------------------------------------------

## 4.1 define OOT & DEV data for SAL
oot_data_combined <-
  model_data_PV_New_A_R %>% filter((date >= as.Date(oot_date_start)) &
                                 (date < as.Date(oot_date_end)) &
                                 (Category %in% c('SAL','SENP','SEP')) &
                                   (tag %in% c('A','R1')))

dev_data_combined <-
  model_data_PV_New_A_R %>% filter(UID %notin% unique(oot_data_combined$UID) &
                                 (Category %in% c('SAL','SENP','SEP')) &
                                   (tag %in% c('A','R1')))



# oot_data_combined <-
#   model_data_PV_New_A_R %>% filter((date >= as.Date(oot_date_start)) &
#                                      (date < as.Date(oot_date_end)) &
#                                      (Category %in% c('SAL','SENP','SEP')) &
#                                      (tag == 'R1'))
# 
# dev_data_combined <-
#   model_data_PV_New_A_R %>% filter(UID %notin% unique(oot_data_combined$UID) &
#                                      (Category %in% c('SAL','SENP','SEP'))&
#                                      (tag == 'R1'))
# 
# 
# 
# oot_data_combined <-
#   model_data_PV_New_A_R %>% filter((date >= as.Date(oot_date_start)) &
#                                      (date < as.Date(oot_date_end)) &
#                                      (Category %in% c('SAL','SENP','SEP'))&
#                                      (tag == 'R2'))
# 
# dev_data_combined <-
#   model_data_PV_New_A_R %>% filter(UID %notin% unique(oot_data_combined$UID) &
#                                      (Category %in% c('SAL','SENP','SEP'))&
#                                      (tag == 'R2'))


## 4.2 get the list of features for SAL segment
selected_var_combined <-
  c(
    'Var_credit_vintage',
    'Var_PO_closed_sanction_amount_excl_CC',
    'Var_sanctioned_amount_live_loans',
    'Var_outstanding_amount_live_loans',
    'Var_outstanding_by_sanctioned_amount_live_loans',
    'Var_sanctioned_by_outstanding_amount_live_loans',
    'Var_PO_closed_excl_CC',
    
    'Var_EN_enquiry_count_6m',
    'Var_EN_enquiry_count_6m_non_CC',
    'Var_EN_enquiry_count_6m_non_CC_CD',
    
    'Var_EN_enquiry_count_3m',
    'Var_EN_enquiry_count_3m_non_CC',
    'Var_EN_enquiry_count_3m_non_CC_CD',
    
    'Var_DL_HL_paid_GE_12mon',
    'Var_DL_HL_paid_GE_12mon_flag',
    'Var_DL_Gold_paid_GE_12mon',
    'Var_DL_Gold_paid_GE_12mon_flag',
    'Var_DL_Gold_paid_GE_6mon',
    'Var_DL_Gold_paid_GE_6mon_flag',
    'Var_DL_Education_paid_GE_6mon',
    'Var_DL_Education_paid_GE_6mon_flag',
    'Var_DL_Education_paid_GE_12mon',
    'Var_DL_Education_paid_GE_12mon_flag',
    
    
    'Var_DL_all_30dpd_3mon',
    'Var_DL_non_CC_CD_30dpd_3mon',
    'Var_DL_all_30dpd_6mon',
    'Var_DL_non_CC_CD_30dpd_6mon',
    'Var_DL_all_30dpd_12mon',
    'Var_DL_non_CC_CD_30dpd_12mon',
    
    'Var_DL_all_60dpd_3mon',
    'Var_DL_non_CC_CD_60dpd_3mon',
    'Var_DL_all_60dpd_6mon',
    'Var_DL_non_CC_CD_60dpd_6mon',
    'Var_DL_all_60dpd_12mon',
    'Var_DL_non_CC_CD_60dpd_12mon'
  )



## 4.3 get DEV bivariates
bivar_PV_combined_dev <-
  get_bivariates(
    model_data = dev_data_combined,
    independent_variables = selected_var_combined,
    dependent_variable = 'bad_loan'
  )

## 4.4 get OOT bivariates
bivar_PV_combined_oot <-
  get_bivariates(
    model_data = oot_data_combined,
    independent_variables = selected_var_combined,
    dependent_variable = 'bad_loan'
  )


## 4.5 save outputs
save_csv_output(bivar_PV_combined_dev, "bivariates//PV_New//A_R//bivar_PV_combined_dev.csv")
save_csv_output(bivar_PV_combined_oot, "bivariates//PV_New//A_R//bivar_PV_combined_oot.csv")

# save_csv_output(bivar_PV_combined_dev, "bivariates//PV_New//A_R//bivar_PV_combined_dev_R1.csv")
# save_csv_output(bivar_PV_combined_oot, "bivariates//PV_New//A_R//bivar_PV_combined_oot_R1.csv")
# 
# 
# save_csv_output(bivar_PV_combined_dev, "bivariates//PV_New//A_R//bivar_PV_combined_dev_R2.csv")
# save_csv_output(bivar_PV_combined_oot, "bivariates//PV_New//A_R//bivar_PV_combined_oot_R2.csv")

rm(list=ls())



# 
# R1_data <- model_data_PV_New_A_R %>% filter(tag == 'R1')
# R1_data$cibil_pentile <- ntile(R1_data$CIBIL_SCORE,5)
# R1_data %>% group_by(cibil_pentile) %>% summarise(min = min(CIBIL_SCORE), max = max(CIBIL_SCORE), default = mean(bad_loan))
