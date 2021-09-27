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

## 1.1 Load 2W New data 
# load_rdata_intermediate("model_data//model_data_2W_New.rdata")
load_rdata_intermediate("model_data//model_data_2W_New_vcheck.rdata")

## 1.2 load PV New and Used data
# load_rdata_intermediate("model_data//model_data_PV_New.rdata")
# load_rdata_intermediate("model_data//model_data_PV_Used.rdata")

load_rdata_intermediate("model_data//model_data_PV_New_vcheck.rdata")
load_rdata_intermediate("model_data//model_data_PV_Used_vcheck.rdata")

load_rdata_intermediate("model_data//model_data_2W_RF.rdata")


## 1.3 set OOT window
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



## 2. Bivariates for 2W New - SENP & SEP Combined ---------------------------------------------------

## 2.1 define OOT & DEV data for SENP-SEP 
oot_data_SENP_SEP <-
  model_data_2W_New %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                 (disbursal_date <= as.Date(oot_date_end)) &
                                 (Category %in% c('SENP','SEP')))

dev_data_SENP_SEP <-
  model_data_2W_New %>% filter(deal_no %notin% unique(oot_data_SENP_SEP$deal_no) &
                                 (Category %in% c('SENP','SEP')))


selected_var_2W_New_SENP_SEP <-
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


## 2.3 get DEV bivariates
bivar_2W_SENP_SEP_dev <-
  get_bivariates(
    model_data = dev_data_SENP_SEP %>% filter(Category %in% c('SENP', 'SEP')),
    independent_variables = selected_var_2W_New_SENP_SEP,
    dependent_variable = 'bad_loan'
  )

## 2.4 get OOT bivariates
bivar_2W_SENP_SEP_oot <-
  get_bivariates(
    model_data = oot_data_SENP_SEP %>% filter(Category %in% c('SENP', 'SEP')),
    independent_variables = selected_var_2W_New_SENP_SEP,
    dependent_variable = 'bad_loan'
  )


## 2.5 save outputs
save_csv_output(bivar_2W_SENP_SEP_dev, "bivariates//2W_New//SENP_SEP//bivar_2W_SENP_SEP_dev.csv")
save_csv_output(bivar_2W_SENP_SEP_oot, "bivariates//2W_New//SENP_SEP//bivar_2W_SENP_SEP_oot.csv")


rm(bivar_2W_SENP_SEP_dev,bivar_2W_SENP_SEP_oot,dev_data_SENP_SEP,oot_data_SENP_SEP)





## 3. Bivariates for 2W New - SAL Combined ---------------------------------------------------

## 3.1 define OOT & DEV data for SAL
oot_data_SAL <-
  model_data_2W_New %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                 (disbursal_date < as.Date(oot_date_end)) &
                                 (Category %in% c('SAL')))

dev_data_SAL <-
  model_data_2W_New %>% filter(deal_no %notin% unique(oot_data_SAL$deal_no) &
                                 (Category %in% c('SAL')))



## 3.2 get the list of features for SAL segment
selected_var_2W_New_SAL <-
  c(
    'Var_EN_enquiry_count_6m_non_CC',
    'Var_EN_enquiry_count_3m_non_CC',
    'Var_outstanding_amount_live_loans',
    'Var_sanctioned_amount_live_loans',
    "Var_sanctioned_by_outstanding_amount_live_loans",
    "Var_outstanding_by_sanctioned_amount_live_loans",
    'Var_credit_vintage',
    'Var_EN_unsec_enquiry_count_6m_non_CC',
    'Var_EN_unsec_enquiry_count_3m_non_CC',
    'Var_EN_enquiry_count_6m',
    'Var_EN_enquiry_count_18m_non_CC',
    'Var_EN_enquiry_count_12m_non_CC',
    'Var_EN_unsec_enquiry_count_18m_non_CC',
    'Var_EN_enquiry_count_3m',
    'Var_DL_HL_paid_GE_12mon',
    'Var_DL_HL_paid_GE_12mon_flag',
    'Var_DL_Gold_paid_GE_12mon',
    "Var_DL_Gold_paid_GE_12mon_flag",
    "Var_DL_Gold_paid_GE_6mon",
    "Var_DL_Gold_paid_GE_6mon_flag",
    "Var_DL_Education_paid_GE_12mon",
    "Var_DL_Education_paid_GE_12mon_flag",            
    "Var_DL_Education_paid_GE_6mon",
    "Var_DL_Education_paid_GE_6mon_flag",   
    'Var_EN_enquiry_count_6m_non_CC_CD',
    'Var_PO_closed_sanction_amount_excl_CC',
    'Var_PO_closed_excl_CC',
    'Var_PO_live_excl_CC',
    'Var_PO_months_24',
    'Var_DL_all_30dpd_3mon',
    'Var_PO_months_6_TW_live',
    'Var_PO_months_6_TW',
    'Var_PO_months_12_Gold_live',
    'Var_DL_non_CC_CD_30dpd_3mon',
    'Var_PO_months_12_TW',
    'Var_PO_months_24_live',
    'Var_DL_unsec_30dpd_6mon',
    'Var_DL_all_30dpd_6mon',
    'Var_PO_months_12_BL',
    'Var_PO_months_12',
    'Var_DL_non_CC_CD_60dpd_3mon',
    'Var_DL_all_60dpd_12mon',
    'Var_DL_unsec_60dpd_6mon',
    'Var_DL_all_60dpd_3mon',
    'Var_DL_unsec_30dpd_3mon'
  )

## 3.3 get DEV bivariates
bivar_2W_SAL_dev <-
  get_bivariates(
    model_data = dev_data_SAL %>% filter(Category %in% c('SAL')),
    independent_variables = selected_var_2W_New_SAL,
    dependent_variable = 'bad_loan'
  )

## 3.4 get OOT bivariates
bivar_2W_SAL_oot <-
  get_bivariates(
    model_data = oot_data_SAL %>% filter(Category %in% c('SAL')),
    independent_variables = selected_var_2W_New_SAL,
    dependent_variable = 'bad_loan'
  )


## 3.5 save outputs
fwrite(bivar_2W_SAL_dev, "data//output//bivariates//bivar_2W_SAL_dev_v2.csv")
fwrite(bivar_2W_SAL_oot, "data//output//bivariates//bivar_2W_SAL_oot_v2.csv")


rm(bivar_2W_SAL_dev,bivar_2W_SAL_oot,dev_data_SAL,oot_data_SAL)








## 4. Bivariates for 2W New - SAL, SENP & SEP Combined ----------------------------------------------

## 4.1 define OOT & DEV data for SAL
oot_data_combined <-
  model_data_2W_New %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                 (disbursal_date < as.Date(oot_date_end)) &
                                 (Category %in% c('SAL','SENP','SEP')))

dev_data_combined <-
  model_data_2W_New %>% filter(deal_no %notin% unique(oot_data_combined$deal_no) &
                                 (Category %in% c('SAL','SENP','SEP')))



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
bivar_2W_combined_dev <-
  get_bivariates(
    model_data = dev_data_combined,
    independent_variables = selected_var_combined,
    dependent_variable = 'bad_loan'
  )

## 4.4 get OOT bivariates
bivar_2W_combined_oot <-
  get_bivariates(
    model_data = oot_data_combined,
    independent_variables = selected_var_combined,
    dependent_variable = 'bad_loan'
  )


## 4.5 save outputs
# save_csv_output(bivar_2W_combined_dev, "bivariates//2W_New//Combined//bivar_2W_combined_dev.csv")
# save_csv_output(bivar_2W_combined_oot, "bivariates//2W_New//Combined//bivar_2W_combined_oot.csv")

save_csv_output(bivar_2W_combined_dev, "bivariates//2W_New//Combined//vcheck//bivar_2W_combined_dev.csv")
save_csv_output(bivar_2W_combined_oot, "bivariates//2W_New//Combined//vcheck//bivar_2W_combined_oot.csv")

rm(bivar_2W_combined_dev,bivar_2W_combined_oot,dev_data_combined,oot_data_combined)
# rm(list=ls())





## 5. Bivariates for PV New SENP,SEP ------------------

## 5.1 define OOT & DEV data for SENP-SEP 
oot_data_SENP_SEP <-
  model_data_PV_New %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                 (disbursal_date <= as.Date(oot_date_end)) &
                                 (Category %in% c('SENP','SEP')))

dev_data_SENP_SEP <-
  model_data_PV_New %>% filter(deal_no %notin% unique(oot_data_SENP_SEP$deal_no) &
                                 (Category %in% c('SENP','SEP')))


selected_var_PV_New_SENP_SEP <-
  c(
    'Var_EN_enquiry_count_12m_non_CC_CD',
    'Var_EN_enquiry_count_12m',
    'Var_EN_enquiry_count_6m_non_CC_CD',
    'Var_outstanding_by_sanctioned_amount_live_loans',
    'Var_sanctioned_by_outstanding_amount_live_loans',
    'Var_EN_enquiry_count_6m',
    'Var_DL_HL_paid_GE_12mon',
    'Var_PO_months_24_Gold',
    'Var_DL_Gold_paid_GE_6mon',
    'Var_DL_Gold_paid_GE_6mon_flag',
    'Var_DL_Education_paid_GE_6mon',
    'Var_PO_months_12_AL',
    'Var_DL_Gold_paid_GE_12mon',
    'Var_DL_Gold_paid_GE_12mon_flag',
    'Var_EN_enquiry_count_3m_non_CC',
    'Var_DL_all_30dpd_6mon',
    'Var_EN_enquiry_count_3m',
    'Var_PO_months_12_AL_live',
    'Var_DL_all_30dpd_12mon',
    'Var_PO_live_excl_CC',
    'Var_DL_all_30dpd_3mon',
    'Var_credit_vintage',
    'Var_DL_HL_paid_GE_12mon_flag',
    'Var_PO_months_6',
    'Var_PO_months_6_live',
    'Var_DL_all_60dpd_1mon',
    'Var_DL_unsec_60dpd_1mon',
    'Var_DL_non_CC_CD_30dpd_12mon',
    'Var_DL_non_CC_CD_30dpd_6mon',
    'Var_PO_closed_excl_CC'
  )


## 5.3 get DEV bivariates
bivar_PV_SENP_SEP_dev <-
  get_bivariates(
    model_data = dev_data_SENP_SEP %>% filter(Category %in% c('SENP', 'SEP')),
    independent_variables = selected_var_PV_New_SENP_SEP,
    dependent_variable = 'bad_loan'
  )

## 5.4 get OOT bivariates
bivar_PV_SENP_SEP_oot <-
  get_bivariates(
    model_data = oot_data_SENP_SEP %>% filter(Category %in% c('SENP', 'SEP')),
    independent_variables = selected_var_PV_New_SENP_SEP,
    dependent_variable = 'bad_loan'
  )


## 5.5 save outputs
fwrite(bivar_PV_SENP_SEP_dev, "data//output//bivariates//PV_New//SENP_SEP//bivar_PV_SENP_SEP_dev.csv")
fwrite(bivar_PV_SENP_SEP_oot, "data//output//bivariates//PV_New//SENP_SEP//bivar_PV_SENP_SEP_oot.csv")


rm(bivar_PV_SENP_SEP_dev,bivar_PV_SENP_SEP_oot,dev_data_SENP_SEP,oot_data_SENP_SEP)





## 6. Bivariates for PV New - SAL ---------------------------------------------------

## 6.1 define OOT & DEV data for SAL
oot_data_SAL <-
  model_data_PV_New %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                 (disbursal_date < as.Date(oot_date_end)) &
                                 (Category %in% c('SAL')))

dev_data_SAL <-
  model_data_PV_New %>% filter(deal_no %notin% unique(oot_data_SAL$deal_no) &
                                 (Category %in% c('SAL')))



## 6.2 get the list of features for SAL segment
selected_var_PV_New_SAL <-
  c(
    'Var_EN_enquiry_count_12m_non_CC_CD',
    'Var_EN_enquiry_count_12m',
    'Var_EN_enquiry_count_6m_non_CC_CD',
    'Var_outstanding_by_sanctioned_amount_live_loans',
    'Var_sanctioned_by_outstanding_amount_live_loans',
    'Var_EN_enquiry_count_6m',
    'Var_DL_HL_paid_GE_12mon',
    'Var_PO_months_24_Gold',
    'Var_DL_Gold_paid_GE_6mon',
    'Var_DL_Gold_paid_GE_6mon_flag',
    'Var_DL_Education_paid_GE_6mon',
    'Var_PO_months_12_AL',
    'Var_DL_Gold_paid_GE_12mon',
    'Var_DL_Gold_paid_GE_12mon_flag',
    'Var_EN_enquiry_count_3m_non_CC',
    'Var_DL_all_30dpd_6mon',
    'Var_EN_enquiry_count_3m',
    'Var_PO_months_12_AL_live',
    'Var_DL_all_30dpd_12mon',
    'Var_PO_live_excl_CC',
    'Var_DL_all_30dpd_3mon',
    'Var_credit_vintage',
    'Var_DL_HL_paid_GE_12mon_flag',
    'Var_PO_months_6',
    'Var_PO_months_6_live',
    'Var_DL_all_60dpd_1mon',
    'Var_DL_unsec_60dpd_1mon',
    'Var_DL_non_CC_CD_30dpd_12mon',
    'Var_DL_non_CC_CD_30dpd_6mon',
    'Var_PO_closed_excl_CC'
  )

## 6.3 get DEV bivariates
bivar_PV_SAL_dev <-
  get_bivariates(
    model_data = dev_data_SAL %>% filter(Category %in% c('SAL')),
    independent_variables = selected_var_PV_New_SAL,
    dependent_variable = 'bad_loan'
  )

## 6.4 get OOT bivariates
bivar_PV_SAL_oot <-
  get_bivariates(
    model_data = oot_data_SAL %>% filter(Category %in% c('SAL')),
    independent_variables = selected_var_PV_New_SAL,
    dependent_variable = 'bad_loan'
  )


## 6.5 save outputs
fwrite(bivar_PV_SAL_dev, "data//output//bivariates//PV_New//SAL//bivar_PV_SAL_dev.csv")
fwrite(bivar_PV_SAL_oot, "data//output//bivariates//PV_New//SAL//bivar_PV_SAL_oot.csv")


rm(bivar_PV_SAL_dev,bivar_PV_SAL_oot,dev_data_SAL,oot_data_SAL)








## 7. Bivariates for PV New - SAL, SENP & SEP Combined ----------------------------------------------

## 7.1 define OOT & DEV data for SAL
oot_data_combined <-
  model_data_PV_New %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                 (disbursal_date < as.Date(oot_date_end)) &
                                 (Category %in% c('SAL','SENP','SEP')))

dev_data_combined <-
  model_data_PV_New %>% filter(deal_no %notin% unique(oot_data_combined$deal_no) &
                                 (Category %in% c('SAL','SENP','SEP')))



## 7.2 get the list of features for combined segment
selected_var_combined <-
  c(
    'Var_credit_vintage',
    'Var_EN_enquiry_count_6m_non_CC',
    'Var_EN_enquiry_count_6m_non_CC_CD',
    'Var_EN_enquiry_count_6m',
    'Var_EN_enquiry_count_12m_non_CC',
    'Var_EN_enquiry_count_3m_non_CC',
    'Var_EN_enquiry_count_3m_non_CC_CD',
    'Var_EN_enquiry_count_3m',
    'Var_EN_enquiry_count_12m',
    'Var_DL_all_30dpd_6mon',
    'Var_DL_non_CC_CD_30dpd_6mon',
    'Var_sanctioned_amount_live_loans',
    'Var_outstanding_by_sanctioned_amount_live_loans',
    'Var_sanctioned_by_outstanding_amount_live_loans',
    'Var_DL_all_30dpd_12mon',
    'Var_DL_all_30dpd_3mon',
    'Var_DL_non_CC_CD_30dpd_12mon',
    'Var_DL_non_CC_CD_30dpd_3mon',
    'Var_DL_HL_paid_GE_12mon',
    'Var_DL_HL_paid_GE_12mon_flag',
    'Var_PO_closed_sanction_amount_excl_CC',
    'Var_DL_Education_paid_GE_12mon',
    'Var_DL_Education_paid_GE_12mon_flag',
    'Var_DL_Education_paid_GE_6mon',
    'Var_DL_Education_paid_GE_6mon_flag',
    'Var_PO_months_18',
    'Var_PO_months_6',
    'Var_PO_months_6_live',
    'Var_DL_non_CC_CD_60dpd_12mon',
    'Var_PO_closed_excl_CC',
    'Var_PO_live_excl_CC',
    'Var_PO_months_12',
    'Var_DL_Gold_paid_GE_12mon',
    'Var_DL_Gold_paid_GE_12mon_flag'
  )
  

## 7.4 get OOT bivariates
bivar_PV_combined_dev <-
  get_bivariates(
    model_data = dev_data_combined,
    independent_variables = selected_var_combined,
    dependent_variable = 'bad_loan'
  )

## 7.5 get OOT bivariates
bivar_PV_combined_oot <-
  get_bivariates(
    model_data = oot_data_combined,
    independent_variables = selected_var_combined,
    dependent_variable = 'bad_loan'
  )




## 7.6 save outputs
# fwrite(bivar_PV_combined_dev, "data//output//bivariates//PV_New//Combined//bivar_PV_combined_dev_v2.csv")
# fwrite(bivar_PV_combined_oot, "data//output//bivariates//PV_New//Combined//bivar_PV_combined_oot.csv")


fwrite(bivar_PV_combined_dev, "data//output//bivariates//PV_New//Combined//vcheck//bivar_PV_combined_dev.csv")
fwrite(bivar_PV_combined_oot, "data//output//bivariates//PV_New//Combined//vcheck//bivar_PV_combined_oot.csv")

rm(bivar_PV_combined_dev,bivar_PV_combined_oot,dev_data_combined,oot_data_combined)


## 8. Bivariates for PV Used - SAL, SENP & SEP Combined ----------------------------------------------

oot_date_start <- '2018-04-01'
# oot_date_end <- '2018-07-31'
oot_date_end <- '2018-06-30'


## 8.1 define OOT & DEV data for SAL
oot_data_combined <-
  model_data_PV_Used %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                 (disbursal_date < as.Date(oot_date_end)) &
                                 (Category %in% c('SAL','SENP','SEP')))

dev_data_combined <-
  model_data_PV_Used %>% filter(deal_no %notin% unique(oot_data_combined$deal_no) &
                                 (Category %in% c('SAL','SENP','SEP')))



## 8.2 get the list of features for combined segment
selected_var_combined <-
  c(
    'Var_EN_unsec_enquiry_count_12m',
    'Var_EN_unsec_enquiry_count_12m_non_CC',
    'Var_EN_enquiry_count_12m',
    'Var_EN_enquiry_count_12m_non_CC',
    
    'Var_EN_unsec_enquiry_count_6m',
    'Var_EN_unsec_enquiry_count_6m_non_CC',
    'Var_EN_enquiry_count_6m',
    'Var_EN_enquiry_count_6m_non_CC',
    
    'Var_EN_unsec_enquiry_count_3m',
    'Var_EN_unsec_enquiry_count_3m_non_CC',
    'Var_EN_enquiry_count_3m',
    'Var_EN_enquiry_count_3m_non_CC',
    
    'Var_credit_vintage',
    
    'Var_PO_closed_sanction_amount_excl_CC',
    'Var_PO_closed_excl_CC',
    
    'Var_outstanding_by_sanctioned_amount_live_loans',
    'Var_sanctioned_by_outstanding_amount_live_loans',
    'Var_outstanding_amount_live_loans',
    'Var_sanctioned_amount_live_loans',
    
    'agri_profile_flag',
    
    'Var_DL_HL_paid_GE_12mon',
    'Var_DL_HL_paid_GE_12mon_flag',
    'Var_DL_Gold_paid_GE_12mon',
    'Var_DL_Gold_paid_GE_12mon_flag',
    
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

# selected_var_combined <- c('Var_sanctioned_amount_live_loans',
#                            'Var_outstanding_amount_live_loans',
#                            'Var_sanctioned_by_outstanding_amount_live_loans'
#                            )

## 7.4 get OOT bivariates
bivar_PV_used_dev <-
  get_bivariates(
    model_data = dev_data_combined,
    independent_variables = selected_var_combined,
    dependent_variable = 'bad_loan'
  )

## 7.5 get OOT bivariates
bivar_PV_used_oot <-
  get_bivariates(
    model_data = oot_data_combined,
    independent_variables = selected_var_combined,
    dependent_variable = 'bad_loan'
  )




## 7.6 save outputs
# fwrite(bivar_PV_used_dev, "data//output//bivariates//PV_Used//bivar_PV_combined_dev.csv")
# fwrite(bivar_PV_used_oot, "data//output//bivariates//PV_Used//bivar_PV_combined_oot.csv")

fwrite(bivar_PV_used_dev, "data//output//bivariates//PV_Used//vcheck//bivar_PV_combined_dev.csv")
fwrite(bivar_PV_used_oot, "data//output//bivariates//PV_Used//vcheck//bivar_PV_combined_oot.csv")

rm(bivar_PV_combined_dev,bivar_PV_combined_oot,dev_data_combined,oot_data_combined)






## Bivariates 2W Refinance ------------------------------------------------------------------------------


oot_date_start <- '2018-4-01'
oot_date_end <- '2018-6-30'


## 8.1 define OOT & DEV data for SAL
oot_data_combined <-
  model_data_2W_RF %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                  (disbursal_date < as.Date(oot_date_end)) &
                                  (Category %in% c('SAL','SENP','SEP')))

dev_data_combined <-
  model_data_2W_RF %>% filter(deal_no %notin% unique(oot_data_combined$deal_no) &
                                  (Category %in% c('SAL','SENP','SEP')))



## 8.2 get the list of features for combined segment
selected_var_combined <-
  c(
    "Var_EN_enquiry_count_6m",
    "Var_EN_enquiry_count_6m_non_CD",
    "Var_EN_enquiry_count_6m_non_CC",
    "Var_EN_unsec_enquiry_count_12m",
    "Var_EN_enquiry_count_6m_non_CC_CD",
    "Var_EN_enquiry_count_12m",
    "Var_EN_unsec_enquiry_count_6m",
    "Var_DL_all_30dpd_6mon",
    "Var_DL_all_30dpd_12mon",
    "Var_PO_months_12_live_unsec",
    "Var_PO_months_12_unsec",
    "Var_DL_non_CC_CD_30dpd_6mon",
    "Var_DL_all_30dpd_3mon",
    "Var_DL_unsec_30dpd_6mon",
    "Var_DL_unsec_30dpd_3mon",
    "Var_DL_unsec_30dpd_12mon",
    "Var_PO_months_6_live_unsec",
    "Var_DL_non_CC_CD_30dpd_3mon",
    "Var_PO_months_6_unsec",
    "Var_credit_vintage",
    "Var_DL_unsec_60dpd_6mon",
    "Var_DL_HL_paid_GE_12mon",
    "Var_DL_HL_paid_GE_12mon_flag",
    "Var_PO_months_12_PL_live",
    "Var_PO_months_12_PL",
    "Var_PO_closed_sanction_amount_excl_CC",
    "Var_PO_months_12_unsec_excl_CC_CD",
    "Var_DL_Gold_paid_GE_12mon",
    "Var_DL_Gold_paid_GE_12mon_flag",
    "Var_PO_months_12_live",
    "Var_PO_months_6_PL_live",
    "Var_PO_months_12_Gold_live",
    "Var_PO_months_6_live",
    "Var_sanctioned_amount_live_loans",
    "Var_sanctioned_by_outstanding_amount_live_loans",
    "Var_outstanding_by_sanctioned_amount_live_loans",
    "Var_outstanding_amount_live_loans"
    
  )


## 7.4 get OOT bivariates
bivar_2W_RF_dev <-
  get_bivariates(
    model_data = dev_data_combined,
    independent_variables = selected_var_combined,
    dependent_variable = 'bad_loan'
  )

## 7.5 get OOT bivariates
bivar_2W_RF_oot <-
  get_bivariates(
    model_data = oot_data_combined,
    independent_variables = selected_var_combined,
    dependent_variable = 'bad_loan'
  )




## 7.6 save outputs
fwrite(bivar_2W_RF_dev, "data//output//bivariates//2W_RF//bivar_2W_RF_dev_new.csv")
fwrite(bivar_2W_RF_oot, "data//output//bivariates//2W_RF//bivar_2W_RF_oot_new.csv")


rm(bivar_2W_RF_dev,bivar_2W_RF_oot,dev_data_combined,oot_data_combined)


rm(list=ls())


