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
load_rdata_intermediate("model_data//ntc_model_data_2W_New.rdata")

## 1.2 load PV New and Used data
load_rdata_intermediate("model_data//ntc_model_data_PV_New.rdata")
load_rdata_intermediate("model_data//ntc_model_data_PV_Used.rdata")


## 1.3 set OOT window
oot_date_start <- '2019-03-01'
oot_date_end <- '2019-03-31'


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



## 2. Bivariates for 2W New  ---------------------------------------------------------------------

selected_var <-
  c(
    'age',
    'phone_count_3m',
    'months_since_last_update_addr',
    'phone_count_6m',
    'addr_count_6m',
    'phone_count_12m',
    'months_since_last_update_phone',
    'addr_count_3m',
    'owner',
    'agri_profile_flag'
  )


## 2.1 define oot data & dev data
oot_data <- ntc_model_data_2W_New %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                 (disbursal_date <= as.Date(oot_date_end)))

dev_data <- ntc_model_data_2W_New %>% filter(deal_no %notin% unique(oot_data$deal_no))


## 2.3 get DEV bivariates
bivar_2W_dev <-
  get_bivariates(
    model_data = dev_data,
    independent_variables = selected_var,
    dependent_variable = 'bad_loan'
  )

## 2.4 get OOT bivariates
bivar_2W_oot <-
  get_bivariates(
    model_data = oot_data,
    independent_variables = selected_var,
    dependent_variable = 'bad_loan'
  )


## 2.5 save outputs
save_csv_output(bivar_2W_dev, "bivariates//2W_New//NTC//bivar_2W_NTC_dev_v2.csv")
save_csv_output(bivar_2W_oot, "bivariates//2W_New//NTC//bivar_2W_NTC_oot_v2.csv")


rm(bivar_2W_dev,bivar_2W_oot,oot_data,dev_data)




## 3. Bivariates for PV New  ---------------------------------------------------------------------

# ntc_model_data_PV_New %>% group_by(year_mon) %>% summarise(n=n(),count = sum(bad_loan),def=mean(bad_loan))

## 3.1 define oot data & dev data
oot_date_start <- '2019-03-01'
oot_date_end <- '2019-03-31'


oot_data <- ntc_model_data_PV_New %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                               (disbursal_date <= as.Date(oot_date_end)))

dev_data <- ntc_model_data_PV_New %>% filter(deal_no %notin% unique(oot_data$deal_no))


## 3.2 get DEV bivariates
bivar_PV_dev <-
  get_bivariates(
    model_data = dev_data,
    independent_variables = selected_var,
    dependent_variable = 'bad_loan'
  )

## 3.3 get OOT bivariates
bivar_PV_oot <-
  get_bivariates(
    model_data = oot_data,
    independent_variables = selected_var,
    dependent_variable = 'bad_loan'
  )


## 3.4 save outputs
save_csv_output(bivar_PV_dev, "bivariates//PV_New//NTC//bivar_PV_NTC_dev_v2.csv")
save_csv_output(bivar_PV_oot, "bivariates//PV_New//NTC//bivar_PV_NTC_oot_v2.csv")


rm(bivar_PV_dev,bivar_PV_oot,oot_data,dev_data)




## 4. Bivariates for PV Used  ---------------------------------------------------------------------

ntc_model_data_PV_Used %>% group_by(year_mon) %>% summarise(n=n(),count = sum(bad_loan),def=mean(bad_loan))

## 3.1 define oot data & dev data
oot_data <- ntc_model_data_PV_Used %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                               (disbursal_date <= as.Date(oot_date_end)))

dev_data <- ntc_model_data_PV_Used %>% filter(deal_no %notin% unique(oot_data$deal_no))


## 3.2 get DEV bivariates
bivar_PV_dev <-
  get_bivariates(
    model_data = dev_data,
    independent_variables = selected_var,
    dependent_variable = 'bad_loan'
  )

## 3.3 get OOT bivariates
bivar_PV_oot <-
  get_bivariates(
    model_data = oot_data,
    independent_variables = selected_var,
    dependent_variable = 'bad_loan'
  )


## 3.4 save outputs
save_csv_output(bivar_PV_dev, "bivariates//PV_Used//NTC//bivar_PV_NTC_dev.csv")
save_csv_output(bivar_PV_oot, "bivariates//PV_Used//NTC//bivar_PV_NTC_oot.csv")


rm(bivar_PV_dev,bivar_PV_oot,oot_data,dev_data)








rm(list=ls())


