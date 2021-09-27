##############################################################################################
############################ Bad Tagging #####################################################
##############################################################################################


# 0. Load libraries & functions --------------------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)


# 1. Load disbursals data in monthly dpd format ----------------------------------------------
load_rdata_intermediate("ADS_data//disbursals_final.rdata")


disbursals_final$mob <- disbursals_final$payment_index - disbursals_final$disbursal_index

deal_with_month_payment <- distinct(disbursals_final %>% filter(mob == 0) %>% dplyr::select(deal_no))
deal_with_month_payment <- deal_with_month_payment %>% ungroup()
deal_with_month_payment <- distinct(deal_with_month_payment %>% dplyr::select(deal_no))
deal_with_month_payment <- unique(deal_with_month_payment$deal_no)

disbursals_final$mob <- ifelse(disbursals_final$deal_no %in% deal_with_month_payment, disbursals_final$mob+1, disbursals_final$mob)


# 2. function to tag default definition ------------------------------------------------------

tag_bad_loan <- function(mob_timeframe,DPD_value_cutoff,minDate,maxDate,temp_loan_type){
 
  # subset for loan type & disbursal date range
  temp_disbursal_subset <- disbursals_final %>% ungroup() %>% filter((disbursal_yearmon %in% c(minDate:maxDate)) & (loan_type == temp_loan_type))
  
  # get disbursals with required MOB 
  temp_disbursal_subset <- temp_disbursal_subset %>% filter(mob %in% c(1:mob_timeframe)) 
  
  # get count of available payment months for disbursals
  temp_month_count <- temp_disbursal_subset %>% group_by(deal_no) %>% summarise(month_count = length(unique(payment_index)))
  
  # get disbursals with enough payment month count (same as MOB)
  disbursals_with_enough_datapoints <- distinct(temp_month_count %>% filter(month_count == mob_timeframe) %>% ungroup() %>% dplyr::select(deal_no))
  disbursals_with_enough_datapoints$mob_check <- 1
  
  # subset for disbursals with enough data points
  temp_disbursal_subset <- left_join(temp_disbursal_subset,disbursals_with_enough_datapoints,by = 'deal_no')
  temp_disbursal_subset <- temp_disbursal_subset %>% filter(mob_check == 1) %>% ungroup()
  temp_disbursal_subset$mob_check <- NULL
  
  
  # tag bad loan
  temp_disbursal_subset$bad_loan <- ifelse(temp_disbursal_subset$dpd_status >= DPD_value_cutoff, 1, 0)
  
  # aggregate at deal level
  tagged_bad_loans <- temp_disbursal_subset %>% group_by(deal_no,loan_type) %>% summarise(bad_loan = max(bad_loan, na.rm = T))
  
  return(tagged_bad_loans)
}



## 3. Tag bad loans across products ------------------------------------------------------------

# mob_timeframe = 15
# DPD_value_cutoff = 60
# minDate = 201803
# maxDate = 201812
# temp_loan_type = '2W-New'

## 3.1 tag bad loans in 2W New
bad_loans_2W_New <- tag_bad_loan(mob_timeframe = 15,
                                 DPD_value_cutoff = 60,
                                 minDate = 201803, 
                                 maxDate = 201812,
                                 temp_loan_type = '2W-New')

## 3.2 tag bad loans in PV New
bad_loans_PV_New <- tag_bad_loan(mob_timeframe = 15,
                                 DPD_value_cutoff = 60,
                                 minDate = 201803, 
                                 maxDate = 201812,
                                 temp_loan_type = 'PV-New')

## 3.3 tag bad loans in PV Used
bad_loans_PV_Used <- tag_bad_loan(mob_timeframe = 15,
                                 DPD_value_cutoff = 60,
                                 minDate = 201803, 
                                 maxDate = 201812,
                                 temp_loan_type = 'PV-Used')


## 3.4 append all disburals across products
bad_loans <- rbindlist(l=list(bad_loans_2W_New,bad_loans_PV_New,bad_loans_PV_Used))
bad_loans <- bad_loans %>% ungroup()

rm(bad_loans_2W_New,bad_loans_PV_New,bad_loans_PV_Used)




## 4. Tag ever 90 DPD or 150 DPD --------------------------------------------------------------

## 4.1 create ever 90 DPD flag
disbursals_final$ever_90dpd <- ifelse(disbursals_final$dpd_status >= 90, 1, 0)

## 4.2 create ever 150 DPD flag
disbursals_final$ever_150dpd <- ifelse(disbursals_final$dpd_status >= 150, 1, 0)

## 4.3 aggregate to get at disbursal level
ever_flags <- disbursals_final %>% group_by(deal_no,loan_type) %>% summarise(ever_90dpd = max(ever_90dpd, na.rm = T),
                                                                             ever_150dpd = max(ever_150dpd, na.rm = T))


ever_flags <- ever_flags %>% ungroup()



## 5. Join ever flags & save output ---------------------------------------------------------

## 5.1 join data
bad_loans <- left_join(bad_loans,ever_flags, by = c("deal_no" = "deal_no",
                                                    "loan_type" = "loan_type"))


## 5.2 save data
assert_data_non_empty(bad_loans)
save(bad_loans,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "bad_loans.rdata"
     )
)


rm(ever_flags)


assert_data_non_empty(bad_loans)
save(bad_loans,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "bad_loans_v2.rdata"
     )
)


rm(bad_loans,ever_flags)


## 3. Tag 60 DPD in 12 MOB (for comparison with NTC) ------------------------------------------------------------

## 3.1 tag bad loans in 2W New
bad_loans_2W_New <- tag_bad_loan(mob_timeframe = 12,
                                 DPD_value_cutoff = 60,
                                 minDate = 201803, 
                                 maxDate = 201812,
                                 temp_loan_type = '2W-New')

## 3.2 tag bad loans in PV New
bad_loans_PV_New <- tag_bad_loan(mob_timeframe = 12,
                                 DPD_value_cutoff = 60,
                                 minDate = 201803, 
                                 maxDate = 201812,
                                 temp_loan_type = 'PV-New')

## 3.3 tag bad loans in PV Used
bad_loans_PV_Used <- tag_bad_loan(mob_timeframe = 12,
                                  DPD_value_cutoff = 60,
                                  minDate = 201803, 
                                  maxDate = 201812,
                                  temp_loan_type = 'PV-Used')


## 3.4 append all disburals across products
bad_loans <- rbindlist(l=list(bad_loans_2W_New,bad_loans_PV_New,bad_loans_PV_Used))
bad_loans <- bad_loans %>% ungroup()

rm(bad_loans_2W_New,bad_loans_PV_New,bad_loans_PV_Used)
bad_loans <- data.frame(bad_loans)


assert_data_non_empty(bad_loans)
save(bad_loans,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "bad_loans_60DPD_12MOB_ETC.rdata"
     )
)

## 6. Tag Intermediaries ---------------------------------------------------------------------


## 6.1 I1 - Not 60 DPD in 15MOB, but 90DPD post 15MOB
tag_intermediary_I1 <- function(mob_timeframe,DPD_value_cutoff,minDate,maxDate, max_DPD_threshold){
  
  # subset for loan type & disbursal date range
  temp_disbursal_subset <- disbursals_final %>% ungroup() %>% filter(disbursal_yearmon %in% c(minDate:maxDate))
  
  # get disbursals with required MOB 
  temp_disbursal_subset <- temp_disbursal_subset %>% filter(mob %in% c(1:mob_timeframe)) 
  
  # get count of available payment months for disbursals
  temp_month_count <- temp_disbursal_subset %>% group_by(deal_no) %>% summarise(month_count = length(unique(payment_index)))
  
  # get disbursals with enough payment month count (same as MOB)
  disbursals_with_enough_datapoints <- distinct(temp_month_count %>% filter(month_count == mob_timeframe) %>% ungroup() %>% dplyr::select(deal_no))
  disbursals_with_enough_datapoints$mob_check <- 1
  
  # subset for disbursals with enough data points
  temp_disbursal_subset <- left_join(temp_disbursal_subset,disbursals_with_enough_datapoints,by = 'deal_no')
  temp_disbursal_subset <- temp_disbursal_subset %>% filter(mob_check == 1) %>% ungroup()
  temp_disbursal_subset$mob_check <- NULL
  
  
  # tag cases which were never 60 DPD in 15 MOB
  temp_disbursal_subset$never_60_dpd <- ifelse(temp_disbursal_subset$dpd_status < DPD_value_cutoff, 0, 1)
  
  # aggregate at deal level
  tagged_bad_loans <- temp_disbursal_subset %>% group_by(deal_no,loan_type) %>% summarise(never_60_dpd = max(never_60_dpd, na.rm = T))
  tagged_bad_loans <- tagged_bad_loans %>% ungroup()
  
  
  # identify cases where 60 DPD was not hit
  never_60_dpd_deals <- tagged_bad_loans %>% filter(never_60_dpd == 0)
  
  
  ## check if these no 60 DPD cases hit 90 DPD post MOB threshold
  never_60_dpd_data <- disbursals_final %>% ungroup() %>% filter(deal_no %in% unique(never_60_dpd_deals$deal_no))
  
  # get disbursals with required MOB 
  never_60_dpd_data <- never_60_dpd_data %>% filter(mob > mob_timeframe)
  
  # tag 90 DPD
  never_60_dpd_data$never_60DPD_but_90DPD_post_MOB <- ifelse(never_60_dpd_data$dpd_status >= max_DPD_threshold, 1, 0)
  never_60_dpd_data <- never_60_dpd_data %>% ungroup()
  
  # aggregate at deal no
  never_60_dpd_but_90_dpd_post_mob <- never_60_dpd_data %>% group_by(deal_no) %>% summarise(never_60DPD_but_90DPD_post_MOB = max(never_60DPD_but_90DPD_post_MOB, na.rm = T))
  never_60_dpd_but_90_dpd_post_mob <- never_60_dpd_but_90_dpd_post_mob %>% ungroup()
  
  return(never_60_dpd_but_90_dpd_post_mob)
}


intermediary_I1 <- tag_intermediary_I1(mob_timeframe = 15, 
                                       DPD_value_cutoff = 60, 
                                       minDate = 201803, 
                                       maxDate = 201812, 
                                       max_DPD_threshold = 90)


loan_type_mapping <- distinct(disbursals_final %>% ungroup() %>% dplyr::select(deal_no, loan_type))

intermediary_I1 <- left_join(intermediary_I1,loan_type_mapping, by = 'deal_no')


assert_data_non_empty(intermediary_I1)
save(intermediary_I1,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "intermediary_I1.rdata"
     )
)


rm(intermediary_I1)





## 6.3 I2 - temporal decay
tag_temporal_decay <- function(mob_timeframe,DPD_value_cutoff,minDate,maxDate){
  
  # subset for loan type & disbursal date range
  temp_disbursal_subset <- disbursals_final %>% ungroup() %>% filter(disbursal_yearmon %in% c(minDate:maxDate))
  
  # get disbursals with required MOB 
  temp_disbursal_subset <- temp_disbursal_subset %>% filter(mob %in% c(1:mob_timeframe)) 
  
  # get count of available payment months for disbursals
  temp_month_count <- temp_disbursal_subset %>% group_by(deal_no) %>% summarise(month_count = length(unique(payment_index)))
  
  # get disbursals with enough payment month count (same as MOB)
  disbursals_with_enough_datapoints <- distinct(temp_month_count %>% filter(month_count == mob_timeframe) %>% ungroup() %>% dplyr::select(deal_no))
  disbursals_with_enough_datapoints$mob_check <- 1
  
  # subset for disbursals with enough data points
  temp_disbursal_subset <- left_join(temp_disbursal_subset,disbursals_with_enough_datapoints,by = 'deal_no')
  temp_disbursal_subset <- temp_disbursal_subset %>% filter(mob_check == 1) %>% ungroup()
  temp_disbursal_subset$mob_check <- NULL
  
  
  # get disbursals with required MOB 
  beyond_mob <- disbursals_final %>% ungroup() %>% filter((deal_no %in% unique(temp_disbursal_subset$deal_no)) & (mob > mob_timeframe))
  
  # tag 90 DPD
  beyond_mob$temporal_decay_post_MOB <- ifelse(beyond_mob$dpd_status >= DPD_value_cutoff, 1, 0)
  beyond_mob <- beyond_mob %>% ungroup()
  
  # aggregate at deal no
  beyond_mob <- beyond_mob %>% group_by(deal_no) %>% summarise(temporal_decay_post_MOB = max(temporal_decay_post_MOB, na.rm = T))
  beyond_mob <- beyond_mob %>% ungroup()
  
  return(beyond_mob)
}


temporal_decay <- tag_temporal_decay(mob_timeframe = 15, 
                                       DPD_value_cutoff = 60, 
                                       minDate = 201803, 
                                       maxDate = 201812)


# mob_timeframe = 15
# DPD_value_cutoff = 60
# minDate = 201803
# maxDate = 201812

temporal_decay <- left_join(temporal_decay,loan_type_mapping, by = 'deal_no')


assert_data_non_empty(temporal_decay)
save(temporal_decay,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "temporal_decay.rdata"
     )
)


# rm(list=ls())

rm(temporal_decay,loan_type_mapping)



##########################################################################################################

## validation datasets - 60dpd in 12mob
v1_2W_New <- tag_bad_loan(mob_timeframe = 12,
                          DPD_value_cutoff = 60,
                          minDate = 201901, 
                          maxDate = 201903,
                          temp_loan_type = '2W-New')

## 3.2 tag bad loans in PV New
v1_PV_New <- tag_bad_loan(mob_timeframe = 12,
                          DPD_value_cutoff = 60,
                          minDate = 201901, 
                          maxDate = 201903,
                          temp_loan_type = 'PV-New')

## 3.3 tag bad loans in PV Used
v1_PV_Used <- tag_bad_loan(mob_timeframe = 12,
                           DPD_value_cutoff = 60,
                           minDate = 201901, 
                           maxDate = 201903,
                           temp_loan_type = 'PV-Used')

validation_60dpd_12mob <- data.frame(rbindlist(l=list(v1_2W_New,v1_PV_New,v1_PV_Used)))
validation_60dpd_12mob <- validation_60dpd_12mob %>% ungroup()

assert_data_non_empty(validation_60dpd_12mob)
save(validation_60dpd_12mob,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "validation_60dpd_12mob.rdata"
     )
)

rm(v1_2W_New,v1_PV_New,v1_PV_Used,validation_60dpd_12mob)



## validation datasets - 60dpd in 9mob
v2_2W_New <- tag_bad_loan(mob_timeframe = 9,
                          DPD_value_cutoff = 60,
                          minDate = 201904, 
                          maxDate = 201906,
                          temp_loan_type = '2W-New')


v2_PV_New <- tag_bad_loan(mob_timeframe = 9,
                          DPD_value_cutoff = 60,
                          minDate = 201904, 
                          maxDate = 201906,
                          temp_loan_type = 'PV-New')


v2_PV_Used <- tag_bad_loan(mob_timeframe = 9,
                           DPD_value_cutoff = 60,
                           minDate = 201904, 
                           maxDate = 201906,
                           temp_loan_type = 'PV-Used')

validation_60dpd_9mob <- data.frame(rbindlist(l=list(v2_2W_New,v2_PV_New,v2_PV_Used)))
validation_60dpd_9mob <- validation_60dpd_9mob %>% ungroup()

assert_data_non_empty(validation_60dpd_9mob)
save(validation_60dpd_9mob,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "validation_60dpd_9mob.rdata"
     )
)

rm(v2_2W_New,v2_PV_New,v2_PV_Used,validation_60dpd_9mob)



rm(disbursals_final)


###
load_rdata_intermediate("ADS_data//disbursals_final_all.rdata")


tag_bad_loan_in_specfic_mob <- function(mob_timeframe,DPD_value_cutoff,minDate,maxDate,temp_loan_type,mob_min_cutoff, mob_max_cutoff){

  # subset for loan type & disbursal date range
  temp_disbursal_subset <- disbursals_final %>% ungroup() %>% filter((disbursal_yearmon %in% c(minDate:maxDate)) & (loan_type == temp_loan_type))
  
  # get disbursals with required MOB
  temp_disbursal_subset <- temp_disbursal_subset %>% filter(mob %in% c(1:mob_max_cutoff))

  # get count of available payment months for disbursals
  temp_month_count <- temp_disbursal_subset %>% group_by(deal_no) %>% summarise(month_count = length(unique(payment_index)))

  # get disbursals with enough payment month count (same as MOB)
  disbursals_with_enough_datapoints <- distinct(temp_month_count %>% filter(month_count >= mob_timeframe) %>% ungroup() %>% dplyr::select(deal_no))
  disbursals_with_enough_datapoints$mob_check <- 1

  # subset for disbursals with enough data points
  temp_disbursal_subset <- left_join(temp_disbursal_subset,disbursals_with_enough_datapoints,by = 'deal_no')
  temp_disbursal_subset <- temp_disbursal_subset %>% filter(mob_check == 1) %>% ungroup()
  temp_disbursal_subset$mob_check <- NULL


  # tag bad loan
  temp_disbursal_subset$bad_loan <- ifelse(temp_disbursal_subset$dpd_status >= DPD_value_cutoff &
                                             temp_disbursal_subset$mob >= mob_min_cutoff &
                                             temp_disbursal_subset$mob <= mob_max_cutoff, 1, 0)

  # aggregate at deal level
  tagged_bad_loans <- temp_disbursal_subset %>% group_by(deal_no,loan_type) %>% summarise(bad_loan = max(bad_loan, na.rm = T))

  return(tagged_bad_loans)
}



v3_2W_New <- tag_bad_loan_in_specfic_mob(mob_timeframe = 12,
                          DPD_value_cutoff = 60,
                          minDate = 201910,
                          maxDate = 201912,
                          temp_loan_type = '2W-New',
                          mob_min_cutoff = 12,
                          mob_max_cutoff = 15)


v3_PV_New <- tag_bad_loan_in_specfic_mob(mob_timeframe = 12,
                                         DPD_value_cutoff = 60,
                                         minDate = 201910,
                                         maxDate = 201912,
                                         temp_loan_type = 'PV-New',
                                         mob_min_cutoff = 12,
                                         mob_max_cutoff = 15)


v3_PV_Used <- tag_bad_loan_in_specfic_mob(mob_timeframe = 12,
                                          DPD_value_cutoff = 60,
                                          minDate = 201910,
                                          maxDate = 201912,
                                          temp_loan_type = 'PV-Used',
                                          mob_min_cutoff = 12,
                                          mob_max_cutoff = 15)


validation_60dpd_oct_to_dec_2020 <- data.frame(rbindlist(l=list(v3_2W_New,v3_PV_New,v3_PV_Used)))
validation_60dpd_oct_to_dec_2020 <- validation_60dpd_oct_to_dec_2020 %>% ungroup()

assert_data_non_empty(validation_60dpd_oct_to_dec_2020)
save(validation_60dpd_oct_to_dec_2020,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "validation_60dpd_oct_to_dec_2020.rdata"
     )
)








## Belief testing ------------------------------------

# load_rdata_intermediate("ADS_data//disbursals_final.rdata")
# 
# 
# # 2. function to tag default definition ------------------------------------------------------
# 
# tag_bad_loan <- function(mob_timeframe,DPD_value_cutoff,minDate,maxDate,temp_loan_type){
#   
#   # subset for loan type & disbursal date range
#   temp_disbursal_subset <- disbursals_final %>% ungroup() %>% filter((disbursal_yearmon %in% c(minDate:maxDate)) & (loan_type == temp_loan_type))
#   
#   # get disbursals with required MOB 
#   temp_disbursal_subset <- temp_disbursal_subset %>% filter(mob %in% c(1:mob_timeframe)) 
#   
#   # get count of available payment months for disbursals
#   temp_month_count <- temp_disbursal_subset %>% group_by(deal_no) %>% summarise(month_count = length(unique(payment_index)))
#   
#   # get disbursals with enough payment month count (same as MOB)
#   disbursals_with_enough_datapoints <- distinct(temp_month_count %>% filter(month_count == mob_timeframe) %>% ungroup() %>% dplyr::select(deal_no))
#   disbursals_with_enough_datapoints$mob_check <- 1
#   
#   # subset for disbursals with enough data points
#   temp_disbursal_subset <- left_join(temp_disbursal_subset,disbursals_with_enough_datapoints,by = 'deal_no')
#   temp_disbursal_subset <- temp_disbursal_subset %>% filter(mob_check == 1) %>% ungroup()
#   temp_disbursal_subset$mob_check <- NULL
#   
#   
#   # tag bad loan
#   temp_disbursal_subset$bad_loan <- ifelse(temp_disbursal_subset$dpd_status >= DPD_value_cutoff, 1, 0)
#   
#   # aggregate at deal level
#   tagged_bad_loans <- temp_disbursal_subset %>% group_by(deal_no,loan_type) %>% summarise(bad_loan = max(bad_loan, na.rm = T))
#   
#   return(tagged_bad_loans)
# }
# 
# 
# 
# ## 3. Tag bad loans across products ------------------------------------------------------------
# 
# # mob_timeframe = 15
# # DPD_value_cutoff = 60
# # minDate = 201803
# # maxDate = 201812
# # temp_loan_type = '2W-New'
# 
# ## 3.1 tag bad loans in 2W New
# bad_loans_2W_New <- tag_bad_loan(mob_timeframe = 15,
#                                  DPD_value_cutoff = 60,
#                                  minDate = 201901, 
#                                  maxDate = 201903,
#                                  temp_loan_type = '2W-New')
# 
# ## 3.2 tag bad loans in PV New
# bad_loans_PV_New <- tag_bad_loan(mob_timeframe = 15,
#                                  DPD_value_cutoff = 60,
#                                  minDate = 201901, 
#                                  maxDate = 201903,
#                                  temp_loan_type = 'PV-New')
# 
# ## 3.3 tag bad loans in PV Used
# bad_loans_PV_Used <- tag_bad_loan(mob_timeframe = 15,
#                                   DPD_value_cutoff = 60,
#                                   minDate = 201901, 
#                                   maxDate = 201903,
#                                   temp_loan_type = 'PV-Used')
# 
# 
# ## 3.4 append all disburals across products
# bad_loans <- rbindlist(l=list(bad_loans_2W_New,bad_loans_PV_New,bad_loans_PV_Used))
# bad_loans <- bad_loans %>% ungroup()
# 
# rm(bad_loans_2W_New,bad_loans_PV_New,bad_loans_PV_Used)
# 
# 
# 
# 
# ## 4. Tag ever 90 DPD or 150 DPD --------------------------------------------------------------
# 
# ## 4.1 create ever 90 DPD flag
# disbursals_final$ever_90dpd <- ifelse(disbursals_final$dpd_status >= 90, 1, 0)
# 
# ## 4.2 create ever 150 DPD flag
# disbursals_final$ever_150dpd <- ifelse(disbursals_final$dpd_status >= 150, 1, 0)
# 
# ## 4.3 aggregate to get at disbursal level
# ever_flags <- disbursals_final %>% group_by(deal_no,loan_type) %>% summarise(ever_90dpd = max(ever_90dpd, na.rm = T),
#                                                                              ever_150dpd = max(ever_150dpd, na.rm = T))
# 
# 
# ever_flags <- ever_flags %>% ungroup()
# 
# 
# 
# ## 5. Join ever flags & save output ---------------------------------------------------------
# 
# ## 5.1 join data
# bad_loans <- left_join(bad_loans,ever_flags, by = c("deal_no" = "deal_no",
#                                                     "loan_type" = "loan_type"))
# 
# 
# ## 5.2 save data
# assert_data_non_empty(bad_loans)
# save(bad_loans,
#      file = file.path("data//intermediate//belief_testing//bad_loans_2019.rdata")
# )
# 
# 
# rm(ever_flags)
# 
# 
# 
# 
# 
# rm(list=ls())
