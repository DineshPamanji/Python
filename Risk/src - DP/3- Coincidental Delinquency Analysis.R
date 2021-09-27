############################################################################################
################## 3 - Coincidental Delinquency Analysis ###################################
############################################################################################


## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)



## 1. Load disbursal data -----------------------------------------------------------------------------

## 1.1 load ADS data
load_rdata_intermediate("ADS_data//base_ads.rdata")

## 1.2 subset for required loan types & columns
req_loan_types <- c('2W-New','PV-New','PV-Used')

base_ads <- base_ads %>% filter(loan_type %in% req_loan_types) %>% dplyr::select(deal_no,
                                                                                 Application_No,
                                                                                 loan_type,
                                                                                 disbursal_date,
                                                                                 year_mon,
                                                                                 CIBIL_SCORE,
                                                                                 Finance_Amount_Chassis,
                                                                                 profile_primary_applicant,
                                                                                 evaluation_type_primary_applicant,
                                                                                 borrower_count,
                                                                                 Cibil_Score_coborrower_1,
                                                                                 profile_coborrower_1,
                                                                                 Cibil_Score_coborrower_2,
                                                                                 profile_coborrower_2,
                                                                                 CUSTOMER_PROFILE
                                                                                 )


## 1.3 load base disbursal data containing DPD status
load_rdata_intermediate("cleaned_data//disbursal_data.rdata")

# state_mapping <- distinct(disbursal_data %>% dplyr::select(deal_no,disbursal_state))
# base_ads <- left_join(base_ads, state_mapping, by = 'deal_no')
# rm(state_mapping)




## 2. Combine datasets -------------------------------------------------------------------------------

## 2.1 filter for disbursals starting from Mar 2018 & select required columns
min_disbursal_date <- '2018-03-01'

req_cols <- c('deal_no',
              'apr18dpd','may18dpd','jun18dpd',
              'jul18dpd','aug18dpd','sep18dpd',
              'oct18dpd','nov18dpd','dec18dpd',
              'jan19dpd','feb19dpd','mar19dpd',
              'apr19dpd','may19dpd','jun19dpd',
              'jul19dpd','aug19dpd','sep19dpd',
              'oct19dpd','nov19dpd','dec19dpd',
              'jan20dpd','feb20dpd','mar20dpd',
              'apr20dpd','may20dpd','jun20dpd',
              'jul20dpd','aug20dpd','sep20dpd',
              'oct20dpd','nov20dpd','dec20dpd'
              )

dpd_data <- disbursal_data %>% filter(disbursal_date >= as.Date(min_disbursal_date)) %>% dplyr::select(req_cols) 


## 2.2 melt data to get dpd status every month at row level
dpd_data_month <- melt(dpd_data, id.vars = c('deal_no'))

## 2.3 change column names for melted data
colnames(dpd_data_month) <- c('deal_no','month_raw','dpd_value')

rm(disbursal_data,dpd_data)



## 3. Get coincidental delinquency numbers ------------------------------------------------------------


################################
## 3.1 Loan type x Monthly level
################################

## 3.1.1 select dpd cols & define output list
dpd_cols <- c('apr18dpd','may18dpd','jun18dpd',
              'jul18dpd','aug18dpd','sep18dpd',
              'oct18dpd','nov18dpd','dec18dpd',
              'jan19dpd','feb19dpd','mar19dpd',
              'apr19dpd','may19dpd','jun19dpd',
              'jul19dpd','aug19dpd','sep19dpd',
              'oct19dpd','nov19dpd','dec19dpd',
              'jan20dpd','feb20dpd','mar20dpd',
              'apr20dpd','may20dpd','jun20dpd',
              'jul20dpd','aug20dpd','sep20dpd',
              'oct20dpd','nov20dpd','dec20dpd'
              )

output_loan_type <- list()
i <- 0

## 3.1.2. Run loop across months & calculate numbers
for (dpd_col in dpd_cols){
  i <- i+1
  
  # subset for required month & remove cases with NA dpd
  subset_temp <- dpd_data_month %>% filter((month_raw == dpd_col) & (!is.na(dpd_value)))
  
  # flag 30,60,90 dpd
  subset_temp$dpd_30_plus <- ifelse(subset_temp$dpd_value > 30, 1, 0)
  subset_temp$dpd_60_plus <- ifelse(subset_temp$dpd_value > 60, 1, 0)
  subset_temp$dpd_90_plus <- ifelse(subset_temp$dpd_value > 90, 1, 0)
  
  # join base ads  
  subset_temp <- left_join(subset_temp, base_ads, by = "deal_no")
  
  #subset_temp <- subset_temp %>% filter(!is.na(Application_No))
  # output_loan_type_temp <- subset_temp %>% group_by(loan_type,dpd_bucket) %>% summarise(volume = length(unique(deal_no)))
  
  # get count of 30, 60, 90 dpd loans
  output_loan_type_temp <- subset_temp %>% group_by(loan_type) %>% summarise(dpd_30_plus = sum(dpd_30_plus,na.rm = T),
                                                                             dpd_60_plus = sum(dpd_60_plus,na.rm = T),
                                                                             dpd_90_plus = sum(dpd_90_plus,na.rm = T)
                                                                             )
  
  # tag month
  output_loan_type_temp$month <- dpd_col
  
  # get count of overall live loans in that month
  total_loans_temp <-  subset_temp %>% group_by(loan_type) %>% summarise(total_disbursed = length(unique(deal_no)))
  
  # join dpd count with monthly live loan count
  output_loan_type_temp <- left_join(output_loan_type_temp,total_loans_temp,by='loan_type')
  
  # select & reorder columns
  output_loan_type_temp <- output_loan_type_temp %>% dplyr::select(loan_type,month,dpd_30_plus,dpd_60_plus,dpd_90_plus,total_disbursed)
  
  # append data into output list
  output_loan_type[[i]] <- output_loan_type_temp
  
  rm(subset_temp,total_loans_temp,output_loan_type_temp)
  print(paste0(i," - ",dpd_col, " done"))
  
}

## 3.1.3. bind output
output_loan_level <- do.call(rbind,output_loan_type)
output_loan_level$loan_type <- ifelse(is.na(output_loan_level$loan_type), 'application_data_unavailable',output_loan_level$loan_type)


## 3.1.4 save output
for(loan_type_temp in c(req_loan_types,'application_data_unavailable')){
  output_temp <- output_loan_level %>% filter(loan_type == loan_type_temp)
  save_csv_output(output_temp, paste0("coincidental_analysis/",loan_type_temp,"_dpd_count.csv"))
  # fwrite(output_temp, paste0("./data/eda/coincidental_analysis/",loan_type_temp,"_dpd_count.csv"))
}

rm(output_loan_level,output_temp,output_loan_type)




##########################################
## 3.2 Loan type x Monthly x profile level
##########################################

## 3.2.1 select dpd cols & define output list
dpd_cols <- c('apr18dpd','may18dpd','jun18dpd',
              'jul18dpd','aug18dpd','sep18dpd',
              'oct18dpd','nov18dpd','dec18dpd',
              'jan19dpd','feb19dpd','mar19dpd',
              'apr19dpd','may19dpd','jun19dpd',
              'jul19dpd','aug19dpd','sep19dpd',
              'oct19dpd','nov19dpd','dec19dpd',
              'jan20dpd','feb20dpd','mar20dpd',
              'apr20dpd','may20dpd','jun20dpd',
              'jul20dpd','aug20dpd','sep20dpd',
              'oct20dpd','nov20dpd','dec20dpd'
)

output_loan_type <- list()
i <- 0

## 3.2.2. Run loop across months & calculate numbers
for (dpd_col in dpd_cols){
  i <- i+1
  
  # subset for required month & remove cases with NA dpd
  subset_temp <- dpd_data_month %>% filter((month_raw == dpd_col) & (!is.na(dpd_value)))
  
  # flag 30,60,90 dpd
  subset_temp$dpd_30_plus <- ifelse(subset_temp$dpd_value > 30, 1, 0)
  subset_temp$dpd_60_plus <- ifelse(subset_temp$dpd_value > 60, 1, 0)
  subset_temp$dpd_90_plus <- ifelse(subset_temp$dpd_value > 90, 1, 0)
  
  # join base ads  
  subset_temp <- left_join(subset_temp, base_ads, by = "deal_no")
  
  
  #subset_temp <- subset_temp %>% filter(!is.na(Application_No))
  #subset_temp <- subset_temp %>% filter(!is.na(profile_primary_applicant))
  # output_loan_type_temp <- subset_temp %>% group_by(loan_type,dpd_bucket) %>% summarise(volume = length(unique(deal_no)))
  
  # get count of 30, 60, 90 dpd loans
  output_loan_type_temp <- subset_temp %>% group_by(loan_type, profile_primary_applicant) %>% summarise(dpd_30_plus = sum(dpd_30_plus,na.rm = T),
                                                                                                       dpd_60_plus = sum(dpd_60_plus,na.rm = T),
                                                                                                       dpd_90_plus = sum(dpd_90_plus,na.rm = T))
  
  # tag month
  output_loan_type_temp$month <- dpd_col
  
  # get count of overall live loans in that month
  total_loans_temp <-  subset_temp %>% group_by(loan_type,profile_primary_applicant) %>% summarise(total_disbursed = length(unique(deal_no)))
  
  # join dpd count with monthly live loan count
  output_loan_type_temp <- left_join(output_loan_type_temp,total_loans_temp,by=c('loan_type','profile_primary_applicant'))
  
  # select & reorder columns
  output_loan_type_temp <- output_loan_type_temp %>% dplyr::select(loan_type,profile_primary_applicant,month,dpd_30_plus,dpd_60_plus,dpd_90_plus,total_disbursed)
  
  # append data into output list
  output_loan_type[[i]] <- output_loan_type_temp
  
  rm(subset_temp,total_loans_temp,output_loan_type_temp)
  print(paste0(i," - ",dpd_col, " done"))
}

## 3.2.3. bind output
output_loan_level <- do.call(rbind,output_loan_type)
output_loan_level$loan_type <- ifelse(is.na(output_loan_level$loan_type), 'application_data_unavailable',output_loan_level$loan_type)


## 3.2.4 save output
for(loan_type_temp in req_loan_types){
  output_temp <- output_loan_level %>% filter(loan_type == loan_type_temp)
  save_csv_output(output_temp, paste0("coincidental_analysis/",loan_type_temp,"_x_profile_dpd_count.csv"))
  # fwrite(output_temp, paste0("./data/eda/coincidental_analysis/",loan_type_temp,"_x_profile","_dpd_count.csv"))
}

rm(output_loan_level,output_temp,output_loan_type)











############################################
## 3.3 Loan type x Monthly x Evaluation Type
############################################

## 3.3.1 select dpd cols & define output list
dpd_cols <- c('apr18dpd','may18dpd','jun18dpd',
              'jul18dpd','aug18dpd','sep18dpd',
              'oct18dpd','nov18dpd','dec18dpd',
              'jan19dpd','feb19dpd','mar19dpd',
              'apr19dpd','may19dpd','jun19dpd',
              'jul19dpd','aug19dpd','sep19dpd',
              'oct19dpd','nov19dpd','dec19dpd',
              'jan20dpd','feb20dpd','mar20dpd',
              'apr20dpd','may20dpd','jun20dpd',
              'jul20dpd','aug20dpd','sep20dpd',
              'oct20dpd','nov20dpd','dec20dpd'
)

output_loan_type <- list()
i <- 0

## 3.3.2. Run loop across months & calculate numbers
for (dpd_col in dpd_cols){
  i <- i+1
  
  # subset for required month & remove cases with NA dpd
  subset_temp <- dpd_data_month %>% filter((month_raw == dpd_col) & (!is.na(dpd_value)))
  
  # flag 30,60,90 dpd
  subset_temp$dpd_30_plus <- ifelse(subset_temp$dpd_value > 30, 1, 0)
  subset_temp$dpd_60_plus <- ifelse(subset_temp$dpd_value > 60, 1, 0)
  subset_temp$dpd_90_plus <- ifelse(subset_temp$dpd_value > 90, 1, 0)
  
  # join base ads  
  subset_temp <- left_join(subset_temp, base_ads, by = "deal_no")
  
  
  # subset_temp <- left_join(subset_temp, base_ads, by = "deal_no")
  # subset_temp <- subset_temp %>% filter(!is.na(Application_No))
  # subset_temp <- subset_temp %>% filter(!is.na(evaluation_type_primary_applicant))
  
  # output_loan_type_temp <- subset_temp %>% group_by(loan_type,dpd_bucket) %>% summarise(volume = length(unique(deal_no)))
  
  # get count of 30, 60, 90 dpd loans
  output_loan_type_temp <- subset_temp %>% group_by(loan_type, evaluation_type_primary_applicant) %>% summarise(dpd_30_plus = sum(dpd_30_plus,na.rm = T),
                                                                                             dpd_60_plus = sum(dpd_60_plus,na.rm = T),
                                                                                             dpd_90_plus = sum(dpd_90_plus,na.rm = T))
  
  # tag month
  output_loan_type_temp$month <- dpd_col
  
  # get count of overall live loans in that month
  total_loans_temp <-  subset_temp %>% group_by(loan_type,evaluation_type_primary_applicant) %>% summarise(total_disbursed = length(unique(deal_no)))
  
  # join dpd count with monthly live loan count
  output_loan_type_temp <- left_join(output_loan_type_temp,total_loans_temp,by=c('loan_type','evaluation_type_primary_applicant'))
  
  # select & reorder columns
  output_loan_type_temp <- output_loan_type_temp %>% dplyr::select(loan_type,evaluation_type_primary_applicant,month,dpd_30_plus,dpd_60_plus,dpd_90_plus,total_disbursed)
  
  # append data into output list
  output_loan_type[[i]] <- output_loan_type_temp
  
  rm(subset_temp,total_loans_temp,output_loan_type_temp)
  print(paste0(i," - ",dpd_col, " done"))
  
}

## 3.3.3. bind output
output_loan_level <- do.call(rbind,output_loan_type)
output_loan_level$loan_type <- ifelse(is.na(output_loan_level$loan_type), 'application_data_unavailable',output_loan_level$loan_type)


## 3.3.4 save output
for(loan_type_temp in req_loan_types){
  output_temp <- output_loan_level %>% filter(loan_type == loan_type_temp)
  save_csv_output(output_temp, paste0("coincidental_analysis/",loan_type_temp,"_x_evaluation_type_primary_applicant_dpd_count.csv"))
  # fwrite(output_temp, paste0("./data/eda/coincidental_analysis/",loan_type_temp,"_x_evaluation_type_primary_applicant","_dpd_count.csv"))
}


rm(output_loan_level,output_temp,output_loan_type)








#########################################
## 3.4 Loan type x Monthly x Cibil Score
#########################################

## 3.4.1 select dpd cols & define output list
dpd_cols <- c('apr18dpd','may18dpd','jun18dpd',
              'jul18dpd','aug18dpd','sep18dpd',
              'oct18dpd','nov18dpd','dec18dpd',
              'jan19dpd','feb19dpd','mar19dpd',
              'apr19dpd','may19dpd','jun19dpd',
              'jul19dpd','aug19dpd','sep19dpd',
              'oct19dpd','nov19dpd','dec19dpd',
              'jan20dpd','feb20dpd','mar20dpd',
              'apr20dpd','may20dpd','jun20dpd',
              'jul20dpd','aug20dpd','sep20dpd',
              'oct20dpd','nov20dpd','dec20dpd'
)

output_loan_type <- list()
i <- 0


## 3.4.2 Create cibil score buckets
base_ads$CIBIL_SCORE <- as.integer(base_ads$CIBIL_SCORE)

base_ads$cibil_category <- ifelse(is.na(base_ads$CIBIL_SCORE) | base_ads$CIBIL_SCORE <= 0, 'NTC',
                                  ifelse(base_ads$CIBIL_SCORE %in% c(1,2,3,4,5), 'Thin file (<6 months)',
                                         ifelse(base_ads$CIBIL_SCORE > 5 & base_ads$CIBIL_SCORE <= 600, 'CIBIL <= 600',
                                                ifelse(base_ads$CIBIL_SCORE > 600 & base_ads$CIBIL_SCORE <= 650, 'CIBIL > 600 & <= 650',
                                                       ifelse(base_ads$CIBIL_SCORE > 650 & base_ads$CIBIL_SCORE<= 750,'CIBIL > 650 & <= 750',
                                                              ifelse(base_ads$CIBIL_SCORE>750, 'CIBIL > 750', 'NTC'))))))


## 3.4.3. Run loop across months & calculate numbers
for (dpd_col in dpd_cols){
  i <- i+1
  
  
  # subset for required month & remove cases with NA dpd
  subset_temp <- dpd_data_month %>% filter((month_raw == dpd_col) & (!is.na(dpd_value)))
  
  # flag 30,60,90 dpd
  subset_temp$dpd_30_plus <- ifelse(subset_temp$dpd_value > 30, 1, 0)
  subset_temp$dpd_60_plus <- ifelse(subset_temp$dpd_value > 60, 1, 0)
  subset_temp$dpd_90_plus <- ifelse(subset_temp$dpd_value > 90, 1, 0)
  
  # join base ads  
  subset_temp <- left_join(subset_temp, base_ads, by = "deal_no")
  
  
  # subset_temp <- left_join(subset_temp, base_ads, by = "deal_no")
  # subset_temp <- subset_temp %>% filter(!is.na(Application_No))
  # subset_temp <- subset_temp %>% filter(!is.na(cibil_category))
  
  # output_loan_type_temp <- subset_temp %>% group_by(loan_type,dpd_bucket) %>% summarise(volume = length(unique(deal_no)))
  
  # get count of 30, 60, 90 dpd loans
  output_loan_type_temp <- subset_temp %>% group_by(loan_type, cibil_category) %>% summarise(dpd_30_plus = sum(dpd_30_plus,na.rm = T),
                                                                                             dpd_60_plus = sum(dpd_60_plus,na.rm = T),
                                                                                             dpd_90_plus = sum(dpd_90_plus,na.rm = T))
  
  # tag month
  output_loan_type_temp$month <- dpd_col
  
  # get count of overall live loans in that month
  total_loans_temp <-  subset_temp %>% group_by(loan_type,cibil_category) %>% summarise(total_disbursed = length(unique(deal_no)))
  
  # join dpd count with monthly live loan count
  output_loan_type_temp <- left_join(output_loan_type_temp,total_loans_temp,by=c('loan_type','cibil_category'))
  
  # select & reorder columns
  output_loan_type_temp <- output_loan_type_temp %>% dplyr::select(loan_type,cibil_category,month,dpd_30_plus,dpd_60_plus,dpd_90_plus,total_disbursed)
  
  # append data into output list
  output_loan_type[[i]] <- output_loan_type_temp
  
  rm(subset_temp,total_loans_temp,output_loan_type_temp)
  print(paste0(i," - ",dpd_col, " done"))
  
}

## 3.4.4. bind output
output_loan_level <- do.call(rbind,output_loan_type)
output_loan_level$loan_type <- ifelse(is.na(output_loan_level$loan_type), 'application_data_unavailable',output_loan_level$loan_type)


## 3.4.5 save output
for(loan_type_temp in req_loan_types){
  output_temp <- output_loan_level %>% filter(loan_type == loan_type_temp)
  save_csv_output(output_temp, paste0("coincidental_analysis/",loan_type_temp,"_x_cibil_dpd_count.csv"))
  # fwrite(output_temp, paste0("./data/eda/coincidental_analysis/",loan_type_temp,"_x_cibil","_dpd_count_v3.csv"))
}






#######################################################################################################################


## loan  x Cibil bucket type


# output_loan_type <- list()
# i <- 0
# 
# 
# base_ads$cibil_category <- ifelse(is.na(base_ads$CIBIL_SCORE),'NTC / low footprint ETC',
#                                   ifelse(base_ads$CIBIL_SCORE > 5 & base_ads$CIBIL_SCORE <= 900, 'ETC (> 6 months)',
#                                          'NTC / low footprint ETC'))
# for (dpd_col in dpd_cols){
#   i <- i+1
#   
#   # dpd_col <- 'apr18dpd'
#   
#   subset_temp <- dpd_data_month %>% filter((month_raw == dpd_col) & (!is.na(dpd_value)))
#   
#   
#   subset_temp$dpd_30_plus <- ifelse(subset_temp$dpd_value > 30, 1, 0)
#   subset_temp$dpd_60_plus <- ifelse(subset_temp$dpd_value > 60, 1, 0)
#   subset_temp$dpd_90_plus <- ifelse(subset_temp$dpd_value > 90, 1, 0)
#   
#   
#   subset_temp <- left_join(subset_temp, base_ads, by = "deal_no")
#   subset_temp <- subset_temp %>% filter(!is.na(Application_No))
#   subset_temp <- subset_temp %>% filter(!is.na(cibil_category))
#   
#   # output_loan_type_temp <- subset_temp %>% group_by(loan_type,dpd_bucket) %>% summarise(volume = length(unique(deal_no)))
#   output_loan_type_temp <- subset_temp %>% group_by(loan_type, cibil_category) %>% summarise(dpd_30_plus = sum(dpd_30_plus,na.rm = T),
#                                                                                              dpd_60_plus = sum(dpd_60_plus,na.rm = T),
#                                                                                              dpd_90_plus = sum(dpd_90_plus,na.rm = T)
#   )
#   output_loan_type_temp$month <- dpd_col
#   
#   total_loans_temp <-  subset_temp %>% group_by(loan_type,cibil_category) %>% summarise(total_disbursed = length(unique(deal_no)))
#   output_loan_type_temp <- left_join(output_loan_type_temp,total_loans_temp,by=c('loan_type','cibil_category'))
#   
#   output_loan_type_temp <- output_loan_type_temp %>% dplyr::select(loan_type,cibil_category,month,dpd_30_plus,dpd_60_plus,dpd_90_plus,total_disbursed)
#   
#   output_loan_type[[i]] <- output_loan_type_temp
#   rm(subset_temp,total_loans_temp,output_loan_type_temp)
#   print(i)
#   
# }
# 
# output_loan_level <- do.call(rbind,output_loan_type)
# output_loan_level$loan_type <- ifelse(is.na(output_loan_level$loan_type), 'application_data_unavailable',output_loan_level$loan_type)
# 
# 
# 
# 
# for(loan_type_temp in req_loan_types){
#   output_temp <- output_loan_level %>% filter(loan_type == loan_type_temp)
#   fwrite(output_temp, paste0("./data/eda/coincidental_analysis/",loan_type_temp,"_x_cibil","_dpd_count.csv"))
# }



##################################################################################################################################

## loan  x Cibil bucket type (finer buckets)


# output_loan_type <- list()
# i <- 0
# 
# base_ads$CIBIL_SCORE <- as.integer(base_ads$CIBIL_SCORE)
# 
# base_ads$cibil_category <- ifelse(is.na(base_ads$CIBIL_SCORE),'NTC',
#                                   ifelse(base_ads$CIBIL_SCORE %in% c(1,2,3,4,5), 'Thin file (<6 months)',
#                                          ifelse(base_ads$CIBIL_SCORE < 0, 'Stale cibil',
#                                                 ifelse(base_ads$CIBIL_SCORE > 5 & base_ads$CIBIL_SCORE <= 500, 'CIBIL <= 500',
#                                                        ifelse(base_ads$CIBIL_SCORE > 500 & base_ads$CIBIL_SCORE <= 600, 'CIBIL > 500 & <= 600',
#                                                               ifelse(base_ads$CIBIL_SCORE > 600 & base_ads$CIBIL_SCORE<= 700,'CIBIL > 600 & <= 700',
#                                                                      ifelse(base_ads$CIBIL_SCORE > 700 & base_ads$CIBIL_SCORE<= 800, 'CIBIL > 700 & <= 800',
#                                                                             ifelse(base_ads$CIBIL_SCORE>800, 'CIBIL > 800', 'NTC'))))))))
# 
# # table(base_ads$cibil_category)
# 
# for (dpd_col in dpd_cols){
#   i <- i+1
#   
#   # dpd_col <- 'apr18dpd'
#   
#   subset_temp <- dpd_data_month %>% filter((month_raw == dpd_col) & (!is.na(dpd_value)))
#   
#   
#   subset_temp$dpd_30_plus <- ifelse(subset_temp$dpd_value > 30, 1, 0)
#   subset_temp$dpd_60_plus <- ifelse(subset_temp$dpd_value > 60, 1, 0)
#   subset_temp$dpd_90_plus <- ifelse(subset_temp$dpd_value > 90, 1, 0)
#   
#   
#   subset_temp <- left_join(subset_temp, base_ads, by = "deal_no")
#   subset_temp <- subset_temp %>% filter(!is.na(Application_No))
#   subset_temp <- subset_temp %>% filter(!is.na(cibil_category))
#   
#   # output_loan_type_temp <- subset_temp %>% group_by(loan_type,dpd_bucket) %>% summarise(volume = length(unique(deal_no)))
#   output_loan_type_temp <- subset_temp %>% group_by(loan_type, cibil_category) %>% summarise(dpd_30_plus = sum(dpd_30_plus,na.rm = T),
#                                                                                              dpd_60_plus = sum(dpd_60_plus,na.rm = T),
#                                                                                              dpd_90_plus = sum(dpd_90_plus,na.rm = T)
#   )
#   output_loan_type_temp$month <- dpd_col
#   
#   total_loans_temp <-  subset_temp %>% group_by(loan_type,cibil_category) %>% summarise(total_disbursed = length(unique(deal_no)))
#   output_loan_type_temp <- left_join(output_loan_type_temp,total_loans_temp,by=c('loan_type','cibil_category'))
#   
#   output_loan_type_temp <- output_loan_type_temp %>% dplyr::select(loan_type,cibil_category,month,dpd_30_plus,dpd_60_plus,dpd_90_plus,total_disbursed)
#   
#   output_loan_type[[i]] <- output_loan_type_temp
#   rm(subset_temp,total_loans_temp,output_loan_type_temp)
#   print(i)
#   
# }
# 
# output_loan_level <- do.call(rbind,output_loan_type)
# output_loan_level$loan_type <- ifelse(is.na(output_loan_level$loan_type), 'application_data_unavailable',output_loan_level$loan_type)
# 
# 
# 
# 
# for(loan_type_temp in req_loan_types){
#   output_temp <- output_loan_level %>% filter(loan_type == loan_type_temp)
#   fwrite(output_temp, paste0("./data/eda/coincidental_analysis/",loan_type_temp,"_x_cibil","_dpd_count_v2.csv"))
# }









