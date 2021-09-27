##############################################################################################
############################ Cohort Analysis #################################################
##############################################################################################


# 0. Load libraries & functions --------------------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)



# 1. Load required data & create ADS for cohort analysis --------------------------------------

## 1.1 load data
load_rdata_intermediate("ADS_data//base_ads.rdata")

## 1.2 subset for required loan types & columns
req_loan_types <- c('2W-New','PV-New','PV-Used')

base_ads <- distinct(base_ads %>% filter(loan_type %in% req_loan_types) %>% dplyr::select(deal_no,
                                                                                          loan_type,
                                                                                          disbursal_date,
                                                                                          year_mon))



## 1.3 load monthly dpd data
load_rdata_intermediate("cleaned_data//disbursal_data.rdata")


## 1.4 filter for disbursals starting from Mar 2018 & select required columns
min_disbursal_date <- '2018-04-01'
max_disbursal_date <- '2019-12-31'

req_cols <- c('deal_no',
              'apr18dpd','may18dpd','jun18dpd',
              'jul18dpd','aug18dpd','sep18dpd',
              'oct18dpd','nov18dpd','dec18dpd',
              'jan19dpd','feb19dpd','mar19dpd',
              'apr19dpd','may19dpd','jun19dpd',
              'jul19dpd','aug19dpd','sep19dpd',
              'oct19dpd','nov19dpd','dec19dpd',
              'jan20dpd','feb20dpd','mar20dpd'
)

disbursal_data <- disbursal_data %>% filter((disbursal_date >= as.Date(min_disbursal_date)) &
                                              (disbursal_date <= as.Date(max_disbursal_date))&
                                              (loan_type %in% req_loan_types)) %>% dplyr::select(req_cols)

## 1.5 melt data to get dpd status every month at row level
dpd_data <- melt(disbursal_data, id.vars = c('deal_no'))

## 1.6 change column names for melted data
colnames(dpd_data) <- c('deal_no','month_raw','dpd_value')

## 1.7 merge datasets
dpd_data <- left_join(dpd_data,base_ads, by ='deal_no')
dpd_data <- dpd_data %>% filter(!is.na(loan_type))
dpd_data <- dpd_data %>% filter(!is.na(dpd_value))

rm(base_ads,disbursal_data)


## 1.8 load year month index data
date_index <- fread_mapping("year_mon_index.csv")

## 1.9 map index to disbursal data & dpd dates
dpd_data$disbursal_yearmon <- as.numeric(paste0(year(dpd_data$disbursal_date), str_pad(month(dpd_data$disbursal_date),2,"0",side = "left")))

dpd_data <- left_join(dpd_data,date_index %>% dplyr::select(yearmon,index),by=c("disbursal_yearmon" = "yearmon"))
colnames(dpd_data)[colnames(dpd_data) == 'index'] <- 'disbursal_index'


## 1.10 map index to payment month
dpd_data <- left_join(dpd_data,date_index %>% dplyr::select(month_raw,index),by="month_raw")
colnames(dpd_data)[colnames(dpd_data) == 'index'] <- 'payment_index'



dpd_data$check <- ifelse(dpd_data$disbursal_index <= dpd_data$payment_index,0,1)
dpd_data <- dpd_data %>% filter(check == 0)
dpd_data$check <- NULL

rm(date_index)



# 2. Get first repayment date -------------------------------------------------------------------

repayment_info <- dpd_data %>% group_by(deal_no) %>% summarise(first_repayment_month = min(payment_index),
                                                               last_repayment_month = max(payment_index))

repayment_info$no_of_months <- repayment_info$last_repayment_month - repayment_info$first_repayment_month + 1


disbursals <- left_join(dpd_data,repayment_info,by='deal_no')
disbursals$diff_disbursal_payment <- disbursals$first_repayment_month - disbursals$disbursal_index

disbursals <- disbursals %>% filter(diff_disbursal_payment %in% c(0,1,2,3))


disbursals_final <- disbursals %>% group_by(deal_no,
                                            loan_type,
                                            disbursal_yearmon,
                                            disbursal_index,
                                            payment_index,
                                            first_repayment_month,
                                            last_repayment_month,
                                            no_of_months) %>% summarise(dpd_status = max(dpd_value))

disbursals_final$mob <- disbursals_final$payment_index - disbursals_final$disbursal_index + 1

rm(disbursals,dpd_data,repayment_info)

# test <- distinct(disbursals_final %>% ungroup() %>% dplyr::select(deal_no))


# assert_data_non_empty(disbursals_final)
# save(disbursals_final,
#      file = file.path(
#        get_data_path()$data$intermediate,
#        "ADS_data",
#        "disbursals_final.rdata"
#      )
# )




load_rdata_intermediate("ADS_data//disbursals_final.rdata")

disbursals_final$mob <- disbursals_final$payment_index - disbursals_final$disbursal_index

deal_with_month_payment <- distinct(disbursals_final %>% filter(mob == 0) %>% dplyr::select(deal_no))
deal_with_month_payment <- deal_with_month_payment %>% ungroup()
deal_with_month_payment <- distinct(deal_with_month_payment %>% dplyr::select(deal_no))
deal_with_month_payment <- unique(deal_with_month_payment$deal_no)

disbursals_final$mob <- ifelse(disbursals_final$deal_no %in% deal_with_month_payment, disbursals_final$mob+1, disbursals_final$mob)


# 3. Get roll rate & capture rate ----------------------------------------------------------------

get_bad_loan_definition <- function(mob_timeframe,DPD_values,minDate,maxDate,temp_loan_type){
  
  # subset for loan type & disbursal date range
  temp_disbursal_subset <- disbursals_final %>% filter((disbursal_yearmon %in% c(minDate:maxDate)) & (loan_type == temp_loan_type))
  
  output_list <- list()
  i <- 0
  
  # iterate over every possible DPD values
  for(every_dpd in DPD_values){
    
    
    # get disbursals with required MOB 
    temp_data <- temp_disbursal_subset %>% filter(mob %in% c(1:mob_timeframe)) 
    
    # get count of available payment months for disbursals
    temp_month_count <- temp_data %>% group_by(deal_no) %>% summarise(month_count = length(unique(payment_index)))
    
    # get disbursals with enough payment month count (same as MOB)
    disbursals_with_enough_datapoints <- distinct(temp_month_count %>% filter(month_count == mob_timeframe) %>% ungroup() %>% dplyr::select(deal_no))
    disbursals_with_enough_datapoints$mob_check <- 1
    
    # subset for disbursals with enough data points
    temp_data <- left_join(temp_data,disbursals_with_enough_datapoints,by = 'deal_no')
    temp_data <- temp_data %>% filter(mob_check == 1)
    temp_data$mob_check <- NULL
    
    
    
    ## output 1 - total loan disbursal count
    total_loans <- length(unique(temp_data$deal_no))
    
    ## output 2 - count of cases where DPD is greater than event DPD
    event_count <- length(unique((temp_data %>% filter((mob %in% c(1:mob_timeframe)) & (dpd_status > every_dpd)))$deal_no))
    
    
    # get cases where event DPD occurs ---------------------------
    
    ## create copy of disbursals
    temp_disbursal_subset_copy <- copy(temp_disbursal_subset)
    
    ## identify disbursals for which events were flagged within given MOB
    event_disbursals <- temp_data %>% filter((mob %in% c(1:mob_timeframe)) & (dpd_status > every_dpd))
    event_disbursals <- distinct(event_disbursals %>% ungroup() %>% dplyr::select(deal_no))
    event_disbursals$event_flag <- 1
    
    ## subset for only disbursals which are flagged
    temp_disbursal_subset_copy <- left_join(temp_disbursal_subset_copy,event_disbursals,by='deal_no')
    temp_disbursal_subset_copy <- temp_disbursal_subset_copy %>% filter(event_flag == 1)
    temp_disbursal_subset_copy$event_flag <- NULL
  
    
    npa_captured <- temp_disbursal_subset_copy %>% group_by(deal_no) %>% summarise(max_dpd = max(dpd_status))
    
    
    # output 3 - get count of disbursals where these event eventually becomes NPA
    npa_captured <- length(unique((npa_captured %>% filter(max_dpd > 90))$deal_no))
    
    
    # overall event npa
    temp_disbursal_subset_copy <- copy(temp_disbursal_subset)
    
    all_disbursals <- distinct(temp_data %>% ungroup() %>% dplyr::select(deal_no))
    all_disbursals$check <- 1
    
    temp_disbursal_subset_copy <- left_join(temp_disbursal_subset_copy,all_disbursals,by = 'deal_no')
    
    temp_disbursal_subset_copy <- temp_disbursal_subset_copy %>% filter(check == 1)
    
    overall_npa <- temp_disbursal_subset_copy %>% group_by(deal_no) %>% summarise(max_dpd = max(dpd_status))
    overall_npa <- length(unique((overall_npa %>% filter(max_dpd > 90))$deal_no))
    
    
    i <- i+1
    temp_output <- data.frame(MOB = mob_timeframe,
                              DPD = every_dpd,
                              total_loans = total_loans,
                              event_count = event_count,
                              npa_captured = npa_captured,
                              overall_npa = overall_npa
                              )
   
    output_list[[i]] <- temp_output
    print(paste0(temp_loan_type," - ",mob_timeframe," MOB ", every_dpd, " DPD done"))
    
  }
  
  return(do.call(rbind,output_list))
}





PV_New_12MOB <- get_bad_loan_definition(mob_timeframe = 12,
                                        DPD_values = c(30,60,90),
                                        minDate = 201803, 
                                        maxDate = 201903,
                                        temp_loan_type = 'PV-New')

PV_New_15MOB <- get_bad_loan_definition(mob_timeframe = 15,
                                        DPD_values = c(30,60,90),
                                        minDate = 201803, 
                                        maxDate = 201812,
                                        temp_loan_type = 'PV-New')

PV_New_18MOB <- get_bad_loan_definition(mob_timeframe = 18,
                                        DPD_values = c(30,60,90),
                                        minDate = 201803, 
                                        maxDate = 201809,
                                        temp_loan_type = 'PV-New')

PV_new_output <- data.frame(rbindlist(l=list(PV_New_12MOB,PV_New_15MOB,PV_New_18MOB)))
rm(PV_New_12MOB,PV_New_15MOB,PV_New_18MOB)


# fwrite(PV_new_output,"./data/eda/PV_new_bad_loan_def.csv",row.names = F)

output_list = list("PV_new" = PV_new_output)
save_xlsx_output(data = output_list, relative_path = "//bad_loan//bad_loan_PV_New.xlsx")




PV_Used_12MOB <- get_bad_loan_definition(mob_timeframe = 12,
                                         DPD_values = c(30,60,90),
                                         minDate = 201803, 
                                         maxDate = 201903,
                                         temp_loan_type = 'PV-Used')

PV_Used_15MOB <- get_bad_loan_definition(mob_timeframe = 15,
                                         DPD_values = c(30,60,90),
                                         minDate = 201803, 
                                         maxDate = 201812,
                                         temp_loan_type = 'PV-Used')

PV_Used_18MOB <- get_bad_loan_definition(mob_timeframe = 18,
                                         DPD_values = c(30,60,90),
                                         minDate = 201803, 
                                         maxDate = 201809,
                                         temp_loan_type = 'PV-Used')

PV_Used_output <- data.frame(rbindlist(l=list(PV_Used_12MOB,PV_Used_15MOB,PV_Used_18MOB)))
rm(PV_Used_12MOB,PV_Used_15MOB,PV_Used_18MOB)


# fwrite(PV_Used_output,"./data/eda/PV_Used_bad_loan_def.csv",row.names = F)
output_list = list("PV_used" = PV_Used_output)
save_xlsx_output(data = output_list, relative_path = "//bad_loan//bad_loan_PV_Used.xlsx")





TW_New_12MOB <- get_bad_loan_definition(mob_timeframe = 12,
                                        DPD_values = c(30,60,90),
                                        minDate = 201803, 
                                        maxDate = 201903,
                                        temp_loan_type = '2W-New')

TW_New_15MOB <- get_bad_loan_definition(mob_timeframe = 15,
                                        DPD_values = c(30,60,90),
                                        minDate = 201803, 
                                        maxDate = 201812,
                                        temp_loan_type = '2W-New')

TW_New_18MOB <- get_bad_loan_definition(mob_timeframe = 18,
                                        DPD_values = c(30,60,90),
                                        minDate = 201803, 
                                        maxDate = 201809,
                                        temp_loan_type = '2W-New')

TW_new_output <- data.frame(rbindlist(l=list(TW_New_12MOB,TW_New_15MOB,TW_New_18MOB)))
rm(TW_New_12MOB,TW_New_15MOB,TW_New_18MOB)


# fwrite(TW_new_output,"./data/eda/2W_new_bad_loan_def.csv",row.names = F)
output_list = list("TW_new" = TW_new_output)
save_xlsx_output(data = output_list, relative_path = "//bad_loan//bad_loan_TW_New.xlsx")








