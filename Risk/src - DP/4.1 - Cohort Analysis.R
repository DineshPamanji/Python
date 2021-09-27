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




## 3. Net vintage curves -------------------------------------------------------------------------------

# 3.1 create copy
disbursals_copy <- copy(disbursals_final)

# 3.2 create dpd flags
disbursals_copy$dpd_30_flag <- ifelse(disbursals_copy$dpd_status > 30,1,0)
disbursals_copy$dpd_60_flag <- ifelse(disbursals_copy$dpd_status > 60,1,0)
disbursals_copy$dpd_90_flag <- ifelse(disbursals_copy$dpd_status > 90,1,0)

# disbursals_copy <- disbursals_copy %>% filter(first_repayment_month <= 24)
disbursals_copy <- disbursals_copy %>% filter(disbursal_index <= 24)
disbursals_copy <- disbursals_copy %>% filter(mob <= 24)

for(temp_loan_type in req_loan_types){
  
  # get overall disbursement counts across months
  temp_disbursals <- disbursals_copy %>% filter(loan_type == temp_loan_type)

  # overall_disbursement <- temp_disbursals %>% group_by(first_repayment_month) %>% summarise(disbursal_count = length(unique(deal_no)))
  # overall_disbursement <- overall_disbursement %>% arrange(first_repayment_month)
  
  overall_disbursement <- temp_disbursals %>% group_by(disbursal_index) %>% summarise(disbursal_count = length(unique(deal_no)))
  overall_disbursement <- overall_disbursement %>% arrange(disbursal_index)
  
  # get 30dpd count
  # count_30_dpd <- dcast(mob ~ first_repayment_month,
  #                       data = temp_disbursals %>% filter(dpd_30_flag == 1),
  #                       value.var = "deal_no", function(x) (length(unique(x))) )
  # 
  # count_60_dpd <- dcast(mob ~ first_repayment_month,
  #                       data = temp_disbursals %>% filter(dpd_60_flag == 1),
  #                       value.var = "deal_no", function(x) (length(unique(x))) )
  # 
  # count_90_dpd <- dcast(mob ~ first_repayment_month,
  #                       data = temp_disbursals %>% filter(dpd_90_flag == 1),
  #                       value.var = "deal_no", function(x) (length(unique(x))) )
  # 
  
  count_30_dpd <- dcast(mob ~ disbursal_index,
                        data = temp_disbursals %>% filter(dpd_30_flag == 1),
                        value.var = "deal_no", function(x) (length(unique(x))) )
  
  count_60_dpd <- dcast(mob ~ disbursal_index,
                        data = temp_disbursals %>% filter(dpd_60_flag == 1),
                        value.var = "deal_no", function(x) (length(unique(x))) )
  
  count_90_dpd <- dcast(mob ~ disbursal_index,
                        data = temp_disbursals %>% filter(dpd_90_flag == 1),
                        value.var = "deal_no", function(x) (length(unique(x))) )
  
  
  # save output
  save_csv_output(count_30_dpd, paste0("\\vintage_curves\\net_30_dpd_",temp_loan_type,".csv"))
  save_csv_output(count_60_dpd, paste0("\\vintage_curves\\net_60_dpd_",temp_loan_type,".csv"))
  save_csv_output(count_90_dpd, paste0("\\vintage_curves\\net_90_dpd_",temp_loan_type,".csv"))
  save_csv_output(overall_disbursement, paste0("\\vintage_curves\\all_disbursements_net_",temp_loan_type,".csv"))
  print(paste0(temp_loan_type," -  done"))
  
  rm(count_30_dpd,count_60_dpd,count_90_dpd,temp_disbursals,overall_disbursement)
  
}



## 4. Gross vintage curves --------------------------------------------------------------

# 4.1 create copy
disbursals_copy <- copy(disbursals_final)

# 4.2 create dpd flags
disbursals_copy$dpd_30_flag <- ifelse(disbursals_copy$dpd_status > 30,1,0)
disbursals_copy$dpd_60_flag <- ifelse(disbursals_copy$dpd_status > 60,1,0)
disbursals_copy$dpd_90_flag <- ifelse(disbursals_copy$dpd_status > 90,1,0)

# disbursals_copy <- disbursals_copy %>% filter(first_repayment_month <= 24)
disbursals_copy <- disbursals_copy %>% filter(disbursal_index <= 24)
disbursals_copy <- disbursals_copy %>% filter(mob <= 24)

for(temp_loan_type in req_loan_types){
  
  # get overall disbursement counts across months
  temp_disbursals <- disbursals_copy %>% filter(loan_type == temp_loan_type)
  
  # overall_disbursement <- temp_disbursals %>% group_by(first_repayment_month) %>% summarise(disbursal_count = length(unique(deal_no)))
  # overall_disbursement <- overall_disbursement %>% arrange(first_repayment_month)
  
  overall_disbursement <- temp_disbursals %>% group_by(disbursal_index) %>% summarise(disbursal_count = length(unique(deal_no)))
  overall_disbursement <- overall_disbursement %>% arrange(disbursal_index)
  
  # sort based on deal no & first repayment month
  temp_disbursals <- temp_disbursals %>% arrange(deal_no,mob)
  
  # filter for 30, 60, 90 cases
  gross_30_dpd <- temp_disbursals %>% filter(dpd_30_flag == 1)
  gross_60_dpd <- temp_disbursals %>% filter(dpd_60_flag == 1)
  gross_90_dpd <- temp_disbursals %>% filter(dpd_90_flag == 1)
  
  # get minimum mob
  gross_30_dpd <- gross_30_dpd %>% group_by(deal_no) %>% summarise(min_30dpd_index = min(mob))
  gross_60_dpd <- gross_60_dpd %>% group_by(deal_no) %>% summarise(min_60dpd_index = min(mob))
  gross_90_dpd <- gross_90_dpd %>% group_by(deal_no) %>% summarise(min_90dpd_index = min(mob))
  
  
  # merge datasets
  temp_disbursals <- left_join(temp_disbursals,gross_30_dpd,by='deal_no')
  temp_disbursals <- left_join(temp_disbursals,gross_60_dpd,by='deal_no')
  temp_disbursals <- left_join(temp_disbursals,gross_90_dpd,by='deal_no')
  
  
  # create gross flags
  temp_disbursals$gross_30_dpd <- ifelse(is.na(temp_disbursals$min_30dpd_index),0,
                                               ifelse(temp_disbursals$mob >= temp_disbursals$min_30dpd_index, 1, 0))
  
  temp_disbursals$gross_60_dpd <- ifelse(is.na(temp_disbursals$min_60dpd_index),0,
                                               ifelse(temp_disbursals$mob >= temp_disbursals$min_60dpd_index, 1, 0))
  
  temp_disbursals$gross_90_dpd <- ifelse(is.na(temp_disbursals$min_90dpd_index),0,
                                               ifelse(temp_disbursals$mob >= temp_disbursals$min_90dpd_index, 1, 0))
  
  
  # get 30dpd count
  # count_30_dpd <- dcast(mob ~ first_repayment_month,
  #                       data = temp_disbursals %>% filter(gross_30_dpd == 1),
  #                       value.var = "deal_no", function(x) (length(unique(x))) )
  # 
  # count_60_dpd <- dcast(mob ~ first_repayment_month,
  #                       data = temp_disbursals %>% filter(gross_60_dpd == 1),
  #                       value.var = "deal_no", function(x) (length(unique(x))) )
  # 
  # count_90_dpd <- dcast(mob ~ first_repayment_month,
  #                       data = temp_gross_disbursals %>% filter(gross_90_dpd == 1),
  #                       value.var = "deal_no", function(x) (length(unique(x))) )
  # 
  
  count_30_dpd <- dcast(mob ~ disbursal_index,
                        data = temp_disbursals %>% filter(gross_30_dpd == 1),
                        value.var = "deal_no", function(x) (length(unique(x))) )
  
  count_60_dpd <- dcast(mob ~ disbursal_index,
                        data = temp_disbursals %>% filter(gross_60_dpd == 1),
                        value.var = "deal_no", function(x) (length(unique(x))) )
  
  count_90_dpd <- dcast(mob ~ disbursal_index,
                        data = temp_disbursals %>% filter(gross_90_dpd == 1),
                        value.var = "deal_no", function(x) (length(unique(x))) )
  
  
  # save output
  save_csv_output(count_30_dpd, paste0("\\vintage_curves\\gross_30_dpd_",temp_loan_type,".csv"))
  save_csv_output(count_60_dpd, paste0("\\vintage_curves\\gross_60_dpd_",temp_loan_type,".csv"))
  save_csv_output(count_90_dpd, paste0("\\vintage_curves\\gross_90_dpd_",temp_loan_type,".csv"))
  save_csv_output(overall_disbursement, paste0("\\vintage_curves\\all_disbursements_gross_",temp_loan_type,".csv"))
  print(paste0(temp_loan_type," -  done"))
  
  rm(count_30_dpd,count_60_dpd,count_90_dpd,temp_disbursals,overall_disbursement,gross_30_dpd,gross_60_dpd,gross_90_dpd)
  
}
 



## 5. Bad loan definition -------------------------------------------------------------------


