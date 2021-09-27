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
min_disbursal_date <- '2019-06-01'
max_disbursal_date <- '2020-12-31'

req_cols <- c('deal_no',
              'jul19dpd','aug19dpd','sep19dpd',
              'oct19dpd','nov19dpd','dec19dpd',
              'jan20dpd','feb20dpd','mar20dpd',
              'apr20dpd','may20dpd','jun20dpd',
              'jul20dpd','aug20dpd','sep20dpd',
              'oct20dpd','nov20dpd','dec20dpd'
              
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

disbursals_final$mob <- disbursals_final$payment_index - disbursals_final$disbursal_index

deal_with_month_payment <- distinct(disbursals_final %>% filter(mob == 0) %>% dplyr::select(deal_no))
deal_with_month_payment <- deal_with_month_payment %>% ungroup()
deal_with_month_payment <- distinct(deal_with_month_payment %>% dplyr::select(deal_no))
deal_with_month_payment <- unique(deal_with_month_payment$deal_no)

disbursals_final$mob <- ifelse(disbursals_final$deal_no %in% deal_with_month_payment, disbursals_final$mob+1, disbursals_final$mob)


rm(disbursals,dpd_data,repayment_info)

# test <- distinct(disbursals_final %>% ungroup() %>% dplyr::select(deal_no))


assert_data_non_empty(disbursals_final)
save(disbursals_final,
     file = file.path(
       get_data_path()$data$intermediate,
       "ADS_data",
       "disbursals_final_all.rdata"
     )
)



rm(list=ls())




