############################################################################################
################## 3 - NTC application variable testing ####################################
############################################################################################


## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")


## 1. Load data ----------------------------------------------------------------------------

## 1.1 Load ADS data & filter for NTC data
load_rdata_intermediate("ADS_data//base_ads.rdata")

ntc_data <- base_ads %>% dplyr::select(deal_no,
                                       loan_type,
                                       disbursal_date,
                                       year_mon,
                                       profile_primary_applicant,
                                       CIBIL_SCORE,
                                       Application_No
                                       )

ntc_data <- ntc_data %>% filter(loan_type %in% c('2W-New','PV-New','PV-Used'))
ntc_data <- ntc_data %>% filter(!is.na(Application_No))
ntc_data <- ntc_data %>% filter((is.na(CIBIL_SCORE)) | (CIBIL_SCORE <= 1))
ntc_data <- ntc_data %>% filter(profile_primary_applicant %in% c('SENP','SEP','SAL'))



## load BEN code & asset classification tags
load_rdata_intermediate("cleaned_data//asset_classification_data.rdata")
asset_classification_data <- distinct(asset_classification_data %>% dplyr::select(Deal_No,
                                                                                  Ben_Code,
                                                                                  Segment_Code))

ntc_data <- left_join(ntc_data,asset_classification_data,by = c('deal_no' = 'Deal_No'))

rm(asset_classification_data)





## load application data
load_rdata_intermediate("cleaned_data//application_data.rdata")

app_data <- distinct(application_data %>% dplyr::select(AppNo,
                                                        customer_type_cleaned,
                                                        evaluation_type_primary_applicant,
                                                        # Marital_Status,
                                                        App_Marital_Status,
                                                        cust_qualification,
                                                        Resi_Years,
                                                        Resi_Years_incity,
                                                        Off_Yrs,
                                                        off_currstability,
                                                        Off_TotExp,
                                                        Off_Exp
                                                        ))



## load validation data - 60 DPD in 9MOB
load_rdata_intermediate("ADS_data//validation_60dpd_9mob.rdata")
validation_60dpd_9mob$loan_type <- NULL


## load validation data - 60 DPD post covid
load_rdata_intermediate("ADS_data//validation_60dpd_oct_to_dec_2020.rdata")
validation_60dpd_oct_to_dec_2020$loan_type <- NULL



## join data ---------------------------------------------------------------------------------

ntc_p1 <- inner_join(validation_60dpd_9mob,ntc_data,by = 'deal_no')
ntc_p1 <- inner_join(ntc_p1,app_data, by = c("Application_No" = "AppNo"))

ntc_p2 <- inner_join(validation_60dpd_oct_to_dec_2020,ntc_data,by = 'deal_no')
ntc_p2 <- inner_join(ntc_p2,app_data, by = c("Application_No" = "AppNo"))



assert_data_non_empty(ntc_p1)
save(ntc_p1,
     file = file.path(
       get_data_path()$data$intermediate,
       "NTC_overlay",
       "NTC_60dpd_9mob_data.rdata"
     )
)


assert_data_non_empty(ntc_p2)
save(ntc_p2,
     file = file.path(
       get_data_path()$data$intermediate,
       "NTC_overlay",
       "NTC_60dpd_post_covid_data.rdata"
     )
)










