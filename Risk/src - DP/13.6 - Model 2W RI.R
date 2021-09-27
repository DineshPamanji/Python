#############################################################################################
################## 12 - Model 2W SENP & SEP  ################################################
#############################################################################################


## 0. Load helper functions & libraries -----------------------------------------------------
load_libaries <- file.path("src", "utils", "load_libraries.R")
source(load_libaries)

io_helper <- file.path("src", "utils", "io_helper.R")
source(io_helper)

model_functions <- file.path("src", "utils", "model_functions.R")
source(model_functions)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)



# 1. Validation 1 - Big ticket & small ticket --------------------------------------------------------------------------------

# load_rdata_output("model//2W_New//Combined//train_data.rdata")
# load_rdata_output("model//2W_New//Combined//test_data.rdata")
# load_rdata_output("model//2W_New//Combined//oot_data.rdata")

load_rdata_intermediate("reject_data//variable_data//model_data_2W_New_A_R.rdata")
oot_date_start <- '2018-04-01'
oot_date_end <- '2018-06-30'

oot_data <-
  model_data_2W_New_A_R %>% filter((date >= as.Date(oot_date_start)) &
                                 (date <= as.Date(oot_date_end)) &
                                 (Category %in% c('SENP','SEP','SAL')))

dev_data <-
  model_data_2W_New_A_R %>% filter(UID %notin% unique(oot_data$UID) &
                                 (Category %in% c('SENP','SEP','SAL')))


# dev_A <- rbind(train_data_final,test_data_final)
# oot_A <- oot_data_final

# rm(model_data_2W_New_rejects, train_data_final,test_data_final,oot_data_final)
rm(model_data_2W_New_A_R)

######################################################################################################################################
## 6.3. Variable transformation 

# 6.3.1 vintage
dev_data$Var_credit_vintage_C <- ifelse(dev_data$Var_credit_vintage <= 0.01, 0.01, 
                                        ifelse(dev_data$Var_credit_vintage >= 12, 12, dev_data$Var_credit_vintage))

oot_data$Var_credit_vintage_C <- ifelse(oot_data$Var_credit_vintage <= 0.01, 0.01, 
                                        ifelse(oot_data$Var_credit_vintage >= 12, 12, oot_data$Var_credit_vintage))


# 6.3.2 count of closed loans
dev_data$Var_PO_closed_excl_CC_C <- dev_data$Var_PO_closed_excl_CC
dev_data$Var_PO_closed_excl_CC_C[is.na(dev_data$Var_PO_closed_excl_CC_C)] <- 0

oot_data$Var_PO_closed_excl_CC_C <- oot_data$Var_PO_closed_excl_CC
oot_data$Var_PO_closed_excl_CC_C[is.na(oot_data$Var_PO_closed_excl_CC_C)] <- 0

woe1 <- -0.1082455
woe2 <- 0.05734978
woe3 <- 0.2372358

dev_data$Var_PO_closed_excl_CC_woe <- ifelse(dev_data$Var_PO_closed_excl_CC_C <= 1, woe1, 
                                             ifelse(dev_data$Var_PO_closed_excl_CC_C == 2, woe2,woe3))

oot_data$Var_PO_closed_excl_CC_woe <- ifelse(oot_data$Var_PO_closed_excl_CC_C <= 1, woe1, 
                                             ifelse(oot_data$Var_PO_closed_excl_CC_C == 2, woe2,woe3))

rm(woe1, woe2, woe3)



# 6.3.3 HL paid 12 month Continuous
dev_data$Var_DL_HL_paid_GE_12mon_C <- dev_data$Var_DL_HL_paid_GE_12mon
dev_data$Var_DL_HL_paid_GE_12mon_C[is.na(dev_data$Var_DL_HL_paid_GE_12mon_C)] <- 0

dev_data$Var_DL_HL_paid_GE_12mon_C <- ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C >= 3, 3, 
                                             ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C <= 0, 0,dev_data$Var_DL_HL_paid_GE_12mon_C))


oot_data$Var_DL_HL_paid_GE_12mon_C <- oot_data$Var_DL_HL_paid_GE_12mon
oot_data$Var_DL_HL_paid_GE_12mon_C[is.na(oot_data$Var_DL_HL_paid_GE_12mon_C)] <- 0

oot_data$Var_DL_HL_paid_GE_12mon_C <- ifelse(oot_data$Var_DL_HL_paid_GE_12mon_C >= 3, 3, 
                                             ifelse(oot_data$Var_DL_HL_paid_GE_12mon_C <= 0, 0,oot_data$Var_DL_HL_paid_GE_12mon_C))


# 6.3.4 Gold paid 12 month continuous
dev_data$Var_DL_Gold_paid_GE_12mon_C <- dev_data$Var_DL_Gold_paid_GE_12mon
dev_data$Var_DL_Gold_paid_GE_12mon_C[is.na(dev_data$Var_DL_Gold_paid_GE_12mon_C)] <- 0

oot_data$Var_DL_Gold_paid_GE_12mon_C <- oot_data$Var_DL_Gold_paid_GE_12mon
oot_data$Var_DL_Gold_paid_GE_12mon_C[is.na(oot_data$Var_DL_Gold_paid_GE_12mon_C)] <- 0

woe1 <- -0.02766028
woe2 <- 0.1895425
woe3 <- 0.3895373

dev_data$Var_DL_Gold_paid_GE_12mon_woe <- ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C == 0, woe1, 
                                                 ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C == 1, woe2,woe3))

oot_data$Var_DL_Gold_paid_GE_12mon_woe <- ifelse(oot_data$Var_DL_Gold_paid_GE_12mon_C == 0, woe1, 
                                                 ifelse(oot_data$Var_DL_Gold_paid_GE_12mon_C == 1, woe2,woe3))

rm(woe1, woe2, woe3)



# 6.3.5 Enquiry in 6m non cc 
dev_data$Var_EN_enquiry_count_6m_non_CC_C <- dev_data$Var_EN_enquiry_count_6m_non_CC
dev_data$Var_EN_enquiry_count_6m_non_CC_C[is.na(dev_data$Var_EN_enquiry_count_6m_non_CC_C)] <- 0

dev_data$Var_EN_enquiry_count_6m_non_CC_C <- ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C <= 0, 0, 
                                                    ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C >= 6, 6, dev_data$Var_EN_enquiry_count_6m_non_CC_C))


oot_data$Var_EN_enquiry_count_6m_non_CC_C <- oot_data$Var_EN_enquiry_count_6m_non_CC
oot_data$Var_EN_enquiry_count_6m_non_CC_C[is.na(oot_data$Var_EN_enquiry_count_6m_non_CC_C)] <- 0

oot_data$Var_EN_enquiry_count_6m_non_CC_C <- ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C <= 0, 0, 
                                                    ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C >= 6, 6, oot_data$Var_EN_enquiry_count_6m_non_CC_C))



# 6.3.6 30 dpd in 6 mon
dev_data$Var_DL_all_30dpd_6mon_C <- dev_data$Var_DL_all_30dpd_6mon
dev_data$Var_DL_all_30dpd_6mon_C[is.na(dev_data$Var_DL_all_30dpd_6mon_C)] <- 0

dev_data$Var_DL_all_30dpd_6mon_flag <- ifelse(dev_data$Var_DL_all_30dpd_6mon_C == 0, 0, 1)


oot_data$Var_DL_all_30dpd_6mon_C <- oot_data$Var_DL_all_30dpd_6mon
oot_data$Var_DL_all_30dpd_6mon_C[is.na(oot_data$Var_DL_all_30dpd_6mon_C)] <- 0

oot_data$Var_DL_all_30dpd_6mon_flag <- ifelse(oot_data$Var_DL_all_30dpd_6mon_C == 0, 0, 1)


## 6.3.7 SENP SEP Flag
dev_data$category_flag_SENP_SEP <- ifelse(dev_data$Category %in% c('SENP', 'SEP'), 1, 0)
oot_data$category_flag_SENP_SEP <- ifelse(oot_data$Category %in% c('SENP', 'SEP'), 1, 0)

## 6.3.8 Agri profile flag
dev_data$agri_profile_flag <- ifelse(is.na(dev_data$agri_profile_flag), 0 , dev_data$agri_profile_flag)
oot_data$agri_profile_flag <- ifelse(is.na(oot_data$agri_profile_flag), 0 , oot_data$agri_profile_flag)


## 6.3.9 cross tab variable
dev_data$Var_sanctioned_amount_live_loans_C <- dev_data$Var_sanctioned_amount_live_loans
dev_data$Var_sanctioned_amount_live_loans_C[is.na(dev_data$Var_sanctioned_amount_live_loans_C)] <- 0


dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- dev_data$Var_outstanding_by_sanctioned_amount_live_loans
dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 1



oot_data$Var_sanctioned_amount_live_loans_C <- oot_data$Var_sanctioned_amount_live_loans
oot_data$Var_sanctioned_amount_live_loans_C[is.na(oot_data$Var_sanctioned_amount_live_loans_C)] <- 0


oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- oot_data$Var_outstanding_by_sanctioned_amount_live_loans
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 1




dev_data$Var_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_sanctioned_amount_live_loans_C <= 175000, 'bin_LE_175k', 
                                                        ifelse(dev_data$Var_sanctioned_amount_live_loans_C > 175000 & dev_data$Var_sanctioned_amount_live_loans_C <= 1000000, 'bin_175k_to_10L', 'bin_GE_10L'))


dev_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.25 & dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.5, 'val_25_to_50_pct','val_GE_50_pct'))


subset1 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct')))
subset2 <- dev_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct'))))
subset3 <- dev_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_50_pct','val_GE_50_pct'))))
subset4 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct')))

woe1 <- -0.1348573
woe2 <- 0.1250062
woe3 <- 0.4968223
woe4 <- 0.6763479



dev_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live <- ifelse(dev_data$application_no %in% unique(subset1$application_no), woe1,
                                                                        ifelse(dev_data$application_no %in% unique(subset2$application_no), woe2,
                                                                               ifelse(dev_data$application_no %in% unique(subset3$application_no), woe3,
                                                                                      woe4)))



oot_data$Var_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_sanctioned_amount_live_loans_C <= 175000, 'bin_LE_175k', 
                                                        ifelse(oot_data$Var_sanctioned_amount_live_loans_C > 175000 & oot_data$Var_sanctioned_amount_live_loans_C <= 1000000, 'bin_175k_to_10L', 'bin_GE_10L'))


oot_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.25 & oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.5, 'val_25_to_50_pct','val_GE_50_pct'))


subset1 <- oot_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct')))
subset2 <- oot_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct'))))
subset3 <- oot_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_50_pct','val_GE_50_pct'))))
subset4 <- oot_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct')))



oot_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live <- ifelse(oot_data$application_no %in% unique(subset1$application_no), woe1,
                                                                        ifelse(oot_data$application_no %in% unique(subset2$application_no), woe2,
                                                                               ifelse(oot_data$application_no %in% unique(subset3$application_no), woe3,
                                                                                      woe4)))


rm(subset1,subset2,subset3,subset4,woe1, woe2, woe3, woe4)





## 6.4 get list of features
shortlisted_var <- c( 
  'Var_credit_vintage_C',
  'Var_PO_closed_excl_CC_woe',
  'cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live',
  'Var_DL_HL_paid_GE_12mon_C',
  'Var_DL_Gold_paid_GE_12mon_woe',
  'Var_EN_enquiry_count_6m_non_CC_C',
  'Var_DL_all_30dpd_6mon_flag',
  'category_flag_SENP_SEP',
  'agri_profile_flag'
)


# 6.5 subset for required columns
performance_data_dev <- dev_data %>% dplyr::select(c('deal_no','application_no','applicant_id','UID','tag',shortlisted_var,'bad_loan'))
performance_data_oot <- oot_data %>% dplyr::select(c('deal_no','application_no','applicant_id','UID','tag',shortlisted_var,'bad_loan'))

# 6.6 load model
# model_2W <- readRDS(file = file.path(get_data_path()$data$model,"model_2W_New.rds"))
model_2W <- readRDS(file = file.path(get_data_path()$data$model,"vcheck//model_2W_New.rds"))


# 6.7 make predictions
predictions_dev <- get_predictions(model_2W, performance_data_dev)
predictions_oot <- get_predictions(model_2W, performance_data_oot)


###################################################################################################################



get_population_summary <- function(subset_data){
  subset_data$prediction_pentile <- ntile(subset_data$predictions,5)
  population_pct_P <- subset_data %>% group_by(prediction_pentile,tag) %>% summarise(count = length(unique(UID)))
  delinquency_pct_P <- subset_data %>% group_by(prediction_pentile,tag) %>% summarise(default = mean(bad_loan))
  
  subset_data$prediction_decile <- ntile(subset_data$predictions,10)
  population_pct_D <- subset_data %>% group_by(prediction_decile,tag) %>% summarise(count = length(unique(UID)))
  delinquency_pct_D <- subset_data %>% group_by(prediction_decile,tag) %>% summarise(default = mean(bad_loan))
  
  output_list <- list("population_pct_P" = population_pct_P,
                      "delinquency_pct_P" = delinquency_pct_P,
                      "population_pct_D" = population_pct_D,
                      "delinquency_pct_D" = delinquency_pct_D
  )
  
  return(output_list)
}


excel_data <- get_population_summary(subset_data = predictions_dev %>% filter(tag %in% c('A')))
save_xlsx_output(data = excel_data, relative_path = "//model//2W_New//Combined//vcheck//RI//1-Check - A.xlsx")


excel_data <- get_population_summary(subset_data = predictions_dev %>% filter(tag %in% c('A','R1')))
save_xlsx_output(data = excel_data, relative_path = "//model//2W_New//Combined//vcheck//RI//2-Check - A_R1.xlsx")

excel_data <- get_population_summary(subset_data = predictions_dev %>% filter(tag %in% c('A','R1','R2')))
save_xlsx_output(data = excel_data, relative_path = "//model//2W_New//Combined//vcheck//RI//3-Check - A_R1_R2.xlsx")

excel_data <- get_population_summary(subset_data = predictions_dev %>% filter(tag %in% c('A','R1','R2','R3')))
save_xlsx_output(data = excel_data, relative_path = "//model//2W_New//Combined//vcheck//RI//4-Check - A_R1_R2_R3.xlsx")


excel_data <- get_population_summary(subset_data = predictions_dev %>% filter(tag %in% c('A','R1','R2','R3','R4')))
save_xlsx_output(data = excel_data, relative_path = "//model//2W_New//Combined//vcheck//RI//5-Check - A_R1_R2_R3_R4.xlsx")






