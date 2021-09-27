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



load_rdata_intermediate("reject_data//variable_data//model_data_PV_New_A_R.rdata")
oot_date_start <- '2018-04-01'
oot_date_end <- '2018-06-30'



oot_data <-
  model_data_PV_New_A_R %>% filter((date >= as.Date(oot_date_start)) &
                                 (date <= as.Date(oot_date_end)) &
                                  (tag %in% c('A','R1','R2','R3','R4')) &
                                 (Category %in% c('SENP','SEP','SAL')))

dev_data <-
  model_data_PV_New_A_R %>% filter(UID %notin% unique(oot_data$UID) &
                                     (tag %in% c('A','R1','R2','R3','R4')) &
                                 (Category %in% c('SENP','SEP','SAL')))


# dev_A <- rbind(train_data_final,test_data_final)
# oot_A <- oot_data_final

# rm(model_data_2W_New_rejects, train_data_final,test_data_final,oot_data_final)
rm(model_data_PV_New_A_R)

######################################################################################################################################
## 6.3. Variable transformation 


## 6.3. Variable transformation 

# 6.3.1 vintage
dev_data$Var_credit_vintage_C <- ifelse(dev_data$Var_credit_vintage <= 0.01, 0.01, 
                                                 ifelse(dev_data$Var_credit_vintage >= 15, 15, dev_data$Var_credit_vintage))

oot_data$Var_credit_vintage_C <- ifelse(oot_data$Var_credit_vintage <= 0.01, 0.01, 
                                                 ifelse(oot_data$Var_credit_vintage >= 15, 15, oot_data$Var_credit_vintage))

# 6.3.2 HL paid 12 month Flag

dev_data$Var_DL_HL_paid_GE_12mon_flag <-  dev_data$Var_DL_HL_paid_GE_12mon

dev_data$Var_DL_HL_paid_GE_12mon_flag[is.na(dev_data$Var_DL_HL_paid_GE_12mon_flag)] <-  0
dev_data$Var_DL_HL_paid_GE_12mon_flag <-  ifelse(dev_data$Var_DL_HL_paid_GE_12mon_flag == 0, 0, 1)

oot_data$Var_DL_HL_paid_GE_12mon_flag <-  oot_data$Var_DL_HL_paid_GE_12mon

oot_data$Var_DL_HL_paid_GE_12mon_flag[is.na(oot_data$Var_DL_HL_paid_GE_12mon_flag)] <-  0
oot_data$Var_DL_HL_paid_GE_12mon_flag <-  ifelse(oot_data$Var_DL_HL_paid_GE_12mon_flag == 0, 0, 1)


# 6.3.3 Gold paid 12 month flag

dev_data$Var_DL_Gold_paid_GE_12mon_flag_var <-  dev_data$Var_DL_Gold_paid_GE_12mon_flag
dev_data$Var_DL_Gold_paid_GE_12mon_flag_var[is.na(dev_data$Var_DL_Gold_paid_GE_12mon_flag_var)] <-  0

oot_data$Var_DL_Gold_paid_GE_12mon_flag_var <-  oot_data$Var_DL_Gold_paid_GE_12mon_flag
oot_data$Var_DL_Gold_paid_GE_12mon_flag_var[is.na(oot_data$Var_DL_Gold_paid_GE_12mon_flag_var)] <-  0

# 6.3.4 Enquiry in 6m non cc 
dev_data$Var_EN_enquiry_count_6m_non_CC_C <- dev_data$Var_EN_enquiry_count_6m_non_CC

dev_data$Var_EN_enquiry_count_6m_non_CC_C[is.na(dev_data$Var_EN_enquiry_count_6m_non_CC_C)] <- 0


dev_data$Var_EN_enquiry_count_6m_non_CC_C <- ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C <= 0, 0, 
                                                             ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C >= 4, 4,
                                                                    dev_data$Var_EN_enquiry_count_6m_non_CC_C))



oot_data$Var_EN_enquiry_count_6m_non_CC_C <- oot_data$Var_EN_enquiry_count_6m_non_CC

oot_data$Var_EN_enquiry_count_6m_non_CC_C[is.na(oot_data$Var_EN_enquiry_count_6m_non_CC_C)] <- 0


oot_data$Var_EN_enquiry_count_6m_non_CC_C <- ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C <= 0, 0, 
                                                             ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C >= 4, 4, oot_data$Var_EN_enquiry_count_6m_non_CC_C))




# 6.3.5 30 dpd in 6 mon flag
dev_data$Var_DL_all_30dpd_6mon_C <- dev_data$Var_DL_all_30dpd_6mon

dev_data$Var_DL_all_30dpd_6mon_C[is.na(dev_data$Var_DL_all_30dpd_6mon_C)] <-  0

dev_data$Var_DL_all_30dpd_6mon_flag <-  ifelse(dev_data$Var_DL_all_30dpd_6mon_C == 0, 0, 1)

oot_data$Var_DL_all_30dpd_6mon_C <- oot_data$Var_DL_all_30dpd_6mon

oot_data$Var_DL_all_30dpd_6mon_C[is.na(oot_data$Var_DL_all_30dpd_6mon_C)] <-  0

oot_data$Var_DL_all_30dpd_6mon_flag <-  ifelse(oot_data$Var_DL_all_30dpd_6mon_C == 0, 0, 1)

## 6.3.6 SENP SEP Flag
dev_data$category_flag_SENP_SEP <- ifelse(dev_data$Category %in% c('SENP', 'SEP'), 1, 0)
oot_data$category_flag_SENP_SEP <- ifelse(oot_data$Category %in% c('SENP', 'SEP'), 1, 0)


## 6.3.7 cross tab variable
## 2.9.1 sanctioned amount live loan
dev_data$Var_PO_closed_sanction_amount_excl_CC_C <- dev_data$Var_PO_closed_sanction_amount_excl_CC
dev_data$Var_PO_closed_sanction_amount_excl_CC_C[is.na(dev_data$Var_PO_closed_sanction_amount_excl_CC_C)] <- 0

oot_data$Var_PO_closed_sanction_amount_excl_CC_C <- oot_data$Var_PO_closed_sanction_amount_excl_CC
oot_data$Var_PO_closed_sanction_amount_excl_CC_C[is.na(oot_data$Var_PO_closed_sanction_amount_excl_CC_C)] <- 0


## 2.9.2 Outstanding amount by sanctioned amount in live loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- dev_data$Var_outstanding_by_sanctioned_amount_live_loans
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- oot_data$Var_outstanding_by_sanctioned_amount_live_loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 0.1
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 0.1


## 2.9.3 cross tab

# dev_data_backup <- dev_data
# oot_data_backup <- oot_data
# 
# 
# dev_data <- dev_data %>% filter(tag %in% c('A','R1','R2'))
# oot_data <- oot_data %>% filter(tag %in% c('A','R1','R2'))



dev_data$Var_PO_closed_sanction_amount_excl_CC_bin <-  ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC <= 75000,'bin_LE_75k',
                                                              ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC > 50000 & dev_data$Var_PO_closed_sanction_amount_excl_CC <= 500000,'bin_75k_to_5L','bin_GE_5L'))

oot_data$Var_PO_closed_sanction_amount_excl_CC_bin <-  ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC <= 75000,'bin_LE_75k',
                                                              ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC > 50000 & oot_data$Var_PO_closed_sanction_amount_excl_CC <= 500000,'bin_75k_to_5L','bin_GE_5L'))



dev_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.5, 'val_LE_50_pct', 
                                                                       ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.5 & dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.75, 'val_50_to_75_pct','val_GE_75_pct'))


oot_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.5, 'val_LE_50_pct', 
                                                                       ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.5 & oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.75, 'val_50_to_75_pct','val_GE_75_pct'))





subset1 <- dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct')))
subset2 <- dev_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct'))))
subset3 <- dev_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | (Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_5L')))

woe1 <- -0.3941659
woe2 <- -0.03581256
woe3 <- 0.2070746


subset1_oot <- oot_data %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct')))
subset2_oot <- oot_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct'))))
subset3_oot <- oot_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | (Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_5L')))


dev_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed <- ifelse(dev_data$deal_no %in% unique(subset1$deal_no), woe1,
                                                                          ifelse(dev_data$deal_no %in% unique(subset2$deal_no), woe2,
                                                                                 woe3))

oot_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed <- ifelse(oot_data$deal_no %in% unique(subset1_oot$deal_no), woe1,
                                                                          ifelse(oot_data$deal_no %in% unique(subset2_oot$deal_no), woe2,
                                                                                 woe3))


rm(subset1,subset2,subset3,subset1_oot,subset2_oot,subset3_oot, woe1, woe2, woe3)







## 6.4 get list of features
shortlisted_var <- c(
  "Var_credit_vintage_C",
  "Var_DL_HL_paid_GE_12mon_flag",
  "Var_DL_Gold_paid_GE_12mon_flag_var",
  "Var_EN_enquiry_count_6m_non_CC_C",
  "Var_DL_all_30dpd_6mon_flag",
  "category_flag_SENP_SEP",
  "cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed"
)


# 6.5 subset for required columns
performance_data_dev <- dev_data %>% dplyr::select(c('deal_no','application_no','applicant_id','UID','tag',shortlisted_var,'bad_loan'))
performance_data_oot <- oot_data %>% dplyr::select(c('deal_no','application_no','applicant_id','UID','tag',shortlisted_var,'bad_loan'))

# 6.6 load model
# model_2W <- readRDS(file = file.path(get_data_path()$data$model,"model_2W_New.rds"))
model_PV <- readRDS(file = file.path(get_data_path()$data$model,"vcheck//model_PV_New.rds"))


# 6.7 make predictions
predictions_dev <- get_predictions(model_PV, performance_data_dev)
predictions_oot <- get_predictions(model_PV, performance_data_oot)






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
save_xlsx_output(data = excel_data, relative_path = "//model//PV_New//Combined//vcheck//RI//1-Check - A.xlsx")


excel_data <- get_population_summary(subset_data = predictions_dev %>% filter(tag %in% c('A','R1')))
save_xlsx_output(data = excel_data, relative_path = "//model//PV_New//Combined//vcheck//RI//2-Check - A_R1.xlsx")

excel_data <- get_population_summary(subset_data = predictions_dev %>% filter(tag %in% c('A','R1','R2')))
save_xlsx_output(data = excel_data, relative_path = "//model//PV_New//Combined//vcheck//RI//3-Check - A_R1_R2.xlsx")

excel_data <- get_population_summary(subset_data = predictions_dev %>% filter(tag %in% c('A','R1','R2','R3')))
save_xlsx_output(data = excel_data, relative_path = "//model//PV_New//Combined//vcheck//RI//4-Check - A_R1_R2_R3.xlsx")


excel_data <- get_population_summary(subset_data = predictions_dev %>% filter(tag %in% c('A','R1','R2','R3','R4')))
save_xlsx_output(data = excel_data, relative_path = "//model//PV_New//Combined//vcheck//RI//5-Check - A_R1_R2_R3_R4.xlsx")





################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################




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



load_rdata_intermediate("reject_data//variable_data//model_data_PV_New_A_R.rdata")
oot_date_start <- '2018-04-01'
oot_date_end <- '2018-06-30'



oot_data <-
  model_data_PV_New_A_R %>% filter((date >= as.Date(oot_date_start)) &
                                     (date <= as.Date(oot_date_end)) &
                                     (tag %in% c('A','R1')) &
                                     (Category %in% c('SENP','SEP','SAL')))

dev_data <-
  model_data_PV_New_A_R %>% filter(UID %notin% unique(oot_data$UID) &
                                     (tag %in% c('A','R1')) &
                                     (Category %in% c('SENP','SEP','SAL')))


# dev_A <- rbind(train_data_final,test_data_final)
# oot_A <- oot_data_final

# rm(model_data_2W_New_rejects, train_data_final,test_data_final,oot_data_final)
rm(model_data_PV_New_A_R)

######################################################################################################################################
## 6.3. Variable transformation 


## 6.3. Variable transformation 

# 6.3.1 vintage
dev_data$Var_credit_vintage_C <- ifelse(dev_data$Var_credit_vintage <= 0.01, 0.01, 
                                        ifelse(dev_data$Var_credit_vintage >= 15, 15, dev_data$Var_credit_vintage))

oot_data$Var_credit_vintage_C <- ifelse(oot_data$Var_credit_vintage <= 0.01, 0.01, 
                                        ifelse(oot_data$Var_credit_vintage >= 15, 15, oot_data$Var_credit_vintage))




# 6.3.2 HL paid 12 month Flag

dev_data$Var_DL_HL_paid_GE_12mon_flag <-  dev_data$Var_DL_HL_paid_GE_12mon

dev_data$Var_DL_HL_paid_GE_12mon_flag[is.na(dev_data$Var_DL_HL_paid_GE_12mon_flag)] <-  0
dev_data$Var_DL_HL_paid_GE_12mon_flag <-  ifelse(dev_data$Var_DL_HL_paid_GE_12mon_flag == 0, 0, 1)

oot_data$Var_DL_HL_paid_GE_12mon_flag <-  oot_data$Var_DL_HL_paid_GE_12mon

oot_data$Var_DL_HL_paid_GE_12mon_flag[is.na(oot_data$Var_DL_HL_paid_GE_12mon_flag)] <-  0
oot_data$Var_DL_HL_paid_GE_12mon_flag <-  ifelse(oot_data$Var_DL_HL_paid_GE_12mon_flag == 0, 0, 1)


# 6.3.3 Gold paid 12 month flag

dev_data$Var_DL_Gold_paid_GE_12mon_flag_var <-  dev_data$Var_DL_Gold_paid_GE_12mon_flag
dev_data$Var_DL_Gold_paid_GE_12mon_flag_var[is.na(dev_data$Var_DL_Gold_paid_GE_12mon_flag_var)] <-  0

oot_data$Var_DL_Gold_paid_GE_12mon_flag_var <-  oot_data$Var_DL_Gold_paid_GE_12mon_flag
oot_data$Var_DL_Gold_paid_GE_12mon_flag_var[is.na(oot_data$Var_DL_Gold_paid_GE_12mon_flag_var)] <-  0

# 6.3.4 Enquiry in 6m non cc 
dev_data$Var_EN_enquiry_count_6m_non_CC_C <- dev_data$Var_EN_enquiry_count_6m_non_CC

dev_data$Var_EN_enquiry_count_6m_non_CC_C[is.na(dev_data$Var_EN_enquiry_count_6m_non_CC_C)] <- 0


dev_data$Var_EN_enquiry_count_6m_non_CC_C <- ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C <= 0, 0, 
                                                    ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C >= 4, 4,
                                                           dev_data$Var_EN_enquiry_count_6m_non_CC_C))



oot_data$Var_EN_enquiry_count_6m_non_CC_C <- oot_data$Var_EN_enquiry_count_6m_non_CC

oot_data$Var_EN_enquiry_count_6m_non_CC_C[is.na(oot_data$Var_EN_enquiry_count_6m_non_CC_C)] <- 0


oot_data$Var_EN_enquiry_count_6m_non_CC_C <- ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C <= 0, 0, 
                                                    ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C >= 4, 4, oot_data$Var_EN_enquiry_count_6m_non_CC_C))




# 6.3.5 30 dpd in 6 mon flag
dev_data$Var_DL_all_30dpd_6mon_C <- dev_data$Var_DL_all_30dpd_6mon

dev_data$Var_DL_all_30dpd_6mon_C[is.na(dev_data$Var_DL_all_30dpd_6mon_C)] <-  0

dev_data$Var_DL_all_30dpd_6mon_flag <-  ifelse(dev_data$Var_DL_all_30dpd_6mon_C == 0, 0, 1)

oot_data$Var_DL_all_30dpd_6mon_C <- oot_data$Var_DL_all_30dpd_6mon

oot_data$Var_DL_all_30dpd_6mon_C[is.na(oot_data$Var_DL_all_30dpd_6mon_C)] <-  0

oot_data$Var_DL_all_30dpd_6mon_flag <-  ifelse(oot_data$Var_DL_all_30dpd_6mon_C == 0, 0, 1)

## 6.3.6 SENP SEP Flag
dev_data$category_flag_SENP_SEP <- ifelse(dev_data$Category %in% c('SENP', 'SEP'), 1, 0)
oot_data$category_flag_SENP_SEP <- ifelse(oot_data$Category %in% c('SENP', 'SEP'), 1, 0)


## 6.3.7 cross tab variable
## 2.9.1 sanctioned amount live loan
dev_data$Var_PO_closed_sanction_amount_excl_CC_C <- dev_data$Var_PO_closed_sanction_amount_excl_CC
dev_data$Var_PO_closed_sanction_amount_excl_CC_C[is.na(dev_data$Var_PO_closed_sanction_amount_excl_CC_C)] <- 0

oot_data$Var_PO_closed_sanction_amount_excl_CC_C <- oot_data$Var_PO_closed_sanction_amount_excl_CC
oot_data$Var_PO_closed_sanction_amount_excl_CC_C[is.na(oot_data$Var_PO_closed_sanction_amount_excl_CC_C)] <- 0


## 2.9.2 Outstanding amount by sanctioned amount in live loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- dev_data$Var_outstanding_by_sanctioned_amount_live_loans
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- oot_data$Var_outstanding_by_sanctioned_amount_live_loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 0.1
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 0.1


## 2.9.3 cross tab

# dev_data_backup <- dev_data
# oot_data_backup <- oot_data
# 
# 
# dev_data <- dev_data %>% filter(tag %in% c('A','R1','R2'))
# oot_data <- oot_data %>% filter(tag %in% c('A','R1','R2'))



dev_data$Var_PO_closed_sanction_amount_excl_CC_bin <-  ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC <= 75000,'bin_LE_75k',
                                                              ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC > 75000 & dev_data$Var_PO_closed_sanction_amount_excl_CC <= 500000,'bin_75k_to_5L','bin_GE_5L'))

oot_data$Var_PO_closed_sanction_amount_excl_CC_bin <-  ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC <= 75000,'bin_LE_75k',
                                                              ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC > 75000 & oot_data$Var_PO_closed_sanction_amount_excl_CC <= 500000,'bin_75k_to_5L','bin_GE_5L'))



dev_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.5, 'val_LE_50_pct', 
                                                                       ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.5 & dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.75, 'val_50_to_75_pct','val_GE_75_pct'))


oot_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.5, 'val_LE_50_pct', 
                                                                       ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.5 & oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.75, 'val_50_to_75_pct','val_GE_75_pct'))





subset1 <- dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct')))
subset2 <- dev_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct'))))
subset3 <- dev_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | (Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_5L')))



mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)

woe1 <- get_3bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe2 <- get_3bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe3 <- get_3bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')

# woe1 <- -0.3906258
# woe2 <- -0.0263269
# woe3 <- 0.1947842


subset1_oot <- oot_data %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct')))
subset2_oot <- oot_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct'))))
subset3_oot <- oot_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | (Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_5L')))


dev_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed <- ifelse(dev_data$UID %in% unique(subset1$UID), woe1,
                                                                          ifelse(dev_data$UID %in% unique(subset2$UID), woe2,
                                                                                 woe3))

oot_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed <- ifelse(oot_data$UID %in% unique(subset1_oot$UID), woe1,
                                                                          ifelse(oot_data$UID %in% unique(subset2_oot$UID), woe2,
                                                                                 woe3))


rm(subset1,subset2,subset3,subset1_oot,subset2_oot,subset3_oot, woe1, woe2, woe3)







## 6.4 get list of features
shortlisted_var <- c(
  "Var_credit_vintage_C",
  "Var_DL_HL_paid_GE_12mon_flag",
  "Var_DL_Gold_paid_GE_12mon_flag_var",
  "Var_EN_enquiry_count_6m_non_CC_C",
  "Var_DL_all_30dpd_6mon_flag",
  "category_flag_SENP_SEP",
  "cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed"
)


seed_iterations_output <- get_seed_iterations(seed_list = c(1:200),
                                              dev_data = dev_data,
                                              oot = oot_data,
                                              feature_list = shortlisted_var, 
                                              p_value_threshold = 0.05)

selected_seeds <- seed_iterations_output %>% filter((p_value_check == 0) & 
                                                      (RO_decile_overall >= 0) & 
                                                      (RO_pentile_overall == 3))








selected_seed <- 112

set.seed(selected_seed)


split <- sample.split(dev_data$bad_loan, SplitRatio = 0.8)
train_data_final <- data.frame(subset(dev_data, split == TRUE))
test_data_final <- data.frame(subset(dev_data, split == FALSE))


final_model <- glm(bad_loan ~., data = train_data_final %>% dplyr::select(c(shortlisted_var,'bad_loan')), family = binomial("logit"))

summary(final_model)


train_data_final <- get_predictions(final_model, train_data_final)
test_data_final <- get_predictions(final_model, test_data_final)
oot_data_final <- get_predictions(final_model, oot_data)



model_performance_final <- get_model_performance(train_data_final,test_data_final,oot_data_final)
model_performance_final




## get RO

# Decile
RO_overall <- data.frame(get_RO(train_data_final,test_data_final,oot_data_final))


RO_SENP_SEP <-  data.frame(get_RO(train_data_final %>% filter(Category %in% c('SENP','SEP')),
                                  test_data_final %>% filter(Category %in% c('SENP','SEP')),
                                  oot_data_final%>% filter(Category %in% c('SENP','SEP'))))


RO_SAL <-  data.frame(get_RO(train_data_final %>% filter(Category %in% c('SAL')),
                             test_data_final %>% filter(Category %in% c('SAL')),
                             oot_data_final%>% filter(Category %in% c('SAL'))))


# Pentile
RO_overall_pentile <- data.frame(get_RO_pentile(train_data_final,test_data_final,oot_data_final))


RO_SENP_SEP_pentile <-  data.frame(get_RO_pentile(train_data_final %>% filter(Category %in% c('SENP','SEP')),
                                                  test_data_final %>% filter(Category %in% c('SENP','SEP')),
                                                  oot_data_final%>% filter(Category %in% c('SENP','SEP'))))

RO_SAL_pentile <-  data.frame(get_RO_pentile(train_data_final %>% filter(Category %in% c('SAL')),
                                             test_data_final %>% filter(Category %in% c('SAL')),
                                             oot_data_final%>% filter(Category %in% c('SAL'))))



# Scored Decile
RO_overall_scored <- data.frame(get_RO_scored(train_data_final,test_data_final,oot_data_final))


RO_SENP_SEP_scored <-  data.frame(get_RO_scored(train_data_final %>% filter(Category %in% c('SENP','SEP')),
                                                test_data_final %>% filter(Category %in% c('SENP','SEP')),
                                                oot_data_final%>% filter(Category %in% c('SENP','SEP'))))


RO_SAL_scored <-  data.frame(get_RO_scored(train_data_final %>% filter(Category %in% c('SAL')),
                                           test_data_final %>% filter(Category %in% c('SAL')),
                                           oot_data_final%>% filter(Category %in% c('SAL'))))





# Pentile scored
RO_overall_pentile_scored <- data.frame(get_RO_scored_pentile(train_data_final,test_data_final,oot_data_final))


RO_SENP_SEP_pentile_scored <-  data.frame(get_RO_scored_pentile(train_data_final %>% filter(Category %in% c('SENP','SEP')),
                                                                test_data_final %>% filter(Category %in% c('SENP','SEP')),
                                                                oot_data_final%>% filter(Category %in% c('SENP','SEP'))))

RO_SAL_pentile_scored <-  data.frame(get_RO_scored_pentile(train_data_final %>% filter(Category %in% c('SAL')),
                                                           test_data_final %>% filter(Category %in% c('SAL')),
                                                           oot_data_final%>% filter(Category %in% c('SAL'))))




############################################

output_list <- list("model_summary" = data.frame(tidy(final_model)),
                    "model_performance" = model_performance_final,
                    "Var_Imp" = get_variable_importance(final_model),
                    "VIF" = get_vif(final_model),
                    "wald_chi_sq" = get_wald_chi_sq(final_model),
                    "seed" = data.frame(selected_seed),
                    "RO_overall_D" = RO_overall,
                    "RO_SENP_SEP_D" = RO_SENP_SEP,
                    "RO_SAL_D" = RO_SAL,
                    "RO_overall_P" = RO_overall_pentile,
                    "RO_SENP_SEP_P" = RO_SENP_SEP_pentile,
                    "RO_SAL_P" = RO_SAL_pentile,
                    "RO_overall_SD" = RO_overall_scored,
                    "RO_SENP_SEP_SD" = RO_SENP_SEP_scored,
                    "RO_SAL_SD" = RO_SAL_scored,
                    "RO_overall_SP" = RO_overall_pentile_scored,
                    "RO_SENP_SEP_SP" = RO_SENP_SEP_pentile_scored,
                    "RO_SAL_SP" = RO_SAL_pentile_scored
)


# save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//Model_summary_PV_New_Combined.xlsx")
# write_xlsx(output_list, "data//output//model//PV_New//Combined//Model_summary_PV_New_Combined_v2.xlsx")

# save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//vcheck//Post_RI//Model_summary_PV_New_Combined_vf.xlsx")
save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//vcheck//Post_RI//Model_summary_PV_New_Combined_dpd_check.xlsx")


model_PV_New <- final_model
# saveRDS(model_PV, file = file.path(get_data_path()$data$model,"model_PV_New.rds"))
# saveRDS(model_PV, file = file.path("output//model_PV_New.rds"))

saveRDS(model_PV_New, file = file.path(get_data_path()$data$model,"//vcheck//model_PV_New_RI.rds"))







assert_data_non_empty(train_data_final)
save(train_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "PV_New",
       "Combined",
       "vcheck",
       "Post_RI",
       "train_data.rdata"
     )
)

assert_data_non_empty(test_data_final)
save(test_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "PV_New",
       "Combined",
       "vcheck",
       "Post_RI",
       "test_data.rdata"
     )
)

assert_data_non_empty(oot_data_final)
save(oot_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "PV_New",
       "Combined",
       "vcheck",
       "Post_RI",
       "oot_data.rdata"
     )
)



###################################


assert_data_non_empty(train_data_final)
save(train_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "PV_New",
       "Combined",
       "vcheck",
       "Post_RI",
       "dpd_check",
       "train_data.rdata"
     )
)

assert_data_non_empty(test_data_final)
save(test_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "PV_New",
       "Combined",
       "vcheck",
       "Post_RI",
       "dpd_check",
       "test_data.rdata"
     )
)

assert_data_non_empty(oot_data_final)
save(oot_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "PV_New",
       "Combined",
       "vcheck",
       "Post_RI",
       "dpd_check",
       "oot_data.rdata"
     )
)

rm(list=ls())



#########################################################################################################################################
#########################################################################################################################################
#########################################################################################################################################
#########################################################################################################################################
#########################################################################################################################################
#########################################################################################################################################
#########################################################################################################################################
############# POST RI ##################################################################################################################




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



load_rdata_intermediate("reject_data//variable_data//model_data_PV_New_A_R.rdata")
oot_date_start <- '2018-04-01'
oot_date_end <- '2018-06-30'



oot_data <-
  model_data_PV_New_A_R %>% filter((date >= as.Date(oot_date_start)) &
                                     (date <= as.Date(oot_date_end)) &
                                     (tag %in% c('A','R1','R2','R3','R4')) &
                                     (Category %in% c('SENP','SEP','SAL')))

dev_data <-
  model_data_PV_New_A_R %>% filter(UID %notin% unique(oot_data$UID) &
                                     (tag %in% c('A','R1','R2','R3','R4')) &
                                     (Category %in% c('SENP','SEP','SAL')))


rm(model_data_PV_New_A_R)



######################################################################################################################################
## 6.3. Variable transformation 


## 6.3. Variable transformation 

# 6.3.1 vintage
dev_data$Var_credit_vintage_C <- ifelse(dev_data$Var_credit_vintage <= 0.01, 0.01, 
                                        ifelse(dev_data$Var_credit_vintage >= 15, 15, dev_data$Var_credit_vintage))

oot_data$Var_credit_vintage_C <- ifelse(oot_data$Var_credit_vintage <= 0.01, 0.01, 
                                        ifelse(oot_data$Var_credit_vintage >= 15, 15, oot_data$Var_credit_vintage))

# 6.3.2 HL paid 12 month Flag

dev_data$Var_DL_HL_paid_GE_12mon_flag <-  dev_data$Var_DL_HL_paid_GE_12mon

dev_data$Var_DL_HL_paid_GE_12mon_flag[is.na(dev_data$Var_DL_HL_paid_GE_12mon_flag)] <-  0
dev_data$Var_DL_HL_paid_GE_12mon_flag <-  ifelse(dev_data$Var_DL_HL_paid_GE_12mon_flag == 0, 0, 1)

oot_data$Var_DL_HL_paid_GE_12mon_flag <-  oot_data$Var_DL_HL_paid_GE_12mon

oot_data$Var_DL_HL_paid_GE_12mon_flag[is.na(oot_data$Var_DL_HL_paid_GE_12mon_flag)] <-  0
oot_data$Var_DL_HL_paid_GE_12mon_flag <-  ifelse(oot_data$Var_DL_HL_paid_GE_12mon_flag == 0, 0, 1)


# 6.3.3 Gold paid 12 month flag

dev_data$Var_DL_Gold_paid_GE_12mon_flag_var <-  dev_data$Var_DL_Gold_paid_GE_12mon_flag
dev_data$Var_DL_Gold_paid_GE_12mon_flag_var[is.na(dev_data$Var_DL_Gold_paid_GE_12mon_flag_var)] <-  0

oot_data$Var_DL_Gold_paid_GE_12mon_flag_var <-  oot_data$Var_DL_Gold_paid_GE_12mon_flag
oot_data$Var_DL_Gold_paid_GE_12mon_flag_var[is.na(oot_data$Var_DL_Gold_paid_GE_12mon_flag_var)] <-  0

# 6.3.4 Enquiry in 6m non cc 
dev_data$Var_EN_enquiry_count_6m_non_CC_C <- dev_data$Var_EN_enquiry_count_6m_non_CC

dev_data$Var_EN_enquiry_count_6m_non_CC_C[is.na(dev_data$Var_EN_enquiry_count_6m_non_CC_C)] <- 0


dev_data$Var_EN_enquiry_count_6m_non_CC_C <- ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C <= 0, 0, 
                                                    ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C >= 4, 4,
                                                           dev_data$Var_EN_enquiry_count_6m_non_CC_C))



oot_data$Var_EN_enquiry_count_6m_non_CC_C <- oot_data$Var_EN_enquiry_count_6m_non_CC

oot_data$Var_EN_enquiry_count_6m_non_CC_C[is.na(oot_data$Var_EN_enquiry_count_6m_non_CC_C)] <- 0


oot_data$Var_EN_enquiry_count_6m_non_CC_C <- ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C <= 0, 0, 
                                                    ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C >= 4,4, oot_data$Var_EN_enquiry_count_6m_non_CC_C))




# 6.3.5 30 dpd in 6 mon flag
dev_data$Var_DL_all_30dpd_6mon_C <- dev_data$Var_DL_all_30dpd_6mon

dev_data$Var_DL_all_30dpd_6mon_C[is.na(dev_data$Var_DL_all_30dpd_6mon_C)] <-  0

dev_data$Var_DL_all_30dpd_6mon_flag <-  ifelse(dev_data$Var_DL_all_30dpd_6mon_C == 0, 0, 1)

oot_data$Var_DL_all_30dpd_6mon_C <- oot_data$Var_DL_all_30dpd_6mon

oot_data$Var_DL_all_30dpd_6mon_C[is.na(oot_data$Var_DL_all_30dpd_6mon_C)] <-  0

oot_data$Var_DL_all_30dpd_6mon_flag <-  ifelse(oot_data$Var_DL_all_30dpd_6mon_C == 0, 0, 1)

## 6.3.6 SENP SEP Flag
dev_data$category_flag_SENP_SEP <- ifelse(dev_data$Category %in% c('SENP', 'SEP'), 1, 0)
oot_data$category_flag_SENP_SEP <- ifelse(oot_data$Category %in% c('SENP', 'SEP'), 1, 0)



## 6.3.7 cross tab variable
## 2.9.1 sanctioned amount live loan
dev_data$Var_PO_closed_sanction_amount_excl_CC_C <- dev_data$Var_PO_closed_sanction_amount_excl_CC
dev_data$Var_PO_closed_sanction_amount_excl_CC_C[is.na(dev_data$Var_PO_closed_sanction_amount_excl_CC_C)] <- 0

oot_data$Var_PO_closed_sanction_amount_excl_CC_C <- oot_data$Var_PO_closed_sanction_amount_excl_CC
oot_data$Var_PO_closed_sanction_amount_excl_CC_C[is.na(oot_data$Var_PO_closed_sanction_amount_excl_CC_C)] <- 0


## 2.9.2 Outstanding amount by sanctioned amount in live loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- dev_data$Var_outstanding_by_sanctioned_amount_live_loans
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- oot_data$Var_outstanding_by_sanctioned_amount_live_loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 0.1
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 0.1



dev_data$Var_PO_closed_sanction_amount_excl_CC_bin <-  ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC <= 75000,'bin_LE_75k',
                                                              ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC > 75000 & dev_data$Var_PO_closed_sanction_amount_excl_CC <= 500000,'bin_75k_to_5L','bin_GE_5L'))

oot_data$Var_PO_closed_sanction_amount_excl_CC_bin <-  ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC <= 75000,'bin_LE_75k',
                                                              ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC > 75000 & oot_data$Var_PO_closed_sanction_amount_excl_CC <= 500000,'bin_75k_to_5L','bin_GE_5L'))



dev_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.5, 'val_LE_50_pct', 
                                                                       ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.5 & dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.75, 'val_50_to_75_pct','val_GE_75_pct'))


oot_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.5, 'val_LE_50_pct', 
                                                                       ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.5 & oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.75, 'val_50_to_75_pct','val_GE_75_pct'))





subset1 <- dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct')))
subset2 <- dev_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct'))))
subset3 <- dev_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | (Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_5L')))


woe1 <- -0.3906258
woe2 <- -0.0263269
woe3 <- 0.1947842


subset1_oot <- oot_data %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct')))
subset2_oot <- oot_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct'))))
subset3_oot <- oot_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | (Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_5L')))


dev_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed <- ifelse(dev_data$UID %in% unique(subset1$UID), woe1,
                                                                          ifelse(dev_data$UID %in% unique(subset2$UID), woe2,
                                                                                 woe3))

oot_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed <- ifelse(oot_data$UID %in% unique(subset1_oot$UID), woe1,
                                                                          ifelse(oot_data$UID %in% unique(subset2_oot$UID), woe2,
                                                                                 woe3))


rm(subset1,subset2,subset3,subset1_oot,subset2_oot,subset3_oot, woe1, woe2, woe3)







## 6.4 get list of features
shortlisted_var <- c(
  "Var_credit_vintage_C",
  "Var_DL_HL_paid_GE_12mon_flag",
  "Var_DL_Gold_paid_GE_12mon_flag_var",
  "Var_EN_enquiry_count_6m_non_CC_C",
  "Var_DL_all_30dpd_6mon_flag",
  "category_flag_SENP_SEP",
  "cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed"
)


# 6.5 subset for required columns
performance_data_dev <- dev_data %>% dplyr::select(c('deal_no','application_no','applicant_id','UID','tag',shortlisted_var,'bad_loan'))
performance_data_oot <- oot_data %>% dplyr::select(c('deal_no','application_no','applicant_id','UID','tag',shortlisted_var,'bad_loan'))

# 6.6 load model
model_PV <- readRDS(file = file.path(get_data_path()$data$model,"vcheck//model_PV_New_RI.rds"))


# 6.7 make predictions
predictions_dev <- get_predictions(model_PV, performance_data_dev)
predictions_oot <- get_predictions(model_PV, performance_data_oot)






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
save_xlsx_output(data = excel_data, relative_path = "//model//PV_New//Combined//vcheck//Post_RI//RI//1-Check - A.xlsx")


excel_data <- get_population_summary(subset_data = predictions_dev %>% filter(tag %in% c('A','R1')))
save_xlsx_output(data = excel_data, relative_path = "//model//PV_New//Combined//vcheck//Post_RI//RI//2-Check - A_R1.xlsx")

excel_data <- get_population_summary(subset_data = predictions_dev %>% filter(tag %in% c('A','R1','R2')))
save_xlsx_output(data = excel_data, relative_path = "//model//PV_New//Combined//vcheck//Post_RI//RI//3-Check - A_R1_R2.xlsx")

excel_data <- get_population_summary(subset_data = predictions_dev %>% filter(tag %in% c('A','R1','R2','R3')))
save_xlsx_output(data = excel_data, relative_path = "//model//PV_New//Combined//vcheck//Post_RI//RI//4-Check - A_R1_R2_R3.xlsx")


excel_data <- get_population_summary(subset_data = predictions_dev %>% filter(tag %in% c('A','R1','R2','R3','R4')))
save_xlsx_output(data = excel_data, relative_path = "//model//PV_New//Combined//vcheck//Post_RI//RI//5-Check - A_R1_R2_R3_R4.xlsx")


rm(list=ls())
