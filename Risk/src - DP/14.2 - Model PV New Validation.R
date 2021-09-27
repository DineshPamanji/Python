#############################################################################################
################## 14 - Model PV New Combined   #############################################
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



# 1.1 load data 

# load_rdata_output("model//PV_New//Combined//train_data.rdata")
# load_rdata_output("model//PV_New//Combined//test_data.rdata")
# load_rdata_output("model//PV_New//Combined//oot_data.rdata")


load_rdata_output("model//PV_New//Combined//vcheck//train_data.rdata")
load_rdata_output("model//PV_New//Combined//vcheck//test_data.rdata")
load_rdata_output("model//PV_New//Combined//vcheck//oot_data.rdata")





# 3. Validation 3 - Temporal Decay -----------------------------------------------------------------------------

## 3.1 load data
load_rdata_intermediate("ADS_data//temporal_decay.rdata")

## 3.2 select required data & rename columns
temporal_decay <- temporal_decay %>% filter(loan_type == 'PV-New')
colnames(temporal_decay)[colnames(temporal_decay) == 'temporal_decay_post_MOB'] <- 'bad_loan'

## 3.3 select prediction data
temporal_decay_train <- train_data_final %>% dplyr::select(deal_no, applicant_id, predictions)
temporal_decay_test <- test_data_final %>% dplyr::select(deal_no, applicant_id, predictions)
temporal_decay_oot <- oot_data_final %>% dplyr::select(deal_no, applicant_id, predictions)

## 3.4 get temporal decay flag
temporal_decay_train <- inner_join(temporal_decay_train,temporal_decay, by = 'deal_no')
temporal_decay_test <- inner_join(temporal_decay_test,temporal_decay, by = 'deal_no')
temporal_decay_oot <- inner_join(temporal_decay_oot,temporal_decay, by = 'deal_no')

## 3.5 get rank ordering
temporal_decay_RO_decile <- data.frame(get_RO(temporal_decay_train,temporal_decay_test,temporal_decay_oot))
temporal_decay_RO_pentile <- data.frame(get_RO_pentile(temporal_decay_train,temporal_decay_test,temporal_decay_oot))

temporal_decay_RO_decile_scored <- data.frame(get_RO_scored(temporal_decay_train,temporal_decay_test,temporal_decay_oot))
temporal_decay_RO_pentile_scored <- data.frame(get_RO_scored_pentile(temporal_decay_train,temporal_decay_test,temporal_decay_oot))


## 3.6 save outputs
output_list <- list("temporal_decay_RO_D" = temporal_decay_RO_decile,
                    "temporal_decay_RO_P" = temporal_decay_RO_pentile,
                    "temporal_decay_RO_SD" = temporal_decay_RO_decile_scored,
                    "temporal_decay_RO_SP" = temporal_decay_RO_pentile_scored
                    
)


# save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//Model_validation_PV_New_3-Temporal decay.xlsx")
# write_xlsx(output_list, "data//output//model//PV_New//Combined//Model_validation_PV_New_3-Temporal decay.xlsx")

save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//vcheck//Model_validation_PV_New_1-Temporal decay.xlsx")

rm(temporal_decay_train,temporal_decay_test,temporal_decay_oot,
   temporal_decay_RO_decile,temporal_decay_RO_pentile,
   temporal_decay_RO_decile_scored,temporal_decay_RO_pentile_scored,temporal_decay,output_list)





## 4. Validation 4 - Intermediary validation ---------------------------------------------------------------------

## 4.1 load data
load_rdata_intermediate("ADS_data//intermediary_I1.rdata")

## 4.2 select required data & rename columns
intermediary_I1 <- intermediary_I1 %>% filter(loan_type == 'PV-New')
colnames(intermediary_I1)[colnames(intermediary_I1) == 'never_60DPD_but_90DPD_post_MOB'] <- 'bad_loan'


## 4.3 select prediction data
I1_train <- train_data_final %>% dplyr::select(deal_no, applicant_id, predictions)
I1_test <- test_data_final %>% dplyr::select(deal_no, applicant_id, predictions)
I1_oot <- oot_data_final %>% dplyr::select(deal_no, applicant_id, predictions)

## 4.4 get intermediary flag
I1_train <- inner_join(I1_train,intermediary_I1, by = 'deal_no')
I1_test <- inner_join(I1_test,intermediary_I1, by = 'deal_no')
I1_oot <- inner_join(I1_oot,intermediary_I1, by = 'deal_no')


## 4.5 get rank ordering
I1_RO_decile <- data.frame(get_RO(I1_train,I1_test,I1_oot))
I1_RO_pentile <- data.frame(get_RO_pentile(I1_train,I1_test,I1_oot))

I1_RO_decile_scored <- data.frame(get_RO_scored(I1_train,I1_test,I1_oot))
I1_RO_pentile_scored <- data.frame(get_RO_scored_pentile(I1_train,I1_test,I1_oot))



output_list <- list("I1_RO_D" = I1_RO_decile,
                    "I1_RO_P" = I1_RO_pentile,
                    "I1_RO_SD" = I1_RO_decile_scored,
                    "I1_RO_SP" = I1_RO_pentile_scored
)


# save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//Model_validation_PV_New_4-Intermediary.xlsx")
# write_xlsx(output_list,"data//output//model//PV_New//Combined//Model_validation_PV_New_4-Intermediary.xlsx" )

save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//vcheck//Model_validation_PV_New_2-Intermediary.xlsx")

rm(I1_RO_decile,I1_RO_pentile,I1_RO_decile_scored,I1_RO_pentile_scored,intermediary_I1,I1_oot,I1_test,I1_train, output_list)

# rm(oot_data_final,test_data_final,train_data_final)




## 5. Get RO with ever 90dpd & ever 150dpd ----------------------------------------------------------------------------------------

## 5.1 subset data for ever 90 dpd
ever_train <- train_data_final %>% dplyr::select(predictions,ever_90dpd)
ever_test <- test_data_final %>% dplyr::select(predictions,ever_90dpd)
ever_oot <- oot_data_final %>% dplyr::select(predictions,ever_90dpd)


## 5.2 rename columns - 90 dpd
colnames(ever_train) <- c('predictions','bad_loan')
colnames(ever_test) <- c('predictions','bad_loan')
colnames(ever_oot) <- c('predictions','bad_loan')


## 5.3 get rank ordering - 90 dpd
ever_90dpd_RO_decile <- data.frame(get_RO(ever_train,ever_test,ever_oot))
ever_90dpd_RO_pentile <- data.frame(get_RO_pentile(ever_train,ever_test,ever_oot))

ever_90dpd_RO_decile_scored <- data.frame(get_RO_scored(ever_train,ever_test,ever_oot))
ever_90dpd_RO_pentile_scored <- data.frame(get_RO_scored_pentile(ever_train,ever_test,ever_oot))



## 5.4 subset data for ever 150 dpd
ever_train <- train_data_final %>% dplyr::select(predictions,ever_150dpd)
ever_test <- test_data_final %>% dplyr::select(predictions,ever_150dpd)
ever_oot <- oot_data_final %>% dplyr::select(predictions,ever_150dpd)


## 5.5 rename columns - 150 dpd
colnames(ever_train) <- c('predictions','bad_loan')
colnames(ever_test) <- c('predictions','bad_loan')
colnames(ever_oot) <- c('predictions','bad_loan')


## 5.6 get rank ordering - 150 dpd
ever_150dpd_RO_decile <- data.frame(get_RO(ever_train,ever_test,ever_oot))
ever_150dpd_RO_pentile <- data.frame(get_RO_pentile(ever_train,ever_test,ever_oot))

ever_150dpd_RO_decile_scored <- data.frame(get_RO_scored(ever_train,ever_test,ever_oot))
ever_150dpd_RO_pentile_scored <- data.frame(get_RO_scored_pentile(ever_train,ever_test,ever_oot))


## 5.7 save output
output_list <- list("90dpd_RO_D" = ever_90dpd_RO_decile,
                    "90dpd_RO_P" = ever_90dpd_RO_pentile,
                    "90dpd_RO_SD" = ever_90dpd_RO_decile_scored,
                    "90dpd_RO_SP" = ever_90dpd_RO_pentile_scored,
                    "150dpd_RO_D" = ever_150dpd_RO_decile,
                    "150dpd_RO_P" = ever_150dpd_RO_pentile,
                    "150dpd_RO_SD" = ever_150dpd_RO_decile_scored,
                    "150dpd_RO_SP" = ever_150dpd_RO_pentile_scored
)


# save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//Model_validation_PV_New_5-Ever 90 150 DPD.xlsx")
# write_xlsx(output_list, "data//output//model//PV_New//Combined//Model_validation_PV_New_5-Ever 90 150 DPD.xlsx")

save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//vcheck//Model_validation_PV_New_3-Ever 90 150 DPD.xlsx")


rm(output_list,ever_150dpd_RO_decile,ever_150dpd_RO_decile_scored,ever_150dpd_RO_pentile,ever_150dpd_RO_pentile_scored,
   ever_90dpd_RO_decile,ever_90dpd_RO_decile_scored,ever_90dpd_RO_pentile,ever_90dpd_RO_pentile_scored,ever_oot,ever_test,
   ever_train,train_data_final,test_data_final,oot_data_final)



## 6. Validation 6 - Out of time & out of sample validation ---------------------------------------------------------------------

## 6.1 Load PV New data 
load_rdata_intermediate("model_data//model_data_PV_New_validation.rdata")


## 6.2 load validation datasets
load_rdata_intermediate("ADS_data//validation_60dpd_12mob.rdata")
load_rdata_intermediate("ADS_data//validation_60dpd_9mob.rdata")
load_rdata_intermediate("ADS_data//validation_60dpd_oct_to_dec_2020.rdata")



## 6.3. Variable transformation 

# 6.3.1 vintage
model_data_PV_New$Var_credit_vintage_C <- ifelse(model_data_PV_New$Var_credit_vintage <= 0.01, 0.01, 
                                                 ifelse(model_data_PV_New$Var_credit_vintage >= 15, 15, model_data_PV_New$Var_credit_vintage))


# 6.3.2 HL paid 12 month Flag

model_data_PV_New$Var_DL_HL_paid_GE_12mon_flag <-  model_data_PV_New$Var_DL_HL_paid_GE_12mon

model_data_PV_New$Var_DL_HL_paid_GE_12mon_flag[is.na(model_data_PV_New$Var_DL_HL_paid_GE_12mon_flag)] <-  0
model_data_PV_New$Var_DL_HL_paid_GE_12mon_flag <-  ifelse(model_data_PV_New$Var_DL_HL_paid_GE_12mon_flag == 0, 0, 1)


# 6.3.3 Gold paid 12 month flag## 5.4 subset data for ever 150 dpd

model_data_PV_New$Var_DL_Gold_paid_GE_12mon_flag_var <-  model_data_PV_New$Var_DL_Gold_paid_GE_12mon_flag

model_data_PV_New$Var_DL_Gold_paid_GE_12mon_flag_var[is.na(model_data_PV_New$Var_DL_Gold_paid_GE_12mon_flag_var)] <-  0


# 6.3.4 Enquiry in 6m non cc 
model_data_PV_New$Var_EN_enquiry_count_6m_non_CC_C <- model_data_PV_New$Var_EN_enquiry_count_6m_non_CC

model_data_PV_New$Var_EN_enquiry_count_6m_non_CC_C[is.na(model_data_PV_New$Var_EN_enquiry_count_6m_non_CC_C)] <- 0


model_data_PV_New$Var_EN_enquiry_count_6m_non_CC_C <- ifelse(model_data_PV_New$Var_EN_enquiry_count_6m_non_CC_C <= 0, 0, 
                                                    ifelse(model_data_PV_New$Var_EN_enquiry_count_6m_non_CC_C >= 4, 4, model_data_PV_New$Var_EN_enquiry_count_6m_non_CC_C))



# 6.3.5 30 dpd in 6 mon flag
model_data_PV_New$Var_DL_all_30dpd_6mon_C <- model_data_PV_New$Var_DL_all_30dpd_6mon

model_data_PV_New$Var_DL_all_30dpd_6mon_C[is.na(model_data_PV_New$Var_DL_all_30dpd_6mon_C)] <-  0

model_data_PV_New$Var_DL_all_30dpd_6mon_flag <-  ifelse(model_data_PV_New$Var_DL_all_30dpd_6mon_C == 0, 0, 1)


## 6.3.6 SENP SEP Flag
model_data_PV_New$category_flag_SENP_SEP <- ifelse(model_data_PV_New$Category %in% c('SENP', 'SEP'), 1, 0)


## 6.3.7 cross tab variable
## 2.9.1 sanctioned amount live loan
model_data_PV_New$Var_PO_closed_sanction_amount_excl_CC_C <- model_data_PV_New$Var_PO_closed_sanction_amount_excl_CC


model_data_PV_New$Var_PO_closed_sanction_amount_excl_CC_C[is.na(model_data_PV_New$Var_PO_closed_sanction_amount_excl_CC_C)] <- 0



## 2.9.2 Outstanding amount by sanctioned amount in live loans

model_data_PV_New$Var_outstanding_by_sanctioned_amount_live_loans_C <- model_data_PV_New$Var_outstanding_by_sanctioned_amount_live_loans

model_data_PV_New$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(model_data_PV_New$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 0.1

model_data_PV_New$Var_PO_closed_sanction_amount_excl_CC_bin <-  ifelse(model_data_PV_New$Var_PO_closed_sanction_amount_excl_CC_C <= 75000,'bin_LE_75k',
                                                              ifelse(model_data_PV_New$Var_PO_closed_sanction_amount_excl_CC_C > 50000 & model_data_PV_New$Var_PO_closed_sanction_amount_excl_CC_C <= 500000,'bin_75k_to_5L','bin_GE_5L'))


model_data_PV_New$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(model_data_PV_New$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.5, 'val_LE_50_pct', 
                                                                       ifelse(model_data_PV_New$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.5 & model_data_PV_New$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.75, 'val_50_to_75_pct','val_GE_75_pct'))



subset1 <- model_data_PV_New %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct')))
subset2 <- model_data_PV_New %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_75k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct'))))
subset3 <- model_data_PV_New %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_75k_to_5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_50_pct','val_50_to_75_pct'))) | (Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_5L')))

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)

woe1 <- -0.3941659
woe2 <- -0.03581256
woe3 <- 0.2070746


model_data_PV_New$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed <- ifelse(model_data_PV_New$deal_no %in% unique(subset1$deal_no), woe1,
                                                                        ifelse(model_data_PV_New$deal_no %in% unique(subset2$deal_no), woe2,
                                                                               woe3))


rm(subset1,subset2,subset3, woe1, woe2, woe3)





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
performance_data <- model_data_PV_New %>% dplyr::select(c('deal_no','applicant_id',shortlisted_var,'bad_loan'))


# 6.6 load model
# model_PV <- readRDS(file = file.path(get_data_path()$data$model,"model_PV_New.rds"))

model_PV <- readRDS(file = file.path(get_data_path()$data$model,"vcheck//model_PV_New.rds"))


# 6.7 make predictions
performance_data <- get_predictions(model_PV, performance_data)


# 6.8 get rank ordering for 60 dpd in 12mob
RO_60dpd_12mob_D <-  data.frame(get_RO_validation_decile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_12mob$deal_no))))
RO_60dpd_12mob_P <-  data.frame(get_RO_validation_pentile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_12mob$deal_no))))


# 6.9 get rank ordering for 60 dpd in 9mob
RO_60dpd_9mob_D <-  data.frame(get_RO_validation_decile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_9mob$deal_no))))
RO_60dpd_9mob_P <-  data.frame(get_RO_validation_pentile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_9mob$deal_no))))


# 6.10 get rank ordering for 60 dod in oct-dec 2020
RO_60dpd_oct_to_dec_2020_D <-  data.frame(get_RO_validation_decile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_oct_to_dec_2020$deal_no))))
RO_60dpd_oct_to_dec_2020_P <-  data.frame(get_RO_validation_pentile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_oct_to_dec_2020$deal_no))))


output_list <- list("RO_60dpd_12mob_D" = RO_60dpd_12mob_D,
                    "RO_60dpd_12mob_P" = RO_60dpd_12mob_P,
                    "RO_60dpd_9mob_D" = RO_60dpd_9mob_D,
                    "RO_60dpd_9mob_P" = RO_60dpd_9mob_P,
                    "RO_60dpd_oct_to_dec_2020_D" = RO_60dpd_oct_to_dec_2020_D,
                    "RO_60dpd_oct_to_dec_2020_P" = RO_60dpd_oct_to_dec_2020_P
)


# save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//Model_validation_PV_New_6-OOT.xlsx")
# write_xlsx(output_list, "data//output//model//PV_New//Combined//Model_validation_PV_New_6-OOT.xlsx")

save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//vcheck//Model_validation_PV_New_4-OOT.xlsx")

rm(list=ls())

####################################################



load_rdata_output("model//PV_New//Combined//vcheck//train_data.rdata")
load_rdata_output("model//PV_New//Combined//vcheck//test_data.rdata")


dev_data <- rbind(train_data_final,test_data_final)

dev_data$profile <- ifelse(dev_data$category_flag_SENP_SEP == 0, "SAL",
                           ifelse(dev_data$category_flag_SENP_SEP == 1 & dev_data$agri_profile_flag == 0, "SENP_Non_Agri","SENP_Agri"))



dev_data$pd_decile <- ntile(dev_data$predictions,10)



output <- dev_data %>% group_by(pd_decile,profile) %>% summarise(population = n(),
                                                                 delinquency = mean(bad_loan))



output_list <- list("profile_analysis" = output
)


save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//vcheck//Model_validation_PV_New_5-Applicant Profile analysis.xlsx")
