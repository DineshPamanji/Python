#############################################################################################
################## 15 - Model PV Used Validation   ##########################################
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



## Load data 
# 
# load_rdata_output("model//PV_Used//Combined//train_data.rdata")
# load_rdata_output("model//PV_Used//Combined//test_data.rdata")
# load_rdata_output("model//PV_Used//Combined//oot_data.rdata")

load_rdata_output("model//PV_Used//Combined//vcheck//train_data.rdata")
load_rdata_output("model//PV_Used//Combined//vcheck//test_data.rdata")
load_rdata_output("model//PV_Used//Combined//vcheck//oot_data.rdata")


## 1. Validation 1 - Temporal Decay -----------------------------------------------------------------------------

## 1.1 load data
load_rdata_intermediate("ADS_data//temporal_decay.rdata")

## 1.2 select required data & rename columns
temporal_decay <- temporal_decay %>% filter(loan_type == 'PV-Used')
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


temporal_decay_RO_3bin <- data.frame(get_RO_3bin(temporal_decay_train,temporal_decay_test,temporal_decay_oot))
temporal_decay_RO_3bin_scored <- data.frame(get_RO_scored_3bin(temporal_decay_train,temporal_decay_test,temporal_decay_oot))


## 3.6 save outputs
output_list <- list("temporal_decay_RO_D" = temporal_decay_RO_decile,
                    "temporal_decay_RO_P" = temporal_decay_RO_pentile,
                    "temporal_decay_RO_SD" = temporal_decay_RO_decile_scored,
                    "temporal_decay_RO_SP" = temporal_decay_RO_pentile_scored,
                    "temportal_decay_RO_3bin" = temporal_decay_RO_3bin,
                    "temportal_decay_RO_3bin_S" = temporal_decay_RO_3bin_scored
                    
)


write_xlsx(output_list, "data//output//model//PV_Used/Combined//vcheck//Model_validation_PV_Used_1-Temporal decay.xlsx")


rm(temporal_decay_train,temporal_decay_test,temporal_decay_oot,
   temporal_decay_RO_decile,temporal_decay_RO_pentile,
   temporal_decay_RO_decile_scored,temporal_decay_RO_pentile_scored,temporal_decay,output_list,
   temporal_decay_RO_3bin,temporal_decay_RO_3bin_scored)





## 2. Validation 2 - Intermediary validation ---------------------------------------------------------------------

## 2.1 load data
load_rdata_intermediate("ADS_data//intermediary_I1.rdata")

## 2.2 select required data & rename columns
intermediary_I1 <- intermediary_I1 %>% filter(loan_type == 'PV-Used')
colnames(intermediary_I1)[colnames(intermediary_I1) == 'never_60DPD_but_90DPD_post_MOB'] <- 'bad_loan'


## 2.3 select prediction data
I1_train <- train_data_final %>% dplyr::select(deal_no, applicant_id, predictions)
I1_test <- test_data_final %>% dplyr::select(deal_no, applicant_id, predictions)
I1_oot <- oot_data_final %>% dplyr::select(deal_no, applicant_id, predictions)

## 2.4 get intermediary flag
I1_train <- inner_join(I1_train,intermediary_I1, by = 'deal_no')
I1_test <- inner_join(I1_test,intermediary_I1, by = 'deal_no')
I1_oot <- inner_join(I1_oot,intermediary_I1, by = 'deal_no')


## 2.5 get rank ordering
I1_RO_decile <- data.frame(get_RO(I1_train,I1_test,I1_oot))
I1_RO_pentile <- data.frame(get_RO_pentile(I1_train,I1_test,I1_oot))

I1_RO_decile_scored <- data.frame(get_RO_scored(I1_train,I1_test,I1_oot))
I1_RO_pentile_scored <- data.frame(get_RO_scored_pentile(I1_train,I1_test,I1_oot))



output_list <- list("I1_RO_D" = I1_RO_decile,
                    "I1_RO_P" = I1_RO_pentile,
                    "I1_RO_SD" = I1_RO_decile_scored,
                    "I1_RO_SP" = I1_RO_pentile_scored
)


write_xlsx(output_list, "data//output//model//PV_Used//Combined//vcheck//Model_validation_PV_Used_2-Intermediary.xlsx")

rm(I1_RO_decile,I1_RO_pentile,I1_RO_decile_scored,I1_RO_pentile_scored,intermediary_I1,I1_oot,I1_test,I1_train, output_list)





## 3. Get RO with ever 90dpd & ever 150dpd ----------------------------------------------------------------------------------------

## 3.1 subset data for ever 90 dpd
ever_train <- train_data_final %>% dplyr::select(predictions,ever_90dpd)
ever_test <- test_data_final %>% dplyr::select(predictions,ever_90dpd)
ever_oot <- oot_data_final %>% dplyr::select(predictions,ever_90dpd)


## 3.2 rename columns - 90 dpd
colnames(ever_train) <- c('predictions','bad_loan')
colnames(ever_test) <- c('predictions','bad_loan')
colnames(ever_oot) <- c('predictions','bad_loan')


## 3.3 get rank ordering - 90 dpd
ever_90dpd_RO_decile <- data.frame(get_RO(ever_train,ever_test,ever_oot))
ever_90dpd_RO_pentile <- data.frame(get_RO_pentile(ever_train,ever_test,ever_oot))

ever_90dpd_RO_decile_scored <- data.frame(get_RO_scored(ever_train,ever_test,ever_oot))
ever_90dpd_RO_pentile_scored <- data.frame(get_RO_scored_pentile(ever_train,ever_test,ever_oot))

ever_90dpd_RO_3bin <- data.frame(get_RO_3bin(ever_train,ever_test,ever_oot))
ever_90dpd_RO_3bin_scored <- data.frame(get_RO_scored_3bin(ever_train,ever_test,ever_oot))


## 3.4 subset data for ever 150 dpd
ever_train <- train_data_final %>% dplyr::select(predictions,ever_150dpd)
ever_test <- test_data_final %>% dplyr::select(predictions,ever_150dpd)
ever_oot <- oot_data_final %>% dplyr::select(predictions,ever_150dpd)


## 3.5 rename columns - 150 dpd
colnames(ever_train) <- c('predictions','bad_loan')
colnames(ever_test) <- c('predictions','bad_loan')
colnames(ever_oot) <- c('predictions','bad_loan')


## 3.6 get rank ordering - 150 dpd
ever_150dpd_RO_decile <- data.frame(get_RO(ever_train,ever_test,ever_oot))
ever_150dpd_RO_pentile <- data.frame(get_RO_pentile(ever_train,ever_test,ever_oot))

ever_150dpd_RO_decile_scored <- data.frame(get_RO_scored(ever_train,ever_test,ever_oot))
ever_150dpd_RO_pentile_scored <- data.frame(get_RO_scored_pentile(ever_train,ever_test,ever_oot))


## 3.7 save output
output_list <- list("90dpd_RO_D" = ever_90dpd_RO_decile,
                    "90dpd_RO_P" = ever_90dpd_RO_pentile,
                    "90dpd_RO_SD" = ever_90dpd_RO_decile_scored,
                    "90dpd_RO_SP" = ever_90dpd_RO_pentile_scored,
                    "150dpd_RO_D" = ever_150dpd_RO_decile,
                    "150dpd_RO_P" = ever_150dpd_RO_pentile,
                    "150dpd_RO_SD" = ever_150dpd_RO_decile_scored,
                    "150dpd_RO_SP" = ever_150dpd_RO_pentile_scored
)


write_xlsx(output_list, "data//output//model//PV_Used//Combined//vcheck//Model_validation_PV_Used_3-Ever 90 150 DPD.xlsx")


rm(output_list,ever_150dpd_RO_decile,ever_150dpd_RO_decile_scored,ever_150dpd_RO_pentile,ever_150dpd_RO_pentile_scored,
   ever_90dpd_RO_decile,ever_90dpd_RO_decile_scored,ever_90dpd_RO_pentile,ever_90dpd_RO_pentile_scored,ever_oot,ever_test,
   ever_train,train_data_final,test_data_final,oot_data_final,ever_90dpd_RO_3bin,ever_90dpd_RO_3bin_scored)


## 5. Validation 5 - Out of time & out of sample validation ---------------------------------------------------------------------

## 5.1 Load data 
load_rdata_intermediate("model_data//model_data_PV_Used_validation.rdata")


## 5.2 load validation datasets
load_rdata_intermediate("ADS_data//validation_60dpd_12mob.rdata")
load_rdata_intermediate("ADS_data//validation_60dpd_9mob.rdata")
load_rdata_intermediate("ADS_data//validation_60dpd_oct_to_dec_2020.rdata")



## 5.3. Variable transformation 

# credit vintage
model_data_PV_Used$Var_credit_vintage_C <- ifelse(model_data_PV_Used$Var_credit_vintage <= 0.01, 0.01, 
                                                  ifelse(model_data_PV_Used$Var_credit_vintage >= 12, 12, model_data_PV_Used$Var_credit_vintage))



### Unsecured Enquiry count 12m non CC
model_data_PV_Used$Var_EN_unsec_enquiry_count_12m_non_CC_C <- model_data_PV_Used$Var_EN_unsec_enquiry_count_12m_non_CC
model_data_PV_Used$Var_EN_unsec_enquiry_count_12m_non_CC_C[is.na(model_data_PV_Used$Var_EN_unsec_enquiry_count_12m_non_CC_C)] <- 0

woe1 <- 0.1107157
woe2 <- -0.1002811
woe3 <- -0.654442
woe4 <- -0.8558241


model_data_PV_Used$Var_EN_unsec_enquiry_count_12m_non_CC_woe <- ifelse(model_data_PV_Used$Var_EN_unsec_enquiry_count_12m_non_CC_C == 0, woe1, 
                                                                       ifelse(model_data_PV_Used$Var_EN_unsec_enquiry_count_12m_non_CC_C %in% c(1,2), woe2,
                                                                              ifelse(model_data_PV_Used$Var_EN_unsec_enquiry_count_12m_non_CC_C == 3, woe3,woe4)))
rm(woe1,woe2,woe3,woe4,woe5)



### SENP_SEP Flag
model_data_PV_Used$category_flag_SENP_SEP <- ifelse(model_data_PV_Used$Category %in% c('SENP', 'SEP'), 1, 0)





## 2.9 Cross tab - outstanding by sanctioned amount loan  x total sanctioned amount closed

## 2.9.1 sanctioned amount closed loan
model_data_PV_Used$Var_PO_closed_sanction_amount_excl_CC[is.na(model_data_PV_Used$Var_PO_closed_sanction_amount_excl_CC)] <- 0

## 2.9.2 Outstanding amount by sanctioned amount in live loans

model_data_PV_Used$Var_outstanding_by_sanctioned_amount_live_loans_C <- model_data_PV_Used$Var_outstanding_by_sanctioned_amount_live_loans
model_data_PV_Used$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(model_data_PV_Used$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 0.7


## 2.9.3 cross tab
model_data_PV_Used$Var_PO_closed_sanction_amount_excl_CC_bin <-  ifelse(model_data_PV_Used$Var_PO_closed_sanction_amount_excl_CC <= 100000,'bin_LE_1L',
                                                                        ifelse(model_data_PV_Used$Var_PO_closed_sanction_amount_excl_CC > 100000 & model_data_PV_Used$Var_PO_closed_sanction_amount_excl_CC <= 350000,'bin_1L_to_3.5L','bin_GE_3.5L'))


model_data_PV_Used$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(model_data_PV_Used$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.25, 'val_LE_25_pct', 
                                                                                 ifelse(model_data_PV_Used$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.25 & model_data_PV_Used$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.75, 'val_25_to_75_pct','val_GE_75_pct'))

subset1 <- model_data_PV_Used %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_1L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct')))
subset2 <- model_data_PV_Used %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_1L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_75_pct'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_1L_to_3.5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_75_pct','val_GE_75_pct'))))
subset3 <- model_data_PV_Used %>% filter( (Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_3.5L')) | (Var_outstanding_by_sanctioned_amount_live_loans_C %in% c('val_LE_25_pct')) | ( (Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_1L_to_3.5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct'))   ))



woe1 <- -0.2972441
woe2 <- -0.06637368
woe3 <- 0.2122154

model_data_PV_Used$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed_woe <- ifelse(model_data_PV_Used$deal_no %in% unique(subset1$deal_no), woe1,
                                                                                        ifelse(model_data_PV_Used$deal_no %in% unique(subset2$deal_no), woe2,
                                                                                               woe3))

rm(subset1,subset2,subset3,woe1, woe2, woe3)



## 60 DPD in 3 mon
model_data_PV_Used$Var_DL_all_60dpd_3mon_C <- model_data_PV_Used$Var_DL_all_60dpd_3mon
model_data_PV_Used$Var_DL_all_60dpd_3mon_C[is.na(model_data_PV_Used$Var_DL_all_60dpd_3mon_C)] <- 0

model_data_PV_Used$Var_DL_all_60dpd_3mon_C <- ifelse(model_data_PV_Used$Var_DL_all_60dpd_3mon_C <= 0, 0, 
                                                     ifelse(model_data_PV_Used$Var_DL_all_60dpd_3mon_C >= 2, 2, model_data_PV_Used$Var_DL_all_60dpd_3mon_C))


###

shortlisted_var <- c(
   "Var_credit_vintage_C",
   "cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed_woe",
   "category_flag_SENP_SEP",
   "Var_EN_unsec_enquiry_count_12m_non_CC_woe",
   "Var_DL_all_60dpd_3mon_C")



# 5.5 subset for required columns
performance_data <- model_data_PV_Used %>% dplyr::select(c('deal_no','applicant_id',shortlisted_var,'bad_loan'))


# 5.6 load model
model_PV_Used <- readRDS(file = file.path(get_data_path()$data$model,"vcheck//model_PV_Used.rds"))



# 5.7 make predictions
performance_data <- get_predictions(model_PV_Used, performance_data)


# 5.8 get rank ordering for 60 dpd in 12mob
RO_60dpd_12mob_D <-  data.frame(get_RO_validation_decile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_12mob$deal_no))))
RO_60dpd_12mob_P <-  data.frame(get_RO_validation_pentile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_12mob$deal_no))))


# 5.9 get rank ordering for 60 dpd in 9mob
RO_60dpd_9mob_D <-  data.frame(get_RO_validation_decile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_9mob$deal_no))))
RO_60dpd_9mob_P <-  data.frame(get_RO_validation_pentile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_9mob$deal_no))))


# 5.10 get rank ordering for 60 dod in oct-dec 2020
RO_60dpd_oct_to_dec_2020_D <-  data.frame(get_RO_validation_decile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_oct_to_dec_2020$deal_no))))
RO_60dpd_oct_to_dec_2020_P <-  data.frame(get_RO_validation_pentile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_oct_to_dec_2020$deal_no))))


output_list <- list("RO_60dpd_12mob_D" = RO_60dpd_12mob_D,
                    "RO_60dpd_12mob_P" = RO_60dpd_12mob_P,
                    "RO_60dpd_9mob_D" = RO_60dpd_9mob_D,
                    "RO_60dpd_9mob_P" = RO_60dpd_9mob_P,
                    "RO_60dpd_oct_to_dec_2020_D" = RO_60dpd_oct_to_dec_2020_D,
                    "RO_60dpd_oct_to_dec_2020_P" = RO_60dpd_oct_to_dec_2020_P
)


write_xlsx(output_list, "data//output//model//PV_Used//Combined//vcheck//Model_validation_PV_Used_4-OOT.xlsx")




rm(list=ls())


###################################################################################################################################


## 7. Validation 7 -  CIBIL v3 cross tab   ---------------------------------------------------------------------



## 1. load train, test & OOT data
load_rdata_output("model//PV_Used//Combined//vcheck//train_data.rdata")
load_rdata_output("model//PV_Used//Combined//vcheck//test_data.rdata")
load_rdata_output("model//PV_Used//Combined//vcheck//oot_data.rdata")



## 2. load cibil data
# load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
# cibil_v3 <- distinct(cibil_v3 %>% dplyr::select(deal_no,customer_code,cibil_score_v3))


## 3. get cibil v3 scores by joining with data
# train_data_final <- left_join(train_data_final,cibil_v3,by=c('deal_no','customer_code'))
# test_data_final <- left_join(test_data_final,cibil_v3,by=c('deal_no','customer_code'))
# oot_data_final <- left_join(oot_data_final,cibil_v3,by=c('deal_no','customer_code'))


# View(train_data_final %>% group_by(cibil_score_v3) %>% summarise(n=n()))
# View(test_data_final %>% group_by(cibil_score_v3) %>% summarise(n=n()))
# View(oot_data_final %>% group_by(cibil_score_v3) %>% summarise(n=n()))



## 4. remove NA & -1 cibil scores
train_data <- train_data_final %>% filter(!is.na(cibil_score_v3) & cibil_score_v3 != -1)
test_data <- test_data_final %>% filter(!is.na(cibil_score_v3) & cibil_score_v3 != -1)
oot_data <- oot_data_final %>% filter(!is.na(cibil_score_v3) & cibil_score_v3 != -1)


## 5. combine train + test
dev_data <- rbind(train_data,test_data)


## 6. create cibil score pentiles & PD pentiles
dev_data$pd_pentile <- ntile(dev_data$predictions,5)

dev_data_p5 <- dev_data %>% filter(pd_pentile == 5)
dev_data_p5$pd_pentile <- ntile(dev_data_p5$predictions,2)
dev_data_p5$pd_pentile <- ifelse(dev_data_p5$pd_pentile == 1, 5.1, 5.2)

dev_data <- rbind(dev_data %>% filter(pd_pentile <= 4), dev_data_p5)

# pd_scores <- dev_data %>% group_by(pd_pentile) %>% summarise(max_pd = max(predictions),
#                                                              min_pd = min(predictions),
#                                                              n = n())

rm(dev_data_p5,train_data_final,test_data_final,train_data,test_data)

# fwrite(pd_scores, "pentiles_PV_Used.csv")
# rm(oot_data)


# dev_data$cibil_pentile <- ntile(dev_data$cibil_score_v3,5)


dev_data$cibil_pentile <- ifelse(dev_data$cibil_score_v3 <=675 , "Cibil <= 675",
                                 ifelse(dev_data$cibil_score_v3 > 675 & dev_data$cibil_score_v3 <=700 , "Cibil 676 - 700",
                                        ifelse(dev_data$cibil_score_v3 > 700 & dev_data$cibil_score_v3 <=730 , "Cibil 701 - 730",
                                               ifelse(dev_data$cibil_score_v3 > 730 & dev_data$cibil_score_v3 <=750 , "Cibil 731 - 750",
                                                      ifelse(dev_data$cibil_score_v3 > 750 & dev_data$cibil_score_v3 <= 780 , "Cibil 751 - 780",
                                                             ifelse(dev_data$cibil_score_v3 > 780 , "Cibil >= 781",
                                                                    "Invalid"))))))


cibil_crosstab <- dev_data %>% group_by(pd_pentile,cibil_pentile) %>% summarise(cibil_min = min(cibil_score_v3),
                                                                                cibil_max = max(cibil_score_v3),
                                                                                applicant_count = n(),
                                                                                defaults_bad_loan = sum(bad_loan),
                                                                                defaults_90dpd = sum(ever_90dpd),
                                                                                defaults_150dpd = sum(ever_150dpd),
                                                                                def_pct_bad_loan = mean(bad_loan),
                                                                                def_pct_90dpd = mean(ever_90dpd),
                                                                                def_pct_150dpd = mean(ever_150dpd)
                                                                                
)


pd_pentile_delinquency <- dev_data %>% group_by(pd_pentile) %>% summarise(cibil_min = min(cibil_score_v3),
                                                                          cibil_max = max(cibil_score_v3),
                                                                          applicant_count = n(),
                                                                          defaults_bad_loan = sum(bad_loan),
                                                                          defaults_90dpd = sum(ever_90dpd),
                                                                          defaults_150dpd = sum(ever_150dpd),
                                                                          def_pct_bad_loan = mean(bad_loan),
                                                                          def_pct_90dpd = mean(ever_90dpd),
                                                                          def_pct_150dpd = mean(ever_150dpd))



cibil_pentile_delinquency <- dev_data %>% group_by(cibil_pentile) %>% summarise(cibil_min = min(cibil_score_v3),
                                                                                cibil_max = max(cibil_score_v3),
                                                                                applicant_count = n(),
                                                                                defaults_bad_loan = sum(bad_loan),
                                                                                defaults_90dpd = sum(ever_90dpd),
                                                                                defaults_150dpd = sum(ever_150dpd),
                                                                                def_pct_bad_loan = mean(bad_loan),
                                                                                def_pct_90dpd = mean(ever_90dpd),
                                                                                def_pct_150dpd = mean(ever_150dpd))





pentile_limits <- dev_data %>% group_by(pd_pentile) %>% summarise(min_score = min(predictions),
                                                                  max_score = max(predictions))





output_list <- list("cibil_crosstab" = cibil_crosstab,
                    "pd_pentile_delinquency" = pd_pentile_delinquency,
                    "cibil_pentile_delinquency" = cibil_pentile_delinquency,
                    "pentile_limits" = pentile_limits
)



write_xlsx(output_list, "data//output//model//PV_Used//Combined//vcheck//Model_validation_PV_Used_5-cibil crosstab.xlsx")




# rm(list=ls())

### OOT Data Cibil crosstab


## 6. create cibil score pentiles & PD pentiles
# oot_data$pd_pentile <- ntile(oot_data$predictions,5)
# 
# oot_data_p5 <- oot_data %>% filter(pd_pentile == 5)
# oot_data_p5$pd_pentile <- ntile(oot_data_p5$predictions,2)
# oot_data_p5$pd_pentile <- ifelse(oot_data_p5$pd_pentile == 1, 5.1, 5.2)
# 
# oot_data <- rbind(oot_data %>% filter(pd_pentile <= 4), oot_data_p5)


# rm(oot_data_p5,train_data_final,test_data_final,train_data,test_data)
# rm(oot_data)


# oot_data$cibil_pentile <- ntile(oot_data$cibil_score_v3,5)


oot_data$pd_pentile <- ifelse(oot_data$predictions < 0.0468, 1,
                              ifelse(oot_data$predictions >= 0.0468 & oot_data$predictions < 0.0577, 2,
                                     ifelse(oot_data$predictions >= 0.0577 & oot_data$predictions < 0.0739, 3,
                                            ifelse(oot_data$predictions >= 0.0739 & oot_data$predictions < 0.0835, 4,
                                                   ifelse(oot_data$predictions >= 0.0835 & oot_data$predictions < 0.0979, 5.1, 5.2)))))
                                                          
                                                          
                                                   
                              


oot_data$cibil_pentile <- ifelse(oot_data$cibil_score_v3 <=675 , "Cibil <= 675",
                                 ifelse(oot_data$cibil_score_v3 > 675 & oot_data$cibil_score_v3 <=700 , "Cibil 675 - 700",
                                        ifelse(oot_data$cibil_score_v3 > 700 & oot_data$cibil_score_v3 <=730 , "Cibil 700 - 730",
                                               ifelse(oot_data$cibil_score_v3 > 730 & oot_data$cibil_score_v3 <=750 , "Cibil 730 - 750",
                                                      ifelse(oot_data$cibil_score_v3 > 750 & oot_data$cibil_score_v3 <= 780 , "Cibil 750 - 780",
                                                             ifelse(oot_data$cibil_score_v3 > 780 , "Cibil >= 780",
                                                                    "Invalid"))))))



cibil_crosstab <- oot_data %>% group_by(pd_pentile,cibil_pentile) %>% summarise(cibil_min = min(cibil_score_v3),
                                                                                cibil_max = max(cibil_score_v3),
                                                                                applicant_count = n(),
                                                                                defaults_bad_loan = sum(bad_loan),
                                                                                defaults_90dpd = sum(ever_90dpd),
                                                                                defaults_150dpd = sum(ever_150dpd),
                                                                                def_pct_bad_loan = mean(bad_loan),
                                                                                def_pct_90dpd = mean(ever_90dpd),
                                                                                def_pct_150dpd = mean(ever_150dpd)
                                                                                
)


pd_pentile_delinquency <- oot_data %>% group_by(pd_pentile) %>% summarise(cibil_min = min(cibil_score_v3),
                                                                          cibil_max = max(cibil_score_v3),
                                                                          applicant_count = n(),
                                                                          defaults_bad_loan = sum(bad_loan),
                                                                          defaults_90dpd = sum(ever_90dpd),
                                                                          defaults_150dpd = sum(ever_150dpd),
                                                                          def_pct_bad_loan = mean(bad_loan),
                                                                          def_pct_90dpd = mean(ever_90dpd),
                                                                          def_pct_150dpd = mean(ever_150dpd))



cibil_pentile_delinquency <- oot_data %>% group_by(cibil_pentile) %>% summarise(cibil_min = min(cibil_score_v3),
                                                                                cibil_max = max(cibil_score_v3),
                                                                                applicant_count = n(),
                                                                                defaults_bad_loan = sum(bad_loan),
                                                                                defaults_90dpd = sum(ever_90dpd),
                                                                                defaults_150dpd = sum(ever_150dpd),
                                                                                def_pct_bad_loan = mean(bad_loan),
                                                                                def_pct_90dpd = mean(ever_90dpd),
                                                                                def_pct_150dpd = mean(ever_150dpd))






output_list <- list("cibil_crosstab" = cibil_crosstab,
                    "pd_pentile_delinquency" = pd_pentile_delinquency,
                    "cibil_pentile_delinquency" = cibil_pentile_delinquency
)


write_xlsx(output_list, path = file.path("data//output//model//PV_Used//Combined//Model_validation_PV_Used_7_OOT-CIBIL_v3_crosstab_scored_pd.xlsx"))




rm(list=ls())

##########################################################################################################################################

load_rdata_output("model//PV_Used//Combined//vcheck//train_data.rdata")
load_rdata_output("model//PV_Used//Combined//vcheck//test_data.rdata")

dev_data <- rbind(train_data_final,test_data_final)

dev_data$profile <- ifelse(dev_data$category_flag_SENP_SEP == 0, "SAL",
                           ifelse(dev_data$category_flag_SENP_SEP == 1 & dev_data$agri_profile_flag == 0, "SENP_Non_Agri","SENP_Agri"))



dev_data$pd_decile <- ntile(dev_data$predictions,10)
dev_data$pd_pentile <- ntile(dev_data$predictions,5)



decile <- dev_data %>% group_by(pd_decile,profile) %>% summarise(population = n(),
                                                                 delinquency = mean(bad_loan))

pentile <- dev_data %>% group_by(pd_pentile,profile) %>% summarise(population = n(),
                                                                 delinquency = mean(bad_loan))


output_list <- list("decile" = decile,
                    "pentile" = pentile
)


# save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//vcheck//Model_validation_PV_New_5-Applicant Profile analysis.xlsx")
write_xlsx(output_list, "data//output//model//PV_Used//Combined//vcheck//Model_validation_PV_Used_6-Applicant Profile analysis.xlsx")
