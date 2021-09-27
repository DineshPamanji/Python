#############################################################################################
################## 23 - Model 2W RF Validation  ################################################
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

## 1.1 load data 
load_rdata_intermediate("ADS_data//base_ads.rdata")

load_rdata_output("model//2W_RF//train_data.rdata")
load_rdata_output("model//2W_RF//test_data.rdata")
load_rdata_output("model//2W_RF//oot_data.rdata")


base_ads_small <- base_ads %>% dplyr::select('deal_no','loan_type','CIBIL_SCORE','Application_No','Customer_Code')

train_data_final <- inner_join(train_data_final, base_ads_small)
test_data_final <- inner_join(test_data_final, base_ads_small)
oot_data_final <- inner_join(oot_data_final, base_ads_small)



# 
# 
# ## 1.2 define small & large ticket size
# ticket_size <- distinct(base_ads %>% filter((!is.na(Finance_Amount_Chassis)) & (loan_type == '2W-New')) %>% dplyr::select(deal_no, Finance_Amount_Chassis))
# 
# ticket_size_small <- ticket_size %>% filter(Finance_Amount_Chassis < 100000)
# ticket_size_big <- ticket_size %>% filter(Finance_Amount_Chassis >= 100000)
# 
# rm(base_ads,ticket_size)
# 
# 
# 
# ## 1.3 get rank ordering
# 
# # 1.3.1 decile
# RO_ticket_small_decile <- data.frame(get_RO(train_data_final %>% filter(deal_no %in% unique(ticket_size_small$deal_no)),
#                                             test_data_final %>% filter(deal_no %in% unique(ticket_size_small$deal_no)),
#                                             oot_data_final%>% filter(deal_no %in% unique(ticket_size_small$deal_no))))
# 
# 
# RO_ticket_big_decile <- data.frame(get_RO(train_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no)),
#                                           test_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no)),
#                                           oot_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no))))
# 
# ## 1.3.2 pentile
# RO_ticket_small_pentile <- data.frame(get_RO_pentile(train_data_final %>% filter(deal_no %in% unique(ticket_size_small$deal_no)),
#                                                      test_data_final %>% filter(deal_no %in% unique(ticket_size_small$deal_no)),
#                                                      oot_data_final%>% filter(deal_no %in% unique(ticket_size_small$deal_no))))
# 
# 
# RO_ticket_big_pentile <- data.frame(get_RO_pentile(train_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no)),
#                                                    test_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no)),
#                                                    oot_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no))))
# 
# 
# # 1.3.3 scored decile
# RO_ticket_small_decile_scored <- data.frame(get_RO_scored(train_data_final %>% filter(deal_no %in% unique(ticket_size_small$deal_no)),
#                                                           test_data_final %>% filter(deal_no %in% unique(ticket_size_small$deal_no)),
#                                                           oot_data_final%>% filter(deal_no %in% unique(ticket_size_small$deal_no))))
# 
# 
# RO_ticket_big_decile_scored <- data.frame(get_RO_scored(train_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no)),
#                                                         test_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no)),
#                                                         oot_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no))))
# 
# 
# 
# ## 1.3.4 scored pentile
# RO_ticket_small_pentile_scored <- data.frame(get_RO_scored_pentile(train_data_final %>% filter(deal_no %in% unique(ticket_size_small$deal_no)),
#                                                                    test_data_final %>% filter(deal_no %in% unique(ticket_size_small$deal_no)),
#                                                                    oot_data_final%>% filter(deal_no %in% unique(ticket_size_small$deal_no))))
# 
# 
# RO_ticket_big_pentile_scored <- data.frame(get_RO_scored_pentile(train_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no)),
#                                                                  test_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no)),
#                                                                  oot_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no))))
# 
# 
# 
# ## 1.4 save output
# output_list <- list("RO_small_ticket_D" = RO_ticket_small_decile,
#                     "RO_big_ticket_D" = RO_ticket_big_decile,
#                     "RO_small_ticket_P" = RO_ticket_small_pentile,
#                     "RO_big_ticket_P" = RO_ticket_big_pentile,
#                     "RO_small_ticket_SD" = RO_ticket_small_decile_scored,
#                     "RO_big_ticket_SD" = RO_ticket_big_decile_scored,
#                     "RO_small_ticket_SP" = RO_ticket_small_pentile_scored,
#                     "RO_big_ticket_SP" = RO_ticket_big_pentile_scored
#                     
# )
# 
# 
# save_xlsx_output(data = output_list, relative_path = "//model//2W_New//Combined//Model_validation_2W_New_1-Ticket size.xlsx")
# 
# rm(RO_ticket_small_decile,RO_ticket_big_decile, RO_ticket_small_pentile,RO_ticket_big_pentile,
#    RO_ticket_small_decile_scored,RO_ticket_big_decile_scored,RO_ticket_small_pentile_scored,RO_ticket_big_pentile_scored,
#    output_list,ticket_size_small,ticket_size_big)
# 
# 


# 2. Validation 2 - Cibil Score Rank ordering ------------------------------------------------------------------------------

## 2.1 create cibil buckets
train_data_final$cibil_buckets <- ifelse(is.na(train_data_final$CIBIL_SCORE), "NTC",
                                         ifelse(train_data_final$CIBIL_SCORE <= 0, "Negative CIBIL",
                                                ifelse(train_data_final$CIBIL_SCORE >= 1 & train_data_final$CIBIL_SCORE <=5, train_data_final$CIBIL_SCORE,
                                                       ifelse(train_data_final$CIBIL_SCORE > 5 & train_data_final$CIBIL_SCORE <=550 , "Cibil <= 550",
                                                              ifelse(train_data_final$CIBIL_SCORE > 550 & train_data_final$CIBIL_SCORE <=650 , "Cibil 550 - 650",
                                                                     ifelse(train_data_final$CIBIL_SCORE > 650 & train_data_final$CIBIL_SCORE <=750 , "Cibil 650 - 750",
                                                                            ifelse(train_data_final$CIBIL_SCORE > 750 , "Cibil > 750","NTC")))))))


test_data_final$cibil_buckets <- ifelse(is.na(test_data_final$CIBIL_SCORE), "NTC",
                                        ifelse(test_data_final$CIBIL_SCORE <= 0, "Negative CIBIL",
                                               ifelse(test_data_final$CIBIL_SCORE >= 1 & test_data_final$CIBIL_SCORE <=5, test_data_final$CIBIL_SCORE,
                                                      ifelse(test_data_final$CIBIL_SCORE > 5 & test_data_final$CIBIL_SCORE <=550 , "Cibil <= 550",
                                                             ifelse(test_data_final$CIBIL_SCORE > 550 & test_data_final$CIBIL_SCORE <=650 , "Cibil 550 - 650",
                                                                    ifelse(test_data_final$CIBIL_SCORE > 650 & test_data_final$CIBIL_SCORE <=750 , "Cibil 650 - 750",
                                                                           ifelse(test_data_final$CIBIL_SCORE > 750 , "Cibil > 750","NTC")))))))


oot_data_final$cibil_buckets <- ifelse(is.na(oot_data_final$CIBIL_SCORE), "NTC",
                                       ifelse(oot_data_final$CIBIL_SCORE <= 0, "Negative CIBIL",
                                              ifelse(oot_data_final$CIBIL_SCORE >= 1 & oot_data_final$CIBIL_SCORE <=5, oot_data_final$CIBIL_SCORE,
                                                     ifelse(oot_data_final$CIBIL_SCORE > 5 & oot_data_final$CIBIL_SCORE <=550 , "Cibil <= 550",
                                                            ifelse(oot_data_final$CIBIL_SCORE > 550 & oot_data_final$CIBIL_SCORE <=650 , "Cibil 550 - 650",
                                                                   ifelse(oot_data_final$CIBIL_SCORE > 650 & oot_data_final$CIBIL_SCORE <=750 , "Cibil 650 - 750",
                                                                          ifelse(oot_data_final$CIBIL_SCORE > 750 , "Cibil > 750","NTC")))))))



## 2.2 create subsets with required columns
temp_train_all <- train_data_final
temp_train_all$decile <- ntile(temp_train_all$predictions,10)
temp_train_all$pentile <- ntile(temp_train_all$predictions,5)
temp_train_all <- temp_train_all %>% dplyr::select(applicant_id,Category,predictions,bad_loan,decile,pentile, CIBIL_SCORE,cibil_buckets)
temp_train_all <- temp_train_all %>% filter(cibil_buckets %notin% c('NTC','Negative CIBIL')) %>% group_by(pentile,cibil_buckets) %>% summarise(applicant_count = length(unique(applicant_id)),
                                                                                                                                               default_count = sum(bad_loan),
                                                                                                                                               default_pct = mean(bad_loan)
)


temp_test_all <- test_data_final
temp_test_all$decile <- ntile(temp_test_all$predictions,10)
temp_test_all$pentile <- ntile(temp_test_all$predictions,5)
temp_test_all <- temp_test_all %>% dplyr::select(applicant_id,Category,predictions,bad_loan,decile,pentile, CIBIL_SCORE,cibil_buckets)
temp_test_all <- temp_test_all %>% filter(cibil_buckets %notin% c('NTC','Negative CIBIL')) %>% group_by(pentile,cibil_buckets) %>% summarise(applicant_count = length(unique(applicant_id)),
                                                                                                                                             default_count = sum(bad_loan),
                                                                                                                                             default_pct = mean(bad_loan)
)


temp_oot_all <- oot_data_final
temp_oot_all$decile <- ntile(temp_oot_all$predictions,10)
temp_oot_all$pentile <- ntile(temp_oot_all$predictions,5)
temp_oot_all <- temp_oot_all %>% dplyr::select(applicant_id,Category,predictions,bad_loan,decile,pentile, CIBIL_SCORE,cibil_buckets)
temp_oot_all <- temp_oot_all %>% filter(cibil_buckets %notin% c('NTC','Negative CIBIL')) %>% group_by(pentile,cibil_buckets) %>% summarise(applicant_count = length(unique(applicant_id)),
                                                                                                                                           default_count = sum(bad_loan),
                                                                                                                                           default_pct = mean(bad_loan)
)



## 2.3 save outputs
output_list <- list("train_cibil_cross_tab" = temp_train_all,
                    "test_cibil_cross_tab" = temp_test_all,
                    "oot_cibil_cross_tab" = temp_oot_all
                    
)


save_xlsx_output(data = output_list, relative_path = "//model//2W_RF//Model_validation_2W_RF_2-Cibilv2 crosstab.xlsx")
 # write_xlsx(output_list, path = file.path("data//output//model//2W_RF//Model_validation_2W_RF_2-Cibilv2 crosstab.xlsx"))
           

rm(temp_train_all,temp_test_all, temp_oot_all,output_list)




train_temp <- train_data_final %>% filter(CIBIL_SCORE %in% c(100:900))
test_temp <- test_data_final %>% filter(CIBIL_SCORE %in% c(100:900))
oot_temp <- oot_data_final %>% filter(CIBIL_SCORE %in% c(100:900))


# train_temp <- train_data_final %>% filter((CIBIL_SCORE %in% c(100:900)) & (Category == 'SAL'))
# test_temp <- test_data_final %>% filter((CIBIL_SCORE %in% c(100:900)) & (Category == 'SAL'))
# oot_temp <- oot_data_final %>% filter(CIBIL_SCORE %in% c(100:900))


train_temp$pd_pentile <- ntile(train_temp$predictions,5)
train_temp$cibil_pentile <- ntile(train_temp$CIBIL_SCORE,5)

oot_temp$pd_pentile <- ntile(oot_temp$predictions,5)
oot_temp$cibil_pentile <- ntile(oot_temp$CIBIL_SCORE,5)


train_output <- train_temp %>% group_by(pd_pentile,cibil_pentile) %>% summarise(cibil_min = min(CIBIL_SCORE),
                                                                                cibil_max = max(CIBIL_SCORE),
                                                                                n=n(),
                                                                                defaults = sum(bad_loan),
                                                                                def_pct = mean(bad_loan)
)


oot_output <- oot_temp %>% group_by(pd_pentile,cibil_pentile) %>% summarise(cibil_min = min(CIBIL_SCORE),
                                                                            cibil_max = max(CIBIL_SCORE),
                                                                            n=n(),
                                                                            defaults = sum(bad_loan),
                                                                            def_pct = mean(bad_loan)
)




output_list <- list("train_pd_x_cibil" = train_output,
                    "oot_pd_x_cibil" = oot_output
                    
)


save_xlsx_output(data = output_list, relative_path = "//model//2W_New//Combined//Model_validation_2W_New_3-PD x Cibil crosstab.xlsx")
 # write_xlsx(output_list, path = file.path("data//output//model//2W_RF//Model_validation_2W_RF_3-PD X Cibilv2 crosstab.xlsx"))


## 3. Validation 3 - Temporal Decay -----------------------------------------------------------------------------

## 3.1 load data
load_rdata_intermediate("ADS_data//temporal_decay_2W_RF.rdata")

## 3.2 select required data & rename columns
temporal_decay <- temporal_decay %>% filter(loan_type == 'Refinance-2W')
colnames(temporal_decay)[colnames(temporal_decay) == 'temporal_decay_post_MOB'] <- 'bad_loan'

## 3.3 select prediction data
temporal_decay_train <- train_data_final %>% dplyr::select(deal_no, applicant_id, predictions)
temporal_decay_test <- test_data_final %>% dplyr::select(deal_no, applicant_id, predictions)
temporal_decay_oot <- oot_data_final %>% dplyr::select(deal_no, applicant_id, predictions)


# test <- data.frame(rbindlist(l = list(temporal_decay_train,temporal_decay_test,temporal_decay_oot)))
# test <- inner_join(test,temporal_decay, by = 'deal_no')

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


save_xlsx_output(data = output_list, relative_path = "//model//2W_RF//Model_validation_2W_New_4-Temporal decay.xlsx")
 # write_xlsx(output_list, path = file.path("data//output//model//2W_RF//Model_validation_2W_RF_4-Temporal decay.xlsx"))


rm(temporal_decay_train,temporal_decay_test,temporal_decay_oot,
   temporal_decay_RO_decile,temporal_decay_RO_pentile,
   temporal_decay_RO_decile_scored,temporal_decay_RO_pentile_scored,temporal_decay,output_list)





## 4. Validation 4 - Intermediary validation ---------------------------------------------------------------------

## 4.1 load data
load_rdata_intermediate("ADS_data//intermediary_I1_2W_RF.rdata")

## 4.2 select required data & rename columns
intermediary_I1 <- intermediary_I1 %>% filter(loan_type == 'Refinance-2W')
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


save_xlsx_output(data = output_list, relative_path = "//model//2W_RF//Model_validation_2W_New_5-Intermediary.xlsx")
 # write_xlsx(output_list, path = file.path("data//output//model//2W_RF//Model_validation_2W_RF_5-Intermediary.xlsx"))

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

# test2 <- data.frame(rbindlist(l = list(ever_train,ever_test,ever_oot)))



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


save_xlsx_output(data = output_list, relative_path = "//model//2W_RF//Model_validation_2W_New_6-Ever 90 150 DPD.xlsx")
 # write_xlsx(output_list, path = file.path("data//output//model//2W_RF//Model_validation_2W_RF_6-Ever 90 150 DPD.xlsx"))

rm(output_list,ever_150dpd_RO_decile,ever_150dpd_RO_decile_scored,ever_150dpd_RO_pentile,ever_150dpd_RO_pentile_scored,
   ever_90dpd_RO_decile,ever_90dpd_RO_decile_scored,ever_90dpd_RO_pentile,ever_90dpd_RO_pentile_scored,ever_oot,ever_test,
   ever_train,train_data_final,test_data_final,oot_data_final)



## 6. Validation 6 - Out of time & out of sample validation ---------------------------------------------------------------------

## 6.1 Load 2W New data 
load_rdata_intermediate("model_data//model_data_2W_RF_validation.rdata")


## 6.2 load validation datasets
load_rdata_intermediate("ADS_data//validation_60dpd_12mob_2W_RF.rdata")
load_rdata_intermediate("ADS_data//validation_60dpd_9mob_2W_RF.rdata")
load_rdata_intermediate("ADS_data//validation_60dpd_oct_to_dec_2020_2W_RF.rdata")



## 6.3. Variable transformation 

# 6.3.1 vintage
model_data_2W_RF$Var_credit_vintage_C2 <- ifelse(model_data_2W_RF$Var_credit_vintage <= 0.5, 0.5, 
                                                 ifelse(model_data_2W_RF$Var_credit_vintage >= 10, 10, model_data_2W_RF$Var_credit_vintage))

# 6.3.2 Var_total_sanctioned_amount woe 

model_data_2W_RF$Var_sanctioned_amount_live_loans_C <- model_data_2W_RF$Var_sanctioned_amount_live_loans

model_data_2W_RF$Var_sanctioned_amount_live_loans_C[is.na(model_data_2W_RF$Var_sanctioned_amount_live_loans_C)] <- 0

model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_C <- model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans

model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 0.05


model_data_2W_RF$Var_sanctioned_amount_live_loans_bin <- ifelse(model_data_2W_RF$Var_sanctioned_amount_live_loans_C <= 60000, 'bin_LE_60k', 
                                                        ifelse(model_data_2W_RF$Var_sanctioned_amount_live_loans_C > 60000 & model_data_2W_RF$Var_sanctioned_amount_live_loans_C <= 300000, 'bin_60k_to_3L', 'bin_GE_3L'))

model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.25 & model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.9, 'val_25_to_90_pct','val_GE_90_pct'))

subset1 <- model_data_2W_RF %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_60k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_90_pct')))
subset2 <- model_data_2W_RF %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_60k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_90_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_60k_to_3L','bin_GE_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_90_pct'))))
subset3 <- model_data_2W_RF %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_60k_to_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_90_pct'))) |
                                 ((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_90_pct'))) | 
                                 ((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_60k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct'))))
subset4 <- model_data_2W_RF %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct')))

woe1 <- -0.7003537
woe2 <- -0.2105082
woe3 <- 0.03703576
woe4 <- 0.4658788

model_data_2W_RF$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live <- ifelse(model_data_2W_RF$deal_no %in% unique(subset1$deal_no), woe1,
                                                                        ifelse(model_data_2W_RF$deal_no %in% unique(subset2$deal_no), woe2,
                                                                               ifelse(model_data_2W_RF$deal_no %in% unique(subset3$deal_no), woe3,
                                                                                      woe4)))

# 6.3.5 Enquiry in 6m

model_data_2W_RF$Var_EN_enquiry_count_6m_C1 <- model_data_2W_RF$Var_EN_enquiry_count_6m
model_data_2W_RF$Var_EN_enquiry_count_6m_C1[is.na(model_data_2W_RF$Var_EN_enquiry_count_6m_C1)] <- 0

model_data_2W_RF$Var_EN_enquiry_count_6m_C1 <- ifelse(model_data_2W_RF$Var_EN_enquiry_count_6m_C1 <= 0,
                                                      0, model_data_2W_RF$Var_EN_enquiry_count_6m_C1)

subset1 <- model_data_2W_RF %>% filter(Var_EN_enquiry_count_6m_C1 == 0)
subset2 <- model_data_2W_RF %>% filter(Var_EN_enquiry_count_6m_C1 == 1)
subset3 <- model_data_2W_RF %>% filter(Var_EN_enquiry_count_6m_C1 > 1 & Var_EN_enquiry_count_6m_C1 <= 4)
subset4 <- model_data_2W_RF %>% filter(Var_EN_enquiry_count_6m_C1 > 4)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)


woe1 <- 0.113
woe2 <- -0.132
woe3 <- -0.449
woe4 <- -0.629


model_data_2W_RF$Var_EN_enquiry_count_6m_C1_woe1 <-
  ifelse(
    model_data_2W_RF$Var_EN_enquiry_count_6m_C1 == 0, woe1,
    ifelse(model_data_2W_RF$Var_EN_enquiry_count_6m_C1 == 1, woe2,
           ifelse(model_data_2W_RF$Var_EN_enquiry_count_6m_C1 > 1 & model_data_2W_RF$Var_EN_enquiry_count_6m_C1 <= 4,
                  woe3, woe4)
    ))


# 6.3.6 30 dpd in 6 mon

model_data_2W_RF$Var_DL_all_30dpd_12mon_C1 <- model_data_2W_RF$Var_DL_all_30dpd_12mon
model_data_2W_RF$Var_DL_all_30dpd_12mon_C1[is.na(model_data_2W_RF$Var_DL_all_30dpd_12mon_C1)] <- 0

model_data_2W_RF$Var_DL_30dpd_12mon_flag <- ifelse(model_data_2W_RF$Var_DL_all_30dpd_12mon_C1 == 0, 0, 1)


## 6.3.8 Agri profile flag
model_data_2W_RF$agri_profile_flag <- ifelse(is.na(model_data_2W_RF$agri_profile_flag), 0 , model_data_2W_RF$agri_profile_flag)






## 6.4 get list of features
shortlisted_var <- c( 
  'Var_credit_vintage_C2',
  'Var_EN_enquiry_count_6m_C1_woe1',
  'Var_DL_30dpd_12mon_flag',
  'cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live',
  'agri_profile_flag'
)


# 6.5 subset for required columns
performance_data <- model_data_2W_RF %>% dplyr::select(c('deal_no','applicant_id',shortlisted_var,'bad_loan'))


# 6.6 load model
model_2W <- readRDS(file = file.path(get_data_path()$data$model,"model_2W_RF.rds"))
 # model_2W <- readRDS(file = file.path("output//model_2W_RF_updated.rds"))


# 6.7 make predictions
performance_data <- get_predictions(model_2W, performance_data)
save(performance_data, file = "data//output//model//2W_RF//validation_postcovid.rdata")

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


save_xlsx_output(data = output_list, relative_path = "//model//2W_RF//Model_validation_2W_New_7-OOT.xlsx")
# write_xlsx(output_list, path = file.path("data//output//model//2W_RF//Model_validation_2W_RF_7-OOT.xlsx"))




rm(list=ls())


###################################################################################################################################

## 7. Validation 7 -  CIBIL v3 cross tab   ---------------------------------------------------------------------



## 1. load train, test & OOT data
load_rdata_output("model//2W_RF//train_data.rdata")
load_rdata_output("model//2W_RF//test_data.rdata")
load_rdata_output("model//2W_RF//oot_data.rdata")


## 2. load cibil data
load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
cibil_v3 <- distinct(cibil_v3 %>% dplyr::select(deal_no,customer_code,cibil_score_v3))


## 3. get cibil v3 scores by joining with data
train_data_final <- left_join(train_data_final,cibil_v3,by=c('deal_no'))
test_data_final <- left_join(test_data_final,cibil_v3,by=c('deal_no'))
oot_data_final <- left_join(oot_data_final,cibil_v3,by=c('deal_no'))


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

rm(dev_data_p5,train_data_final,test_data_final,train_data,test_data)
# rm(oot_data)

# dev_data$cibil_pentile <- ntile(dev_data$cibil_score_v3,5)

# dev_data$cibil_pentile <- ifelse(dev_data$cibil_score_v3 <=700 , "Cibil <= 700",
#                                  ifelse(dev_data$cibil_score_v3 > 700 & dev_data$cibil_score_v3 <=730 , "Cibil 700 - 730",
#                                         ifelse(dev_data$cibil_score_v3 > 730 & dev_data$cibil_score_v3 <=750 , "Cibil 730 - 750",
#                                                ifelse(dev_data$cibil_score_v3 > 750 & dev_data$cibil_score_v3 <= 780 , "Cibil 750 - 780",
#                                                       ifelse(dev_data$cibil_score_v3 > 780 , "Cibil >= 780",
#                                                       "Invalid")))))
# 
# dev_data$cibil_pentile2 <- ifelse(dev_data$cibil_score_v3 <=675 , "Cibil <= 675",
#                                   ifelse(dev_data$cibil_score_v3 > 675 & dev_data$cibil_score_v3 <=700 , "Cibil 675 - 700",
#                                  ifelse(dev_data$cibil_score_v3 > 700 & dev_data$cibil_score_v3 <=725 , "Cibil 700 - 725",
#                                         ifelse(dev_data$cibil_score_v3 > 725 & dev_data$cibil_score_v3 <=750 , "Cibil 725 - 750",
#                                                ifelse(dev_data$cibil_score_v3 > 750 & dev_data$cibil_score_v3 <= 775 , "Cibil 750 - 775",
#                                                       ifelse(dev_data$cibil_score_v3 > 775 , "Cibil >= 775",
#                                                              "Invalid"))))))

dev_data$cibil_pentile <- ifelse(dev_data$cibil_score_v3 <=675 , "Cibil <= 675",
                                 ifelse(dev_data$cibil_score_v3 > 675 & dev_data$cibil_score_v3 <=700 , "Cibil 675 - 700",
                                        ifelse(dev_data$cibil_score_v3 > 700 & dev_data$cibil_score_v3 <=730 , "Cibil 700 - 730",
                                               ifelse(dev_data$cibil_score_v3 > 730 & dev_data$cibil_score_v3 <=750 , "Cibil 730 - 750",
                                                      ifelse(dev_data$cibil_score_v3 > 750 & dev_data$cibil_score_v3 <= 780 , "Cibil 750 - 780",
                                                             ifelse(dev_data$cibil_score_v3 > 780 , "Cibil >= 780",
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






output_list <- list("cibil_crosstab" = cibil_crosstab,
                    "pd_pentile_delinquency" = pd_pentile_delinquency,
                    "cibil_pentile_delinquency" = cibil_pentile_delinquency
)


save_xlsx_output(data = output_list, relative_path = "//model//2W_RF//Model_validation_2W_New_7-CIBIL_v3_crosstab_v5.xlsx")
# write_xlsx(output_list, path = file.path("data//output//model//2W_RF//Model_validation_2W_New_7-CIBIL_v3_crosstab_v5.xlsx"))

pd_scores <- dev_data %>% group_by(pd_pentile) %>% summarise(min = min(predictions), max = max(predictions))



# rm(list=ls())

## oot data  ----------------------------------------------------------------------------------
## 6. create cibil score pentiles & PD pentiles

# oot_data$pd_pentile <- ntile(oot_data$predictions,5)
# 
# oot_data_p5 <- oot_data %>% filter(pd_pentile == 5)
# oot_data_p5$pd_pentile <- ntile(oot_data_p5$predictions,2)
# oot_data_p5$pd_pentile <- ifelse(oot_data_p5$pd_pentile == 1, 5.1, 5.2)
# 
# oot_data <- rbind(oot_data %>% filter(pd_pentile <= 4), oot_data_p5)

# rm(oot_data_p5,train_data_final)
# rm(oot_data)



oot_data$pd_pentile <- ifelse(oot_data$predictions < 0.0610, 1,
                              ifelse(oot_data$predictions >= 0.0610 & oot_data$predictions < 0.0623, 2,
                                     ifelse(oot_data$predictions >= 0.0623 & oot_data$predictions < 0.0726, 3,
                                            ifelse(oot_data$predictions >= 0.0726 & oot_data$predictions < 0.0790, 4,
                                                   ifelse(oot_data$predictions >= 0.0790 & oot_data$predictions < 0.103, 5.1, 5.2)))))



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


write_xlsx(output_list,
           path = file.path("data//output//model//2W_RF//Model_validation_2W_New_7_OOT-CIBIL_v3_crosstab_scored_pd.xlsx"))




rm(list=ls())



####################################################################################################33
## Running 2W New model on 2W Used data
######################################################################################################

load_rdata_intermediate("model_data//model_data_2W_RF_validation.rdata")


## 6.2 load validation datasets
load_rdata_intermediate("ADS_data//validation_60dpd_12mob_2W_RF.rdata")
load_rdata_intermediate("ADS_data//validation_60dpd_9mob_2W_RF.rdata")
load_rdata_intermediate("ADS_data//validation_60dpd_oct_to_dec_2020_2W_RF.rdata")




## 6.3. Variable transformation 

# 6.3.1 vintage
model_data_2W_RF$Var_credit_vintage_C <- ifelse(model_data_2W_RF$Var_credit_vintage <= 0.01, 0.01, 
                                                ifelse(model_data_2W_RF$Var_credit_vintage >= 12, 12, model_data_2W_RF$Var_credit_vintage))

# 6.3.2 count of closed loans
model_data_2W_RF$Var_PO_closed_excl_CC_C <- model_data_2W_RF$Var_PO_closed_excl_CC
model_data_2W_RF$Var_PO_closed_excl_CC_C[is.na(model_data_2W_RF$Var_PO_closed_excl_CC_C)] <- 0

woe1 <- -0.1083156
woe2 <- 0.05311667
woe3 <- 0.2382278


model_data_2W_RF$Var_PO_closed_excl_CC_woe <- ifelse(model_data_2W_RF$Var_PO_closed_excl_CC_C <= 1, woe1, 
                                                     ifelse(model_data_2W_RF$Var_PO_closed_excl_CC_C == 2, woe2,woe3))

rm(woe1, woe2, woe3)



# 6.3.3 HL paid 12 month Continuous
model_data_2W_RF$Var_DL_HL_paid_GE_12mon_C <- model_data_2W_RF$Var_DL_HL_paid_GE_12mon
model_data_2W_RF$Var_DL_HL_paid_GE_12mon_C[is.na(model_data_2W_RF$Var_DL_HL_paid_GE_12mon_C)] <- 0

model_data_2W_RF$Var_DL_HL_paid_GE_12mon_C <- ifelse(model_data_2W_RF$Var_DL_HL_paid_GE_12mon_C >= 3, 3, 
                                                     ifelse(model_data_2W_RF$Var_DL_HL_paid_GE_12mon_C <= 0, 0,model_data_2W_RF$Var_DL_HL_paid_GE_12mon_C))




# 6.3.4 Gold paid 12 month continuous
model_data_2W_RF$Var_DL_Gold_paid_GE_12mon_C <- model_data_2W_RF$Var_DL_Gold_paid_GE_12mon
model_data_2W_RF$Var_DL_Gold_paid_GE_12mon_C[is.na(model_data_2W_RF$Var_DL_Gold_paid_GE_12mon_C)] <- 0

woe1 <- -0.02766028
woe2 <- 0.1895425
woe3 <- 0.3895373

model_data_2W_RF$Var_DL_Gold_paid_GE_12mon_woe <- ifelse(model_data_2W_RF$Var_DL_Gold_paid_GE_12mon_C == 0, woe1, 
                                                         ifelse(model_data_2W_RF$Var_DL_Gold_paid_GE_12mon_C == 1, woe2,woe3))

rm(woe1, woe2, woe3)



# 6.3.5 Enquiry in 6m non cc 
model_data_2W_RF$Var_EN_enquiry_count_6m_non_CC_C <- model_data_2W_RF$Var_EN_enquiry_count_6m_non_CC
model_data_2W_RF$Var_EN_enquiry_count_6m_non_CC_C[is.na(model_data_2W_RF$Var_EN_enquiry_count_6m_non_CC_C)] <- 0

model_data_2W_RF$Var_EN_enquiry_count_6m_non_CC_C <- ifelse(model_data_2W_RF$Var_EN_enquiry_count_6m_non_CC_C <= 0, 0, 
                                                            ifelse(model_data_2W_RF$Var_EN_enquiry_count_6m_non_CC_C >= 6, 6, model_data_2W_RF$Var_EN_enquiry_count_6m_non_CC_C))




# 6.3.6 30 dpd in 6 mon
model_data_2W_RF$Var_DL_all_30dpd_6mon_C <- model_data_2W_RF$Var_DL_all_30dpd_6mon
model_data_2W_RF$Var_DL_all_30dpd_6mon_C[is.na(model_data_2W_RF$Var_DL_all_30dpd_6mon_C)] <- 0

model_data_2W_RF$Var_DL_all_30dpd_6mon_flag <- ifelse(model_data_2W_RF$Var_DL_all_30dpd_6mon_C == 0, 0, 1)


## 6.3.7 SENP SEP Flag
model_data_2W_RF$category_flag_SENP_SEP <- ifelse(model_data_2W_RF$Category %in% c('SENP', 'SEP'), 1, 0)


## 6.3.8 Agri profile flag
model_data_2W_RF$agri_profile_flag <- ifelse(is.na(model_data_2W_RF$agri_profile_flag), 0 , model_data_2W_RF$agri_profile_flag)



## 6.3.9 cross tab variable
model_data_2W_RF$Var_sanctioned_amount_live_loans_C <- model_data_2W_RF$Var_sanctioned_amount_live_loans
model_data_2W_RF$Var_sanctioned_amount_live_loans_C[is.na(model_data_2W_RF$Var_sanctioned_amount_live_loans_C)] <- 0


model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_C <- model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans
model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 1




model_data_2W_RF$Var_sanctioned_amount_live_loans_bin <- ifelse(model_data_2W_RF$Var_sanctioned_amount_live_loans_C <= 175000, 'bin_LE_175k', 
                                                                ifelse(model_data_2W_RF$Var_sanctioned_amount_live_loans_C > 175000 & model_data_2W_RF$Var_sanctioned_amount_live_loans_C <= 1000000, 'bin_175k_to_10L', 'bin_GE_10L'))


model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.25, 'val_LE_25_pct', 
                                                                               ifelse(model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.25 & model_data_2W_RF$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.5, 'val_25_to_50_pct','val_GE_50_pct'))


subset1 <- model_data_2W_RF %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct')))
subset2 <- model_data_2W_RF %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct'))))
subset3 <- model_data_2W_RF %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_50_pct','val_GE_50_pct'))))
subset4 <- model_data_2W_RF %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct')))

woe1 <- -0.1334553
woe2 <- 0.1301698
woe3 <- 0.5085008
woe4 <- 0.6125813




model_data_2W_RF$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live <- ifelse(model_data_2W_RF$deal_no %in% unique(subset1$deal_no), woe1,
                                                                                ifelse(model_data_2W_RF$deal_no %in% unique(subset2$deal_no), woe2,
                                                                                       ifelse(model_data_2W_RF$deal_no %in% unique(subset3$deal_no), woe3,
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
performance_data <- model_data_2W_RF %>% dplyr::select(c('deal_no','applicant_id',shortlisted_var,'bad_loan'))


# 6.6 load model
# model_2W <- readRDS(file = file.path(get_data_path()$data$model,"model_2W_New.rds"))
model_2W <- readRDS(file = file.path(get_data_path()$data$model,"vcheck//model_2W_New.rds"))


# 6.7 make predictions
performance_data <- get_predictions(model_2W, performance_data)


# 6.8 get rank ordering for 60 dpd in 12mob
RO_D <-  data.frame(get_RO_validation_decile(performance_data))
RO_P <-  data.frame(get_RO_validation_pentile(performance_data))


# 6.8 get rank ordering for 60 dpd in 12mob
RO_60dpd_12mob_D <-  data.frame(get_RO_validation_decile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_12mob$deal_no))))
RO_60dpd_12mob_P <-  data.frame(get_RO_validation_pentile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_12mob$deal_no))))


# 6.9 get rank ordering for 60 dpd in 9mob
RO_60dpd_9mob_D <-  data.frame(get_RO_validation_decile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_9mob$deal_no))))
RO_60dpd_9mob_P <-  data.frame(get_RO_validation_pentile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_9mob$deal_no))))


# 6.10 get rank ordering for 60 dod in oct-dec 2020
RO_60dpd_oct_to_dec_2020_D <-  data.frame(get_RO_validation_decile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_oct_to_dec_2020$deal_no))))
RO_60dpd_oct_to_dec_2020_P <-  data.frame(get_RO_validation_pentile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_oct_to_dec_2020$deal_no))))

perf <- get_model_performance_no_test(performance_data, performance_data)


output_list <- list("RO_D" = RO_D,
                    "RO_P" = RO_P,
                    "Gini" = perf,
                    "RO_60dpd_12mob_D" = RO_60dpd_12mob_D,
                    "RO_60dpd_12mob_P" = RO_60dpd_12mob_P,
                    "RO_60dpd_9mob_D" = RO_60dpd_9mob_D,
                    "RO_60dpd_9mob_P" = RO_60dpd_9mob_P,
                    "RO_60dpd_oct_to_dec_2020_D" = RO_60dpd_oct_to_dec_2020_D,
                    "RO_60dpd_oct_to_dec_2020_P" = RO_60dpd_oct_to_dec_2020_P
)


# save_xlsx_output(data = output_list, relative_path = "//model//2W_New//Combined//Model_validation_2W_New_6-OOT.xlsx")
save_xlsx_output(data = output_list, relative_path = "//model//2W_RF//using_2W_new//Model_2W_RF_dev_RO.xlsx")
######################################################################################

load_rdata_intermediate("model_data//model_data_2W_RF.rdata")

dev_data <- model_data_2W_RF %>% dplyr::select('deal_no','bad_loan','ever_90dpd','ever_150dpd')
dev_data <- inner_join(performance_data, dev_data)

## 6. create cibil score pentiles & PD pentiles
dev_data$pd_pentile <- ntile(dev_data$predictions,5)

dev_data_p5 <- dev_data %>% filter(pd_pentile == 5)
dev_data_p5$pd_pentile <- ntile(dev_data_p5$predictions,2)
dev_data_p5$pd_pentile <- ifelse(dev_data_p5$pd_pentile == 1, 5.1, 5.2)

dev_data <- rbind(dev_data %>% filter(pd_pentile <= 4), dev_data_p5)

rm(dev_data_p5,train_data_final,test_data_final)
rm(oot_data)

# load cibil data
load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
cibil_v3 <- distinct(cibil_v3 %>% dplyr::select(deal_no,customer_code,cibil_score_v3))

### 3. get cibil v3 scores by joining with data
dev_data <- left_join(dev_data,cibil_v3,by=c('deal_no'))
dev_data$cibil_pentile <- ifelse(dev_data$cibil_score_v3 <=675 , "Cibil <= 675",
                                 ifelse(dev_data$cibil_score_v3 > 675 & dev_data$cibil_score_v3 <=700 , "Cibil 676 - 700",
                                        ifelse(dev_data$cibil_score_v3 > 700 & dev_data$cibil_score_v3 <=730 , "Cibil 701 - 730",
                                               ifelse(dev_data$cibil_score_v3 > 730 & dev_data$cibil_score_v3 <=750 , "Cibil 731 - 750",
                                                      ifelse(dev_data$cibil_score_v3 > 750 & dev_data$cibil_score_v3 <= 780 , "Cibil 751 - 780",
                                                             ifelse(dev_data$cibil_score_v3 > 780 , "Cibil >= 781",
                                                                    "Invalid"))))))



dev_data$cibil_pentile_num <- ifelse(dev_data$cibil_score_v3 <= 675 , 6,
                                     ifelse(dev_data$cibil_score_v3 > 675 & dev_data$cibil_score_v3 <=700 , 5,
                                            ifelse(dev_data$cibil_score_v3 > 700 & dev_data$cibil_score_v3 <=730 , 4,
                                                   ifelse(dev_data$cibil_score_v3 > 730 & dev_data$cibil_score_v3 <=750 , 3,
                                                          ifelse(dev_data$cibil_score_v3 > 750 & dev_data$cibil_score_v3 <= 780 , 2,
                                                                 ifelse(dev_data$cibil_score_v3 > 780 , 1,
                                                                        0))))))
# 
# dev_data$pd_x_cibil_tag <- paste0(dev_data$pd_pentile,"-",dev_data$cibil_pentile_num)
# 
# dark_green_band <- c('1-1','1-2','1-3','1-4','2-1','2-2','2-3','3-1','3-2','4-1','4-2')
# light_green_band <- c('2-4','3-3','4-3','5.1-1','5.1-2','5.2-3')
# orange_band <- c('4-5','4-6','5.1-5')
# red_band <- c('5.1-6','5.2-4','5.2-5','5.2-6')
# 
# 
# dev_data$risk_band <- ifelse(dev_data$pd_x_cibil_tag %in% dark_green_band, "dark_green",
#                              ifelse(dev_data$pd_x_cibil_tag %in% light_green_band, "light_green",
#                                     ifelse(dev_data$pd_x_cibil_tag %in% orange_band, "orange",
#                                            ifelse(dev_data$pd_x_cibil_tag %in% red_band, "red", "yellow"))))
# 
# 
# 
# summary_stats <- dev_data %>% group_by(risk_band) %>% summarise(applicants = n(),
#                                                                 default_count = sum(bad_loan),
#                                                                 default_90_count = sum(ever_90dpd),
#                                                                 default = mean(bad_loan),
#                                                                 default_90 = mean(ever_90dpd),
#                                                                 population = n()/nrow(belief_testing),
#                                                                 disbursals = length(unique(deal_no)))
# 



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


# save_xlsx_output(data = output_list, relative_path = "//model//2W_New//Combined//Model_validation_2W_New_7-CIBIL_v3_crosstab_v5.xlsx")
save_xlsx_output(data = output_list, relative_path = "//model//2W_RF//using_2W_new//Model_validation_2W_New_6-Cibil crosstab.xlsx")



rm(list=ls())

#################################################################################3



