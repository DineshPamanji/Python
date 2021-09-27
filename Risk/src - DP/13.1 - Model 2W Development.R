#############################################################################################
################## 12 - Model 2W SENP & SEP  ################################################
#############################################################################################


## 0. Load helper functions & libraries -----------------------------------------------------
load_libaries <- file.path("src - DP", "utils", "load_libraries.R")
source(load_libaries)

io_helper <- file.path("src - DP", "utils", "io_helper.R")
source(io_helper)

model_functions <- file.path("src - DP", "utils", "model_functions.R")
source(model_functions)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)



## 1. Load data & define functions ----------------------------------------------------------

## 1.1 Load 2W New data 
# load_rdata_intermediate("model_data//model_data_2W_New.rdata")

load_rdata_intermediate("model_data//model_data_2W_New_vcheck.rdata")



load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
cibil_v3 <- distinct(cibil_v3 %>% dplyr::select(deal_no,customer_code,cibil_score_v3))



model_data_2W_New <- left_join(model_data_2W_New,cibil_v3,by=c('deal_no','customer_code'))
model_data_2W_New <- model_data_2W_New %>% filter(!is.na(cibil_score_v3) & cibil_score_v3 != -1)


## 1.2 set OOT window
oot_date_start <- '2018-04-01'
oot_date_end <- '2018-06-30'


## 1.3 define OOT & DEV data for SENP-SEP 
oot_data <-
  model_data_2W_New %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                 (disbursal_date <= as.Date(oot_date_end)) &
                                 (Category %in% c('SENP','SEP','SAL')))

dev_data <-
  model_data_2W_New %>% filter(deal_no %notin% unique(oot_data$deal_no) &
                                 (Category %in% c('SENP','SEP','SAL')))


# model_data_2W_New <- rbind(dev_data,oot_data)

mean(dev_data$bad_loan)
mean(dev_data$ever_90dpd)
mean(dev_data$ever_150dpd)


rm(model_data_2W_New,cibil_v3)




## 2. Variable transformation ----------------------------------------------------------------

## 2.1 Vintage
dev_data$Var_credit_vintage_C <- ifelse(dev_data$Var_credit_vintage <= 0.01, 0.01, 
                                        ifelse(dev_data$Var_credit_vintage >= 12, 12, dev_data$Var_credit_vintage))
oot_data$Var_credit_vintage_C <- ifelse(oot_data$Var_credit_vintage <= 0.01, 0.01, 
                                        ifelse(oot_data$Var_credit_vintage >= 12, 12, oot_data$Var_credit_vintage))

## 2.2 Closed loans excl CC
dev_data$Var_PO_closed_excl_CC_C <- dev_data$Var_PO_closed_excl_CC
oot_data$Var_PO_closed_excl_CC_C <- oot_data$Var_PO_closed_excl_CC

dev_data$Var_PO_closed_excl_CC_C[is.na(dev_data$Var_PO_closed_excl_CC_C)] <- 0
oot_data$Var_PO_closed_excl_CC_C[is.na(oot_data$Var_PO_closed_excl_CC_C)] <- 0


subset1 <- dev_data %>% filter(Var_PO_closed_excl_CC_C <= 1)
subset2 <- dev_data %>% filter(Var_PO_closed_excl_CC_C == 2)
subset3 <- dev_data %>% filter(Var_PO_closed_excl_CC_C > 2)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <- get_3bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe2 <- get_3bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe3 <- get_3bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')


dev_data$Var_PO_closed_excl_CC_woe <- ifelse(dev_data$Var_PO_closed_excl_CC_C <= 1, woe1, 
                                              ifelse(dev_data$Var_PO_closed_excl_CC_C == 2, woe2,woe3))


oot_data$Var_PO_closed_excl_CC_woe <- ifelse(oot_data$Var_PO_closed_excl_CC_C <= 1, woe1, 
                                              ifelse(oot_data$Var_PO_closed_excl_CC_C == 2, woe2,woe3))


rm(subset1,subset2,subset3, woe1, woe2, woe3)




## 2.3 HL paid 12 month Continuous
dev_data$Var_DL_HL_paid_GE_12mon_C <- dev_data$Var_DL_HL_paid_GE_12mon
oot_data$Var_DL_HL_paid_GE_12mon_C <- oot_data$Var_DL_HL_paid_GE_12mon

dev_data$Var_DL_HL_paid_GE_12mon_C[is.na(dev_data$Var_DL_HL_paid_GE_12mon_C)] <- 0
oot_data$Var_DL_HL_paid_GE_12mon_C[is.na(oot_data$Var_DL_HL_paid_GE_12mon_C)] <- 0


dev_data$Var_DL_HL_paid_GE_12mon_C <- ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C >= 3, 3, 
                                              ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C <= 0, 0,dev_data$Var_DL_HL_paid_GE_12mon_C))

oot_data$Var_DL_HL_paid_GE_12mon_C <- ifelse(oot_data$Var_DL_HL_paid_GE_12mon_C >= 3, 3, 
                                              ifelse(oot_data$Var_DL_HL_paid_GE_12mon_C <= 0, 0, oot_data$Var_DL_HL_paid_GE_12mon_C))




## 2.4 gold paid 12 month Continuous
dev_data$Var_DL_Gold_paid_GE_12mon_C <- dev_data$Var_DL_Gold_paid_GE_12mon
oot_data$Var_DL_Gold_paid_GE_12mon_C <- oot_data$Var_DL_Gold_paid_GE_12mon

dev_data$Var_DL_Gold_paid_GE_12mon_C[is.na(dev_data$Var_DL_Gold_paid_GE_12mon_C)] <- 0
oot_data$Var_DL_Gold_paid_GE_12mon_C[is.na(oot_data$Var_DL_Gold_paid_GE_12mon_C)] <- 0


# dev_data$Var_DL_Gold_paid_GE_12mon_C <- ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C >= 7, 7, 
#                                                 ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C <= 0, 0,dev_data$Var_DL_Gold_paid_GE_12mon_C))
# 
# oot_data$Var_DL_Gold_paid_GE_12mon_C <- ifelse(oot_data$Var_DL_Gold_paid_GE_12mon_C >= 7, 7, 
#                                                 ifelse(oot_data$Var_DL_Gold_paid_GE_12mon_C <= 0, 0, oot_data$Var_DL_Gold_paid_GE_12mon_C))




subset1 <- dev_data %>% filter(Var_DL_Gold_paid_GE_12mon_C == 0)
subset2 <- dev_data %>% filter(Var_DL_Gold_paid_GE_12mon_C == 1)
subset3 <- dev_data %>% filter(Var_DL_Gold_paid_GE_12mon_C >= 2 )

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <- get_3bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe2 <- get_3bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe3 <- get_3bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')


dev_data$Var_DL_Gold_paid_GE_12mon_woe <- ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C == 0, woe1, 
                                                         ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C == 1, woe2,woe3))


oot_data$Var_DL_Gold_paid_GE_12mon_woe <- ifelse(oot_data$Var_DL_Gold_paid_GE_12mon_C == 0, woe1, 
                                                 ifelse(oot_data$Var_DL_Gold_paid_GE_12mon_C == 1, woe2,woe3))

rm(subset1,subset2,subset3, woe1, woe2, woe3)






## 2.5 Enquiry count 6m non CC
dev_data$Var_EN_enquiry_count_6m_non_CC_C <- dev_data$Var_EN_enquiry_count_6m_non_CC
oot_data$Var_EN_enquiry_count_6m_non_CC_C <- oot_data$Var_EN_enquiry_count_6m_non_CC

dev_data$Var_EN_enquiry_count_6m_non_CC_C[is.na(dev_data$Var_EN_enquiry_count_6m_non_CC_C)] <- 0
oot_data$Var_EN_enquiry_count_6m_non_CC_C[is.na(oot_data$Var_EN_enquiry_count_6m_non_CC_C)] <- 0


dev_data$Var_EN_enquiry_count_6m_non_CC_C <- ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C <= 0, 0, 
                                                     ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C >= 6, 6, dev_data$Var_EN_enquiry_count_6m_non_CC_C))

oot_data$Var_EN_enquiry_count_6m_non_CC_C <- ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C <= 0, 0, 
                                                     ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C >= 6, 6, oot_data$Var_EN_enquiry_count_6m_non_CC_C))




## 2.6 30 DPD in 6 mon
dev_data$Var_DL_all_30dpd_6mon_C <- dev_data$Var_DL_all_30dpd_6mon
oot_data$Var_DL_all_30dpd_6mon_C <- oot_data$Var_DL_all_30dpd_6mon

dev_data$Var_DL_all_30dpd_6mon_C[is.na(dev_data$Var_DL_all_30dpd_6mon_C)] <- 0
oot_data$Var_DL_all_30dpd_6mon_C[is.na(oot_data$Var_DL_all_30dpd_6mon_C)] <- 0


dev_data$Var_DL_all_30dpd_6mon_flag <- ifelse(dev_data$Var_DL_all_30dpd_6mon_C == 0, 0, 1)
oot_data$Var_DL_all_30dpd_6mon_flag <- ifelse(oot_data$Var_DL_all_30dpd_6mon_C == 0, 0, 1)

# dev_data$Var_DL_all_30dpd_6mon_C <- ifelse(dev_data$Var_DL_all_30dpd_6mon_C <= 0, 0, 
#                                             ifelse(dev_data$Var_DL_all_30dpd_6mon_C >= 5, 5, dev_data$Var_DL_all_30dpd_6mon_C))
# 
# oot_data$Var_DL_all_30dpd_6mon_C <- ifelse(oot_data$Var_DL_all_30dpd_6mon_C <= 0, 0, 
#                                             ifelse(oot_data$Var_DL_all_30dpd_6mon_C >= 5, 5, oot_data$Var_DL_all_30dpd_6mon_C))



## 2.7 SENP_SEP Flag
dev_data$category_flag_SENP_SEP <- ifelse(dev_data$Category %in% c('SENP', 'SEP'), 1, 0)
oot_data$category_flag_SENP_SEP <- ifelse(oot_data$Category %in% c('SENP', 'SEP'), 1, 0)


## 2.8 Agri profile flag
dev_data$agri_profile_flag <- ifelse(is.na(dev_data$agri_profile_flag), 0 , dev_data$agri_profile_flag)
oot_data$agri_profile_flag <- ifelse(is.na(oot_data$agri_profile_flag), 0 , oot_data$agri_profile_flag)



## 2.9 Cross tab - outstanding by sanctioned amount live loan  x total sanctioned amount live

## 2.9.1 sanctioned amount live loan
dev_data$Var_sanctioned_amount_live_loans_C <- dev_data$Var_sanctioned_amount_live_loans
oot_data$Var_sanctioned_amount_live_loans_C <- oot_data$Var_sanctioned_amount_live_loans


dev_data$Var_sanctioned_amount_live_loans_C[is.na(dev_data$Var_sanctioned_amount_live_loans_C)] <- 0
oot_data$Var_sanctioned_amount_live_loans_C[is.na(oot_data$Var_sanctioned_amount_live_loans_C)] <- 0



## 2.9.2 Outstanding amount by sanctioned amount in live loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- dev_data$Var_outstanding_by_sanctioned_amount_live_loans
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- oot_data$Var_outstanding_by_sanctioned_amount_live_loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 1
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 1


## 2.9.3 cross tab

dev_data$Var_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_sanctioned_amount_live_loans_C <= 175000, 'bin_LE_175k', 
                                                        ifelse(dev_data$Var_sanctioned_amount_live_loans_C > 175000 & dev_data$Var_sanctioned_amount_live_loans_C <= 1000000, 'bin_175k_to_10L', 'bin_GE_10L'))


oot_data$Var_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_sanctioned_amount_live_loans_C <= 175000, 'bin_LE_175k', 
                                                        ifelse(oot_data$Var_sanctioned_amount_live_loans_C > 175000 & oot_data$Var_sanctioned_amount_live_loans_C <= 1000000, 'bin_175k_to_10L', 'bin_GE_10L'))



dev_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.25 & dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.5, 'val_25_to_50_pct','val_GE_50_pct'))


oot_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.25 & oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.5, 'val_25_to_50_pct','val_GE_50_pct'))




subset1 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct')))
subset2 <- dev_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct'))))
subset3 <- dev_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_50_pct','val_GE_50_pct'))))
subset4 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct')))

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)

woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')


subset1_oot <- oot_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct')))
subset2_oot <- oot_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct'))))
subset3_oot <- oot_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_50_pct','val_GE_50_pct'))))
subset4_oot <- oot_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct')))


dev_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live <- ifelse(dev_data$deal_no %in% unique(subset1$deal_no), woe1,
                                                                        ifelse(dev_data$deal_no %in% unique(subset2$deal_no), woe2,
                                                                               ifelse(dev_data$deal_no %in% unique(subset3$deal_no), woe3,
                                                                                      woe4)))

oot_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live <- ifelse(oot_data$deal_no %in% unique(subset1_oot$deal_no), woe1,
                                                                        ifelse(oot_data$deal_no %in% unique(subset2_oot$deal_no), woe2,
                                                                               ifelse(oot_data$deal_no %in% unique(subset3_oot$deal_no), woe3,
                                                                                      woe4)))


rm(subset1,subset2,subset3,subset4,subset1_oot,subset2_oot,subset3_oot,subset4_oot, woe1, woe2, woe3, woe4)






## 3. Model building ----------------------------------------------------------------------------------------

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





seed_iterations_output <- get_seed_iterations(seed_list = c(1:200),
                                              dev_data = dev_data,
                                              oot = oot_data,
                                              feature_list = shortlisted_var, 
                                              p_value_threshold = 0.05)

selected_seeds <- seed_iterations_output %>% filter((p_value_check == 0) & 
                                                      (RO_decile_overall >= 3) & 
                                                      (RO_pentile_overall == 3))





# selected_seed <- 175
selected_seed <- 101

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


save_xlsx_output(data = output_list, relative_path = "//model//2W_New//Combined//vcheck//Model_summary_2W_New_Combined_dpd_check.xlsx")


# model_2W <- final_model
# saveRDS(model_2W, file = file.path(get_data_path()$data$model,"model_2W_New.rds"))

model_2W <- final_model
saveRDS(model_2W, file = file.path(get_data_path()$data$model,"//vcheck//model_2W_New.rds"))






assert_data_non_empty(train_data_final)
save(train_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "2W_New",
       "Combined",
       "vcheck",
       "dpd_check",
       "train_data.rdata"
     )
)


assert_data_non_empty(test_data_final)
save(test_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "2W_New",
       "Combined",
       "vcheck",
       "dpd_check",
       "test_data.rdata"
     )
)


assert_data_non_empty(oot_data_final)
save(oot_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "2W_New",
       "Combined",
       "vcheck",
       "dpd_check",
       "oot_data.rdata"
     )
)

