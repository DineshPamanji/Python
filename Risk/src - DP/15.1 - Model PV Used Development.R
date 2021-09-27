#############################################################################################
################## 15 - Model PV Used ######################################################
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



## 1. Load data & define functions ----------------------------------------------------------

## 1.1 Load 2W New data 
load_rdata_intermediate("model_data//model_data_PV_Used_vcheck.rdata")
load_rdata_intermediate("model_data//model_data_PV_Used_dpd_check.rdata")


load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
cibil_v3 <- distinct(cibil_v3 %>% dplyr::select(deal_no,customer_code,cibil_score_v3))



model_data_PV_Used <- left_join(model_data_PV_Used,cibil_v3,by=c('deal_no','customer_code'))
model_data_PV_Used <- model_data_PV_Used %>% filter(!is.na(cibil_score_v3) & cibil_score_v3 != -1)



## 1.2 set OOT window
oot_date_start <- '2018-04-01'
oot_date_end <- '2018-06-30'

## 1.3 define OOT & DEV data for SENP-SEP 
oot_data <-
  model_data_PV_Used %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                  (disbursal_date <= as.Date(oot_date_end)) &
                                  (Category %in% c('SENP','SEP','SAL')))

dev_data <-
  model_data_PV_Used %>% filter(deal_no %notin% unique(oot_data$deal_no) &
                                  (Category %in% c('SENP','SEP','SAL')))


# model_data_PV_Used <- rbind(dev_data,oot_data)
# mean(model_data_PV_Used$bad_loan)
rm(model_data_PV_Used,cibil_v3)


mean(dev_data$ever_150dpd)
mean(dev_data$ever_90dpd)
mean(dev_data$bad_loan)


## 2. Variable transformation ----------------------------------------------------------------

## Vintage
dev_data$Var_credit_vintage_C <- ifelse(dev_data$Var_credit_vintage <= 0.01, 0.01, 
                                         ifelse(dev_data$Var_credit_vintage >= 12, 12, dev_data$Var_credit_vintage))
oot_data$Var_credit_vintage_C <- ifelse(oot_data$Var_credit_vintage <= 0.01, 0.01, 
                                         ifelse(oot_data$Var_credit_vintage >= 12, 12, oot_data$Var_credit_vintage))


### Unsecured Enquiry count 12m non CC
dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C <- dev_data$Var_EN_unsec_enquiry_count_12m_non_CC
oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C <- oot_data$Var_EN_unsec_enquiry_count_12m_non_CC

dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C[is.na(dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C)] <- 0
oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C[is.na(oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C)] <- 0

# subset1 <- dev_data %>% filter(Var_EN_unsec_enquiry_count_12m_non_CC_C <= 2)
# subset2 <- dev_data %>% filter(Var_EN_unsec_enquiry_count_12m_non_CC_C %in% c(3,4))
# subset3 <- dev_data %>% filter(Var_EN_unsec_enquiry_count_12m_non_CC_C == 5)
# subset4 <- dev_data %>% filter(Var_EN_unsec_enquiry_count_12m_non_CC_C == 6)
# subset5 <- dev_data %>% filter(Var_EN_unsec_enquiry_count_12m_non_CC_C >= 7)


subset1 <- dev_data %>% filter(Var_EN_unsec_enquiry_count_12m_non_CC_C == 0)
subset2 <- dev_data %>% filter(Var_EN_unsec_enquiry_count_12m_non_CC_C %in% c(1,2))
subset3 <- dev_data %>% filter(Var_EN_unsec_enquiry_count_12m_non_CC_C == 3)
subset4 <- dev_data %>% filter(Var_EN_unsec_enquiry_count_12m_non_CC_C >= 4)
# subset5 <- dev_data %>% filter(Var_EN_unsec_enquiry_count_12m_non_CC_C >= 5)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)
# mean(subset5$bad_loan)


# woe1 <- get_5bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, bin5 = subset5, dependent_feature = 'bad_loan')
# woe2 <- get_5bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, bin5 = subset5, dependent_feature = 'bad_loan')
# woe3 <- get_5bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, bin5 = subset5, dependent_feature = 'bad_loan')
# woe4 <- get_5bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, bin5 = subset5, dependent_feature = 'bad_loan')
# woe5 <- get_5bin_woe(focus_bin = subset5, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, bin5 = subset5, dependent_feature = 'bad_loan')

woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, dependent_feature = 'bad_loan')


# woe1 # 0.1243837
# woe2 # -0.1093076
# woe3 # -0.5157193
# woe4 # -0.5530151
# woe5 # -0.7677482


# woe1 <- 0.1107157
# woe2 <- -0.1002811
# woe3 <- -0.654442
# woe4 <- -0.8558241

# dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_woe <- ifelse(dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C <= 2, woe1, 
#                                                               ifelse(dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C %in% c(3,4), woe2,
#                                                                      ifelse(dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C == 5, woe3,
#                                                                             ifelse(dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C == 6, woe4,
#                                                                                    woe5))))
# 
# 
# oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_woe <- ifelse(oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C <= 2, woe1, 
#                                                               ifelse(oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C %in% c(3,4), woe2,
#                                                                      ifelse(oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C == 5, woe3,
#                                                                             ifelse(oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C == 6, woe4,
#                                                                                    woe5))))

dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_woe <- ifelse(dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C == 0, woe1, 
                                                             ifelse(dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C %in% c(1,2), woe2,
                                                                    ifelse(dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C == 3, woe3, woe4)))


oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_woe <- ifelse(oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C == 0, woe1, 
                                                             ifelse(oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C %in% c(1,2), woe2,
                                                                    ifelse(oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C == 3, woe3, woe4)))

rm(subset1,subset2,subset3, subset4)
# rm(woe1, woe2, woe3,woe4,woe5)





### SENP_SEP Flag
dev_data$category_flag_SENP_SEP <- ifelse(dev_data$Category %in% c('SENP', 'SEP'), 1, 0)
oot_data$category_flag_SENP_SEP <- ifelse(oot_data$Category %in% c('SENP', 'SEP'), 1, 0)







## 2.9 Cross tab - outstanding by sanctioned amount loan  x total sanctioned amount closed

## 2.9.1 sanctioned amount closed loan

dev_data$Var_PO_closed_sanction_amount_excl_CC[is.na(dev_data$Var_PO_closed_sanction_amount_excl_CC)] <- 0
oot_data$Var_PO_closed_sanction_amount_excl_CC[is.na(oot_data$Var_PO_closed_sanction_amount_excl_CC)] <- 0



## 2.9.2 Outstanding amount by sanctioned amount in live loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- dev_data$Var_outstanding_by_sanctioned_amount_live_loans
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- oot_data$Var_outstanding_by_sanctioned_amount_live_loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 0.7
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 0.7


## 2.9.3 cross tab

dev_data$Var_PO_closed_sanction_amount_excl_CC_bin <-  ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC <= 100000,'bin_LE_1L',
                                                              ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC > 100000 & dev_data$Var_PO_closed_sanction_amount_excl_CC <= 350000,'bin_1L_to_3.5L','bin_GE_3.5L'))

oot_data$Var_PO_closed_sanction_amount_excl_CC_bin <-  ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC <= 100000,'bin_LE_1L',
                                                              ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC > 100000 & oot_data$Var_PO_closed_sanction_amount_excl_CC <= 350000,'bin_1L_to_3.5L','bin_GE_3.5L'))



dev_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.25 & dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.75, 'val_25_to_75_pct','val_GE_75_pct'))


oot_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.25 & oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.75, 'val_25_to_75_pct','val_GE_75_pct'))



# test <- dev_data %>% group_by(Var_PO_closed_sanction_amount_excl_CC_bin, Var_outstanding_by_sanctioned_amount_live_loans_bin)  %>% summarise(n=n(),
#                                                                                                                                 default = mean(bad_loan))
# write.csv(test, "combined_model_cross_tab_PV_Used_vcheck.csv")


subset1 <- dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_1L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct')))
subset2 <- dev_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_1L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_75_pct'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_1L_to_3.5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_75_pct','val_GE_75_pct'))))
subset3 <- dev_data %>% filter( (Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_3.5L')) | (Var_outstanding_by_sanctioned_amount_live_loans_C %in% c('val_LE_25_pct')) | ( (Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_1L_to_3.5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct'))   ))

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)

woe1 <- get_3bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe2 <- get_3bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe3 <- get_3bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')


# woe 1 = -0.2972441
# woe 2 = -0.06637368
# woe 3 = 0.2122154

subset1_oot <- oot_data %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_1L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_75_pct')))
subset2_oot <- oot_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_1L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_75_pct'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_1L_to_3.5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_75_pct','val_GE_75_pct'))))
subset3_oot <- oot_data %>% filter( (Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_3.5L')) | (Var_outstanding_by_sanctioned_amount_live_loans_C %in% c('val_LE_25_pct')) | ( (Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_1L_to_3.5L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct'))   ))


dev_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed_woe <- ifelse(dev_data$deal_no %in% unique(subset1$deal_no), woe1,
                                                                          ifelse(dev_data$deal_no %in% unique(subset2$deal_no), woe2,
                                                                                 woe3))

oot_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed_woe <- ifelse(oot_data$deal_no %in% unique(subset1_oot$deal_no), woe1,
                                                                          ifelse(oot_data$deal_no %in% unique(subset2_oot$deal_no), woe2,
                                                                                 woe3))


rm(subset1,subset2,subset3,subset1_oot,subset2_oot,subset3_oot, woe1, woe2, woe3)






## 60 DPD in 3 mon
dev_data$Var_DL_all_60dpd_3mon_C <- dev_data$Var_DL_all_60dpd_3mon
oot_data$Var_DL_all_60dpd_3mon_C <- oot_data$Var_DL_all_60dpd_3mon

dev_data$Var_DL_all_60dpd_3mon_C[is.na(dev_data$Var_DL_all_60dpd_3mon_C)] <- 0
oot_data$Var_DL_all_60dpd_3mon_C[is.na(oot_data$Var_DL_all_60dpd_3mon_C)] <- 0


dev_data$Var_DL_all_60dpd_3mon_C <- ifelse(dev_data$Var_DL_all_60dpd_3mon_C <= 0, 0, 
                                            ifelse(dev_data$Var_DL_all_60dpd_3mon_C >= 2, 2, dev_data$Var_DL_all_60dpd_3mon_C))

oot_data$Var_DL_all_60dpd_3mon_C <- ifelse(oot_data$Var_DL_all_60dpd_3mon_C <= 0, 0, 
                                            ifelse(oot_data$Var_DL_all_60dpd_3mon_C >= 2, 2, oot_data$Var_DL_all_60dpd_3mon_C))



###

shortlisted_var <- c(
  "Var_credit_vintage_C",
  "cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed_woe",
  "category_flag_SENP_SEP",
  "Var_EN_unsec_enquiry_count_12m_non_CC_woe"
  # "Var_DL_all_60dpd_3mon_C"
)




seed_iterations_output <- get_seed_iterations(seed_list = c(1:200),
                                                      dev_data = dev_data,
                                                      oot = oot_data,
                                                      feature_list = shortlisted_var, 
                                                      p_value_threshold = 0.1)

selected_seeds <- seed_iterations_output %>% filter((p_value_check == 0) &
                                                      (RO_decile_overall >= 0) &
                                                      (RO_pentile_overall >= 2))



# write.csv(seed_iterations_output, "seed_iterations_combined.csv")


# selected_seed <- 47
selected_seed <- 158

set.seed(selected_seed)

split <- sample.split(dev_data$bad_loan, SplitRatio = 0.80)
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

# save_xlsx_output(data = output_list, relative_path = "//model//PV_Used//Combined//vcheck//Model_summary_PV_New_Combined_vf.xlsx")
save_xlsx_output(data = output_list, relative_path = "//model//PV_Used//Combined//vcheck//Model_summary_PV_New_Combined_dpd_check.xlsx")


model_PV_Used <- final_model
# saveRDS(model_PV, file = file.path(get_data_path()$data$model,"model_PV_New.rds"))
# saveRDS(model_PV, file = file.path("output//model_PV_New.rds"))

saveRDS(model_PV_Used, file = file.path(get_data_path()$data$model,"//vcheck//model_PV_Used.rds"))






assert_data_non_empty(train_data_final)
save(train_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "PV_Used",
       "Combined",
       "vcheck",
       "train_data.rdata"
     )
)

assert_data_non_empty(test_data_final)
save(test_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "PV_Used",
       "Combined",
       "vcheck",
       "test_data.rdata"
     )
)


assert_data_non_empty(oot_data_final)
save(oot_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "PV_Used",
       "Combined",
       "vcheck",
       "oot_data.rdata"
     )
)










rm(list=ls())


