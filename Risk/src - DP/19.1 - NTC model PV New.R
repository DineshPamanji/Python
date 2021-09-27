
## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

model_functions <- file.path("src", "utils", "model_functions.R")
source(model_functions)

options(scipen = 999)
voptions(raise = "all")


`%notin%` <- Negate(`%in%`)


## 1. Load data ----------------------------------------------------------------------------
# load_rdata_intermediate("model_data//ntc_model_data_PV_New.rdata")
load_rdata_intermediate("model_data//ntc_data.rdata")


## 2. Define oot & dev data ----------------------------------------------------------------

# state_region_mapping <- fread("data/mapping/state_region_mapping.csv")
# ntc_model_data$Region <- NULL
# ntc_model_data <- left_join(ntc_model_data, state_region_mapping, by=c('disbursal_state'))


## 1.2 set OOT window
oot_date_start <- '2018-04-01'
oot_date_end <- '2018-06-30'


## 1.3 define OOT & DEV data for SENP-SEP 
oot_data <-
  ntc_model_data %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                              (disbursal_date <= as.Date(oot_date_end)) &
                              (loan_type == 'PV-New') &
                              (Category %in% c('SENP','SEP','SAL')))

dev_data <-
  ntc_model_data %>% filter(deal_no %notin% unique(oot_data$deal_no) &
                              (loan_type == 'PV-New') &
                              (Category %in% c('SENP','SEP','SAL')))

model_data <- rbind(dev_data,oot_data)

T1 <- model_data %>% summarise(default = mean(bad_loan),
                         default_90 = mean(ever_90dpd),
                         default_150 = mean(ever_150dpd))




rm(ntc_model_data)


## 3. Variable creation ----------------------------------------------------------------------

## 3.1 Category flag
dev_data$category_flag_SENP_SEP <- ifelse(dev_data$Category %in% c('SENP', 'SEP'), 1, 0)
oot_data$category_flag_SENP_SEP <- ifelse(oot_data$Category %in% c('SENP', 'SEP'), 1, 0)

dev_data %>% group_by(category_flag_SENP_SEP) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))

## 3.2 agri profile flag
dev_data$agri_profile_flag <- ifelse(is.na(dev_data$agri_profile_flag), 0 , dev_data$agri_profile_flag)
oot_data$agri_profile_flag <- ifelse(is.na(oot_data$agri_profile_flag), 0 , oot_data$agri_profile_flag)

dev_data %>% group_by(agri_profile_flag) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))

## 3.3 region wise woe

## remove NA


dev_data <- dev_data %>% filter(!is.na(Region))
oot_data <- oot_data %>% filter(!is.na(Region))

subset1 <- dev_data %>% filter(Region == 'South')
subset2 <- dev_data %>% filter(Region == 'Central')
subset3 <- dev_data %>% filter(Region == 'West')
subset4 <- dev_data %>% filter(Region == 'North')
subset5 <- dev_data %>% filter(Region == 'East')

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)
mean(subset5$bad_loan)


woe1 <- get_5bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, bin5 = subset5, dependent_feature = 'bad_loan')
woe2 <- get_5bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, bin5 = subset5, dependent_feature = 'bad_loan')
woe3 <- get_5bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, bin5 = subset5, dependent_feature = 'bad_loan')
woe4 <- get_5bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, bin5 = subset5, dependent_feature = 'bad_loan')
woe5 <- get_5bin_woe(focus_bin = subset5, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, bin5 = subset5, dependent_feature = 'bad_loan')

# woe1 <- -0.235776
# woe2 <- -0.06171671
# woe3 <- 0.2854523
# woe4 <- 0.4097233
# woe5 <- 0.5042479

dev_data$region_woe <- ifelse(dev_data$Region == 'South', woe1, 
                              ifelse(dev_data$Region == 'Central', woe2,
                                     ifelse(dev_data$Region == 'West', woe3,
                                            ifelse(dev_data$Region == 'North', woe4, woe5))))

oot_data$region_woe <- ifelse(oot_data$Region == 'South', woe1, 
                              ifelse(oot_data$Region == 'Central', woe2,
                                     ifelse(oot_data$Region == 'West', woe3,
                                            ifelse(oot_data$Region == 'North', woe4, woe5))))


rm(subset1,subset2,subset3,subset4,subset5,woe1,woe2,woe3,woe4,woe5)




## 3.4 No. of phones reported in 3m

## check raw bi-variate
dev_data %>% group_by(phones_reported_3m) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))

## fill missing values
dev_data$phones_reported_3m_C <- ifelse(is.na(dev_data$phones_reported_3m), 1 , dev_data$phones_reported_3m)
oot_data$phones_reported_3m_C <- ifelse(is.na(oot_data$phones_reported_3m), 1 , oot_data$phones_reported_3m)

## transformation
dev_data$phones_reported_3m_C1 <- ifelse(dev_data$phones_reported_3m_C >= 4, 4, 
                                         ifelse(dev_data$phones_reported_3m_C <= 1, 1, dev_data$phones_reported_3m_C))
oot_data$phones_reported_3m_C1 <- ifelse(oot_data$phones_reported_3m_C >= 4, 4, 
                                         ifelse(oot_data$phones_reported_3m_C <= 1, 1, oot_data$phones_reported_3m_C))

## check final bivariate
dev_data %>% group_by(phones_reported_3m_C1) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))






## 3.5 No. of phones reported in 6m

## check raw bi-variate
dev_data %>% group_by(phones_reported_6m) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))

## fill missing values
dev_data$phones_reported_6m_C <- ifelse(is.na(dev_data$phones_reported_6m), 0 , dev_data$phones_reported_6m)
oot_data$phones_reported_6m_C <- ifelse(is.na(oot_data$phones_reported_6m), 0 , oot_data$phones_reported_6m)

## transformation
dev_data$phones_reported_6m_C1 <- ifelse(dev_data$phones_reported_6m_C >= 3, 3, 
                                         ifelse(dev_data$phones_reported_6m_C <= 0, 0, dev_data$phones_reported_6m_C))
oot_data$phones_reported_6m_C1 <- ifelse(oot_data$phones_reported_6m_C >= 3, 3, 
                                         ifelse(oot_data$phones_reported_6m_C <= 0, 0, oot_data$phones_reported_6m_C))

## check final bivariate
dev_data %>% group_by(phones_reported_6m_C1) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))






## 3.6 No. of phones reported in 12m

## check raw bi-variate
dev_data %>% group_by(phones_reported_12m) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))

## fill missing values
dev_data$phones_reported_12m_C <- ifelse(is.na(dev_data$phones_reported_12m), 1 , dev_data$phones_reported_12m)
oot_data$phones_reported_12m_C <- ifelse(is.na(oot_data$phones_reported_12m), 1 , oot_data$phones_reported_12m)

## transformation
dev_data$phones_reported_12m_C1 <- ifelse(dev_data$phones_reported_12m_C >= 4, 4, 
                                          ifelse(dev_data$phones_reported_12m_C <= 0, 0, dev_data$phones_reported_12m_C))
oot_data$phones_reported_12m_C1 <- ifelse(oot_data$phones_reported_12m_C >= 4, 4, 
                                          ifelse(oot_data$phones_reported_12m_C <= 0, 0, oot_data$phones_reported_12m_C))

## check final bivariate
dev_data %>% group_by(phones_reported_12m_C1) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))





## 3.7 No. of addresses reported in 3m

## check raw bi-variate
dev_data %>% group_by(addresses_reported_3m) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))

## fill missing values
dev_data$addresses_reported_3m_C <- ifelse(is.na(dev_data$addresses_reported_3m), 0 , dev_data$addresses_reported_3m)
oot_data$addresses_reported_3m_C <- ifelse(is.na(oot_data$addresses_reported_3m), 0 , oot_data$addresses_reported_3m)

## transformation
dev_data$addresses_reported_3m_C1 <- ifelse(dev_data$addresses_reported_3m_C >= 3, 3, 
                                            ifelse(dev_data$addresses_reported_3m_C <= 0, 0, dev_data$addresses_reported_3m_C))
oot_data$addresses_reported_3m_C1 <- ifelse(oot_data$addresses_reported_3m_C >= 3, 3, 
                                            ifelse(oot_data$addresses_reported_3m_C <= 0, 0, oot_data$addresses_reported_3m_C))

## check final bivariate
dev_data %>% group_by(addresses_reported_3m_C1) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))







## 3.8 No. of addresses reported in 6m

## check raw bi-variate
dev_data %>% group_by(addresses_reported_6m) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))

## fill missing values
dev_data$addresses_reported_6m_C <- ifelse(is.na(dev_data$addresses_reported_6m), 0 , dev_data$addresses_reported_6m)
oot_data$addresses_reported_6m_C <- ifelse(is.na(oot_data$addresses_reported_6m), 0 , oot_data$addresses_reported_6m)

## transformation
dev_data$addresses_reported_6m_C1 <- ifelse(dev_data$addresses_reported_6m_C >= 5, 5, 
                                            ifelse(dev_data$addresses_reported_6m_C <= 0, 0, dev_data$addresses_reported_6m_C))
oot_data$addresses_reported_6m_C1 <- ifelse(oot_data$addresses_reported_6m_C >= 5, 5, 
                                            ifelse(oot_data$addresses_reported_6m_C <= 0, 0, oot_data$addresses_reported_6m_C))

## check final bivariate
dev_data %>% group_by(addresses_reported_6m_C1) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))






## 3.9 No. of addresses reported in 12m

## check raw bi-variate
dev_data %>% group_by(addresses_reported_12m) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))

## fill missing values
dev_data$addresses_reported_12m_C <- ifelse(is.na(dev_data$addresses_reported_12m), 0 , dev_data$addresses_reported_12m)
oot_data$addresses_reported_12m_C <- ifelse(is.na(oot_data$addresses_reported_12m), 0 , oot_data$addresses_reported_12m)

## transformation
dev_data$addresses_reported_12m_C1 <- ifelse(dev_data$addresses_reported_12m_C >= 4, 4, 
                                             ifelse(dev_data$addresses_reported_12m_C <= 0, 0, dev_data$addresses_reported_12m_C))
oot_data$addresses_reported_12m_C1 <- ifelse(oot_data$addresses_reported_12m_C >= 4, 4, 
                                             ifelse(oot_data$addresses_reported_12m_C <= 0, 0, oot_data$addresses_reported_12m_C))

## check final bivariate
dev_data %>% group_by(addresses_reported_12m_C1) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))






## 3.10 Asset tag variable

## check raw bi-variate
dev_data %>% group_by(asset_tag) %>% summarise(disbursal_count=n(),default_pct = mean(bad_loan)) %>% mutate(data_pct = disbursal_count/nrow(dev_data))
dev_data$asset_tag <- ifelse(is.na(dev_data$asset_tag), 'OTHERS' , dev_data$asset_tag)
oot_data$asset_tag <- ifelse(is.na(oot_data$asset_tag), 'OTHERS' , oot_data$asset_tag)


subset1 <- dev_data %>% filter(asset_tag == 'AMUV')
subset2 <- dev_data %>% filter(asset_tag == 'BCAR')
subset3 <- dev_data %>% filter(asset_tag %notin% c('AMUV','BCAR','ACAR'))
subset4 <- dev_data %>% filter(asset_tag == 'ACAR')

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)


woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4, dependent_feature = 'bad_loan')


# woe1 <- -0.1039703
# woe2 <- -0.01628752
# woe3 <- 0.0009001635
# woe4 <- 0.03465383

dev_data$asset_tag_woe <- ifelse(dev_data$asset_tag == 'AMUV', woe1, 
                                 ifelse(dev_data$asset_tag == 'BCAR', woe2,
                                        ifelse(dev_data$asset_tag == 'ACAR', woe4, woe3)))

oot_data$asset_tag_woe <- ifelse(oot_data$asset_tag == 'AMUV', woe1, 
                                 ifelse(oot_data$asset_tag == 'BCAR', woe2,
                                        ifelse(oot_data$asset_tag == 'ACAR', woe4, woe3)))


rm(subset1,subset2,subset3,subset4,woe1,woe2,woe3,woe4)








## 4. Stepwise model ----------------------------------------------------------------------------------------------------------

## 4.1 define features for model
selected_features <- c( 
  'category_flag_SENP_SEP',
  'agri_profile_flag',
  'region_woe',
  'phones_reported_3m_C1',
  'phones_reported_6m_C1',
  'phones_reported_12m_C1',
  'addresses_reported_3m_C1',
  'addresses_reported_6m_C1',
  'addresses_reported_12m_C1'
  # 'asset_tag_woe'
)


set.seed(1)
split <- sample.split(dev_data$bad_loan, SplitRatio = 0.8)
train_data_stepwise <- data.frame(subset(dev_data, split == TRUE))
test_data_stepwise <- data.frame(subset(dev_data, split == FALSE))


loop_model <- glm(bad_loan ~., data = train_data_stepwise %>% dplyr::select(c(selected_features,'bad_loan')), family = binomial("logit"))

loop_model_selected <- stepAIC(loop_model,
                               family = binomial,
                               data = train_data,
                               k = qchisq(0.05,1, lower.tail = F))

# loop_model_tidy <- tidy(loop_model_selected)
summary(loop_model_selected)


train_data_stepwise <- get_predictions(loop_model_selected, train_data_stepwise)
test_data_stepwise <- get_predictions(loop_model_selected, test_data_stepwise)
oot_data_stepwise <- get_predictions(loop_model_selected, oot_data)



model_performance_stepwise <- get_model_performance(train_data_stepwise,test_data_stepwise,oot_data_stepwise)
model_performance_stepwise

rm(train_data_stepwise,test_data_stepwise,oot_data_stepwise)
rm(model_performance_stepwise,loop_model,loop_model_selected)



## 5. Single variable model -----------------------------------------------------------------------------------------------
set.seed(1)
split <- sample.split(dev_data$bad_loan, SplitRatio = 0.8)
train_data_temp <- data.frame(subset(dev_data, split == TRUE))
test_data_temp <- data.frame(subset(dev_data, split == FALSE))


## single feature model - iteration
single_variable_models <- get_single_variable_models(train = train_data_temp, test = test_data_temp, oot = oot_data, feature_list = selected_features)
rm(train_data_temp, test_data_temp)



## 6. Run final model ----------------------------------------------------------------------------------------------------

shortlisted_var <- c( 
  'region_woe',
  # 'category_flag_SENP_SEP',
  # 'asset_tag_woe',
  'agri_profile_flag',
  'phones_reported_12m_C1',
  'addresses_reported_12m_C1'
  # 'phones_reported_6m_C1',
  # 'addresses_reported_6m_C1'
)

## 6.1 run 100 seed iterations
seed_iterations_output <- get_seed_iterations(seed_list = c(1:100),
                                              dev_data = dev_data,
                                              oot = oot_data,
                                              feature_list = shortlisted_var, 
                                              p_value_threshold = 0.05)

## 6.2 shortlist best models based on p-value & rank ordering
selected_seeds <- seed_iterations_output %>% filter((p_value_check == 0) & 
                                                      # (RO_decile_overall >= 1) & 
                                                      (RO_pentile_overall >= 1))





## 6.3 build final model
# selected_seed <- 89
# set.seed(selected_seed)

selected_seed <- 89
set.seed(selected_seed)

## 6.4 split train - test
split <- sample.split(dev_data$bad_loan, SplitRatio = 0.8)
train_data_final <- data.frame(subset(dev_data, split == TRUE))
test_data_final <- data.frame(subset(dev_data, split == FALSE))

## 6.5 build model
final_model <- glm(bad_loan ~., data = train_data_final %>% dplyr::select(c(shortlisted_var,'bad_loan')), family = binomial("logit"))
summary(final_model)


## 6.6 make model predictions
train_data_final <- get_predictions(final_model, train_data_final)
test_data_final <- get_predictions(final_model, test_data_final)
oot_data_final <- get_predictions(final_model, oot_data)


## 6.7 get model performance
model_performance_final <- get_model_performance(train_data_final,test_data_final,oot_data_final)
model_performance_final


## 6.7 get RO

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






## 7. Save outputs ----------------------------------------------------------------------------------------------
output_list <- list("model_summary" = data.frame(tidy(final_model)),
                    "model_performance" = model_performance_final,
                    "Var_Imp" = get_variable_importance(final_model),
                    "VIF" = get_vif(final_model),
                    "wald_chi_sq" = get_wald_chi_sq(final_model),
                    "seed" = data.frame(selected_seed),
                    # "NTC_cibil_overlay" = data.frame(ntc_cibil_crosstab),
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


save_xlsx_output(data = output_list, relative_path = "//model//PV_New//NTC//vcheck//Model_summary_PV_New_NTC.xlsx")
 # write_xlsx(output_list, path = "data//output//model//PV_New//NTC//vcheck//Model_summary_PV_New_NTC.xlsx")


model_PV_NTC <- final_model
saveRDS(model_PV_NTC, file = file.path(get_data_path()$data$model,"//vcheck//model_PV_New_NTC.rds"))
# saveRDS(model_PV_NTC, file = file.path("data//output//model//PV_New//NTC//vcheck//model_PV_New_NTC.rds"))






assert_data_non_empty(train_data_final)
save(train_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "PV_New",
       "NTC",
       "vcheck",
       "train_data.rdata"
     )
)


assert_data_non_empty(test_data_final)
save(test_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "PV_New",
       "NTC",
       "vcheck",
       "test_data.rdata"
     )
)


assert_data_non_empty(oot_data_final)
save(oot_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "PV_New",
       "NTC",
       "vcheck",
       "oot_data.rdata"
     )
)



rm(RO_overall,RO_overall_pentile,RO_overall_pentile_scored,RO_overall_scored,
   RO_SAL,RO_SAL_pentile,RO_SAL_pentile_scored,RO_SAL_scored,
   RO_SENP_SEP,RO_SENP_SEP_pentile,RO_SENP_SEP_pentile_scored,RO_SENP_SEP_scored)

rm(dev_data,model_performance_final,ntc_model_data,oot_data)

# rm(list=ls())


rm(seed_iterations_output,selected_seeds,single_variable_models)




### 8. Validation ---------------------------------------------------------------------------------------------------------------------


## 8.1. Validation - Temporal Decay --------------------------------------------------------------

## 8.1.1 load data
load_rdata_intermediate("ADS_data//temporal_decay.rdata")

## 8.1.2 select required data & rename columns
temporal_decay <- temporal_decay %>% filter(loan_type == 'PV-New')
colnames(temporal_decay)[colnames(temporal_decay) == 'temporal_decay_post_MOB'] <- 'bad_loan'

## 8.1.3 select prediction data
temporal_decay_train <- train_data_final %>% dplyr::select(deal_no, customer_code, predictions)
temporal_decay_test <- test_data_final %>% dplyr::select(deal_no, customer_code, predictions)
temporal_decay_oot <- oot_data_final %>% dplyr::select(deal_no, customer_code, predictions)


## 8.1.4 get temporal decay flag
temporal_decay_train <- inner_join(temporal_decay_train,temporal_decay, by = 'deal_no')
temporal_decay_test <- inner_join(temporal_decay_test,temporal_decay, by = 'deal_no')
temporal_decay_oot <- inner_join(temporal_decay_oot,temporal_decay, by = 'deal_no')

## 8.1.5 get rank ordering
temporal_decay_RO_decile <- data.frame(get_RO(temporal_decay_train,temporal_decay_test,temporal_decay_oot))
temporal_decay_RO_pentile <- data.frame(get_RO_pentile(temporal_decay_train,temporal_decay_test,temporal_decay_oot))

temporal_decay_RO_decile_scored <- data.frame(get_RO_scored(temporal_decay_train,temporal_decay_test,temporal_decay_oot))
temporal_decay_RO_pentile_scored <- data.frame(get_RO_scored_pentile(temporal_decay_train,temporal_decay_test,temporal_decay_oot))


## 8.1.6 save outputs
output_list <- list("temporal_decay_RO_D" = temporal_decay_RO_decile,
                    "temporal_decay_RO_P" = temporal_decay_RO_pentile,
                    "temporal_decay_RO_SD" = temporal_decay_RO_decile_scored,
                    "temporal_decay_RO_SP" = temporal_decay_RO_pentile_scored
                    
)


save_xlsx_output(data = output_list, relative_path = "//model//PV_New//NTC//vcheck//Model_validation_PV_New_1-Temporal decay.xlsx")
# write_xlsx(output_list, path = "data//output//model//PV_New//NTC//vcheck//Model_validation_PV_New_1-Temporal decay.xlsx")


rm(temporal_decay_train,temporal_decay_test,temporal_decay_oot,
   temporal_decay_RO_decile,temporal_decay_RO_pentile,
   temporal_decay_RO_decile_scored,temporal_decay_RO_pentile_scored,temporal_decay,output_list)








## 8.2. Validation - Ever 90 & Ever 150 DPD ----------------------------------------------------------------------

## 8.2.1 subset data for ever 90 dpd
ever_train <- train_data_final %>% dplyr::select(predictions,ever_90dpd)
ever_test <- test_data_final %>% dplyr::select(predictions,ever_90dpd)
ever_oot <- oot_data_final %>% dplyr::select(predictions,ever_90dpd)


## 8.2.2 rename columns - 90 dpd
colnames(ever_train) <- c('predictions','bad_loan')
colnames(ever_test) <- c('predictions','bad_loan')
colnames(ever_oot) <- c('predictions','bad_loan')

## 8.2.3 get rank ordering - 90 dpd
ever_90dpd_RO_decile <- data.frame(get_RO(ever_train,ever_test,ever_oot))
ever_90dpd_RO_pentile <- data.frame(get_RO_pentile(ever_train,ever_test,ever_oot))

ever_90dpd_RO_decile_scored <- data.frame(get_RO_scored(ever_train,ever_test,ever_oot))
ever_90dpd_RO_pentile_scored <- data.frame(get_RO_scored_pentile(ever_train,ever_test,ever_oot))


## 8.2.4 subset data for ever 150 dpd
ever_train <- train_data_final %>% dplyr::select(predictions,ever_150dpd)
ever_test <- test_data_final %>% dplyr::select(predictions,ever_150dpd)
ever_oot <- oot_data_final %>% dplyr::select(predictions,ever_150dpd)


## 8.2.5 rename columns - 150 dpd
colnames(ever_train) <- c('predictions','bad_loan')
colnames(ever_test) <- c('predictions','bad_loan')
colnames(ever_oot) <- c('predictions','bad_loan')


## 8.2.6 get rank ordering - 150 dpd
ever_150dpd_RO_decile <- data.frame(get_RO(ever_train,ever_test,ever_oot))
ever_150dpd_RO_pentile <- data.frame(get_RO_pentile(ever_train,ever_test,ever_oot))

ever_150dpd_RO_decile_scored <- data.frame(get_RO_scored(ever_train,ever_test,ever_oot))
ever_150dpd_RO_pentile_scored <- data.frame(get_RO_scored_pentile(ever_train,ever_test,ever_oot))


## 8.2.7 save output
output_list <- list("90dpd_RO_D" = ever_90dpd_RO_decile,
                    "90dpd_RO_P" = ever_90dpd_RO_pentile,
                    "90dpd_RO_SD" = ever_90dpd_RO_decile_scored,
                    "90dpd_RO_SP" = ever_90dpd_RO_pentile_scored,
                    "150dpd_RO_D" = ever_150dpd_RO_decile,
                    "150dpd_RO_P" = ever_150dpd_RO_pentile,
                    "150dpd_RO_SD" = ever_150dpd_RO_decile_scored,
                    "150dpd_RO_SP" = ever_150dpd_RO_pentile_scored
)



save_xlsx_output(data = output_list, relative_path = "//model//PV_New//NTC//vcheck//Model_validation_PV_New_2-Ever 90 150 DPD.xlsx")
# write_xlsx(output_list, path = "data//output//model//PV_New//NTC//vcheck//Model_validation_PV_New_2-Ever 90 150 DPD.xlsx")

rm(output_list,ever_150dpd_RO_decile,ever_150dpd_RO_decile_scored,ever_150dpd_RO_pentile,ever_150dpd_RO_pentile_scored,
   ever_90dpd_RO_decile,ever_90dpd_RO_decile_scored,ever_90dpd_RO_pentile,ever_90dpd_RO_pentile_scored,ever_oot,ever_test,
   ever_train,train_data_final,test_data_final,oot_data_final)

rm(dev_data,oot_data)





## 8.3 OOT Validation ------------------------------------------------------------------------------

## 8.3.1 Load NTC data for PV
load_rdata_intermediate("model_data//ntc_data_complete.rdata")
model_data <- ntc_model_data %>% filter(loan_type == 'PV-New')
model_data <- model_data %>% filter(Category %in% c('SENP','SEP','SAL'))
rm(ntc_model_data)


## 8.3.2 Load validation datasets
load_rdata_intermediate("ADS_data//validation_60dpd_12mob.rdata")
load_rdata_intermediate("ADS_data//validation_60dpd_9mob.rdata")
load_rdata_intermediate("ADS_data//validation_60dpd_oct_to_dec_2020.rdata")


all_validation <- data.frame(rbind(validation_60dpd_12mob,validation_60dpd_9mob,validation_60dpd_oct_to_dec_2020))
all_validation <- all_validation %>% filter(loan_type == 'PV-New')
all_validation$loan_type <- NULL


## 8.3.3 Create variables

## Category flag
model_data$category_flag_SENP_SEP <- ifelse(model_data$Category %in% c('SENP', 'SEP'), 1, 0)

## Agri profile flag
model_data$agri_profile_flag <- ifelse(is.na(model_data$agri_profile_flag), 0 , model_data$agri_profile_flag)


## Region
woe1 <- -0.235776
woe2 <- -0.06171671
woe3 <- 0.2854523
woe4 <- 0.4097233
woe5 <- 0.5042479

model_data$region_woe <- ifelse(model_data$Region == 'South', woe1, 
                                ifelse(model_data$Region == 'Central', woe2,
                                       ifelse(model_data$Region == 'West', woe3,
                                              ifelse(model_data$Region == 'North', woe4, woe5))))
rm(woe1,woe2,woe3,woe4,woe5)


## Phones reported 12m
model_data$phones_reported_12m_C <- ifelse(is.na(model_data$phones_reported_12m), 1 , model_data$phones_reported_12m)

model_data$phones_reported_12m_C1 <- ifelse(model_data$phones_reported_12m_C >= 4, 4, 
                                           ifelse(model_data$phones_reported_12m_C <= 0, 0, model_data$phones_reported_12m_C))

## address reported 12m
model_data$addresses_reported_12m_C <- ifelse(is.na(model_data$addresses_reported_12m), 0 , model_data$addresses_reported_12m)

model_data$addresses_reported_12m_C1 <- ifelse(model_data$addresses_reported_12m_C >= 4, 4, 
                                              ifelse(model_data$addresses_reported_12m_C <= 0, 0, model_data$addresses_reported_12m_C))


# ## Asset tag
# woe1 <- -0.3037655
# woe2 <- -0.0199037
# woe3 <- 0.0377001
# woe4 <- 0.6376891
# 
# model_data$asset_tag_woe <- ifelse(model_data$asset_tag == 'MOPEDS', woe1, 
#                                    ifelse(model_data$asset_tag == 'MOTORCYC', woe2,
#                                           ifelse(model_data$asset_tag == 'SCOOTER', woe3, woe4)))
# rm(woe1,woe2,woe3,woe4)






# 8.3.4 subset for required columns
performance_data <- model_data %>% dplyr::select(c('deal_no','customer_code',shortlisted_var))
performance_data <- inner_join(performance_data,all_validation,by = 'deal_no')



# 8.3.5 make predictions
performance_data <- get_predictions(final_model, performance_data)


# 8.3.6 get rank ordering for 60 dpd in 12mob
RO_60dpd_12mob_D <-  data.frame(get_RO_validation_decile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_12mob$deal_no))))
RO_60dpd_12mob_P <-  data.frame(get_RO_validation_pentile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_12mob$deal_no))))


# 8.3.7 get rank ordering for 60 dpd in 9mob
RO_60dpd_9mob_D <-  data.frame(get_RO_validation_decile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_9mob$deal_no))))
RO_60dpd_9mob_P <-  data.frame(get_RO_validation_pentile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_9mob$deal_no))))


# 8.3.8 get rank ordering for 60 dod in oct-dec 2020
RO_60dpd_oct_to_dec_2020_D <-  data.frame(get_RO_validation_decile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_oct_to_dec_2020$deal_no))))
RO_60dpd_oct_to_dec_2020_P <-  data.frame(get_RO_validation_pentile(performance_data %>% filter(deal_no %in% unique(validation_60dpd_oct_to_dec_2020$deal_no))))


output_list <- list("RO_60dpd_12mob_D" = RO_60dpd_12mob_D,
                    "RO_60dpd_12mob_P" = RO_60dpd_12mob_P,
                    "RO_60dpd_9mob_D" = RO_60dpd_9mob_D,
                    "RO_60dpd_9mob_P" = RO_60dpd_9mob_P,
                    "RO_60dpd_oct_to_dec_2020_D" = RO_60dpd_oct_to_dec_2020_D,
                    "RO_60dpd_oct_to_dec_2020_P" = RO_60dpd_oct_to_dec_2020_P
)


save_xlsx_output(data = output_list, relative_path = "//model//PV_New//NTC//vcheck//Model_validation_PV_New_3-OOT.xlsx")
# write_xlsx(output_list, path = "data//output//model//PV_New//NTC//vcheck//Model_validation_PV_New_3-OOT.xlsx")



rm(list=ls())




#########################################################################################3
########### NTC Crosstab ###################################


load("data/output/model/PV_New/NTC/vcheck/train_data_final.rdata")
load("data/output/model/PV_New/NTC/vcheck/test_data_final.rdata")
load("data/output/model/PV_New/NTC/vcheck/oot_data_final.rdata")

load("data/intermediate/cleaned_data/ntc_cibil_new.rdata")


train_data_final <- inner_join(train_data_final,cibil_ntc)
test_data_final <- inner_join(test_data_final,cibil_ntc)
oot_data_final <- inner_join(oot_data_final,cibil_ntc)



## 4. remove NA & -1 cibil scores
train_data <- train_data_final %>% filter(!is.na(cibil_ntc_score) & cibil_ntc_score != -1)
test_data <- test_data_final %>% filter(!is.na(cibil_ntc_score) & cibil_ntc_score != -1)
oot_data <- oot_data_final %>% filter(!is.na(cibil_ntc_score) & cibil_ntc_score != -1)


## 5. combine train + test
dev_data <- rbind(train_data,test_data)


## 6. create cibil score pentiles & PD pentiles
dev_data$pd_pentile <- ntile(dev_data$predictions,5)

dev_data$pentile <- ifelse(dev_data$predictions <= 0.02063991 , 1,
                           ifelse(dev_data$predictions > 0.02063991 & dev_data$predictions <= 0.02713468, 2,
                                  ifelse(dev_data$predictions > 0.02713468 & dev_data$predictions <= 0.03363095, 3,
                                         ifelse(dev_data$predictions > 0.03363095 & dev_data$predictions <= 0.04198996, 4,
                                                ifelse(dev_data$predictions > 0.04198996 & dev_data$predictions <= 0.043, 5.1,
                                                ifelse(dev_data$predictions > 0.043 , 5.2,
                                                       0))))))

# dev_data_p5 <- dev_data %>% filter(pd_pentile == 5)
# dev_data_p5$pd_pentile <- ntile(dev_data_p5$predictions,2)
# dev_data_p5$pd_pentile <- ifelse(dev_data_p5$pd_pentile == 1, 5.1, 5.2)
# 
# dev_data <- rbind(dev_data %>% filter(pd_pentile <= 4), dev_data_p5)
# 
# rm(dev_data_p5,train_data_final,test_data_final)
# rm(oot_data)



rm(train_data_final,test_data_final,train_data,test_data)
rm(oot_data)

pd_scores <- dev_data %>% group_by(pentile) %>% summarise(max_pd = max(predictions),
                                                             min_pd = min(predictions),
                                                             n = n())


# dev_data$cibil_pentile <- ntile(dev_data$cibil_ntc_score,5)

dev_data$cibil_pentile <- ifelse(dev_data$cibil_ntc_score <= 120 , "Cibil 100 - 120",
                                 ifelse(dev_data$cibil_ntc_score > 120 & dev_data$cibil_ntc_score <= 140 , "Cibil 121 - 140",
                                        ifelse(dev_data$cibil_ntc_score > 140 & dev_data$cibil_ntc_score <= 160 , "Cibil 141 - 160",
                                               ifelse(dev_data$cibil_ntc_score > 160 & dev_data$cibil_ntc_score <= 180 , "Cibil 161 - 180",
                                                      ifelse(dev_data$cibil_ntc_score > 180, "Cibil 181 - 200",
                                                             "Invalid")))))


cibil_crosstab <- dev_data %>% group_by(pentile,cibil_pentile) %>% summarise(cibil_min = min(cibil_ntc_score),
                                                                                cibil_max = max(cibil_ntc_score),
                                                                                applicant_count = n(),
                                                                                defaults_bad_loan = sum(bad_loan),
                                                                                defaults_90dpd = sum(ever_90dpd),
                                                                                defaults_150dpd = sum(ever_150dpd),
                                                                                def_pct_bad_loan = mean(bad_loan),
                                                                                def_pct_90dpd = mean(ever_90dpd),
                                                                                def_pct_150dpd = mean(ever_150dpd)
                                                                                
)


pd_pentile_delinquency <- dev_data %>% group_by(pentile) %>% summarise(cibil_min = min(cibil_ntc_score),
                                                                          cibil_max = max(cibil_ntc_score),
                                                                          applicant_count = n(),
                                                                          defaults_bad_loan = sum(bad_loan),
                                                                          defaults_90dpd = sum(ever_90dpd),
                                                                          defaults_150dpd = sum(ever_150dpd),
                                                                          def_pct_bad_loan = mean(bad_loan),
                                                                          def_pct_90dpd = mean(ever_90dpd),
                                                                          def_pct_150dpd = mean(ever_150dpd))



cibil_pentile_delinquency <- dev_data %>% group_by(cibil_pentile) %>% summarise(cibil_min = min(cibil_ntc_score),
                                                                                cibil_max = max(cibil_ntc_score),
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


# save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//NTC//vcheck//Model_validation_PV_New_1-CIBIL_NTC_crosstab.xlsx")
write_xlsx(output_list, path = file.path("data//output//model//PV_New//NTC//vcheck//Model_validation_PV_New_1-CIBIL_NTC_crosstab vF.xlsx"))


dev_data$cibil_pentile_num <- ifelse(dev_data$cibil_ntc_score <= 120 , 5,
                                     ifelse(dev_data$cibil_ntc_score > 120 & dev_data$cibil_ntc_score <= 140 , 4,
                                            ifelse(dev_data$cibil_ntc_score > 140 & dev_data$cibil_ntc_score <= 160 , 3,
                                                   ifelse(dev_data$cibil_ntc_score > 160 & dev_data$cibil_ntc_score <= 180 , 2,
                                                          ifelse(dev_data$cibil_ntc_score > 180, 1,
                                                                 0)))))



## 3.3 create risk bands
dev_data$pd_x_cibil_tag <- paste0(dev_data$pentile,"_",dev_data$cibil_pentile_num)

dark_green_band <- c('1_1','2_1','3_1')
light_green_band <- c('1_2','2_2','3_2','1_3')
yellow_band <- c('1_4','2_3','3_3','4_1','4_2','4_3','5.1_1','5.1_2','5.1_3','5.2_1')
red_band <- c('4_5','5.1_5','5.2_5')


dev_data$risk_band <- ifelse(dev_data$pd_x_cibil_tag %in% dark_green_band, "dark_green",
                             ifelse(dev_data$pd_x_cibil_tag %in% light_green_band, "light_green",
                                    ifelse(dev_data$pd_x_cibil_tag %in% yellow_band, "yellow",
                                           ifelse(dev_data$pd_x_cibil_tag %in% red_band, "red", "orange"))))

save(dev_data,file = "data/output/model/PV_New/NTC/vcheck/dev_data.rdata")
write.csv(dev_data, "data/output/model/PV_New/NTC/vcheck/dev_data.csv")
