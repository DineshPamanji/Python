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

load_rdata_intermediate("reject_data//variable_data//model_data_PV_Used_A_R.rdata")
oot_date_start <- '2018-04-01'
oot_date_end <- '2018-06-30'


oot_data <-
  model_data_PV_Used_A_R %>% filter((date >= as.Date(oot_date_start)) &
                                 (date <= as.Date(oot_date_end)) &
                                 (Category %in% c('SENP','SEP','SAL')))

dev_data <-
  model_data_PV_Used_A_R %>% filter(UID %notin% unique(oot_data$UID) &
                                 (Category %in% c('SENP','SEP','SAL')))


# dev_A <- rbind(train_data_final,test_data_final)
# oot_A <- oot_data_final

# rm(model_data_2W_New_rejects, train_data_final,test_data_final,oot_data_final)
rm(model_data_PV_Used_A_R)

######################################################################################################################################
## 6.3. Variable transformation 


## 1. enquiry count 12m non cc
dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C <- dev_data$Var_EN_unsec_enquiry_count_12m_non_CC
oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C <- oot_data$Var_EN_unsec_enquiry_count_12m_non_CC

dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C[is.na(dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C)] <- 0
oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C[is.na(oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C)] <- 0

dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C <- ifelse(dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C <= 0, 0, 
                                                           ifelse(dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C >= 9, 9, dev_data$Var_EN_unsec_enquiry_count_12m_non_CC_C))

oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C <- ifelse(oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C <= 0, 0, 
                                                           ifelse(oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C >= 9, 9, oot_data$Var_EN_unsec_enquiry_count_12m_non_CC_C))

## 2. SENP SEP Flag
dev_data$category_flag_SENP_SEP <- ifelse(dev_data$Category %in% c('SENP', 'SEP'), 1, 0)
oot_data$category_flag_SENP_SEP <- ifelse(oot_data$Category %in% c('SENP', 'SEP'), 1, 0)



## 3. Closed loan sanction amount

woe1 <- -0.1901108
woe2 <- 0.02158464
woe3 <- 0.1753055

dev_data$Var_PO_closed_sanction_amount_excl_CC_woe <- ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC <= 50000, woe1, 
                                                             ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC > 50000 & dev_data$Var_PO_closed_sanction_amount_excl_CC <= 500000, woe2,woe3)) 


oot_data$Var_PO_closed_sanction_amount_excl_CC_woe <- ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC <= 50000, woe1, 
                                                             ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC > 50000 & oot_data$Var_PO_closed_sanction_amount_excl_CC <= 500000, woe2,woe3)) 





## 6.4 get list of features
shortlisted_var <- c( 
  "Var_EN_unsec_enquiry_count_12m_non_CC_C",
  "Var_PO_closed_sanction_amount_excl_CC_woe",
  "category_flag_SENP_SEP"
)


# 6.5 subset for required columns
performance_data_dev <- dev_data %>% dplyr::select(c('deal_no','application_no','applicant_id','UID','tag',shortlisted_var,'bad_loan'))
performance_data_oot <- oot_data %>% dplyr::select(c('deal_no','application_no','applicant_id','UID','tag',shortlisted_var,'bad_loan'))
# combined_A_R <- rbindlist(l =list(performance_data_dev_A,performance_data_dev_R), use.names = T, fill = T)
# rm(performance_data_dev_A,performance_data_dev_R)

# 6.6 load model
model_PV_used <- readRDS(file = file.path(get_data_path()$data$model,"model_PV_Used.rds"))


# 6.7 make predictions
predictions_dev <- get_predictions(model_PV_used, performance_data_dev)
predictions_oot <- get_predictions(model_PV_used, performance_data_oot)


###################################################################################################################


## A+R1

dev_A_R1 <- predictions_dev %>% filter(tag %in% c('A','R1'))
dev_A_R1$prediction_decile <- ntile(dev_A_R1$predictions,10)

population_pct <- dev_A_R1 %>% group_by(prediction_decile,tag) %>% summarise(count = length(unique(UID)))
delinquency_pct <- dev_A_R1 %>% group_by(prediction_decile,tag) %>% summarise(default = mean(bad_loan))

dev_A_R <- predictions_dev %>% filter(tag %in% c('A','R1','R2'))
dev_A_R$prediction_decile <- ntile(dev_A_R$predictions,10)

population_pct_A_R <- dev_A_R %>% group_by(prediction_decile,tag) %>% summarise(count = length(unique(UID)))
delinquency_pct_A_R <- dev_A_R %>% group_by(prediction_decile,tag) %>% summarise(default = mean(bad_loan))



output_list <- list("population_pct_A_R1" = population_pct,
                    "delinquency_pct_A_R1" = delinquency_pct,
                    "population_pct_A_R" = population_pct_A_R,
                    "delinquency_pct_A_R" = delinquency_pct_A_R
)


save_xlsx_output(data = output_list, relative_path = "//model//PV_Used//RI//RI_1-Check.xlsx")



dev_A_R1 <- predictions_dev %>% filter(tag %in% c('A','R1'))
dev_A_R1$prediction_decile <- ntile(dev_A_R1$predictions,5)

population_pct <- dev_A_R1 %>% group_by(prediction_decile,tag) %>% summarise(count = length(unique(UID)))
delinquency_pct <- dev_A_R1 %>% group_by(prediction_decile,tag) %>% summarise(default = mean(bad_loan))

dev_A_R <- predictions_dev %>% filter(tag %in% c('A','R1','R2'))
dev_A_R$prediction_decile <- ntile(dev_A_R$predictions,5)

population_pct_A_R <- dev_A_R %>% group_by(prediction_decile,tag) %>% summarise(count = length(unique(UID)))
delinquency_pct_A_R <- dev_A_R %>% group_by(prediction_decile,tag) %>% summarise(default = mean(bad_loan))


output_list <- list("population_pct_A_R1" = population_pct,
                    "delinquency_pct_A_R1" = delinquency_pct,
                    "population_pct_A_R" = population_pct_A_R,
                    "delinquency_pct_A_R" = delinquency_pct_A_R
)


save_xlsx_output(data = output_list, relative_path = "//model//PV_Used//RI//RI_2-Pentile Check.xlsx")








# combined_A_R <- rbindlist(l =list(performance_data_dev_A,performance_data_dev_R), use.names = T, fill = T)
# rm(performance_data_dev_A,performance_data_dev_R,performance_data)



combined_A_R$prediction_decile <- ntile(combined_A_R$predictions,10)
combined_A_R %>% group_by(prediction_decile) %>% summarise(default = mean(bad_loan))

test <- combined_A_R %>% group_by(prediction_decile,tag) %>% summarise(n = n())
test2 <- combined_A_R %>% group_by(prediction_decile) %>% summarise(total = n())

test <- left_join(test,test2,by='prediction_decile')
test$pct <- (test$n/test$total) * 100




combined_A_R$prediction_pentile <- ntile(combined_A_R$predictions,5)
combined_A_R %>% group_by(prediction_pentile) %>% summarise(default = mean(bad_loan))

test3 <- combined_A_R %>% group_by(prediction_pentile,tag) %>% summarise(n = n())
test4 <- combined_A_R %>% group_by(prediction_pentile) %>% summarise(total = n())

test3 <- left_join(test3,test4,by='prediction_pentile')
test3$pct <- (test3$n/test3$total) * 100


fwrite(combined_A_R %>% group_by(prediction_decile,tag) %>% summarise(default = mean(bad_loan)),'delin.csv')
