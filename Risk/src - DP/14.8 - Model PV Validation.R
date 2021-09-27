#############################################################################################
################## 15 - Model PV Combined  ##################################################
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



## 1. load train, test & OOT data
load_rdata_output("model//PV_New//Combined//vcheck//Post_RI//train_data.rdata")
load_rdata_output("model//PV_New//Combined//vcheck//Post_RI//test_data.rdata")
load_rdata_output("model//PV_New//Combined//vcheck//Post_RI//oot_data.rdata")

# ## 2. load cibil data
# load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
# cibil_v3 <- distinct(cibil_v3 %>% dplyr::select(deal_no,customer_code,cibil_score_v3))
# 
# 
# load_rdata_intermediate("cleaned_data//cibil_v3_applications.rdata")
# cibil_v3_applications <- distinct(cibil_v3_applications %>% dplyr::select(Application_No,cibil_score_v3))
# 
# 
# 
# ## 3. get cibil v3 scores by joining with data
# train_data_final_A <- left_join(train_data_final %>% filter(tag == 'A'),cibil_v3,by=c('deal_no','customer_code'))
# test_data_final_A <- left_join(test_data_final %>% filter(tag == 'A'),cibil_v3,by=c('deal_no','customer_code'))
# oot_data_final_A <- left_join(oot_data_final %>% filter(tag == 'A'),cibil_v3,by=c('deal_no','customer_code'))
# 
# 
# train_data_final_R <- left_join(train_data_final %>% filter(tag == 'R1'),cibil_v3_applications,by=c('application_no' = 'Application_No'))
# test_data_final_R <- left_join(test_data_final %>% filter(tag == 'R1'),cibil_v3_applications,by=c('application_no' = 'Application_No'))
# oot_data_final_R <- left_join(oot_data_final %>% filter(tag == 'R1'),cibil_v3_applications,by=c('application_no' = 'Application_No'))
# 
# 
# train_data_final <- rbind(train_data_final_A,train_data_final_R)
# test_data_final <- rbind(test_data_final_A,test_data_final_R)
# oot_data_final <- rbind(oot_data_final_A,oot_data_final_R)
# 
# rm(train_data_final_A,train_data_final_R,test_data_final_A,test_data_final_R,oot_data_final_A,oot_data_final_R)
# rm(cibil_v3,cibil_v3_applications)
# 

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
rm(oot_data)



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


save_xlsx_output(data = output_list, relative_path = "//model//PV_New//Combined//vcheck//Post_RI//Model_validation_PV_New_1-CIBIL_v3_crosstab.xlsx")

# fwrite(pd_scores, "pentiles_PV_New.csv")


# rm(list=ls())


### OOT Data

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




oot_data$pd_pentile <- ifelse(oot_data$predictions < 0.0236, 1,
                              ifelse(oot_data$predictions >= 0.0236 & oot_data$predictions < 0.0301, 2,
                                     ifelse(oot_data$predictions >= 0.0301 & oot_data$predictions < 0.0367, 3,
                                            ifelse(oot_data$predictions >= 0.0367 & oot_data$predictions < 0.0489, 4,
                                                   ifelse(oot_data$predictions >= 0.0489 & oot_data$predictions < 0.0564, 5.1, 5.2)))))



# oot_data$cibil_pentile <- ntile(oot_data$cibil_score_v3,5)

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


write_xlsx(output_list, path = file.path("data//output//model//PV_New//RI//Model_validation_PV_New_1_OOT-CIBIL_v3_crosstab_scored_pentile.xlsx"))




rm(list=ls())

