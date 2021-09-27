#########################################################################################################
#################### Belief testing data creation - Disbursals ##########################################
#########################################################################################################


## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)


## 1. Load data ------------------------------------------------------------------------------

## 1.1 load disbursal belief testing rules
load_rdata_intermediate("belief_testing//all_rules//belief_testing_rules_disbursals.rdata")
load_rdata_intermediate("belief_testing//all_rules//belief_testing_rules_applications.rdata")

## 1.2 load train & test data
# load_rdata_output("model//PV_New//Combined//vcheck//Post_RI//train_data.rdata")
# load_rdata_output("model//PV_New//Combined//vcheck//Post_RI//test_data.rdata")


load_rdata_output("model//PV_New//Combined//vcheck//Post_RI//dpd_check//train_data.rdata")
load_rdata_output("model//PV_New//Combined//vcheck//Post_RI//dpd_check//test_data.rdata")

# ## 1.3 load cibil data
# load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
# cibil_v3 <- distinct(cibil_v3 %>% dplyr::select(deal_no,customer_code,cibil_score_v3))
# 
# 
# load_rdata_intermediate("cleaned_data//cibil_v3_applications.rdata")
# cibil_v3_applications <- distinct(cibil_v3_applications %>% dplyr::select(Application_No,cibil_score_v3))



## 2. Create combined dev data ---------------------------------------------------------------

## 2.1 combine train + test
dev_data <- rbind(train_data_final,test_data_final)
rm(train_data_final,test_data_final)

dev_data <- distinct(dev_data %>% dplyr::select(deal_no,
                                       customer_code,
                                       applicant_id,
                                       application_no,
                                       UID,
                                       tag,
                                       bad_loan,
                                       ever_90dpd,
                                       ever_150dpd,
                                       predictions,
                                       cibil_score_v3))

## 2.2 join to get cibil v3 score
# dev_data_A <- left_join(dev_data %>% filter(tag == 'A'), cibil_v3,by=c('deal_no','customer_code'))
# dev_data_R <- left_join(dev_data %>% filter(tag == 'R1'), cibil_v3_applications,by=c('application_no' = 'Application_No'))
# 
# rm(cibil_v3,cibil_v3_applications)
# 
# dev_data <- rbind(dev_data_A,dev_data_R)

## 3. Create cibil & risk pentiles ------------------------------------------------------------

## 3.1 remove invalid cibil scores
dev_data <- dev_data %>% filter(!is.na(cibil_score_v3) & cibil_score_v3 != -1)



dev_data$pd_pentile <- ntile(dev_data$predictions,5)

dev_data_p5 <- dev_data %>% filter(pd_pentile == 5)
dev_data_p5$pd_pentile <- ntile(dev_data_p5$predictions,2)
dev_data_p5$pd_pentile <- ifelse(dev_data_p5$pd_pentile == 1, 5.1, 5.2)

dev_data <- rbind(dev_data %>% filter(pd_pentile <= 4), dev_data_p5)


pd_scores <- dev_data %>% group_by(pd_pentile) %>% summarise(max_pd = max(predictions),
                                                             min_pd = min(predictions),
                                                             n = n())


rm(dev_data_p5)



dev_data$cibil_pentile <- ifelse(dev_data$cibil_score_v3 <=675 , "Cibil <= 675",
                                 ifelse(dev_data$cibil_score_v3 > 675 & dev_data$cibil_score_v3 <=700 , "Cibil 676 - 700",
                                        ifelse(dev_data$cibil_score_v3 > 700 & dev_data$cibil_score_v3 <=730 , "Cibil 701 - 730",
                                               ifelse(dev_data$cibil_score_v3 > 730 & dev_data$cibil_score_v3 <=750 , "Cibil 731 - 750",
                                                      ifelse(dev_data$cibil_score_v3 > 750 & dev_data$cibil_score_v3 <= 780 , "Cibil 751 - 780",
                                                             ifelse(dev_data$cibil_score_v3 > 780 , "Cibil >= 781",
                                                                    "Invalid"))))))

dev_data$cibil_pentile_num <- ifelse(dev_data$cibil_score_v3 <=675 , 6,
                                     ifelse(dev_data$cibil_score_v3 > 675 & dev_data$cibil_score_v3 <=700 , 5,
                                            ifelse(dev_data$cibil_score_v3 > 700 & dev_data$cibil_score_v3 <=730 , 4,
                                                   ifelse(dev_data$cibil_score_v3 > 730 & dev_data$cibil_score_v3 <=750 , 3,
                                                          ifelse(dev_data$cibil_score_v3 > 750 & dev_data$cibil_score_v3 <= 780 , 2,
                                                                 ifelse(dev_data$cibil_score_v3 > 780 , 1,
                                                                        0))))))



## 3.3 create risk bands
dev_data$pd_x_cibil_tag <- paste0(dev_data$pd_pentile,"_",dev_data$cibil_pentile_num)


dark_green_band <- c('1_1','1_2','1_3','1_4','2_1','2_2','3_1','3_2','4_1','4_2')
light_green_band <- c('2_3','2_4','3_3','3_4','4_3','4_4','5.1_1','5.1_2')
yellow_band <- c('1_5','2_5','5.1_3','5.1_4','5.2_1','5.2_2')
red_band <- c('5.1_5','5.1_6','5.2_5','5.2_6')


dev_data$risk_band <- ifelse(dev_data$pd_x_cibil_tag %in% dark_green_band, "dark_green",
                             ifelse(dev_data$pd_x_cibil_tag %in% light_green_band, "light_green",
                                    ifelse(dev_data$pd_x_cibil_tag %in% yellow_band, "yellow",
                                           ifelse(dev_data$pd_x_cibil_tag %in% red_band, "red", "orange"))))




## 4. Join rules in this dev data ----------------------------------------------------------------------------------

belief_testing_A <- inner_join(dev_data %>% filter(tag == 'A'), belief_testing_rules_disbursals,by = c('deal_no','customer_code','applicant_id')) 

belief_testing_R <- inner_join(dev_data %>% filter(tag == 'R1'),belief_testing_rules_applications,by = c('application_no','applicant_id')) 


rm(belief_testing_rules_disbursals,belief_testing_rules_applications)

asset_cols <- colnames(belief_testing_A %>% dplyr::select(starts_with('Rule_asset')))

common_cols <- intersect(colnames(belief_testing_A),colnames(belief_testing_R))

common_cols_A <- c(common_cols, asset_cols)

belief_testing_A <- belief_testing_A %>% dplyr::select(common_cols_A)
belief_testing_R <- belief_testing_R %>% dplyr::select(common_cols)


belief_testing <- dplyr::bind_rows(belief_testing_A,belief_testing_R)
rm(belief_testing_R,belief_testing_A)


## 5. Function to calculate band based rule statistics ------------------------------------------------------------

## 5.1 function to calculate rule statistics
calculate_risk_band_rule_statistics <- function(gating_rules_data, risk_band_temp){
  
  ## subset for required risk band & select columns
  gating_rules_subset <- gating_rules_data %>% filter(risk_band == risk_band_temp)
  
  ## select rules
  rule_names <- colnames(gating_rules_subset)
  rule_names <- rule_names[rule_names %notin% c('deal_no',
                                                'customer_code',
                                                'applicant_id',
                                                'bad_loan',
                                                'ever_90dpd',
                                                'ever_150dpd', 
                                                'predictions',
                                                'cibil_score_v3',
                                                'pd_pentile',
                                                'cibil_pentile',
                                                'pd_x_cibil_tag',
                                                'risk_band',
                                                'loan_type')]
  
  ## create output list
  rules_output <- list()
  i <- 0
  
  ## iterate over rules & calculate summary numbers
  for(rule in rule_names){
    temp_data <- gating_rules_subset %>% 
      dplyr::select(c('deal_no','customer_code','applicant_id', 'bad_loan','ever_90dpd','ever_150dpd', rule))
    colnames(temp_data) <- c('deal_no', 'customer_code','applicant_id', 'bad_loan','ever_90dpd','ever_150dpd', 'rule')
    temp_data_subset <- temp_data %>% filter(rule == 1)
    
    temp_output_df <- data.frame(rule = rule,
                                 tagged_population_pct = nrow(temp_data_subset)/nrow(temp_data),
                                 tagged_population_count = nrow(temp_data_subset),
                                 rule_default_rate = mean(temp_data_subset$bad_loan),
                                 rule_ever_90dpd = mean(temp_data_subset$ever_90dpd),
                                 rule_ever_150dpd = mean(temp_data_subset$ever_150dpd),
                                 count_ever_150dpd = sum(temp_data_subset$ever_150dpd),
                                 overall_default_rate = mean(temp_data$bad_loan),
                                 overall_ever_90dpd = mean(temp_data$ever_90dpd),
                                 overall_ever_150dpd = mean(temp_data$ever_150dpd),
                                 overall_population = nrow(temp_data_subset)/nrow(gating_rules_data)
    )
    
    i <- i+1
    rules_output[[i]] <- temp_output_df
  }
  
  
  ## rbind all rule dataframes into 1 output dataframe
  rules_output_df <- do.call(rbind,rules_output)
  
  ## replace NAN with NA in data
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  
  rules_output_df[is.nan(rules_output_df)] <- NA
  
  ## return output
  return(rules_output_df)
}


## 5.2 Get rule statistics for all risk bands
rules_agg_dark_green <- calculate_risk_band_rule_statistics(gating_rules_data = belief_testing,
                                                            risk_band_temp = 'dark_green')

rules_agg_light_green <- calculate_risk_band_rule_statistics(gating_rules_data = belief_testing,
                                                             risk_band_temp = 'light_green')

rules_agg_yellow <- calculate_risk_band_rule_statistics(gating_rules_data = belief_testing,
                                                        risk_band_temp = 'yellow')

rules_agg_orange <- calculate_risk_band_rule_statistics(gating_rules_data = belief_testing,
                                                        risk_band_temp = 'orange')

rules_agg_red <- calculate_risk_band_rule_statistics(gating_rules_data = belief_testing,
                                                     risk_band_temp = 'red')



## 5.3 Combine all bands
output_rules_p1 <- inner_join(rules_agg_dark_green, rules_agg_light_green, by = 'rule', suffix = c("_dark_green","_light_green"))

output_rules_p2 <- rules_agg_yellow
colnames(output_rules_p2)[2:11] <- paste(colnames(output_rules_p2)[2:11], "yellow", sep = "_")

output_rules_p3 <- inner_join(rules_agg_orange, rules_agg_red, by = 'rule', suffix = c("_orange","_red"))

output_rules <- inner_join(output_rules_p1, output_rules_p2, by = 'rule')
output_rules <- inner_join(output_rules, output_rules_p3, by = 'rule')


rm(output_rules_p1,output_rules_p2,output_rules_p3, rules_agg_dark_green,rules_agg_light_green,rules_agg_orange,rules_agg_red,rules_agg_yellow)

# 

## 6. Save outputs ----------------------------------------------------------------------------------------

## 6.1 Summary
summary_stats <- belief_testing %>% group_by(risk_band) %>% summarise(applicants = n(),
                                                                      default_count = sum(bad_loan),
                                                                      default_90_count = sum(ever_90dpd),
                                                                      default = mean(bad_loan),
                                                                      default_90 = mean(ever_90dpd),
                                                                      population = n(),
                                                                      population_pct = n()/nrow(belief_testing),
                                                                      disbursals = length(unique(deal_no)))

## 6.2 subset output 
short_output <- output_rules %>% dplyr::select(
  "rule",
  "tagged_population_count_dark_green",
  "tagged_population_pct_dark_green",
  "rule_default_rate_dark_green",
  "rule_ever_90dpd_dark_green",
  
  "tagged_population_count_light_green",
  "tagged_population_pct_light_green",
  "rule_default_rate_light_green",
  "rule_ever_90dpd_light_green",
  
  "tagged_population_count_yellow",
  "tagged_population_pct_yellow",
  "rule_default_rate_yellow",
  "rule_ever_90dpd_yellow",
  
  "tagged_population_count_orange",
  "tagged_population_pct_orange",
  "rule_default_rate_orange",
  "rule_ever_90dpd_orange",
  
  "tagged_population_count_red",
  "tagged_population_pct_red",
  "rule_default_rate_red",
  "rule_ever_90dpd_red"
)



output_list <- list(
  "Rules_complete" = output_rules,
  "Summary" = summary_stats,
  "Rules_subset" = short_output
)

write_xlsx(output_list, "data//output//belief_testing//vcheck//belief_testing_PV_New_Jun_22.xlsx")




rm(dev_data,output_list,output_rules,pd_scores,short_output,summary_stats)


#################################################################################################################


## Adjust for Rules


belief_testing_adj <- belief_testing

belief_testing_adj$band_num_original <- ifelse(belief_testing_adj$risk_band == 'dark_green', 1,
                                               ifelse(belief_testing_adj$risk_band == 'light_green', 2,
                                                      ifelse(belief_testing_adj$risk_band == 'yellow', 3,
                                                             ifelse(belief_testing_adj$risk_band == 'orange', 4,
                                                             5))))


## 1- Complete bad
rules_bad <- c('Rule_CV_Demog_addresses_reported_6m_GE_2',
               'Rule_CV_Demog_phones_reported_3m_GE_2',
               'Rule_EN_ratio_enquiry_to_loans_12mon_GE_3')


green_cleanup <- belief_testing_adj %>% filter(risk_band %in% c('dark_green','light_green')) %>% dplyr::select(c('UID',rules_bad))
green_cleanup[,'bad_flag'] <- apply(green_cleanup[,rules_bad], 1, max)
green_cleanup <- green_cleanup %>% filter(bad_flag == 1)


# ## 2 - orange cleanup
# rules_bad_in_orange <- c('Rule_CV_Demog_phones_reported_6m_GE_4')
# 
# bad_orange <- belief_testing_adj %>% filter(risk_band == 'orange') %>% dplyr::select(c('UID',rules_bad_in_orange))
# # bad_orange[,'bad_flag'] <- apply(bad_orange[,rules_bad_in_orange], 1, max)
# bad_orange$bad_flag <- bad_orange$Rule_CV_Demog_phones_reported_6m_GE_4
# bad_orange <- bad_orange %>% filter(bad_flag == 1)
# 
# 
# ## 3 - multiple bad
# rules_bad_multiple <- c('Rule_PO_months_12_Gold_GE_5',
#                         'Rule_EN_ratio_enquiry_to_loans_12mon_4to7',
#                         'Rule_PO_Gold_live_sanctioned_amount_12mon_Low')
# 
# 
# bad_multiple <- belief_testing_adj %>% filter(risk_band %in% c('yellow','orange')) %>% dplyr::select(c('UID',rules_bad_multiple))
# bad_multiple[,'bad_flag'] <- apply(bad_multiple[,rules_bad_multiple], 1, max)
# bad_multiple <- bad_multiple %>% filter(bad_flag == 1)






## make adjustments
belief_testing_adj$band_num_new <- belief_testing_adj$band_num_original


belief_testing_adj$band_num_new <- ifelse(belief_testing_adj$UID %in% unique(green_cleanup$UID), 3 , belief_testing_adj$band_num_new)

# belief_testing_adj$band_num_new <- ifelse(belief_testing_adj$UID %in% unique(bad_multiple$UID) & 
#                                             belief_testing_adj$UID %notin% unique(bad_orange$UID), belief_testing_adj$band_num_new + 1, belief_testing_adj$band_num_new)
# 
# belief_testing_adj$band_num_new <- ifelse(belief_testing_adj$UID %in% unique(bad_complete$UID), 4, belief_testing_adj$band_num_new)


belief_testing_adj$risk_band_new <- ifelse(belief_testing_adj$band_num_new == 1, "dark_green",
                                           ifelse(belief_testing_adj$band_num_new == 2, "light_green",
                                                  ifelse(belief_testing_adj$band_num_new == 3, "yellow",
                                                         ifelse(belief_testing_adj$band_num_new == 4, "orange",
                                                                "red"))))




summary_stats_adj <- belief_testing_adj %>% group_by(risk_band_new) %>% summarise(applicants = n(),
                                                                                  default_count = sum(bad_loan),
                                                                                  default_90_count = sum(ever_90dpd),
                                                                                  default = mean(bad_loan),
                                                                                  default_90 = mean(ever_90dpd),
                                                                                  population = n()/nrow(belief_testing),
                                                                                  disbursals = length(unique(deal_no)))



summary_stats <- belief_testing %>% group_by(risk_band) %>% summarise(applicants = n(),
                                                                      default_count = sum(bad_loan),
                                                                      default_90_count = sum(ever_90dpd),
                                                                      default = mean(bad_loan),
                                                                      default_90 = mean(ever_90dpd),
                                                                      population = n()/nrow(belief_testing),
                                                                      disbursals = length(unique(deal_no)))



output_list <- list(
  "original" = summary_stats,
  "modified" = summary_stats_adj
)

write_xlsx(output_list, "data//output//belief_testing//vcheck//Summary_belief_testing_PV_New_Jun_22.xlsx")




selected_colnames <- c(
  "deal_no",
  "application_no",
  "customer_code",
  # "loan_type",
  "applicant_id",
  "UID",
  "bad_loan",
  "ever_90dpd",
  # "ever_150dpd",
  "predictions",
  "cibil_score_v3",
  "pd_pentile",
  "cibil_pentile",
  "cibil_pentile_num",
  "pd_x_cibil_tag",
  "risk_band",
  "band_num_original",
  'Rule_CV_Demog_addresses_reported_6m_GE_2',
  'Rule_CV_Demog_phones_reported_3m_GE_2',
  'Rule_EN_ratio_enquiry_to_loans_12mon_GE_3',
  "band_num_new",
  "risk_band_new"
)

belief_testing_PV_New <- belief_testing_adj %>% dplyr::select(selected_colnames)

save(belief_testing_PV_New,file = "data//output//reason_code//PV_New//belief_testing_PV_New.rdata")
save(belief_testing_PV_New,file = "data//output//reason_code//PV_New//belief_testing_PV_New_dpd_check.rdata")



