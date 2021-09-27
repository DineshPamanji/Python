############################################################################################
################## 10 - Remove Gated out population  #######################################
############################################################################################


## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)


## 1. Load gating rules data --------------------------------------------------------------

# load_rdata_intermediate("reject_data//gating_rules//2W-New//gating_rules_deal_no_level_reject_2W.rdata")
# 
# rules_2W_new <- gating_rules_deal_no_level_reject_2W %>% filter(tag %in% c('R1','R2','R3','R4'))


load_rdata_intermediate("reject_data//gating_rules//gating_rules_deal_no_level_reject.rdata")

load_rdata_intermediate("reject_data//reject_data_2W.rdata")

rules_2W_new <- gating_rules_deal_no_level_reject %>% filter(application_no %in% unique(reject_data_2W$application_no))

rm(gating_rules_deal_no_level_reject,reject_data_2W)


## 2. identify gated out ids in 2W New ----------------------------------------------------

common_cols <- c('application_no','tag')

selected_rules <-
        c(
                'Rule_DR_non_Gold_Edu_Agri_90dpd_1mon_GE_1',
                'Rule_DR_non_Gold_Edu_Agri_60dpd_6mon_GE_4',
                'Rule_DR_non_Gold_Edu_Agri_90dpd_12mon_GE_3',
                'Rule_DR_non_Gold_Edu_Agri_60dpd_1mon_GE_1',
                'Rule_PO_months_1_TW_live_GE_1',
                'Rule_PO_months_3_BL_live_GE_4',
                'Rule_PO_months_3_CV_live_GE_2',
                'Rule_PO_months_1_AL_live_GE_1',
                'Rule_PO_months_3_Tractor_live_GE_1',
                'Rule_PO_months_6_live_unsec_GE_4',
                'Rule_PO_months_3_live_unsec_GE_2',
                'Rule_PO_months_6_PL_live_GE_2',
                'Rule_EN_enquiry_count_3m_GE_7',
                'Rule_EN_enquiry_count_1m_GE_5',
                'Rule_EN_enquiry_count_6m_GE_9'
                
        )

req_cols <- c(common_cols,selected_rules)

rules_2W_new <- data.frame(rules_2W_new %>% dplyr::select(req_cols))

rules_2W_new[,'gated_flag'] <- apply(rules_2W_new[,selected_rules], 1, max)


gated_out_2W_new_rejects <- rules_2W_new %>% filter(gated_flag == 1) %>% dplyr::select(application_no,tag)


assert_data_non_empty(gated_out_2W_new_rejects)
save(gated_out_2W_new_rejects,
     file = file.path(
             get_data_path()$data$intermediate,
             "reject_data",
             "gating_rules",
             "2W-New",
             "gated_out_2W_new_rejects.rdata"
     )
)


rm(rules_2W_new)

###################################################################################################################


## load bureau variables
load_rdata_intermediate("reject_data//variable_data//X_var_bureau_all.rdata")


## load application data
load_rdata_intermediate("reject_data//variable_data//X_var_application_rejects.rdata")


## load reject bad loan
load_rdata_intermediate("reject_data//reject_data_2W.rdata")

## load application data
load_rdata_intermediate("cleaned_data//application_data_subset.rdata")
application_data_subset <- distinct(application_data_subset %>% dplyr::select(AppNo,application_date))


## load cibil data
load_rdata_intermediate("cleaned_data//cibil_v3_applications.rdata")


## load reject bureau data
# load_rdata_intermediate("reject_data//reject_bureau_tradelines.rdata")
# trades_clean <- distinct(trades_clean %>% dplyr::select(application_no,product,type))


model_data_2W_New_rejects <- left_join(all_X_var_bureau_rejects, all_X_var_application_rejects, by = c('application_no' = 'AppNo'))
model_data_2W_New_rejects <- model_data_2W_New_rejects %>% filter(application_no %notin% unique(gated_out_2W_new_rejects$application_no))
model_data_2W_New_rejects <- inner_join(reject_data_2W,model_data_2W_New_rejects,by = 'application_no')
model_data_2W_New_rejects <- left_join(model_data_2W_New_rejects,application_data_subset, by = c('application_no' = 'AppNo'))
model_data_2W_New_rejects <- model_data_2W_New_rejects %>% filter(tag %in% c('R1','R2','R3','R4'))
model_data_2W_New_rejects <- model_data_2W_New_rejects %>% filter(Category %in% c('SAL','SENP','SEP'))
model_data_2W_New_rejects <- left_join(model_data_2W_New_rejects,cibil_v3_applications %>% dplyr::select(Application_No,cibil_score_v3), by = c('application_no' = 'Application_No'))
model_data_2W_New_rejects <- model_data_2W_New_rejects %>% filter(CIBIL_SCORE >= 600)
# model_data_2W_New_rejects <- left_join(model_data_2W_New_rejects,trades_clean,by = 'application_no')



assert_data_non_empty(model_data_2W_New_rejects)
save(model_data_2W_New_rejects,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "variable_data",
       "model_data_2W_New_rejects.rdata"
     )
)

rm(all_X_var_application_rejects,application_data_subset,gated_out_2W_new_rejects,reject_data_2W)
rm(all_X_var_bureau_rejects,cibil_v3_applications)


###################################################################################################################################


# load_rdata_intermediate("model_data//model_data_2W_New.rdata")
load_rdata_intermediate("model_data//model_data_2W_New_vcheck.rdata")
model_data_2W_New <- model_data_2W_New %>% filter(Category %in% c('SAL','SENP','SEP'))

load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
cibil_v3 <- distinct(cibil_v3 %>% dplyr::select(deal_no,customer_code,cibil_score_v3))



model_data_2W_New <- left_join(model_data_2W_New,cibil_v3,by=c('deal_no','customer_code'))
model_data_2W_New <- model_data_2W_New %>% filter(!is.na(cibil_score_v3) & cibil_score_v3 != -1)

rm(cibil_v3)

model_data_2W_New$tag <- 'A'
model_data_2W_New$date <- model_data_2W_New$disbursal_date
model_data_2W_New$UID <- model_data_2W_New$applicant_id

model_data_2W_New_rejects$date <- model_data_2W_New_rejects$application_date
model_data_2W_New_rejects$loan_type <- '2W-New'
model_data_2W_New_rejects$UID <- model_data_2W_New_rejects$application_no


model_data_2W_New_A_R <- data.frame(rbindlist(l=list(model_data_2W_New,model_data_2W_New_rejects),use.names = T, fill = T))

assert_data_non_empty(model_data_2W_New_A_R)
save(model_data_2W_New_A_R,
     file = file.path(
             get_data_path()$data$intermediate,
             "reject_data",
             "variable_data",
             "model_data_2W_New_A_R.rdata"
     )
)








rm(list=ls())
