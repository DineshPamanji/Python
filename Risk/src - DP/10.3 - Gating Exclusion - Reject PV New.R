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

load_rdata_intermediate("reject_data//gating_rules//gating_rules_deal_no_level_reject.rdata")

load_rdata_intermediate("reject_data//reject_data_PV_New.rdata")

rules_PV_new <- gating_rules_deal_no_level_reject %>% filter(application_no %in% unique(reject_data_PV_New$application_no))


rm(gating_rules_deal_no_level_reject,reject_data_PV_New)

## 2. identify gated out ids in 2W New ----------------------------------------------------

common_cols <- c('application_no','tag')

selected_rules <-
        c(
                'Rule_DR_non_Gold_Edu_Agri_90dpd_1mon_GE_1',
                # 'Rule_PO_months_3_Tractor_live_GE_1',
                'Rule_PO_months_3_live_unsec_GE_2',
                # 'Rule_EN_enquiry_count_6m_GE_9',
                # 'Rule_EN_enquiry_count_3m_GE_5'
                'Rule_EN_enquiry_count_1m_GE_6'
        )

req_cols <- c(common_cols,selected_rules)

rules_PV_new <- data.frame(rules_PV_new %>% dplyr::select(req_cols))

rules_PV_new[,'gated_flag'] <- apply(rules_PV_new[,selected_rules], 1, max)


gated_out_PV_new_rejects <- rules_PV_new %>% filter(gated_flag == 1) %>% dplyr::select(application_no,tag)


assert_data_non_empty(gated_out_PV_new_rejects)
save(gated_out_PV_new_rejects,
     file = file.path(
             get_data_path()$data$intermediate,
             "reject_data",
             "gating_rules",
          #   "2W-New",
             "gated_out_PV_new_rejects.rdata"
     )
)


rm(rules_PV_new)

###################################################################################################################


## load bureau variables
load_rdata_intermediate("reject_data//variable_data//X_var_bureau_all.rdata")

# X_var_bureau_dpd_check.rdata

## load application data
load_rdata_intermediate("reject_data//variable_data//X_var_application_rejects.rdata")


## load reject bad loan
load_rdata_intermediate("reject_data//reject_data_PV_New.rdata")

## load application data
load_rdata_intermediate("cleaned_data//application_data_subset.rdata")
application_data_subset <- distinct(application_data_subset %>% dplyr::select(AppNo,application_date))


## load cibil data
load_rdata_intermediate("cleaned_data//cibil_v3_applications.rdata")


model_data_PV_New_rejects <- left_join(all_X_var_bureau_rejects, all_X_var_application_rejects, by = c('application_no' = 'AppNo'))
model_data_PV_New_rejects <- model_data_PV_New_rejects %>% filter(application_no %notin% unique(gated_out_PV_new_rejects$application_no))
model_data_PV_New_rejects <- inner_join(reject_data_PV_New,model_data_PV_New_rejects,by = 'application_no')
model_data_PV_New_rejects <- left_join(model_data_PV_New_rejects,application_data_subset, by = c('application_no' = 'AppNo'))
model_data_PV_New_rejects <- model_data_PV_New_rejects %>% filter(tag %in% c('R1','R2','R3','R4'))
model_data_PV_New_rejects <- model_data_PV_New_rejects %>% filter(Category %in% c('SAL','SENP','SEP'))
model_data_PV_New_rejects <- left_join(model_data_PV_New_rejects,cibil_v3_applications %>% dplyr::select(Application_No,cibil_score_v3), by = c('application_no' = 'Application_No'))
model_data_PV_New_rejects <- model_data_PV_New_rejects %>% filter(CIBIL_SCORE >= 600)


assert_data_non_empty(model_data_PV_New_rejects)
save(model_data_PV_New_rejects,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "variable_data",
       "model_data_PV_New_rejects.rdata"
     )
)

rm(all_X_var_application_rejects,all_X_var_bureau_rejects,application_data_subset,gated_out_PV_new_rejects,reject_data_PV_New)

###################################################################################################################################


# load_rdata_intermediate("model_data//model_data_PV_New.rdata")
load_rdata_intermediate("model_data//model_data_PV_New_vcheck.rdata")
model_data_PV_New <- model_data_PV_New %>% filter(Category %in% c('SAL','SENP','SEP'))

load_rdata_intermediate("cleaned_data//cibil_v3_disbursals.rdata")
cibil_v3 <- distinct(cibil_v3 %>% dplyr::select(deal_no,customer_code,cibil_score_v3))



model_data_PV_New <- left_join(model_data_PV_New,cibil_v3,by=c('deal_no','customer_code'))
model_data_PV_New <- model_data_PV_New %>% filter(!is.na(cibil_score_v3) & cibil_score_v3 != -1)

rm(cibil_v3)


model_data_PV_New$tag <- 'A'
model_data_PV_New$date <- model_data_PV_New$disbursal_date
model_data_PV_New$UID <- model_data_PV_New$applicant_id

model_data_PV_New_rejects$date <- model_data_PV_New_rejects$application_date
model_data_PV_New_rejects$loan_type <- 'PV-New'
model_data_PV_New_rejects$UID <- model_data_PV_New_rejects$application_no


model_data_PV_New_A_R <- data.frame(rbindlist(l=list(model_data_PV_New,model_data_PV_New_rejects),use.names = T, fill = T))

assert_data_non_empty(model_data_PV_New_A_R)
save(model_data_PV_New_A_R,
     file = file.path(
             get_data_path()$data$intermediate,
             "reject_data",
             "variable_data",
             "model_data_PV_New_A_R.rdata"
     )
)








rm(list=ls())
