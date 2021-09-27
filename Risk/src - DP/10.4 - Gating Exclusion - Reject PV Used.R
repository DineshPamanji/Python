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
load_rdata_intermediate("reject_data//gating_rules//gating_rules_deal_no_level_reject.rdata")

load_rdata_intermediate("cleaned_data//reject_app_tradelines.rdata")
product_mapping <- distinct(reject_app_tradelines %>% dplyr::select(application_no, product, type))
rm(reject_app_tradelines)

gating_rules_deal_no_level_reject <- left_join(gating_rules_deal_no_level_reject,product_mapping,by = 'application_no')

# gating_rules_deal_no_level_reject %>% group_by(product,type) %>% summarise(n=n())


gating_rules_deal_no_level_reject_PV_Used <- gating_rules_deal_no_level_reject %>% filter(product == 'C' & type == 'U')
rules_PV_Used <- gating_rules_deal_no_level_reject_PV_Used %>% filter(tag %in% c('R1','R2'))


## 2. identify gated out ids in 2W New ----------------------------------------------------

common_cols <- c('application_no','tag')

selected_rules <-
        c(
                'Rule_DR_non_Gold_Edu_Agri_90dpd_1mon_GE_1',
                'Rule_DR_non_Gold_Edu_Agri_90dpd_6mon_GE_3',
                'Rule_PO_months_12_TW_live_GE_2',
                'Rule_PO_months_3_live_unsec_GE_2',
                'Rule_EN_enquiry_count_1m_GE_5',
                'Rule_EN_enquiry_count_3m_GE_7'
                
        )

req_cols <- c(common_cols,selected_rules)

rules_PV_Used <- data.frame(rules_PV_Used %>% dplyr::select(req_cols))

rules_PV_Used[,'gated_flag'] <- apply(rules_PV_Used[,selected_rules], 1, max)


gated_out_PV_used_rejects <- rules_PV_Used %>% filter(gated_flag == 1) %>% dplyr::select(application_no,tag)


assert_data_non_empty(gated_out_PV_used_rejects)
save(gated_out_PV_used_rejects,
     file = file.path(
             get_data_path()$data$intermediate,
             "reject_data",
             "gating_rules",
          #   "2W-New",
             "gated_out_PV_used_rejects.rdata"
     )
)


rm(rules_PV_Used,gating_rules_deal_no_level_reject_PV_Used,product_mapping,gating_rules_deal_no_level_reject)

###################################################################################################################


## load bureau variables
load_rdata_intermediate("reject_data//variable_data//X_var_bureau_all.rdata")

## load application data
load_rdata_intermediate("reject_data//variable_data//X_var_application_rejects.rdata")


## load reject bad loan
load_rdata_intermediate("reject_data//reject_data_PV_Used.rdata")

## load application data
load_rdata_intermediate("cleaned_data//application_data_subset.rdata")
application_data_subset <- distinct(application_data_subset %>% dplyr::select(AppNo,application_date))


model_data_PV_Used_rejects <- left_join(all_X_var_bureau_rejects, all_X_var_application_rejects, by = c('application_no' = 'AppNo'))
model_data_PV_Used_rejects <- model_data_PV_Used_rejects %>% filter(application_no %notin% unique(gated_out_PV_used_rejects$application_no))
model_data_PV_Used_rejects <- inner_join(reject_data_PV_Used,model_data_PV_Used_rejects,by = 'application_no')
model_data_PV_Used_rejects <- left_join(model_data_PV_Used_rejects,application_data_subset, by = c('application_no' = 'AppNo'))
model_data_PV_Used_rejects <- model_data_PV_Used_rejects %>% filter(tag %in% c('R1','R2'))
model_data_PV_Used_rejects <- model_data_PV_Used_rejects %>% filter(Category %in% c('SAL','SENP','SEP'))
model_data_PV_Used_rejects <- model_data_PV_Used_rejects %>% filter(CIBIL_SCORE >= 600)


assert_data_non_empty(model_data_PV_Used_rejects)
save(model_data_PV_Used_rejects,
     file = file.path(
       get_data_path()$data$intermediate,
       "reject_data",
       "variable_data",
       "model_data_PV_Used_rejects.rdata"
     )
)

rm(all_X_var_application_rejects,all_X_var_bureau_rejects,application_data_subset,gated_out_PV_used_rejects,reject_data_PV_Used)

###################################################################################################################################


load_rdata_intermediate("model_data//model_data_PV_Used.rdata")
model_data_PV_Used <- model_data_PV_Used %>% filter(Category %in% c('SAL','SENP','SEP'))

model_data_PV_Used$tag <- 'A'
model_data_PV_Used$date <- model_data_PV_Used$disbursal_date
model_data_PV_Used$UID <- model_data_PV_Used$applicant_id

model_data_PV_Used_rejects$date <- model_data_PV_Used_rejects$application_date
model_data_PV_Used_rejects$loan_type <- 'PV-New'
model_data_PV_Used_rejects$UID <- model_data_PV_Used_rejects$application_no


model_data_PV_Used_A_R <- data.frame(rbindlist(l=list(model_data_PV_Used,model_data_PV_Used_rejects),use.names = T, fill = T))

assert_data_non_empty(model_data_PV_Used_A_R)
save(model_data_PV_Used_A_R,
     file = file.path(
             get_data_path()$data$intermediate,
             "reject_data",
             "variable_data",
             "model_data_PV_Used_A_R.rdata"
     )
)








rm(list=ls())
