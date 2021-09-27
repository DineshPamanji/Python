############################################################################################
################## 10 - Remove Gated out population  #######################################
############################################################################################


## 0. Load helper functions & libraries ----------------------------------------------------
load_libaries <- file.path("src - DP","utils","load_libraries.R")
source(load_libaries)

io_helper <- file.path("src - DP","utils","io_helper.R")
source(io_helper)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)


## 1. Load gating rules data --------------------------------------------------------------

# load_rdata_output("gating_rules/gating_rules_deal_no_level_vcheck.rdata")
load_rdata_output("gating_rules/gating_rules_deal_no_level_dpd_check.rdata")

rules_2W_new <- gating_rules_deal_no_level %>% filter(loan_type == '2W-New')
rules_PV_new <- gating_rules_deal_no_level %>% filter(loan_type == 'PV-New')
rules_PV_used <- gating_rules_deal_no_level %>% filter(loan_type == 'PV-Used')


## 2. identify gated out ids in 2W New ----------------------------------------------------

common_cols <- c('deal_no')

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


gated_out_2W_new <- rules_2W_new %>% filter(gated_flag == 1) %>% dplyr::select(deal_no,selected_rules)


assert_data_non_empty(gated_out_2W_new)
save(gated_out_2W_new,
     file = file.path(
       get_data_path()$data$output,
       "gating_rules",
       "gated_out_2W_new_vcheck.rdata"
     )
)



## 3. identify gated out ids in PV New ----------------------------------------------------

common_cols <- c('deal_no')

selected_rules <-
        c(
                'Rule_DR_non_Gold_Edu_Agri_90dpd_1mon_GE_1',
                # 'Rule_PO_months_3_Tractor_live_GE_1',
                'Rule_PO_months_3_live_unsec_GE_2',
                'Rule_EN_enquiry_count_1m_GE_6'
                # 'Rule_EN_enquiry_count_6m_GE_9',
                # 'Rule_EN_enquiry_count_3m_GE_5'
        )

req_cols <- c(common_cols,selected_rules)

rules_PV_new <- data.frame(rules_PV_new %>% dplyr::select(req_cols))

rules_PV_new[,'gated_flag'] <- apply(rules_PV_new[,selected_rules], 1, max)


gated_out_PV_new <- rules_PV_new %>% filter(gated_flag == 1) %>% dplyr::select(deal_no, selected_rules)



assert_data_non_empty(gated_out_PV_new)
save(gated_out_PV_new,
     file = file.path(
       get_data_path()$data$output,
       "gating_rules",
       "gated_out_PV_new_vcheck.rdata"
     )
)


# rm(list=ls())

## 4. identify gated out ids in PV Used ----------------------------------------------------

common_cols <- c('deal_no')

# selected_rules <-
#         c(
#                 'Rule_DR_non_Gold_Edu_Agri_90dpd_1mon_GE_1',
#                 'Rule_DR_non_Gold_Edu_Agri_90dpd_6mon_GE_3',
#                 'Rule_PO_months_12_TW_live_GE_2',
#                 'Rule_PO_months_3_live_unsec_GE_2',
#                 # 'Rule_EN_enquiry_count_1m_GE_5',
#                 # 'Rule_EN_enquiry_count_3m_GE_7'
#                 'Rule_EN_enquiry_count_1m_GE_6'
#         )

selected_rules <-
        c(
                # 'Rule_DR_non_Gold_Edu_Agri_90dpd_1mon_GE_1',
                # 'Rule_DR_non_Gold_Edu_Agri_90dpd_6mon_GE_3',
                'Rule_PO_months_12_TW_live_GE_2',
                'Rule_PO_months_3_live_unsec_GE_2',
                # 'Rule_EN_enquiry_count_1m_GE_5',
                # 'Rule_EN_enquiry_count_3m_GE_7'
                'Rule_EN_enquiry_count_1m_GE_6'
        )

req_cols <- c(common_cols,selected_rules)

rules_PV_used <- data.frame(rules_PV_used %>% dplyr::select(req_cols))

rules_PV_used[,'gated_flag'] <- apply(rules_PV_used[,selected_rules], 1, max)


gated_out_PV_used <- rules_PV_used %>% filter(gated_flag == 1) %>% dplyr::select(deal_no, selected_rules)



# assert_data_non_empty(gated_out_PV_used)
# save(gated_out_PV_used,
#      file = file.path(
#              get_data_path()$data$output,
#              "gating_rules",
#              "gated_out_PV_used_vcheck.rdata"
#      )
# )

save(gated_out_PV_used,
     file = file.path(
             get_data_path()$data$output,
             "gating_rules",
             "gated_out_PV_used_dpd_check.rdata"
     )
)

# save(gated_out_PV_used, file = "data//output//gating_rules//gated_out_PV_used.rdata")


## 5. identify gated out ids in 2W Refinance ----------------------------------------------------

load("data/output/gating_rules/gating_rules_deal_no_level_2W_RF.rdata")

common_cols <- c('deal_no')

selected_rules <-
        c(
        #         "Rule_DR_non_Gold_Edu_Agri_90dpd_3mon_GE_1",
        #         "Rule_DR_non_Gold_Edu_Agri_60dpd_6mon_GE_2",
        #         "Rule_DR_non_Gold_Edu_Agri_60dpd_3mon_GE_1",
        #         
        #         "Rule_PO_months_3_CV_live_GE_2",
        #         "Rule_PO_months_3_live_unsec_GE_2",
        #         "Rule_PO_months_3_PL_live_GE_1",
        #         "Rule_PO_months_6_TW_live_GE_1",
        #         "Rule_PO_months_3_BL_live_GE_1",
        #         "Rule_PO_months_6_live_unsec_GE_4",
        #         
        #         "Rule_EN_enquiry_count_3m_GE_5",
        #         "Rule_EN_enquiry_count_6m_GE_7",
        #         "Rule_EN_enquiry_count_1m_GE_3"
                
                "Rule_DR_non_Gold_Edu_Agri_60dpd_3mon_GE_2",
                "Rule_DR_non_Gold_Edu_Agri_60dpd_6mon_GE_5",     # "Rule_DR_non_Gold_Edu_Agri_60dpd_6mon_GE_4",
                "Rule_DR_non_Gold_Edu_Agri_90dpd_3mon_GE_1",
                
                "Rule_PO_months_3_BL_live_GE_2",
                "Rule_PO_months_3_CV_live_GE_2",
                "Rule_PO_months_3_live_unsec_GE_3", # "Rule_PO_months_3_live_unsec_GE_2",
                
                "Rule_PO_months_3_PL_live_GE_1",
                "Rule_PO_months_6_live_unsec_GE_4",
                "Rule_PO_months_3_TW_live_GE_1",    # "Rule_PO_months_6_TW_live_GE_1",
                
                "Rule_EN_enquiry_count_1m_GE_5",
                "Rule_EN_enquiry_count_3m_GE_7",
                "Rule_EN_enquiry_count_6m_GE_9"
                
        )

req_cols <- c(common_cols,selected_rules)

rules_2W_RF <- data.frame(gating_rules_deal_no_level %>% dplyr::select(req_cols))

rules_2W_RF[,'gated_flag'] <- apply(rules_2W_RF[,selected_rules], 1, max)


gated_out_2W_RF <- rules_2W_RF %>% filter(gated_flag == 1) %>% dplyr::select(deal_no, selected_rules)



assert_data_non_empty(gated_out_2W_RF)
save(gated_out_2W_RF,
     file = file.path(
             get_data_path()$data$output,
             "gating_rules",
             "gated_out_2W_RF.rdata"
     )
)

# save(gated_out_2W_RF, file = "data//output//gating_rules//gated_out_2W_RF.rdata")





rm(list=ls())
