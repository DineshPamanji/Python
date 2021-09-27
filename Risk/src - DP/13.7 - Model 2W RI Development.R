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

load_rdata_intermediate("reject_data//variable_data//model_data_2W_New_A_R.rdata")
oot_date_start <- '2018-04-01'
oot_date_end <- '2018-06-30'


oot_data <-
  model_data_2W_New_A_R %>% filter((date >= as.Date(oot_date_start)) &
                                     (date < as.Date(oot_date_end)) &
                                     (Category %in% c('SAL','SENP','SEP')) &
                                     (tag %in% c('A','R1')))

dev_data <-
  model_data_2W_New_A_R %>% filter(UID %notin% unique(oot_data$UID) &
                                     (Category %in% c('SAL','SENP','SEP')) &
                                     (tag %in% c('A','R1')))


rm(model_data_2W_New_A_R)


######################################################################################################################################
## 2. Variable transformation

## 2.1 Vintage
dev_data$Var_credit_vintage_C1 <- ifelse(dev_data$Var_credit_vintage <= 0.05, 0.05, 
                                         ifelse(dev_data$Var_credit_vintage >= 12, 12, dev_data$Var_credit_vintage))
oot_data$Var_credit_vintage_C1 <- ifelse(oot_data$Var_credit_vintage <= 0.05, 0.05, 
                                         ifelse(oot_data$Var_credit_vintage >= 12, 12, oot_data$Var_credit_vintage))


dev_data$Var_credit_vintage_C2 <- ifelse(dev_data$Var_credit_vintage <= 0.05, 0.05, 
                                         ifelse(dev_data$Var_credit_vintage >= 10, 10, dev_data$Var_credit_vintage))
oot_data$Var_credit_vintage_C2 <- ifelse(oot_data$Var_credit_vintage <= 0.05, 0.05, 
                                         ifelse(oot_data$Var_credit_vintage >= 10, 10, oot_data$Var_credit_vintage))


dev_data$Var_credit_vintage_C3 <- ifelse(dev_data$Var_credit_vintage <= 0.05, 0.05, 
                                         ifelse(dev_data$Var_credit_vintage >= 15, 15, dev_data$Var_credit_vintage))
oot_data$Var_credit_vintage_C3 <- ifelse(oot_data$Var_credit_vintage <= 0.5, 0.5, 
                                         ifelse(oot_data$Var_credit_vintage >= 15, 15, oot_data$Var_credit_vintage))

dev_data$Var_credit_vintage_C4 <- ifelse(dev_data$Var_credit_vintage <= 0.05, 0.05, 
                                         ifelse(dev_data$Var_credit_vintage >= 14, 14, dev_data$Var_credit_vintage))
oot_data$Var_credit_vintage_C4 <- ifelse(oot_data$Var_credit_vintage <= 0.5, 0.5, 
                                         ifelse(oot_data$Var_credit_vintage >= 14, 14, oot_data$Var_credit_vintage))



## 2.2 Closed loan sanctioned amount

## WOE 1
subset1 <- dev_data %>% filter(Var_PO_closed_sanction_amount_excl_CC <= 25000)
subset2 <- dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC > 25000) & (Var_PO_closed_sanction_amount_excl_CC <= 50000))
subset3 <- dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC > 50000) & (Var_PO_closed_sanction_amount_excl_CC <= 125000))
subset4 <- dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC > 125000) & (Var_PO_closed_sanction_amount_excl_CC <= 500000))
subset5 <- dev_data %>% filter(Var_PO_closed_sanction_amount_excl_CC > 500000)

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

dev_data$Var_PO_closed_sanction_amount_excl_CC_woe1 <- ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC <= 25000, woe1, 
                                                              ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC > 25000 & dev_data$Var_PO_closed_sanction_amount_excl_CC <= 50000, woe2, 
                                                                     ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC > 50000 & dev_data$Var_PO_closed_sanction_amount_excl_CC <= 125000, woe3, 
                                                                            ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC > 125000 & dev_data$Var_PO_closed_sanction_amount_excl_CC <= 500000,woe4,woe5))))


oot_data$Var_PO_closed_sanction_amount_excl_CC_woe1 <-  ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC <= 25000, woe1, 
                                                               ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC > 25000 & oot_data$Var_PO_closed_sanction_amount_excl_CC <= 50000, woe2, 
                                                                      ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC > 50000 & oot_data$Var_PO_closed_sanction_amount_excl_CC <= 125000, woe3, 
                                                                             ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC > 125000 & oot_data$Var_PO_closed_sanction_amount_excl_CC <= 500000,woe4,woe5))))


rm(subset1,subset2,subset3,subset4, subset5, woe1, woe2, woe3, woe4, woe5)




## WOE 2

subset1 <- dev_data %>% filter(Var_PO_closed_sanction_amount_excl_CC <= 15000)
subset2 <- dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC > 15000) & (Var_PO_closed_sanction_amount_excl_CC <= 100000))
subset3 <- dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC > 100000) & (Var_PO_closed_sanction_amount_excl_CC <= 250000))
subset4 <- dev_data %>% filter(Var_PO_closed_sanction_amount_excl_CC > 250000)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)

woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')

dev_data$Var_PO_closed_sanction_amount_excl_CC_woe2 <- ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC <= 15000, woe1, 
                                                              ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC > 15000 & dev_data$Var_PO_closed_sanction_amount_excl_CC <= 100000, woe2, 
                                                                     ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC > 100000 & dev_data$Var_PO_closed_sanction_amount_excl_CC <= 250000, woe3, woe4)))


oot_data$Var_PO_closed_sanction_amount_excl_CC_woe2 <- ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC <= 15000, woe1, 
                                                              ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC > 15000 & oot_data$Var_PO_closed_sanction_amount_excl_CC <= 100000, woe2, 
                                                                     ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC > 100000 & oot_data$Var_PO_closed_sanction_amount_excl_CC <= 250000, woe3, woe4)))


rm(subset1,subset2,subset3,subset4, woe1, woe2, woe3, woe4)






## WOE 3
subset1 <- dev_data %>% filter(Var_PO_closed_sanction_amount_excl_CC <= 50000)
subset2 <- dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC > 50000) & (Var_PO_closed_sanction_amount_excl_CC <= 100000))
subset3 <- dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC > 100000) & (Var_PO_closed_sanction_amount_excl_CC <= 250000))
subset4 <- dev_data %>% filter(Var_PO_closed_sanction_amount_excl_CC > 250000)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)

woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')

dev_data$Var_PO_closed_sanction_amount_excl_CC_woe3 <- ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC <= 50000, woe1, 
                                                              ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC > 50000 & dev_data$Var_PO_closed_sanction_amount_excl_CC <= 100000, woe2, 
                                                                     ifelse(dev_data$Var_PO_closed_sanction_amount_excl_CC > 100000 & dev_data$Var_PO_closed_sanction_amount_excl_CC <= 250000, woe3, woe4)))


oot_data$Var_PO_closed_sanction_amount_excl_CC_woe3 <- ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC <= 50000, woe1, 
                                                              ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC > 50000 & oot_data$Var_PO_closed_sanction_amount_excl_CC <= 100000, woe2, 
                                                                     ifelse(oot_data$Var_PO_closed_sanction_amount_excl_CC > 100000 & oot_data$Var_PO_closed_sanction_amount_excl_CC <= 250000, woe3, woe4)))


rm(subset1,subset2,subset3,subset4, woe1, woe2, woe3, woe4)





## 2.3 Closed loans excl CC
dev_data$Var_PO_closed_excl_CC_C1 <- dev_data$Var_PO_closed_excl_CC
oot_data$Var_PO_closed_excl_CC_C1 <- oot_data$Var_PO_closed_excl_CC

dev_data$Var_PO_closed_excl_CC_C1[is.na(dev_data$Var_PO_closed_excl_CC_C1)] <- 0
oot_data$Var_PO_closed_excl_CC_C1[is.na(oot_data$Var_PO_closed_excl_CC_C1)] <- 0


dev_data$Var_PO_closed_excl_CC_C1 <- ifelse(dev_data$Var_PO_closed_excl_CC_C1 >= 15, 15, 
                                            ifelse(dev_data$Var_PO_closed_excl_CC_C1 <= 0, 0,dev_data$Var_PO_closed_excl_CC_C1))

oot_data$Var_PO_closed_excl_CC_C1 <- ifelse(oot_data$Var_PO_closed_excl_CC_C1 >= 15, 15, 
                                            ifelse(oot_data$Var_PO_closed_excl_CC_C1 <= 0, 0, oot_data$Var_PO_closed_excl_CC_C1))


dev_data$Var_PO_closed_excl_CC_C2 <- ifelse(dev_data$Var_PO_closed_excl_CC_C1 >= 14, 14, 
                                            ifelse(dev_data$Var_PO_closed_excl_CC_C1 <= 0, 0,dev_data$Var_PO_closed_excl_CC_C1))

oot_data$Var_PO_closed_excl_CC_C2 <- ifelse(oot_data$Var_PO_closed_excl_CC_C1 >= 14, 14, 
                                            ifelse(oot_data$Var_PO_closed_excl_CC_C1 <= 0, 0, oot_data$Var_PO_closed_excl_CC_C1))



dev_data$Var_PO_closed_excl_CC_C3 <- ifelse(dev_data$Var_PO_closed_excl_CC_C1 >= 11, 11, 
                                            ifelse(dev_data$Var_PO_closed_excl_CC_C1 <= 0, 0,dev_data$Var_PO_closed_excl_CC_C1))

oot_data$Var_PO_closed_excl_CC_C3 <- ifelse(oot_data$Var_PO_closed_excl_CC_C1 >= 11, 11, 
                                            ifelse(oot_data$Var_PO_closed_excl_CC_C1 <= 0, 0, oot_data$Var_PO_closed_excl_CC_C1))




## WOE 1
subset1 <- dev_data %>% filter(Var_PO_closed_excl_CC_C1 <= 1)
subset2 <- dev_data %>% filter(Var_PO_closed_excl_CC_C1 == 2)
subset3 <- dev_data %>% filter(Var_PO_closed_excl_CC_C1 > 2)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <- get_3bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe2 <- get_3bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe3 <- get_3bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')


dev_data$Var_PO_closed_excl_CC_woe1 <- ifelse(dev_data$Var_PO_closed_excl_CC_C1 <= 1, woe1, 
                                              ifelse(dev_data$Var_PO_closed_excl_CC_C1 == 2, woe2,woe3))


oot_data$Var_PO_closed_excl_CC_woe1 <- ifelse(oot_data$Var_PO_closed_excl_CC_C1 <= 1, woe1, 
                                              ifelse(oot_data$Var_PO_closed_excl_CC_C1 == 2, woe2,woe3))


rm(subset1,subset2,subset3, woe1, woe2, woe3)








## 2.5 HL paid 12 month flag
dev_data$Var_DL_HL_paid_GE_12mon_flag_var <- dev_data$Var_DL_HL_paid_GE_12mon_flag
oot_data$Var_DL_HL_paid_GE_12mon_flag_var <- oot_data$Var_DL_HL_paid_GE_12mon_flag

dev_data$Var_DL_HL_paid_GE_12mon_flag_var[is.na(dev_data$Var_DL_HL_paid_GE_12mon_flag_var)] <- 0
oot_data$Var_DL_HL_paid_GE_12mon_flag_var[is.na(oot_data$Var_DL_HL_paid_GE_12mon_flag_var)] <- 0




## 2.8 HL paid 12 month Continuous
dev_data$Var_DL_HL_paid_GE_12mon_C <- dev_data$Var_DL_HL_paid_GE_12mon
oot_data$Var_DL_HL_paid_GE_12mon_C <- oot_data$Var_DL_HL_paid_GE_12mon

dev_data$Var_DL_HL_paid_GE_12mon_C[is.na(dev_data$Var_DL_HL_paid_GE_12mon_C)] <- 0
oot_data$Var_DL_HL_paid_GE_12mon_C[is.na(oot_data$Var_DL_HL_paid_GE_12mon_C)] <- 0


dev_data$Var_DL_HL_paid_GE_12mon_C1 <- ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C >= 3, 3, 
                                              ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C1 <= 0, 0,dev_data$Var_DL_HL_paid_GE_12mon_C))

oot_data$Var_DL_HL_paid_GE_12mon_C1 <- ifelse(oot_data$Var_DL_HL_paid_GE_12mon_C >= 3, 3, 
                                              ifelse(oot_data$Var_DL_HL_paid_GE_12mon_C <= 0, 0, oot_data$Var_DL_HL_paid_GE_12mon_C))


dev_data$Var_DL_HL_paid_GE_12mon_C2 <- ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C >= 4, 4, 
                                              ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C1 <= 0, 0,dev_data$Var_DL_HL_paid_GE_12mon_C))

oot_data$Var_DL_HL_paid_GE_12mon_C2 <- ifelse(oot_data$Var_DL_HL_paid_GE_12mon_C >= 4, 4, 
                                              ifelse(oot_data$Var_DL_HL_paid_GE_12mon_C <= 0, 0, oot_data$Var_DL_HL_paid_GE_12mon_C))



subset1 <- dev_data %>% filter(Var_DL_HL_paid_GE_12mon_C1 == 0)
subset2 <- dev_data %>% filter(Var_DL_HL_paid_GE_12mon_C1 == 1)
subset3 <- dev_data %>% filter(Var_DL_HL_paid_GE_12mon_C1 >= 2)


mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <- get_3bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe2 <- get_3bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe3 <- get_3bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')


dev_data$Var_DL_HL_paid_GE_12mon_woe1 <- ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C1 == 0, woe1, 
                                                ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C1 == 1, woe2,woe3))


oot_data$Var_DL_HL_paid_GE_12mon_woe1 <- ifelse(oot_data$Var_DL_HL_paid_GE_12mon_C1 == 0, woe1, 
                                                ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C1 == 1, woe2,woe3))


rm(subset1,subset2,subset3, woe1, woe2, woe3)







## 2.5 Gold paid 12 month flag
dev_data$Var_DL_Gold_paid_GE_12mon_flag_var <- dev_data$Var_DL_Gold_paid_GE_12mon_flag
oot_data$Var_DL_Gold_paid_GE_12mon_flag_var <- oot_data$Var_DL_Gold_paid_GE_12mon_flag

dev_data$Var_DL_Gold_paid_GE_12mon_flag_var[is.na(dev_data$Var_DL_Gold_paid_GE_12mon_flag_var)] <- 0
oot_data$Var_DL_Gold_paid_GE_12mon_flag_var[is.na(oot_data$Var_DL_Gold_paid_GE_12mon_flag_var)] <- 0




## 2.8 gold paid 12 month Continuous
dev_data$Var_DL_Gold_paid_GE_12mon_C1 <- dev_data$Var_DL_Gold_paid_GE_12mon
oot_data$Var_DL_Gold_paid_GE_12mon_C1 <- oot_data$Var_DL_Gold_paid_GE_12mon

dev_data$Var_DL_Gold_paid_GE_12mon_C1[is.na(dev_data$Var_DL_Gold_paid_GE_12mon_C1)] <- 0
oot_data$Var_DL_Gold_paid_GE_12mon_C1[is.na(oot_data$Var_DL_Gold_paid_GE_12mon_C1)] <- 0




dev_data$Var_DL_Gold_paid_GE_12mon_C2 <- ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C1 >= 6, 6, 
                                                ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C1 <= 0, 0,dev_data$Var_DL_Gold_paid_GE_12mon_C1))

oot_data$Var_DL_Gold_paid_GE_12mon_C2 <- ifelse(oot_data$Var_DL_Gold_paid_GE_12mon_C1 >= 6, 6, 
                                                ifelse(oot_data$Var_DL_Gold_paid_GE_12mon_C1 <= 0, 0, oot_data$Var_DL_Gold_paid_GE_12mon_C1))



dev_data$Var_DL_Gold_paid_GE_12mon_C3 <- ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C1 >= 7, 7, 
                                                ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C1 <= 0, 0,dev_data$Var_DL_Gold_paid_GE_12mon_C1))

oot_data$Var_DL_Gold_paid_GE_12mon_C3 <- ifelse(oot_data$Var_DL_Gold_paid_GE_12mon_C1 >= 7, 7, 
                                                ifelse(oot_data$Var_DL_Gold_paid_GE_12mon_C1 <= 0, 0, oot_data$Var_DL_Gold_paid_GE_12mon_C1))





subset1 <- dev_data %>% filter(Var_DL_Gold_paid_GE_12mon_C1 == 0)
subset2 <- dev_data %>% filter(Var_DL_Gold_paid_GE_12mon_C1 == 1)
subset3 <- dev_data %>% filter(Var_DL_Gold_paid_GE_12mon_C1 >= 2)


mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <- get_3bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe2 <- get_3bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe3 <- get_3bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')


dev_data$Var_DL_Gold_paid_GE_12mon_woe1 <- ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C1 == 0, woe1, 
                                                  ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C1 == 1, woe2,woe3))


oot_data$Var_DL_Gold_paid_GE_12mon_woe1 <- ifelse(oot_data$Var_DL_Gold_paid_GE_12mon_C1 == 0, woe1, 
                                                  ifelse(dev_data$Var_DL_Gold_paid_GE_12mon_C1 == 1, woe2,woe3))


rm(subset1,subset2,subset3, woe1, woe2, woe3)





### Enquiry variables

dev_data$Var_EN_enquiry_count_3m_C <- dev_data$Var_EN_enquiry_count_3m
oot_data$Var_EN_enquiry_count_3m_C <- oot_data$Var_EN_enquiry_count_3m

dev_data$Var_EN_enquiry_count_3m_C[is.na(dev_data$Var_EN_enquiry_count_3m_C)] <- 0
oot_data$Var_EN_enquiry_count_3m_C[is.na(oot_data$Var_EN_enquiry_count_3m_C)] <- 0

dev_data$Var_EN_enquiry_count_3m_C1 <- ifelse(dev_data$Var_EN_enquiry_count_3m_C <= 0, 0, 
                                              ifelse(dev_data$Var_EN_enquiry_count_3m_C >= 3, 3, dev_data$Var_EN_enquiry_count_3m_C))

oot_data$Var_EN_enquiry_count_3m_C1 <- ifelse(oot_data$Var_EN_enquiry_count_3m_C <= 0, 0, 
                                              ifelse(oot_data$Var_EN_enquiry_count_3m_C >= 3, 3, oot_data$Var_EN_enquiry_count_3m_C))


dev_data$Var_EN_enquiry_count_3m_C2 <- ifelse(dev_data$Var_EN_enquiry_count_3m_C <= 0, 0, 
                                              ifelse(dev_data$Var_EN_enquiry_count_3m_C >= 4, 4, dev_data$Var_EN_enquiry_count_3m_C))

oot_data$Var_EN_enquiry_count_3m_C2 <- ifelse(oot_data$Var_EN_enquiry_count_3m_C <= 0, 0, 
                                              ifelse(oot_data$Var_EN_enquiry_count_3m_C >= 4, 4, oot_data$Var_EN_enquiry_count_3m_C))




### Enquiry count 3m non CC
dev_data$Var_EN_enquiry_count_3m_non_CC_C1 <- dev_data$Var_EN_enquiry_count_3m_non_CC
oot_data$Var_EN_enquiry_count_3m_non_CC_C1 <- oot_data$Var_EN_enquiry_count_3m_non_CC

dev_data$Var_EN_enquiry_count_3m_non_CC_C1[is.na(dev_data$Var_EN_enquiry_count_3m_non_CC_C1)] <- 0
oot_data$Var_EN_enquiry_count_3m_non_CC_C1[is.na(oot_data$Var_EN_enquiry_count_3m_non_CC_C1)] <- 0

dev_data$Var_EN_enquiry_count_3m_non_CC_C2 <- ifelse(dev_data$Var_EN_enquiry_count_3m_non_CC_C1 <= 0, 0, 
                                                     ifelse(dev_data$Var_EN_enquiry_count_3m_non_CC_C1 >= 3, 3, dev_data$Var_EN_enquiry_count_3m_non_CC_C1))

oot_data$Var_EN_enquiry_count_3m_non_CC_C2 <- ifelse(oot_data$Var_EN_enquiry_count_3m_non_CC_C1 <= 0, 0, 
                                                     ifelse(oot_data$Var_EN_enquiry_count_3m_non_CC_C1 >= 3, 3, oot_data$Var_EN_enquiry_count_3m_non_CC_C1))


dev_data$Var_EN_enquiry_count_3m_non_CC_C3 <- ifelse(dev_data$Var_EN_enquiry_count_3m_non_CC_C1 <= 0, 0, 
                                                     ifelse(dev_data$Var_EN_enquiry_count_3m_non_CC_C1 >= 5, 5, dev_data$Var_EN_enquiry_count_3m_non_CC_C1))

oot_data$Var_EN_enquiry_count_3m_non_CC_C3 <- ifelse(oot_data$Var_EN_enquiry_count_3m_non_CC_C1 <= 0, 0, 
                                                     ifelse(oot_data$Var_EN_enquiry_count_3m_non_CC_C1 >= 5, 5, oot_data$Var_EN_enquiry_count_3m_non_CC_C1))


### Enquiry count 3m non CC CD
dev_data$Var_EN_enquiry_count_3m_non_CC_CD_C1 <- dev_data$Var_EN_enquiry_count_3m_non_CC_CD
oot_data$Var_EN_enquiry_count_3m_non_CC_CD_C1 <- oot_data$Var_EN_enquiry_count_3m_non_CC_CD

dev_data$Var_EN_enquiry_count_3m_non_CC_CD_C1[is.na(dev_data$Var_EN_enquiry_count_3m_non_CC_CD_C1)] <- 0
oot_data$Var_EN_enquiry_count_3m_non_CC_CD_C1[is.na(oot_data$Var_EN_enquiry_count_3m_non_CC_CD_C1)] <- 0


dev_data$Var_EN_enquiry_count_3m_non_CC_CD_C2 <- ifelse(dev_data$Var_EN_enquiry_count_3m_non_CC_CD_C1 <= 0, 0, 
                                                        ifelse(dev_data$Var_EN_enquiry_count_3m_non_CC_CD_C1 >= 3, 3, dev_data$Var_EN_enquiry_count_3m_non_CC_CD_C1))

oot_data$Var_EN_enquiry_count_3m_non_CC_CD_C2 <- ifelse(oot_data$Var_EN_enquiry_count_3m_non_CC_CD_C1 <= 0, 0, 
                                                        ifelse(oot_data$Var_EN_enquiry_count_3m_non_CC_CD_C1 >= 3, 3, oot_data$Var_EN_enquiry_count_3m_non_CC_CD_C1))


dev_data$Var_EN_enquiry_count_3m_non_CC_CD_C3 <- ifelse(dev_data$Var_EN_enquiry_count_3m_non_CC_CD_C1 <= 0, 0, 
                                                        ifelse(dev_data$Var_EN_enquiry_count_3m_non_CC_CD_C1 >= 5, 5, dev_data$Var_EN_enquiry_count_3m_non_CC_CD_C1))

oot_data$Var_EN_enquiry_count_3m_non_CC_CD_C3 <- ifelse(oot_data$Var_EN_enquiry_count_3m_non_CC_CD_C1 <= 0, 0, 
                                                        ifelse(oot_data$Var_EN_enquiry_count_3m_non_CC_CD_C1 >= 5, 5, oot_data$Var_EN_enquiry_count_3m_non_CC_CD_C1))





### Enquiry count 6m 
dev_data$Var_EN_enquiry_count_6m_C1 <- dev_data$Var_EN_enquiry_count_6m
oot_data$Var_EN_enquiry_count_6m_C1 <- oot_data$Var_EN_enquiry_count_6m

dev_data$Var_EN_enquiry_count_6m_C1[is.na(dev_data$Var_EN_enquiry_count_6m_C1)] <- 0
oot_data$Var_EN_enquiry_count_6m_C1[is.na(oot_data$Var_EN_enquiry_count_6m_C1)] <- 0


dev_data$Var_EN_enquiry_count_6m_C2 <- ifelse(dev_data$Var_EN_enquiry_count_6m_C1 <= 0, 0, 
                                              ifelse(dev_data$Var_EN_enquiry_count_6m_C1 >= 4, 4, dev_data$Var_EN_enquiry_count_6m_C1))

oot_data$Var_EN_enquiry_count_6m_C2 <- ifelse(oot_data$Var_EN_enquiry_count_6m_C1 <= 0, 0, 
                                              ifelse(oot_data$Var_EN_enquiry_count_6m_C1 >= 4, 4, oot_data$Var_EN_enquiry_count_6m_C1))


dev_data$Var_EN_enquiry_count_6m_C3 <- ifelse(dev_data$Var_EN_enquiry_count_6m_C1 <= 0, 0, 
                                              ifelse(dev_data$Var_EN_enquiry_count_6m_C1 >= 5, 5, dev_data$Var_EN_enquiry_count_6m_C1))

oot_data$Var_EN_enquiry_count_6m_C3 <- ifelse(oot_data$Var_EN_enquiry_count_6m_C1 <= 0, 0, 
                                              ifelse(oot_data$Var_EN_enquiry_count_6m_C1 >= 5, 5, oot_data$Var_EN_enquiry_count_6m_C1))





###  Enquiry count 6m non CC
dev_data$Var_EN_enquiry_count_6m_non_CC_C1 <- dev_data$Var_EN_enquiry_count_6m_non_CC
oot_data$Var_EN_enquiry_count_6m_non_CC_C1 <- oot_data$Var_EN_enquiry_count_6m_non_CC

dev_data$Var_EN_enquiry_count_6m_non_CC_C1[is.na(dev_data$Var_EN_enquiry_count_6m_non_CC_C1)] <- 0
oot_data$Var_EN_enquiry_count_6m_non_CC_C1[is.na(oot_data$Var_EN_enquiry_count_6m_non_CC_C1)] <- 0


dev_data$Var_EN_enquiry_count_6m_non_CC_C2 <- ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C1 <= 0, 0, 
                                                     ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C1 >= 4, 4, dev_data$Var_EN_enquiry_count_6m_non_CC_C1))

oot_data$Var_EN_enquiry_count_6m_non_CC_C2 <- ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C1 <= 0, 0, 
                                                     ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C1 >= 4, 4, oot_data$Var_EN_enquiry_count_6m_non_CC_C1))


dev_data$Var_EN_enquiry_count_6m_non_CC_C3 <- ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C1 <= 0, 0, 
                                                     ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C1 >= 5, 5, dev_data$Var_EN_enquiry_count_6m_non_CC_C1))

oot_data$Var_EN_enquiry_count_6m_non_CC_C3 <- ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C1 <= 0, 0, 
                                                     ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C1 >= 5, 5, oot_data$Var_EN_enquiry_count_6m_non_CC_C1))


dev_data$Var_EN_enquiry_count_6m_non_CC_C4 <- ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C1 <= 0, 0, 
                                                     ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_C1 >= 6, 6, dev_data$Var_EN_enquiry_count_6m_non_CC_C1))

oot_data$Var_EN_enquiry_count_6m_non_CC_C4 <- ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C1 <= 0, 0, 
                                                     ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_C1 >= 6, 6, oot_data$Var_EN_enquiry_count_6m_non_CC_C1))


###  Enquiry count 6m non CC CD
dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 <- dev_data$Var_EN_enquiry_count_6m_non_CC_CD
oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 <- oot_data$Var_EN_enquiry_count_6m_non_CC_CD

dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1[is.na(dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1)] <- 0
oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1[is.na(oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1)] <- 0



dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C2 <- ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 <= 0, 0, 
                                                        ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 >= 4, 4, dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1))

oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C2 <- ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 <= 0, 0, 
                                                        ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 >= 4, 4, oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1))


dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C3 <- ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 <= 0, 0, 
                                                        ifelse(dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 >= 5, 5, dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1))

oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C3 <- ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 <= 0, 0, 
                                                        ifelse(oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 >= 5, 5, oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1))








## 30 DPD in 3 mon
dev_data$Var_DL_all_30dpd_3mon_C1 <- dev_data$Var_DL_all_30dpd_3mon
oot_data$Var_DL_all_30dpd_3mon_C1 <- oot_data$Var_DL_all_30dpd_3mon

dev_data$Var_DL_all_30dpd_3mon_C1[is.na(dev_data$Var_DL_all_30dpd_3mon_C1)] <- 0
oot_data$Var_DL_all_30dpd_3mon_C1[is.na(oot_data$Var_DL_all_30dpd_3mon_C1)] <- 0

dev_data$Var_DL_all_30dpd_3mon_flag <- ifelse(dev_data$Var_DL_all_30dpd_3mon_C1 == 0, 0, 1)
oot_data$Var_DL_all_30dpd_3mon_flag <- ifelse(oot_data$Var_DL_all_30dpd_3mon_C1 == 0, 0, 1)



## 30 DPD in 6 mon
dev_data$Var_DL_all_30dpd_6mon_C1 <- dev_data$Var_DL_all_30dpd_6mon
oot_data$Var_DL_all_30dpd_6mon_C1 <- oot_data$Var_DL_all_30dpd_6mon

dev_data$Var_DL_all_30dpd_6mon_C1[is.na(dev_data$Var_DL_all_30dpd_6mon_C1)] <- 0
oot_data$Var_DL_all_30dpd_6mon_C1[is.na(oot_data$Var_DL_all_30dpd_6mon_C1)] <- 0

dev_data$Var_DL_all_30dpd_6mon_flag <- ifelse(dev_data$Var_DL_all_30dpd_6mon_C1 == 0, 0, 1)
oot_data$Var_DL_all_30dpd_6mon_flag <- ifelse(oot_data$Var_DL_all_30dpd_6mon_C1 == 0, 0, 1)


dev_data$Var_DL_all_30dpd_6mon_C2 <- ifelse(dev_data$Var_DL_all_30dpd_6mon_C1 <= 0, 0, 
                                            ifelse(dev_data$Var_DL_all_30dpd_6mon_C1 >= 5, 5, dev_data$Var_DL_all_30dpd_6mon_C1))

oot_data$Var_DL_all_30dpd_6mon_C2 <- ifelse(oot_data$Var_DL_all_30dpd_6mon_C1 <= 0, 0, 
                                            ifelse(oot_data$Var_DL_all_30dpd_6mon_C1 >= 5, 5, oot_data$Var_DL_all_30dpd_6mon_C1))




## 30 DPD in 3m mon CC CD
dev_data$Var_DL_non_CC_CD_30dpd_3mon_C1 <- dev_data$Var_DL_non_CC_CD_30dpd_3mon
oot_data$Var_DL_non_CC_CD_30dpd_3mon_C1 <- oot_data$Var_DL_non_CC_CD_30dpd_3mon

dev_data$Var_DL_non_CC_CD_30dpd_3mon_C1[is.na(dev_data$Var_DL_non_CC_CD_30dpd_3mon_C1)] <- 0
oot_data$Var_DL_non_CC_CD_30dpd_3mon_C1[is.na(oot_data$Var_DL_non_CC_CD_30dpd_3mon_C1)] <- 0


dev_data$Var_DL_non_CC_CD_30dpd_3mon_flag <- ifelse(dev_data$Var_DL_non_CC_CD_30dpd_3mon_C1 == 0, 0, 1)
oot_data$Var_DL_non_CC_CD_30dpd_3mon_flag <- ifelse(oot_data$Var_DL_non_CC_CD_30dpd_3mon_C1 == 0, 0, 1)



## 30 DPD in 6m mon CC CD
dev_data$Var_DL_non_CC_CD_30dpd_6mon_C1 <- dev_data$Var_DL_non_CC_CD_30dpd_6mon
oot_data$Var_DL_non_CC_CD_30dpd_6mon_C1 <- oot_data$Var_DL_non_CC_CD_30dpd_6mon

dev_data$Var_DL_non_CC_CD_30dpd_6mon_C1[is.na(dev_data$Var_DL_non_CC_CD_30dpd_6mon_C1)] <- 0
oot_data$Var_DL_non_CC_CD_30dpd_6mon_C1[is.na(oot_data$Var_DL_non_CC_CD_30dpd_6mon_C1)] <- 0


dev_data$Var_DL_non_CC_CD_30dpd_6mon_flag <- ifelse(dev_data$Var_DL_non_CC_CD_30dpd_6mon_C1 == 0, 0, 1)
oot_data$Var_DL_non_CC_CD_30dpd_6mon_flag <- ifelse(oot_data$Var_DL_non_CC_CD_30dpd_6mon_C1 == 0, 0, 1)










## 60 DPD in 3 mon
dev_data$Var_DL_all_60dpd_3mon_C1 <- dev_data$Var_DL_all_60dpd_3mon
oot_data$Var_DL_all_60dpd_3mon_C1 <- oot_data$Var_DL_all_60dpd_3mon

dev_data$Var_DL_all_60dpd_3mon_C1[is.na(dev_data$Var_DL_all_60dpd_3mon_C1)] <- 0
oot_data$Var_DL_all_60dpd_3mon_C1[is.na(oot_data$Var_DL_all_60dpd_3mon_C1)] <- 0

dev_data$Var_DL_all_60dpd_3mon_flag <- ifelse(dev_data$Var_DL_all_60dpd_3mon_C1 == 0, 0, 1)
oot_data$Var_DL_all_60dpd_3mon_flag <- ifelse(oot_data$Var_DL_all_60dpd_3mon_C1 == 0, 0, 1)



## 60 DPD in 6 mon
dev_data$Var_DL_all_60dpd_6mon_C1 <- dev_data$Var_DL_all_60dpd_6mon
oot_data$Var_DL_all_60dpd_6mon_C1 <- oot_data$Var_DL_all_60dpd_6mon

dev_data$Var_DL_all_60dpd_6mon_C1[is.na(dev_data$Var_DL_all_60dpd_6mon_C1)] <- 0
oot_data$Var_DL_all_60dpd_6mon_C1[is.na(oot_data$Var_DL_all_60dpd_6mon_C1)] <- 0

dev_data$Var_DL_all_60dpd_6mon_flag <- ifelse(dev_data$Var_DL_all_60dpd_6mon_C1 == 0, 0, 1)
oot_data$Var_DL_all_60dpd_6mon_flag <- ifelse(oot_data$Var_DL_all_60dpd_6mon_C1 == 0, 0, 1)



## 60 DPD in 3m mon CC CD
dev_data$Var_DL_non_CC_CD_60dpd_3mon_C1 <- dev_data$Var_DL_non_CC_CD_60dpd_3mon
oot_data$Var_DL_non_CC_CD_60dpd_3mon_C1 <- oot_data$Var_DL_non_CC_CD_60dpd_3mon

dev_data$Var_DL_non_CC_CD_60dpd_3mon_C1[is.na(dev_data$Var_DL_non_CC_CD_60dpd_3mon_C1)] <- 0
oot_data$Var_DL_non_CC_CD_60dpd_3mon_C1[is.na(oot_data$Var_DL_non_CC_CD_60dpd_3mon_C1)] <- 0


dev_data$Var_DL_non_CC_CD_60dpd_3mon_flag <- ifelse(dev_data$Var_DL_non_CC_CD_60dpd_3mon_C1 == 0, 0, 1)
oot_data$Var_DL_non_CC_CD_60dpd_3mon_flag <- ifelse(oot_data$Var_DL_non_CC_CD_60dpd_3mon_C1 == 0, 0, 1)



## 60 DPD in 6m mon CC CD
dev_data$Var_DL_non_CC_CD_60dpd_6mon_C1 <- dev_data$Var_DL_non_CC_CD_60dpd_6mon
oot_data$Var_DL_non_CC_CD_60dpd_6mon_C1 <- oot_data$Var_DL_non_CC_CD_60dpd_6mon

dev_data$Var_DL_non_CC_CD_60dpd_6mon_C1[is.na(dev_data$Var_DL_non_CC_CD_60dpd_6mon_C1)] <- 0
oot_data$Var_DL_non_CC_CD_60dpd_6mon_C1[is.na(oot_data$Var_DL_non_CC_CD_60dpd_6mon_C1)] <- 0


dev_data$Var_DL_non_CC_CD_60dpd_6mon_flag <- ifelse(dev_data$Var_DL_non_CC_CD_60dpd_6mon_C1 == 0, 0, 1)
oot_data$Var_DL_non_CC_CD_60dpd_6mon_flag <- ifelse(oot_data$Var_DL_non_CC_CD_60dpd_6mon_C1 == 0, 0, 1)




### SENP_SEP Flag
dev_data$category_flag_SENP_SEP <- ifelse(dev_data$Category %in% c('SENP', 'SEP'), 1, 0)
oot_data$category_flag_SENP_SEP <- ifelse(oot_data$Category %in% c('SENP', 'SEP'), 1, 0)







### cross tab variable

dev_data$Var_sanctioned_amount_live_loans_C1 <- dev_data$Var_sanctioned_amount_live_loans
oot_data$Var_sanctioned_amount_live_loans_C1 <- oot_data$Var_sanctioned_amount_live_loans

dev_data$Var_sanctioned_amount_live_loans_C1[is.na(dev_data$Var_sanctioned_amount_live_loans_C1)] <- 0
oot_data$Var_sanctioned_amount_live_loans_C1[is.na(oot_data$Var_sanctioned_amount_live_loans_C1)] <- 0




# subset1 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 <= 150000)
# subset2 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 > 150000 & Var_sanctioned_amount_live_loans_C1 <= 1000000)
# subset3 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 > 1000000)
# 
# 
# mean(subset1$bad_loan)
# mean(subset2$bad_loan)
# mean(subset3$bad_loan)


dev_data$Var_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 <= 150000, 'bin_LE_150k', 
                                                        ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 > 150000 & dev_data$Var_sanctioned_amount_live_loans_C1 <= 1000000, 'bin_150k_to_10L', 'bin_GE_10L'))


oot_data$Var_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 <= 150000, 'bin_LE_155k', 
                                                        ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 > 150000 & oot_data$Var_sanctioned_amount_live_loans_C1 <= 1000000, 'bin_150k_to_10L', 'bin_GE_10L'))






dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <- dev_data$Var_outstanding_by_sanctioned_amount_live_loans
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <- oot_data$Var_outstanding_by_sanctioned_amount_live_loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1[is.na(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1)] <- 1.08
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1[is.na(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1)] <- 1.08



# subset1 <- dev_data %>% filter(Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.25)
# subset2 <- dev_data %>% filter(Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.25 & Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.5)
# subset3 <- dev_data %>% filter(Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.5)
# 
# 
# mean(subset1$bad_loan)
# mean(subset2$bad_loan)
# mean(subset3$bad_loan)


dev_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.25 & dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.5, 'val_25_to_50_pct','val_GE_50_pct'))


oot_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.25 & oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.5, 'val_25_to_50_pct','val_GE_50_pct'))

# 
# test <- dev_data %>% group_by(Var_sanctioned_amount_live_loans_bin, Var_outstanding_by_sanctioned_amount_live_loans_bin)  %>% summarise(n=n(),
#                                                                                                                                 default = mean(bad_loan))
# write.csv(test, "combined_model_cross_tab_vf.csv")




subset1 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_150k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct')))
subset2 <- dev_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_150k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_150k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct'))))
subset3 <- dev_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_150k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_50_pct','val_GE_50_pct'))))
subset4 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct')))

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)

woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')


subset1_oot <- oot_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_150k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct')))
subset2_oot <- oot_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_150k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_150k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct'))))
subset3_oot <- oot_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_150k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_50_pct','val_GE_50_pct'))))
subset4_oot <- oot_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct')))

mean(subset1_oot$bad_loan)
mean(subset2_oot$bad_loan)
mean(subset3_oot$bad_loan)
mean(subset4_oot$bad_loan)


dev_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live <- ifelse(dev_data$deal_no %in% unique(subset1$deal_no), woe1,
                                                                        ifelse(dev_data$deal_no %in% unique(subset2$deal_no), woe2,
                                                                               ifelse(dev_data$deal_no %in% unique(subset3$deal_no), woe3,
                                                                                      woe4)))

oot_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live <- ifelse(oot_data$deal_no %in% unique(subset1_oot$deal_no), woe1,
                                                                        ifelse(oot_data$deal_no %in% unique(subset2_oot$deal_no), woe2,
                                                                               ifelse(oot_data$deal_no %in% unique(subset3_oot$deal_no), woe3,
                                                                                      woe4)))


rm(subset1,subset2,subset3,subset4,subset1_oot,subset2_oot,subset3_oot,subset4_oot, woe1, woe2, woe3, woe4)





dev_data$agri_profile_flag <- ifelse(is.na(dev_data$agri_profile_flag), 0 , dev_data$agri_profile_flag)
oot_data$agri_profile_flag <- ifelse(is.na(oot_data$agri_profile_flag), 0 , oot_data$agri_profile_flag)








selected_features <- c(
                       'Var_credit_vintage_C1',
                       'Var_credit_vintage_C2',
                       'Var_credit_vintage_C3',
                       'Var_credit_vintage_C4',
                       
                       'Var_PO_closed_sanction_amount_excl_CC_woe1',
                       'Var_PO_closed_sanction_amount_excl_CC_woe2',
                       'Var_PO_closed_sanction_amount_excl_CC_woe3',
                       
                       
                       'Var_PO_closed_excl_CC_C1',
                       'Var_PO_closed_excl_CC_C2',
                       'Var_PO_closed_excl_CC_C3',
                       'Var_PO_closed_excl_CC_woe1',
                       
                       'cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live',
                       
                       
                       'Var_DL_HL_paid_GE_12mon_flag_var',
                       'Var_DL_HL_paid_GE_12mon_C1',
                       'Var_DL_HL_paid_GE_12mon_woe1',
                       
                       'Var_DL_Gold_paid_GE_12mon_flag_var',
                       'Var_DL_Gold_paid_GE_12mon_C1',
                       'Var_DL_Gold_paid_GE_12mon_woe1',
                       
                       'Var_EN_enquiry_count_3m_C1',
                       'Var_EN_enquiry_count_3m_C2',
                       
                       'Var_EN_enquiry_count_3m_non_CC_C1',
                       'Var_EN_enquiry_count_3m_non_CC_C2',
                       'Var_EN_enquiry_count_3m_non_CC_C3',
                       
                       'Var_EN_enquiry_count_3m_non_CC_CD_C1',
                       'Var_EN_enquiry_count_3m_non_CC_CD_C2',
                       'Var_EN_enquiry_count_3m_non_CC_CD_C3',
                       
                       
                       'Var_EN_enquiry_count_6m_C1',
                       'Var_EN_enquiry_count_6m_C2',
                       'Var_EN_enquiry_count_6m_C3',
                       
                       'Var_EN_enquiry_count_6m_non_CC_C1',
                       'Var_EN_enquiry_count_6m_non_CC_C2',
                       'Var_EN_enquiry_count_6m_non_CC_C3',
                       'Var_EN_enquiry_count_6m_non_CC_C4',
                       
                       'Var_EN_enquiry_count_6m_non_CC_CD_C1',
                       'Var_EN_enquiry_count_6m_non_CC_CD_C2',
                       'Var_EN_enquiry_count_6m_non_CC_CD_C3',
                       
                       'Var_DL_all_30dpd_3mon_C1',
                       'Var_DL_all_30dpd_3mon_flag',
                       'Var_DL_all_30dpd_6mon_C1',
                       'Var_DL_all_30dpd_6mon_flag',
                       
                       'Var_DL_non_CC_CD_30dpd_3mon_C1',
                       'Var_DL_non_CC_CD_30dpd_3mon_flag',
                       'Var_DL_non_CC_CD_30dpd_6mon_C1',
                       'Var_DL_non_CC_CD_30dpd_6mon_flag',
                       
                       'Var_DL_all_60dpd_3mon_C1',
                       'Var_DL_all_60dpd_3mon_flag',
                       'Var_DL_all_60dpd_6mon_C1',
                       'Var_DL_all_60dpd_6mon_flag',
                       
                       'Var_DL_non_CC_CD_60dpd_3mon_C1',
                       'Var_DL_non_CC_CD_60dpd_3mon_flag',
                       'Var_DL_non_CC_CD_60dpd_6mon_C1',
                       'Var_DL_non_CC_CD_60dpd_6mon_flag',
                       
                       'category_flag_SENP_SEP'
)



## Stepwise model ----------------------------------------------------------------------------------------------------------
set.seed(1)
split <- sample.split(dev_data$bad_loan, SplitRatio = 0.8)
train_data <- data.frame(subset(dev_data, split == TRUE))
test_data <- data.frame(subset(dev_data, split == FALSE))


loop_model <- glm(bad_loan ~., data = train_data %>% dplyr::select(c(selected_features,'bad_loan')), family = binomial("logit"))

qchisq(0.05,1, lower.tail = F)
# 3.841459

loop_model_selected <- stepAIC(loop_model,
                               family = binomial,
                               data = train_data,
                               k = qchisq(0.05,1, lower.tail = F))

# loop_model_tidy <- tidy(loop_model_selected)
summary(loop_model_selected)


train_data_stepwise <- get_predictions(loop_model_selected, train_data)
test_data_stepwise <- get_predictions(loop_model_selected, test_data)
oot_data_stepwise <- get_predictions(loop_model_selected, oot_data)


model_performance_stepwise <- get_model_performance(train_data_stepwise,test_data_stepwise,oot_data_stepwise)
model_performance_stepwise


RO_stepwise <- get_RO(train_data_stepwise, test_data_stepwise,oot_data_stepwise)

rm(train_data_stepwise, test_data_stepwise,oot_data_stepwise)






## single variable model --------------------------------------------------------------------------------------------------
set.seed(1)
split <- sample.split(dev_data$bad_loan, SplitRatio = 0.8)
train_data_temp <- data.frame(subset(dev_data, split == TRUE))
test_data_temp <- data.frame(subset(dev_data, split == FALSE))


## 3.2 single feature model - iteration
single_variable_models <- get_single_variable_models(train = train_data_temp, test = test_data_temp, oot = oot_data, feature_list = selected_features)

rm(train_data_temp, test_data_temp)



########################################################################################################################


shortlisted_var <- c( 
  'Var_credit_vintage_C4',
  'Var_PO_closed_excl_CC_woe1',
  'cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live',
  'Var_DL_HL_paid_GE_12mon_C1',
  'Var_DL_Gold_paid_GE_12mon_woe1',
  'Var_EN_enquiry_count_6m_non_CC_C1',
  'Var_DL_all_30dpd_6mon_flag',
  'category_flag_SENP_SEP',
  'agri_profile_flag'
)


seed_iterations_output <- get_seed_iterations(seed_list = c(1:100),
                                              dev_data = dev_data,
                                              oot = oot_data,
                                              feature_list = shortlisted_var, 
                                              p_value_threshold = 0.05)

selected_seeds <- seed_iterations_output %>% filter((p_value_check == 0) & 
                                                      (RO_decile_overall >= 2) & 
                                                      (RO_pentile_overall >= 2))




selected_seed <- 92

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



dev_A_R1 <- train_data_final %>% filter(tag %in% c('A','R1'))
dev_A_R1$prediction_decile <- ntile(dev_A_R1$predictions,10)

population_pct <- dev_A_R1 %>% group_by(prediction_decile,tag) %>% summarise(count = length(unique(UID)))
delinquency_pct <- dev_A_R1 %>% group_by(prediction_decile,tag) %>% summarise(default = mean(bad_loan))



output_list <- list("population_pct_A_R1" = population_pct,
                    "delinquency_pct_A_R1" = delinquency_pct
                  
)


save_xlsx_output(data = output_list, relative_path = "//model//2W_New//RI//RI_3-Model output.xlsx")

