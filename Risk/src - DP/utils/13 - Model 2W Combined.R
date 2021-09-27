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



## 1. Load data & define functions ----------------------------------------------------------

## 1.1 Load 2W New data 
load_rdata_intermediate("model_data//model_data_2W_New.rdata")

load_rdata_intermediate("ADS_data//base_ads.rdata")

model_data_2W_New <- left_join(model_data_2W_New, distinct(base_ads %>% dplyr::select(deal_no,CIBIL_SCORE)), by = 'deal_no')


## 1.2 set OOT window
oot_date_start <- '2018-04-01'
oot_date_end <- '2018-06-30'

# oot_date_start <- '2018-10-01'
# oot_date_end <- '2018-12-31'

# oot_date_start <- '2018-07-01'
# oot_date_end <- '2018-09-30'

## 1.3 define OOT & DEV data for SENP-SEP 
oot_data <-
  model_data_2W_New %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                 (disbursal_date <= as.Date(oot_date_end)) &
                                 (Category %in% c('SENP','SEP','SAL')))

dev_data <-
  model_data_2W_New %>% filter(deal_no %notin% unique(oot_data$deal_no) &
                                 (Category %in% c('SENP','SEP','SAL')))

rm(model_data_2W_New,base_ads)

## 2. Variable transformation ----------------------------------------------------------------

## 2.1 Vintage
dev_data$Var_credit_vintage_C1 <- ifelse(dev_data$Var_credit_vintage >= 15, 15, dev_data$Var_credit_vintage)
oot_data$Var_credit_vintage_C1 <- ifelse(oot_data$Var_credit_vintage >= 15, 15, oot_data$Var_credit_vintage)


dev_data$Var_credit_vintage_C2 <- ifelse(dev_data$Var_credit_vintage >= 12, 12, dev_data$Var_credit_vintage)
oot_data$Var_credit_vintage_C2 <- ifelse(oot_data$Var_credit_vintage >= 12, 12, oot_data$Var_credit_vintage)


dev_data$Var_credit_vintage_C3 <- ifelse(dev_data$Var_credit_vintage >= 10, 10, dev_data$Var_credit_vintage)
oot_data$Var_credit_vintage_C3 <- ifelse(oot_data$Var_credit_vintage >= 10, 10, oot_data$Var_credit_vintage)


dev_data$Var_credit_vintage_C4 <- ifelse(dev_data$Var_credit_vintage <= 0.05, 0.05, 
                                         ifelse(dev_data$Var_credit_vintage >= 12, 12, dev_data$Var_credit_vintage))
oot_data$Var_credit_vintage_C4 <- ifelse(oot_data$Var_credit_vintage <= 0.05, 0.05, 
                                         ifelse(oot_data$Var_credit_vintage >= 12, 12, oot_data$Var_credit_vintage))


dev_data$Var_credit_vintage_C5 <- ifelse(dev_data$Var_credit_vintage <= 0.05, 0.05, 
                                         ifelse(dev_data$Var_credit_vintage >= 10, 10, dev_data$Var_credit_vintage))
oot_data$Var_credit_vintage_C5 <- ifelse(oot_data$Var_credit_vintage <= 0.05, 0.05, 
                                         ifelse(oot_data$Var_credit_vintage >= 10, 10, oot_data$Var_credit_vintage))


dev_data$Var_credit_vintage_C6 <- ifelse(dev_data$Var_credit_vintage <= 0.05, 0.05, 
                                         ifelse(dev_data$Var_credit_vintage >= 15, 15, dev_data$Var_credit_vintage))
oot_data$Var_credit_vintage_C6 <- ifelse(oot_data$Var_credit_vintage <= 0.5, 0.5, 
                                         ifelse(oot_data$Var_credit_vintage >= 15, 15, oot_data$Var_credit_vintage))



## final
dev_data$Var_credit_vintage_C2 <- ifelse(dev_data$Var_credit_vintage <= 0.01, 0.01, 
                                         ifelse(dev_data$Var_credit_vintage >= 15, 15, dev_data$Var_credit_vintage))
oot_data$Var_credit_vintage_C2 <- ifelse(oot_data$Var_credit_vintage <= 0.01, 0.01, 
                                         ifelse(oot_data$Var_credit_vintage >= 15, 15, oot_data$Var_credit_vintage))





## 2.2 Closed loan sanctioned amount excl CC

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







## 2.3 Sanction amount live loan
dev_data$Var_sanctioned_amount_live_loans_C1 <- dev_data$Var_sanctioned_amount_live_loans
oot_data$Var_sanctioned_amount_live_loans_C1 <- oot_data$Var_sanctioned_amount_live_loans

## C1
dev_data$Var_sanctioned_amount_live_loans_C1[is.na(dev_data$Var_sanctioned_amount_live_loans_C1)] <- 0
oot_data$Var_sanctioned_amount_live_loans_C1[is.na(oot_data$Var_sanctioned_amount_live_loans_C1)] <- 0


## C2
dev_data$Var_sanctioned_amount_live_loans_C2 <- ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 >= 1500000, 1500000,
                                                       dev_data$Var_sanctioned_amount_live_loans_C1)

oot_data$Var_sanctioned_amount_live_loans_C2 <- ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 >= 1500000, 1500000,
                                                       oot_data$Var_sanctioned_amount_live_loans_C1)

## C3
dev_data$Var_sanctioned_amount_live_loans_C3 <- ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 >= 2500000, 2500000,
                                                       dev_data$Var_sanctioned_amount_live_loans_C1)

oot_data$Var_sanctioned_amount_live_loans_C3 <- ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 >= 2500000, 2500000,
                                                       oot_data$Var_sanctioned_amount_live_loans_C1)


## WOE 1
subset1 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 <= 35000)
subset2 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_C1 > 35000) & (Var_sanctioned_amount_live_loans_C1 <= 175000))
subset3 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_C1 > 175000) & (Var_sanctioned_amount_live_loans_C1 <= 1000000))
subset4 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 > 1000000)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)

woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')

dev_data$Var_sanctioned_amount_live_loans_woe1 <- ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 <= 35000, woe1, 
                                                         ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 > 35000 & dev_data$Var_sanctioned_amount_live_loans_C1 <= 175000, woe2, 
                                                                ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 > 175000 & dev_data$Var_sanctioned_amount_live_loans_C1 <= 1000000, woe3, woe4)))


oot_data$Var_sanctioned_amount_live_loans_woe1 <- ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 <= 35000, woe1, 
                                                         ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 > 35000 & oot_data$Var_sanctioned_amount_live_loans_C1 <= 175000, woe2, 
                                                                ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 > 175000 & oot_data$Var_sanctioned_amount_live_loans_C1 <= 1000000, woe3, woe4)))


rm(subset1,subset2,subset3,subset4, woe1, woe2, woe3, woe4)





## WOE 2
subset1 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 <= 175000)
subset2 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_C1 > 175000) & (Var_sanctioned_amount_live_loans_C1 <= 1000000))
subset3 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 > 1000000)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <- get_3bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe2 <- get_3bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe3 <- get_3bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')


dev_data$Var_sanctioned_amount_live_loans_woe2 <- ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 <= 175000, woe1, 
                                                         ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 > 175000 & dev_data$Var_sanctioned_amount_live_loans_C1 <= 1000000, woe2,woe3))


oot_data$Var_sanctioned_amount_live_loans_woe2 <- ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 <= 175000, woe1, 
                                                         ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 > 175000 & oot_data$Var_sanctioned_amount_live_loans_C1 <= 1000000, woe2,woe3))



rm(subset1,subset2,subset3, woe1, woe2, woe3)





## WOE 3
subset1 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 <= 75000)
subset2 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_C1 > 75000) & (Var_sanctioned_amount_live_loans_C1 <= 200000))
subset3 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_C1 > 20000) & (Var_sanctioned_amount_live_loans_C1 <= 1000000))
subset4 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 > 1000000)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)

woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')

dev_data$Var_sanctioned_amount_live_loans_woe3 <- ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 <= 75000, woe1, 
                                                         ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 > 75000 & dev_data$Var_sanctioned_amount_live_loans_C1 <= 200000, woe2, 
                                                                ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 > 200000 & dev_data$Var_sanctioned_amount_live_loans_C1 <= 1000000, woe3, woe4)))


oot_data$Var_sanctioned_amount_live_loans_woe3 <- ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 <= 75000, woe1, 
                                                         ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 > 75000 & oot_data$Var_sanctioned_amount_live_loans_C1 <= 200000, woe2, 
                                                                ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 > 200000 & oot_data$Var_sanctioned_amount_live_loans_C1 <= 1000000, woe3, woe4)))


rm(subset1,subset2,subset3,subset4, woe1, woe2, woe3, woe4)




## 2.4 Closed loans excl CC
dev_data$Var_PO_closed_excl_CC_C1 <- dev_data$Var_PO_closed_excl_CC
oot_data$Var_PO_closed_excl_CC_C1 <- oot_data$Var_PO_closed_excl_CC

dev_data$Var_PO_closed_excl_CC_C1[is.na(dev_data$Var_PO_closed_excl_CC_C1)] <- 0
oot_data$Var_PO_closed_excl_CC_C1[is.na(oot_data$Var_PO_closed_excl_CC_C1)] <- 0


dev_data$Var_PO_closed_excl_CC_C1 <- ifelse(dev_data$Var_PO_closed_excl_CC_C1 >= 15, 15, 
                                            ifelse(dev_data$Var_PO_closed_excl_CC_C1 <= 0, 0,dev_data$Var_PO_closed_excl_CC_C1))

oot_data$Var_PO_closed_excl_CC_C1 <- ifelse(oot_data$Var_PO_closed_excl_CC_C1 >= 15, 15, 
                                            ifelse(oot_data$Var_PO_closed_excl_CC_C1 <= 0, 0, oot_data$Var_PO_closed_excl_CC_C1))

dev_data$Var_PO_closed_excl_CC_C2 <- ifelse(dev_data$Var_PO_closed_excl_CC_C1 >= 11, 11, dev_data$Var_PO_closed_excl_CC_C1)
oot_data$Var_PO_closed_excl_CC_C2 <- ifelse(oot_data$Var_PO_closed_excl_CC_C1 >= 11, 11, oot_data$Var_PO_closed_excl_CC_C1)

dev_data$Var_PO_closed_excl_CC_C3 <- ifelse(dev_data$Var_PO_closed_excl_CC_C1 >= 10, 10, dev_data$Var_PO_closed_excl_CC_C1)
oot_data$Var_PO_closed_excl_CC_C3 <- ifelse(oot_data$Var_PO_closed_excl_CC_C1 >= 10, 10, oot_data$Var_PO_closed_excl_CC_C1)


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
dev_data$Var_DL_HL_paid_GE_12mon_C1 <- dev_data$Var_DL_HL_paid_GE_12mon
oot_data$Var_DL_HL_paid_GE_12mon_C1 <- oot_data$Var_DL_HL_paid_GE_12mon

dev_data$Var_DL_HL_paid_GE_12mon_C1[is.na(dev_data$Var_DL_HL_paid_GE_12mon_C1)] <- 0
oot_data$Var_DL_HL_paid_GE_12mon_C1[is.na(oot_data$Var_DL_HL_paid_GE_12mon_C1)] <- 0


dev_data$Var_DL_HL_paid_GE_12mon_C2 <- ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C1 >= 3, 3, 
                                            ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C1 <= 0, 0,dev_data$Var_DL_HL_paid_GE_12mon_C1))

oot_data$Var_DL_HL_paid_GE_12mon_C2 <- ifelse(oot_data$Var_DL_HL_paid_GE_12mon_C1 >= 3, 3, 
                                            ifelse(oot_data$Var_DL_HL_paid_GE_12mon_C1 <= 0, 0, oot_data$Var_DL_HL_paid_GE_12mon_C1))




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






## 2.9 Outstanding amount in live loans
dev_data$Var_outstanding_amount_live_loans_C1 <- dev_data$Var_outstanding_amount_live_loans
oot_data$Var_outstanding_amount_live_loans_C1 <- oot_data$Var_outstanding_amount_live_loans

dev_data$Var_outstanding_amount_live_loans_C1[is.na(dev_data$Var_outstanding_amount_live_loans_C1)] <- 0
oot_data$Var_outstanding_amount_live_loans_C1[is.na(oot_data$Var_outstanding_amount_live_loans_C1)] <- 0



## woe1
subset1 <- dev_data %>% filter(Var_outstanding_amount_live_loans_C1 <= 100000)
subset2 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_C1 > 100000) & (Var_sanctioned_amount_live_loans_C1 <= 250000))
subset3 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_C1 > 250000) & (Var_sanctioned_amount_live_loans_C1 <= 500000))
subset4 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 > 500000)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)

woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')



dev_data$Var_outstanding_amount_live_loans_woe1 <- ifelse(dev_data$Var_outstanding_amount_live_loans_C1 <= 100000, woe1, 
                                                         ifelse(dev_data$Var_outstanding_amount_live_loans_C1 > 100000 & dev_data$Var_outstanding_amount_live_loans_C1 <= 250000, woe2, 
                                                                ifelse(dev_data$Var_outstanding_amount_live_loans_C1 > 250000 & dev_data$Var_outstanding_amount_live_loans_C1 <= 500000, woe3, woe4)))


oot_data$Var_outstanding_amount_live_loans_woe1 <- ifelse(oot_data$Var_outstanding_amount_live_loans_C1 <= 100000, woe1, 
                                                         ifelse(oot_data$Var_outstanding_amount_live_loans_C1 > 100000 & oot_data$Var_outstanding_amount_live_loans_C1 <= 250000, woe2, 
                                                                ifelse(oot_data$Var_outstanding_amount_live_loans_C1 > 250000 & oot_data$Var_outstanding_amount_live_loans_C1 <= 500000, woe3, woe4)))


rm(subset1,subset2,subset3,subset4, woe1, woe2, woe3, woe4)





## woe2
subset1 <- dev_data %>% filter(Var_outstanding_amount_live_loans_C1 <= 100000)
subset2 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_C1 > 100000) & (Var_sanctioned_amount_live_loans_C1 <= 500000))
subset3 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 > 500000)


mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <- get_3bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe2 <- get_3bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe3 <- get_3bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')




dev_data$Var_outstanding_amount_live_loans_woe2 <- ifelse(dev_data$Var_outstanding_amount_live_loans_C1 <= 100000, woe1, 
                                                          ifelse(dev_data$Var_outstanding_amount_live_loans_C1 > 100000 & dev_data$Var_outstanding_amount_live_loans_C1 <= 500000, woe2,woe3))


oot_data$Var_outstanding_amount_live_loans_woe2 <- ifelse(oot_data$Var_outstanding_amount_live_loans_C1 <= 100000, woe1, 
                                                          ifelse(oot_data$Var_outstanding_amount_live_loans_C1 > 100000 & oot_data$Var_outstanding_amount_live_loans_C1 <= 500000, woe2,woe3))


rm(subset1,subset2,subset3, woe1, woe2, woe3)






# ## 2.10 Sanctioned amount by outstanding amount in live loans
# 
# dev_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 <- dev_data$Var_sanctioned_by_outstanding_amount_live_loans
# oot_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 <- oot_data$Var_sanctioned_by_outstanding_amount_live_loans
# 
# dev_data$Var_sanctioned_by_outstanding_amount_live_loans_C1[is.na(dev_data$Var_sanctioned_by_outstanding_amount_live_loans_C1)] <- 0.93
# oot_data$Var_sanctioned_by_outstanding_amount_live_loans_C1[is.na(oot_data$Var_sanctioned_by_outstanding_amount_live_loans_C1)] <- 0.93
# 
# 
# 
# 
# subset1 <- dev_data %>% filter(Var_sanctioned_by_outstanding_amount_live_loans_C1 <= 1)
# subset2 <- dev_data %>% filter((Var_sanctioned_by_outstanding_amount_live_loans_C1 > 1) & (Var_sanctioned_by_outstanding_amount_live_loans_C1 <= 2))
# subset3 <- dev_data %>% filter(Var_sanctioned_by_outstanding_amount_live_loans_C1 > 2)
# 
# 
# mean(subset1$bad_loan)
# mean(subset2$bad_loan)
# mean(subset3$bad_loan)
# 
# 
# woe1 <- get_3bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
# woe2 <- get_3bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
# woe3 <- get_3bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
# 
# 
# 
# 
# dev_data$Var_sanctioned_by_outstanding_amount_live_loans_woe1 <- ifelse(dev_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 <= 1, woe1, 
#                                                                           ifelse(dev_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 > 1 & dev_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 <= 2, woe2,woe3))
# 
# 
# oot_data$Var_sanctioned_by_outstanding_amount_live_loans_woe1 <- ifelse(oot_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 <= 1, woe1, 
#                                                                           ifelse(oot_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 > 1 & oot_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 <= 2, woe2,woe3))
# 
# 
# rm(subset1,subset2,subset3, woe1, woe2, woe3)
# 
# 



## 2.11 Outstanding amount by sanctioned amount in live loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <- dev_data$Var_outstanding_by_sanctioned_amount_live_loans
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <- oot_data$Var_outstanding_by_sanctioned_amount_live_loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1[is.na(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1)] <- 1.08
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1[is.na(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1)] <- 1.08




subset1 <- dev_data %>% filter(Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.25)
subset2 <- dev_data %>% filter((Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.25) & (Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.5))
subset3 <- dev_data %>% filter(Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.5)


mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <- get_3bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe2 <- get_3bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')
woe3 <- get_3bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, dependent_feature = 'bad_loan')




dev_data$Var_outstanding_by_sanctioned_amount_live_loans_woe1 <- ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.25, woe1, 
                                                                        ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.25 & dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.5, woe2,woe3))


oot_data$Var_outstanding_by_sanctioned_amount_live_loans_woe1 <- ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.25, woe1, 
                                                                        ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.25 & oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.5, woe2,woe3))


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

dev_data$Var_EN_enquiry_count_3m_C1 <- dev_data$Var_EN_enquiry_count_3m
oot_data$Var_EN_enquiry_count_3m_C1 <- oot_data$Var_EN_enquiry_count_3m

dev_data$Var_EN_enquiry_count_3m_C1[is.na(dev_data$Var_EN_enquiry_count_3m_C1)] <- 0
oot_data$Var_EN_enquiry_count_3m_C1[is.na(oot_data$Var_EN_enquiry_count_3m_C1)] <- 0

dev_data$Var_EN_enquiry_count_3m_C2 <- ifelse(dev_data$Var_EN_enquiry_count_3m_C1 <= 0, 0, 
                                              ifelse(dev_data$Var_EN_enquiry_count_3m_C1 >= 3, 3, dev_data$Var_EN_enquiry_count_3m_C1))

oot_data$Var_EN_enquiry_count_3m_C2 <- ifelse(oot_data$Var_EN_enquiry_count_3m_C1 <= 0, 0, 
                                              ifelse(oot_data$Var_EN_enquiry_count_3m_C1 >= 3, 3, oot_data$Var_EN_enquiry_count_3m_C1))


dev_data$Var_EN_enquiry_count_3m_C3 <- ifelse(dev_data$Var_EN_enquiry_count_3m_C1 <= 0, 0, 
                                              ifelse(dev_data$Var_EN_enquiry_count_3m_C1 >= 4, 4, dev_data$Var_EN_enquiry_count_3m_C1))

oot_data$Var_EN_enquiry_count_3m_C3 <- ifelse(oot_data$Var_EN_enquiry_count_3m_C1 <= 0, 0, 
                                              ifelse(oot_data$Var_EN_enquiry_count_3m_C1 >= 4, 4, oot_data$Var_EN_enquiry_count_3m_C1))




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





##### Delinquency variables ---


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

dev_data$Var_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 <= 175000, 'bin_LE_175k', 
                                                        ifelse(dev_data$Var_sanctioned_amount_live_loans_C1 > 175000 & dev_data$Var_sanctioned_amount_live_loans_C1 <= 1000000, 'bin_175k_to_10L', 'bin_GE_10L'))


oot_data$Var_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 <= 175000, 'bin_LE_175k', 
                                                        ifelse(oot_data$Var_sanctioned_amount_live_loans_C1 > 175000 & oot_data$Var_sanctioned_amount_live_loans_C1 <= 1000000, 'bin_175k_to_10L', 'bin_GE_10L'))


dev_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.25 & dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.5, 'val_25_to_50_pct','val_GE_50_pct'))


oot_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.25 & oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.5, 'val_25_to_50_pct','val_GE_50_pct'))

# 
# test <- dev_data %>% group_by(Var_sanctioned_amount_live_loans_bin, Var_outstanding_by_sanctioned_amount_live_loans_bin)  %>% summarise(n=n(),
#                                                                                                                                 default = mean(bad_loan))
# write.csv(test, "combined_model_cross_tab_vf.csv")




subset1 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct')))
subset2 <- dev_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct'))))
subset3 <- dev_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_50_pct','val_GE_50_pct'))))
subset4 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct')))

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)

woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')


subset1_oot <- oot_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct')))
subset2_oot <- oot_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_175k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_50_pct'))))
subset3_oot <- oot_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_175k_to_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_50_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_10L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_50_pct','val_GE_50_pct'))))
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




selected_features <- c('Var_credit_vintage_C1',
                       'Var_credit_vintage_C2',
                       'Var_credit_vintage_C3',
                       'Var_credit_vintage_C4',
                       'Var_credit_vintage_C5',
                       'Var_credit_vintage_C6',
                       
                       'Var_PO_closed_sanction_amount_excl_CC_woe1',
                       'Var_PO_closed_sanction_amount_excl_CC_woe2',
                       'Var_PO_closed_sanction_amount_excl_CC_woe3',
                       
                       # 'Var_sanctioned_amount_live_loans_C1',
                       # 'Var_sanctioned_amount_live_loans_C2',
                       # 'Var_sanctioned_amount_live_loans_C3',
                       # 'Var_sanctioned_amount_live_loans_woe1',
                       # 'Var_sanctioned_amount_live_loans_woe2',
                       # 'Var_sanctioned_amount_live_loans_woe3',
                       
                       'Var_PO_closed_excl_CC_C1',
                       'Var_PO_closed_excl_CC_C2',
                       'Var_PO_closed_excl_CC_C3',
                       'Var_PO_closed_excl_CC_woe1',
                       
                       # 'Var_outstanding_amount_live_loans_C1',
                       # 'Var_outstanding_amount_live_loans_woe1',
                       # 'Var_outstanding_amount_live_loans_woe2',
                       
                       # 'Var_sanctioned_by_outstanding_amount_live_loans_C1',
                       # 'Var_sanctioned_by_outstanding_amount_live_loans_woe1',
                       
                       'Var_outstanding_by_sanctioned_amount_live_loans_woe1',
                       'cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live',
                       
                       
                       'Var_DL_HL_paid_GE_12mon_flag_var',
                       'Var_DL_HL_paid_GE_12mon_C1',
                       'Var_DL_HL_paid_GE_12mon_woe1',
                       
                       'Var_DL_Gold_paid_GE_12mon_flag_var',
                       'Var_DL_Gold_paid_GE_12mon_C1',
                       'Var_DL_Gold_paid_GE_12mon_woe1',
                       
                       'Var_EN_enquiry_count_3m_C1',
                       'Var_EN_enquiry_count_3m_C2',
                       'Var_EN_enquiry_count_3m_C3',
                       
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


## get prediction deciles
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




## seed wise iterations model ---------------------------------------------------------------------------------------------------------

# stepwise_var <- c(
#   'Var_credit_vintage_C2',
#   'Var_PO_closed_sanction_amount_excl_CC_woe2',
#   'Var_sanctioned_amount_live_loans_woe3',
#   'Var_PO_closed_excl_CC_C2',
#   'Var_DL_HL_paid_GE_12mon_C1',
#   'Var_sanctioned_by_outstanding_amount_live_loans_woe1',
#   'Var_EN_enquiry_count_6m_non_CC_C1',
#   'Var_EN_enquiry_count_6m_non_CC_CD_C1',
#   'Var_EN_enquiry_count_6m_non_CC_CD_C2',
#   'Var_DL_non_CC_CD_30dpd_3mon_flag',
#   'Var_DL_non_CC_CD_30dpd_6mon_flag',
#   'Var_DL_all_60dpd_3mon_C1',
#   'Var_DL_all_60dpd_3mon_flag',
#   'Var_DL_non_CC_CD_60dpd_3mon_C1',
#   'Var_DL_non_CC_CD_60dpd_3mon_flag',
#   'category_flag_SENP_SEP'
# )




shortlisted_var <- c( 
                       #  'Var_credit_vintage_C1',
                       'Var_credit_vintage_C2',
                       # 'Var_credit_vintage_C3',
                       # 'Var_credit_vintage_C4',
                       # 'Var_credit_vintage_C5',
                       # 'Var_credit_vintage_C6',
                       
                       # 'Var_PO_closed_sanction_amount_excl_CC_woe1',
                       # 'Var_PO_closed_sanction_amount_excl_CC_woe2',
                       # 'Var_PO_closed_sanction_amount_excl_CC_woe3',
                       
                       # 'Var_sanctioned_amount_live_loans_C1',
                       # 'Var_sanctioned_amount_live_loans_C2',
                       # 'Var_sanctioned_amount_live_loans_C3',
                       # 'Var_sanctioned_amount_live_loans_woe1',
                       # 'Var_sanctioned_amount_live_loans_woe2',
                       # 'Var_sanctioned_amount_live_loans_woe3',
                       
                       # 'Var_PO_closed_excl_CC_C1',
                       # 'Var_PO_closed_excl_CC_C2',
                       # 'Var_PO_closed_excl_CC_C3',
                       'Var_PO_closed_excl_CC_woe1',
                       
                       # 'Var_outstanding_amount_live_loans_C1',
                       # 'Var_outstanding_amount_live_loans_woe1',
                       # 'Var_outstanding_amount_live_loans_woe2',
                       
                       # 'Var_sanctioned_by_outstanding_amount_live_loans_C1',
                       # 'Var_sanctioned_by_outstanding_amount_live_loans_woe1',
                       
                       # 'Var_outstanding_by_sanctioned_amount_live_loans_woe1',
                       'cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live',
                       
                       
                       # 'Var_DL_HL_paid_GE_12mon_flag_var',
                       # 'Var_DL_HL_paid_GE_12mon_C1',-------------------------
                       'Var_DL_HL_paid_GE_12mon_C2',
                       # 'Var_DL_HL_paid_GE_12mon_woe1',
                       
                       # 'Var_DL_Gold_paid_GE_12mon_flag_var',
                       # 'Var_DL_Gold_paid_GE_12mon_C1', --------------------
                       'Var_DL_Gold_paid_GE_12mon_C3',
                       # 'Var_DL_Gold_paid_GE_12mon_woe1',
                       
                       # 'Var_EN_enquiry_count_3m_C1',
                       # 'Var_EN_enquiry_count_3m_C2',
                       # 'Var_EN_enquiry_count_3m_C3',
                       
                       # 'Var_EN_enquiry_count_3m_non_CC_C1',
                       # 'Var_EN_enquiry_count_3m_non_CC_C2',
                       # 'Var_EN_enquiry_count_3m_non_CC_C3',
                       
                       # 'Var_EN_enquiry_count_3m_non_CC_CD_C1',
                       # 'Var_EN_enquiry_count_3m_non_CC_CD_C2',
                       # 'Var_EN_enquiry_count_3m_non_CC_CD_C3',
                       
                       
                       # 'Var_EN_enquiry_count_6m_C1',
                       # 'Var_EN_enquiry_count_6m_C2',
                       # 'Var_EN_enquiry_count_6m_C3',
                       
                       # 'Var_EN_enquiry_count_6m_non_CC_C1', --------------------
                       'Var_EN_enquiry_count_6m_non_CC_C4',
                       # 'Var_EN_enquiry_count_6m_non_CC_C2',
                       # 'Var_EN_enquiry_count_6m_non_CC_C3',
                       
                       # 'Var_EN_enquiry_count_6m_non_CC_CD_C1',
                       # 'Var_EN_enquiry_count_6m_non_CC_CD_C2',
                       # 'Var_EN_enquiry_count_6m_non_CC_CD_C3',
                       
                       # 'Var_DL_all_30dpd_3mon_C1',
                       # 'Var_DL_all_30dpd_3mon_flag',
                       # 'Var_DL_all_30dpd_6mon_C1', ---------------------
                       'Var_DL_all_30dpd_6mon_C2',
                       # 'Var_DL_all_30dpd_6mon_flag',
                       
                       # 'Var_DL_non_CC_CD_30dpd_3mon_C1',
                       # 'Var_DL_non_CC_CD_30dpd_3mon_flag',
                       # 'Var_DL_non_CC_CD_30dpd_6mon_C1',
                       # 'Var_DL_non_CC_CD_30dpd_6mon_flag',
                       # 
                       # 'Var_DL_all_60dpd_3mon_C1',
                       # 'Var_DL_all_60dpd_3mon_flag',
                       # 'Var_DL_all_60dpd_6mon_C1',
                       # 'Var_DL_all_60dpd_6mon_flag',
                       # 
                       # 'Var_DL_non_CC_CD_60dpd_3mon_C1',
                       # 'Var_DL_non_CC_CD_60dpd_3mon_flag',
                       # 'Var_DL_non_CC_CD_60dpd_6mon_C1',
                       # 'Var_DL_non_CC_CD_60dpd_6mon_flag'
                       # 
                       'category_flag_SENP_SEP',
                       'agri_profile_flag'
)



# shortlisted_var <- c(
#   
#   'Var_credit_vintage_C6',
#   
#   
#   'Var_PO_closed_sanction_amount_excl_CC_woe2',
#   
#   'Var_sanctioned_amount_live_loans_woe3',
#   'Var_PO_closed_excl_CC_woe1', ###
#   
#   'Var_DL_HL_paid_GE_12mon_C1',
#   
#   'Var_sanctioned_by_outstanding_amount_live_loans_woe1',
#   
#   'Var_EN_enquiry_count_6m_non_CC_C1',
#   
#   'Var_DL_non_CC_CD_30dpd_3mon_C1',
#   
#   'category_flag_SENP_SEP'
# )


seed_iterations_output <- get_seed_iterations(seed_list = c(1:100),
                                              dev_data = dev_data,
                                              oot = oot_data,
                                              feature_list = shortlisted_var, 
                                              p_value_threshold = 0.05)

selected_seeds <- seed_iterations_output %>% filter((p_value_check == 0) & 
                                                      (RO_decile_overall >= 2) & 
                                                      (RO_pentile_overall == 3))



# write.csv(seed_iterations_output, "seed_iterations_combined.csv")


# selected_seed <- 96
selected_seed <- 10

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


RO_overall <- data.frame(get_RO(train_data_final,test_data_final,oot_data_final))
RO_overall

RO_SENP_SEP <-  data.frame(get_RO(train_data_final %>% filter(Category %in% c('SENP','SEP')),
                                  test_data_final %>% filter(Category %in% c('SENP','SEP')),
                                  oot_data_final%>% filter(Category %in% c('SENP','SEP'))))
RO_SENP_SEP

RO_SAL <-  data.frame(get_RO(train_data_final %>% filter(Category %in% c('SAL')),
                                  test_data_final %>% filter(Category %in% c('SAL')),
                                  oot_data_final%>% filter(Category %in% c('SAL'))))
RO_SAL



RO_overall_pentile <- data.frame(get_RO_pentile(train_data_final,test_data_final,oot_data_final))


RO_SENP_SEP_pentile <-  data.frame(get_RO_pentile(train_data_final %>% filter(Category %in% c('SENP','SEP')),
                                  test_data_final %>% filter(Category %in% c('SENP','SEP')),
                                  oot_data_final%>% filter(Category %in% c('SENP','SEP'))))

RO_SAL_pentile <-  data.frame(get_RO_pentile(train_data_final %>% filter(Category %in% c('SAL')),
                             test_data_final %>% filter(Category %in% c('SAL')),
                             oot_data_final%>% filter(Category %in% c('SAL'))))




############################################
saveRDS(final_model, file = file.path(get_data_path()$data$model,"model_2W_New.rds"))





load_rdata_intermediate("ADS_data//base_ads.rdata")

ticket_size <- distinct(base_ads %>% filter((!is.na(Finance_Amount_Chassis)) & (loan_type == '2W-New')) %>% dplyr::select(deal_no, Finance_Amount_Chassis))

ticket_size_small <- ticket_size %>% filter(Finance_Amount_Chassis < 100000)
ticket_size_big <- ticket_size %>% filter(Finance_Amount_Chassis >= 100000)


RO_ticket_small_decile <- data.frame(get_RO(train_data_final %>% filter(deal_no %in% unique(ticket_size_small$deal_no)),
                                     test_data_final %>% filter(deal_no %in% unique(ticket_size_small$deal_no)),
                                     oot_data_final%>% filter(deal_no %in% unique(ticket_size_small$deal_no))))


RO_ticket_big_decile <- data.frame(get_RO(train_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no)),
                                     test_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no)),
                                     oot_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no))))


RO_ticket_small_pentile <- data.frame(get_RO_pentile(train_data_final %>% filter(deal_no %in% unique(ticket_size_small$deal_no)),
                                            test_data_final %>% filter(deal_no %in% unique(ticket_size_small$deal_no)),
                                            oot_data_final%>% filter(deal_no %in% unique(ticket_size_small$deal_no))))


RO_ticket_big_pentile <- data.frame(get_RO_pentile(train_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no)),
                                          test_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no)),
                                          oot_data_final %>% filter(deal_no %in% unique(ticket_size_big$deal_no))))




train_data_final$cibil_buckets <- ifelse(is.na(train_data_final$CIBIL_SCORE), "NTC",
                                       ifelse(train_data_final$CIBIL_SCORE <= 0, "Negative CIBIL",
                                              ifelse(train_data_final$CIBIL_SCORE >= 1 & train_data_final$CIBIL_SCORE <=5, train_data_final$CIBIL_SCORE,
                                                     ifelse(train_data_final$CIBIL_SCORE > 5 & train_data_final$CIBIL_SCORE <=550 , "Cibil <= 550",
                                                            ifelse(train_data_final$CIBIL_SCORE > 550 & train_data_final$CIBIL_SCORE <=650 , "Cibil 550 - 650",
                                                                   ifelse(train_data_final$CIBIL_SCORE > 650 & train_data_final$CIBIL_SCORE <=750 , "Cibil 650 - 750",
                                                                          ifelse(train_data_final$CIBIL_SCORE > 750 , "Cibil > 750","NTC")))))))
                                                     

test_data_final$cibil_buckets <- ifelse(is.na(test_data_final$CIBIL_SCORE), "NTC",
                                        ifelse(test_data_final$CIBIL_SCORE <= 0, "Negative CIBIL",
                                               ifelse(test_data_final$CIBIL_SCORE >= 1 & test_data_final$CIBIL_SCORE <=5, test_data_final$CIBIL_SCORE,
                                                      ifelse(test_data_final$CIBIL_SCORE > 5 & test_data_final$CIBIL_SCORE <=550 , "Cibil <= 550",
                                                             ifelse(test_data_final$CIBIL_SCORE > 550 & test_data_final$CIBIL_SCORE <=650 , "Cibil 550 - 650",
                                                                    ifelse(test_data_final$CIBIL_SCORE > 650 & test_data_final$CIBIL_SCORE <=750 , "Cibil 650 - 750",
                                                                           ifelse(test_data_final$CIBIL_SCORE > 750 , "Cibil > 750","NTC")))))))


oot_data_final$cibil_buckets <- ifelse(is.na(oot_data_final$CIBIL_SCORE), "NTC",
                                       ifelse(oot_data_final$CIBIL_SCORE <= 0, "Negative CIBIL",
                                              ifelse(oot_data_final$CIBIL_SCORE >= 1 & oot_data_final$CIBIL_SCORE <=5, oot_data_final$CIBIL_SCORE,
                                                     ifelse(oot_data_final$CIBIL_SCORE > 5 & oot_data_final$CIBIL_SCORE <=550 , "Cibil <= 550",
                                                            ifelse(oot_data_final$CIBIL_SCORE > 550 & oot_data_final$CIBIL_SCORE <=650 , "Cibil 550 - 650",
                                                                   ifelse(oot_data_final$CIBIL_SCORE > 650 & oot_data_final$CIBIL_SCORE <=750 , "Cibil 650 - 750",
                                                                          ifelse(oot_data_final$CIBIL_SCORE > 750 , "Cibil > 750","NTC")))))))


temp_train_all <- train_data_final
temp_train_all$decile <- ntile(temp_train_all$predictions,10)
temp_train_all$pentile <- ntile(temp_train_all$predictions,5)
temp_train_all <- temp_train_all %>% dplyr::select(applicant_id,Category,predictions,bad_loan,decile,pentile, CIBIL_SCORE,cibil_buckets)
temp_train_all <- temp_train_all %>% filter(cibil_buckets %notin% c('NTC','Negative CIBIL')) %>% group_by(decile,cibil_buckets) %>% summarise(applicant_count = length(unique(applicant_id)),
                                                                                                                                              default_count = sum(bad_loan),
                                                                                                                                              default_pct = mean(bad_loan)
                                                                                                                                              )


temp_test_all <- test_data_final
temp_test_all$decile <- ntile(temp_test_all$predictions,10)
temp_test_all$pentile <- ntile(temp_test_all$predictions,5)
temp_test_all <- temp_test_all %>% dplyr::select(applicant_id,Category,predictions,bad_loan,decile,pentile, CIBIL_SCORE,cibil_buckets)
temp_test_all <- temp_test_all %>% filter(cibil_buckets %notin% c('NTC','Negative CIBIL')) %>% group_by(decile,cibil_buckets) %>% summarise(applicant_count = length(unique(applicant_id)),
                                                                                                                                              default_count = sum(bad_loan),
                                                                                                                                              default_pct = mean(bad_loan)
)


temp_oot_all <- oot_data_final
temp_oot_all$decile <- ntile(temp_oot_all$predictions,10)
temp_oot_all$pentile <- ntile(temp_oot_all$predictions,5)
temp_oot_all <- temp_oot_all %>% dplyr::select(applicant_id,Category,predictions,bad_loan,decile,pentile, CIBIL_SCORE,cibil_buckets)
temp_oot_all <- temp_oot_all %>% filter(cibil_buckets %notin% c('NTC','Negative CIBIL')) %>% group_by(decile,cibil_buckets) %>% summarise(applicant_count = length(unique(applicant_id)),
                                                                                                                                              default_count = sum(bad_loan),
                                                                                                                                              default_pct = mean(bad_loan)
)





## save outputs

output_list <- list("model_summary" = data.frame(tidy(final_model)),
                    "model_performance" = model_performance_final,
                    "RO_overall_decile" = RO_overall,
                    "RO_SENP_SEP_decile" = RO_SENP_SEP,
                    "RO_SAL_decile" = RO_SAL,
                    "RO_overall_pentile" = RO_overall_pentile,
                    "RO_SENP_SEP_pentile" = RO_SENP_SEP_pentile,
                    "RO_SAL_pentile" = RO_SAL_pentile,
                    "Var_Imp" = get_variable_importance(final_model),
                    "VIF" = get_vif(final_model),
                    "wald_chi_sq" = get_wald_chi_sq(final_model),
                    "seed" = data.frame(selected_seed),
                    "train_data" = temp_train_all,
                    "test_data" = temp_test_all,
                    "oot_data" = temp_oot_all,
                    "RO_decile_small_ticket" = RO_ticket_small_decile,
                    "RO_decile_big_ticket" = RO_ticket_big_decile,
                    "RO_pentile_small_ticket" = RO_ticket_small_pentile,
                    "RO_pentile_big_ticket" = RO_ticket_big_pentile
                    )


save_xlsx_output(data = output_list, relative_path = "//model//2W_New//Combined//combined_model_output_v2_grouped_v3.xlsx")

# rm(oot_data_final,train_data_final,test_data_final)









###############################################################################################################################

load_rdata_intermediate("ADS_data//temporal_decay.rdata")

temporal_decay <- temporal_decay %>% filter(loan_type == '2W-New')
colnames(temporal_decay)[colnames(temporal_decay) == 'temporal_decay_post_MOB'] <- 'bad_loan'

temporal_decay_train <- train_data_final %>% dplyr::select(deal_no, applicant_id, predictions)
temporal_decay_test <- test_data_final %>% dplyr::select(deal_no, applicant_id, predictions)
temporal_decay_oot <- oot_data_final %>% dplyr::select(deal_no, applicant_id, predictions)


temporal_decay_train <- inner_join(temporal_decay_train,temporal_decay, by = 'deal_no')
temporal_decay_test <- inner_join(temporal_decay_test,temporal_decay, by = 'deal_no')
temporal_decay_oot <- inner_join(temporal_decay_oot,temporal_decay, by = 'deal_no')



temporal_decay_RO_decile <- data.frame(get_RO(temporal_decay_train,temporal_decay_test,temporal_decay_oot))
temporal_decay_RO_pentile <- data.frame(get_RO_pentile(temporal_decay_train,temporal_decay_test,temporal_decay_oot))

rm(temporal_decay_train,temporal_decay_test,temporal_decay_oot)
rm(temporal_decay)





load_rdata_intermediate("ADS_data//intermediary_I1.rdata")

intermediary_I1 <- intermediary_I1 %>% filter(loan_type == '2W-New')
colnames(intermediary_I1)[colnames(intermediary_I1) == 'never_60DPD_but_90DPD_post_MOB'] <- 'bad_loan'

I1_train <- train_data_final %>% dplyr::select(deal_no, applicant_id, predictions)
I1_test <- test_data_final %>% dplyr::select(deal_no, applicant_id, predictions)
I1_oot <- oot_data_final %>% dplyr::select(deal_no, applicant_id, predictions)


I1_train <- inner_join(I1_train,intermediary_I1, by = 'deal_no')
I1_test <- inner_join(I1_test,intermediary_I1, by = 'deal_no')
I1_oot <- inner_join(I1_oot,intermediary_I1, by = 'deal_no')



I1_RO_decile <- data.frame(get_RO(I1_train,I1_test,I1_oot))
I1_RO_pentile <- data.frame(get_RO_pentile(I1_train,I1_test,I1_oot))


output_list <- list("temporal_decay_decile" = temporal_decay_RO_decile,
                    "temporal_decay_pentile" = temporal_decay_RO_pentile,
                    "I1_RO_decile" = I1_RO_decile,
                    "I1_RO_pentile" = I1_RO_pentile
)


save_xlsx_output(data = output_list, relative_path = "//model//2W_New//Combined//validation.xlsx")

rm(I1_RO_decile,I1_RO_pentile,temporal_decay_RO_decile,temporal_decay_RO_pentile,intermediary_I1)
rm(I1_oot,I1_test,I1_train)
