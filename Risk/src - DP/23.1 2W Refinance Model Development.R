#############################################################################################
################## 23 - Model 2W RF Development  ##################################################
#############################################################################################


## 0. Load helper functions & libraries -----------------------------------------------------
load_libaries <- file.path("src - DP", "utils", "load_libraries.R")
source(load_libaries)

io_helper <- file.path("src - DP", "utils", "io_helper.R")
source(io_helper)

model_functions <- file.path("src - DP", "utils", "model_functions.R")
source(model_functions)

options(scipen = 999)
voptions(raise = "all")

`%notin%` <- Negate(`%in%`)



## 1. Load data & define functions ----------------------------------------------------------

## 1.1 Load 2W New data
load_rdata_intermediate("model_data//model_data_2W_RF.rdata")

## 1.2 set OOT window
oot_date_start <- '2018-04-01'
oot_date_end <- '2018-06-30'

## 1.3 define OOT & DEV data for Combined
oot_data <-
  model_data_2W_RF %>% filter((disbursal_date >= as.Date(oot_date_start)) &
                                 (disbursal_date <= as.Date(oot_date_end)) &
                                 (Category %in% c('SAL', 'SENP', 'SEP')))

dev_data <-
  model_data_2W_RF %>% filter(deal_no %notin% unique(oot_data$deal_no) &
                                 (Category %in% c('SAL', 'SENP', 'SEP')))

rm(model_data_2W_RF)


## 2. Variable transformation ----------------------------------------------------------------

## 2.1 Vintage---------------------

dev_data$Var_credit_vintage_C1 <-
  ifelse(
    dev_data$Var_credit_vintage <= 0.5,
    0.5,
    ifelse(
      dev_data$Var_credit_vintage >= 12,
      12,
      dev_data$Var_credit_vintage
    )
  )
oot_data$Var_credit_vintage_C1 <-
  ifelse(
    oot_data$Var_credit_vintage <= 0.5,
    0.5,
    ifelse(
      oot_data$Var_credit_vintage >= 12,
      12,
      oot_data$Var_credit_vintage
    )
  )


dev_data$Var_credit_vintage_C2 <-
  ifelse(
    dev_data$Var_credit_vintage <= 0.5,
    0.5,
    ifelse(
      dev_data$Var_credit_vintage >= 10,
      10,
      dev_data$Var_credit_vintage
    )
  )
oot_data$Var_credit_vintage_C2 <-
  ifelse(
    oot_data$Var_credit_vintage <= 0.5,
    0.5,
    ifelse(
      oot_data$Var_credit_vintage >= 10,
      10,
      oot_data$Var_credit_vintage
    )
  )


dev_data$Var_credit_vintage_C3 <-
  ifelse(
    dev_data$Var_credit_vintage <= 0.5,
    0.5,
    ifelse(
      dev_data$Var_credit_vintage >= 9,
      9,
      dev_data$Var_credit_vintage
    )
  )
oot_data$Var_credit_vintage_C3 <-
  ifelse(
    oot_data$Var_credit_vintage <= 0.5,
    0.5,
    ifelse(
      oot_data$Var_credit_vintage >= 9,
      9,
      oot_data$Var_credit_vintage
    )
  )

## vintage woe

subset1 <-
  dev_data %>% filter(Var_credit_vintage_C2 <= 3)
subset2 <-
  dev_data %>% filter((Var_credit_vintage_C2 > 3) &
                        (Var_credit_vintage_C2 <= 9)
  )
subset3 <-
  dev_data %>% filter(Var_credit_vintage_C2 > 9)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)

woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )

dev_data$Var_credit_vintage_woe1 <-
  ifelse(
    dev_data$Var_credit_vintage_C2 <= 3,
    woe1,
    ifelse(
      dev_data$Var_credit_vintage_C2 > 3 &
        dev_data$Var_credit_vintage_C2 <= 9,
      woe2,
      woe3
    )
  )


oot_data$Var_credit_vintage_woe1 <-
  ifelse(
    oot_data$Var_credit_vintage_C2 <= 3,
    woe1,
    ifelse(
      oot_data$Var_credit_vintage_C2 > 3 &
        oot_data$Var_credit_vintage_C2 <= 9,
      woe2,
      woe3
    )
  )


rm(subset1, subset2, subset3, woe1, woe2, woe3)





## 2.2 Closed loan sanctioned amount excl CC--------------

## WOE 1
subset1 <-
  dev_data %>% filter(Var_PO_closed_sanction_amount_excl_CC <= 50000)
subset2 <-
  dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC > 50000) &
                        (Var_PO_closed_sanction_amount_excl_CC <= 300000)
  )
subset3 <-
  dev_data %>% filter(Var_PO_closed_sanction_amount_excl_CC > 300000)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)

woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )

dev_data$Var_PO_closed_sanction_amount_excl_CC_woe1 <-
  ifelse(
    dev_data$Var_PO_closed_sanction_amount_excl_CC <= 50000,
    woe1,
    ifelse(
      dev_data$Var_PO_closed_sanction_amount_excl_CC > 50000 &
        dev_data$Var_PO_closed_sanction_amount_excl_CC <= 300000,
      woe2,
      woe3
    )
  )


oot_data$Var_PO_closed_sanction_amount_excl_CC_woe1 <-
  ifelse(
    oot_data$Var_PO_closed_sanction_amount_excl_CC <= 50000,
    woe1,
    ifelse(
      oot_data$Var_PO_closed_sanction_amount_excl_CC > 50000 &
        oot_data$Var_PO_closed_sanction_amount_excl_CC <= 300000,
      woe2,
      woe3
    )
  )


rm(subset1, subset2, subset3, woe1, woe2, woe3)




## WOE 2

subset1 <-
  dev_data %>% filter(Var_PO_closed_sanction_amount_excl_CC <= 50000)
subset2 <-
  dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC > 50000) &
                        (Var_PO_closed_sanction_amount_excl_CC <= 75000)
  )
subset3 <-
  dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC > 75000) &
                        (Var_PO_closed_sanction_amount_excl_CC <= 100000)
  )
subset4 <-
  dev_data %>% filter(Var_PO_closed_sanction_amount_excl_CC > 100000)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)

woe1 <-
  get_4bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    bin4 = subset4,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_4bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    bin4 = subset4,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_4bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    bin4 = subset4,
    dependent_feature = 'bad_loan'
  )
woe4 <-
  get_4bin_woe(
    focus_bin = subset4,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    bin4 = subset4,
    dependent_feature = 'bad_loan'
  )

dev_data$Var_PO_closed_sanction_amount_excl_CC_woe2 <-
  ifelse(
    dev_data$Var_PO_closed_sanction_amount_excl_CC <= 50000,
    woe1,
    ifelse(
      dev_data$Var_PO_closed_sanction_amount_excl_CC > 50000 &
        dev_data$Var_PO_closed_sanction_amount_excl_CC <= 75000,
      woe2,
      ifelse(
        dev_data$Var_PO_closed_sanction_amount_excl_CC > 75000 &
          dev_data$Var_PO_closed_sanction_amount_excl_CC <= 100000,
        woe3,
        woe4
      )
    )
  )


oot_data$Var_PO_closed_sanction_amount_excl_CC_woe2 <-
  ifelse(
    oot_data$Var_PO_closed_sanction_amount_excl_CC <= 50000,
    woe1,
    ifelse(
      oot_data$Var_PO_closed_sanction_amount_excl_CC > 50000 &
        oot_data$Var_PO_closed_sanction_amount_excl_CC <= 75000,
      woe2,
      ifelse(
        oot_data$Var_PO_closed_sanction_amount_excl_CC > 75000 &
          oot_data$Var_PO_closed_sanction_amount_excl_CC <= 100000,
        woe3,
        woe4
      )
    )
  )


rm(subset1, subset2, subset3, subset4, woe1, woe2, woe3, woe4)






## 2.3 Sanction amount live loan------------------
dev_data$Var_sanctioned_amount_live_loans_C1 <-
  dev_data$Var_sanctioned_amount_live_loans
oot_data$Var_sanctioned_amount_live_loans_C1 <-
  oot_data$Var_sanctioned_amount_live_loans

## C1
dev_data$Var_sanctioned_amount_live_loans_C1[is.na(dev_data$Var_sanctioned_amount_live_loans_C1)] <-
  0
oot_data$Var_sanctioned_amount_live_loans_C1[is.na(oot_data$Var_sanctioned_amount_live_loans_C1)] <-
  0


## C2
dev_data$Var_sanctioned_amount_live_loans_C2 <-
  ifelse(
    dev_data$Var_sanctioned_amount_live_loans_C1 >= 1200000,
    1200000,
    dev_data$Var_sanctioned_amount_live_loans_C1
  )

oot_data$Var_sanctioned_amount_live_loans_C2 <-
  ifelse(
    oot_data$Var_sanctioned_amount_live_loans_C1 >= 1200000,
    1200000,
    oot_data$Var_sanctioned_amount_live_loans_C1
  )

## C3
dev_data$Var_sanctioned_amount_live_loans_C3 <-
  ifelse(
    dev_data$Var_sanctioned_amount_live_loans_C1 >= 1500000,
    1500000,
    dev_data$Var_sanctioned_amount_live_loans_C1
  )

oot_data$Var_sanctioned_amount_live_loans_C3 <-
  ifelse(
    oot_data$Var_sanctioned_amount_live_loans_C1 >= 1500000,
    1500000,
    oot_data$Var_sanctioned_amount_live_loans_C1
  )


## WOE 1
subset1 <-
  dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 <= 30000)
subset2 <-
  dev_data %>% filter((Var_sanctioned_amount_live_loans_C1 > 30000) &
                        (Var_sanctioned_amount_live_loans_C1 <= 100000)
  )
subset3 <-
  dev_data %>% filter((Var_sanctioned_amount_live_loans_C1 > 100000) &
                        (Var_sanctioned_amount_live_loans_C1 <= 500000)
  )
subset4 <-
  dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 > 500000)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)

woe1 <-
  get_4bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    bin4 = subset4,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_4bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    bin4 = subset4,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_4bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    bin4 = subset4,
    dependent_feature = 'bad_loan'
  )
woe4 <-
  get_4bin_woe(
    focus_bin = subset4,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    bin4 = subset4,
    dependent_feature = 'bad_loan'
  )

dev_data$Var_sanctioned_amount_live_loans_woe1 <-
  ifelse(
    dev_data$Var_sanctioned_amount_live_loans_C1 <= 30000,
    woe1,
    ifelse(
      dev_data$Var_sanctioned_amount_live_loans_C1 > 30000 &
        dev_data$Var_sanctioned_amount_live_loans_C1 <= 100000,
      woe2,
      ifelse(
        dev_data$Var_sanctioned_amount_live_loans_C1 > 100000 &
          dev_data$Var_sanctioned_amount_live_loans_C1 <= 500000,
        woe3,
        woe4
      )
    )
  )


oot_data$Var_sanctioned_amount_live_loans_woe1 <-
  ifelse(
    oot_data$Var_sanctioned_amount_live_loans_C1 <= 30000,
    woe1,
    ifelse(
      oot_data$Var_sanctioned_amount_live_loans_C1 > 30000 &
        oot_data$Var_sanctioned_amount_live_loans_C1 <= 100000,
      woe2,
      ifelse(
        oot_data$Var_sanctioned_amount_live_loans_C1 > 100000 &
          oot_data$Var_sanctioned_amount_live_loans_C1 <= 500000,
        woe3,
        woe4
      )
    )
  )


rm(subset1, subset2, subset3, subset4, woe1, woe2, woe3, woe4)





## WOE 2
subset1 <-
  dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 <= 60000)
subset2 <-
  dev_data %>% filter((Var_sanctioned_amount_live_loans_C1 > 60000) &
                        (Var_sanctioned_amount_live_loans_C1 <= 500000)
  )
subset3 <-
  dev_data %>% filter(Var_sanctioned_amount_live_loans_C1 > 500000)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )


dev_data$Var_sanctioned_amount_live_loans_woe2 <-
  ifelse(
    dev_data$Var_sanctioned_amount_live_loans_C1 <= 60000,
    woe1,
    ifelse(
      dev_data$Var_sanctioned_amount_live_loans_C1 > 60000 &
        dev_data$Var_sanctioned_amount_live_loans_C1 <= 500000,
      woe2,
      woe3
    )
  )


oot_data$Var_sanctioned_amount_live_loans_woe2 <-
  ifelse(
    oot_data$Var_sanctioned_amount_live_loans_C1 <= 60000,
    woe1,
    ifelse(
      oot_data$Var_sanctioned_amount_live_loans_C1 > 60000 &
        oot_data$Var_sanctioned_amount_live_loans_C1 <= 500000,
      woe2,
      woe3
    )
  )



rm(subset1, subset2, subset3, woe1, woe2, woe3)





## 2.4 Closed loans excl CC---------------
dev_data$Var_PO_closed_excl_CC_C1 <- dev_data$Var_PO_closed_excl_CC
oot_data$Var_PO_closed_excl_CC_C1 <- oot_data$Var_PO_closed_excl_CC

dev_data$Var_PO_closed_excl_CC_C1[is.na(dev_data$Var_PO_closed_excl_CC_C1)] <-
  0
oot_data$Var_PO_closed_excl_CC_C1[is.na(oot_data$Var_PO_closed_excl_CC_C1)] <-
  0


dev_data$Var_PO_closed_excl_CC_C1 <-
  ifelse(dev_data$Var_PO_closed_excl_CC_C1 >= 15,
         15,
         dev_data$Var_PO_closed_excl_CC_C1)
oot_data$Var_PO_closed_excl_CC_C1 <-
  ifelse(oot_data$Var_PO_closed_excl_CC_C1 >= 15,
         15,
         oot_data$Var_PO_closed_excl_CC_C1)

dev_data$Var_PO_closed_excl_CC_C2 <-
  ifelse(dev_data$Var_PO_closed_excl_CC_C1 >= 12,
         12,
         dev_data$Var_PO_closed_excl_CC_C1)
oot_data$Var_PO_closed_excl_CC_C2 <-
  ifelse(oot_data$Var_PO_closed_excl_CC_C1 >= 12,
         12,
         oot_data$Var_PO_closed_excl_CC_C1)

dev_data$Var_PO_closed_excl_CC_C3 <-
  ifelse(dev_data$Var_PO_closed_excl_CC_C1 >= 10,
         10,
         dev_data$Var_PO_closed_excl_CC_C1)
oot_data$Var_PO_closed_excl_CC_C3 <-
  ifelse(oot_data$Var_PO_closed_excl_CC_C1 >= 10,
         10,
         oot_data$Var_PO_closed_excl_CC_C1)


## WOE 1
subset1 <- dev_data %>% filter(Var_PO_closed_excl_CC_C1 <= 1)
subset2 <-
  dev_data %>% filter((Var_PO_closed_excl_CC_C1 >= 2) &
                        (Var_PO_closed_excl_CC_C1 < 4))
subset3 <- dev_data %>% filter(Var_PO_closed_excl_CC_C1 >= 4)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )


dev_data$Var_PO_closed_excl_CC_woe1 <-
  ifelse(
    dev_data$Var_PO_closed_excl_CC_C1 <= 1,
    woe1,
    ifelse(
      dev_data$Var_PO_closed_excl_CC_C1 >= 2 &
        dev_data$Var_PO_closed_excl_CC_C1 < 4,
      woe2,
      woe3
    )
  )


oot_data$Var_PO_closed_excl_CC_woe1 <-
  ifelse(
    oot_data$Var_PO_closed_excl_CC_C1 <= 1,
    woe1,
    ifelse(
      oot_data$Var_PO_closed_excl_CC_C1 >= 2 &
        oot_data$Var_PO_closed_excl_CC_C1 < 4,
      woe2,
      woe3
    )
  )


rm(subset1, subset2, subset3, woe1, woe2, woe3)








## 2.5 HL paid 12 month flag----------------

dev_data$Var_DL_HL_paid_GE_12mon_flag_var <-
  dev_data$Var_DL_HL_paid_GE_12mon_flag
oot_data$Var_DL_HL_paid_GE_12mon_flag_var <-
  oot_data$Var_DL_HL_paid_GE_12mon_flag


# dev_data$Var_DL_HL_paid_GE_12mon_flag[is.infinite(dev_data$Var_DL_HL_paid_GE_12mon_flag)] <- NA


# dev_data[sapply(dev_data, Negate(is.finite))] <- 0
# oot_data[sapply(oot_data, Negate(is.finite))] <- 0

dev_data$Var_DL_HL_paid_GE_12mon_flag_var[is.na(dev_data$Var_DL_HL_paid_GE_12mon_flag_var)] <-
  0
oot_data$Var_DL_HL_paid_GE_12mon_flag_var[is.na(oot_data$Var_DL_HL_paid_GE_12mon_flag_var)] <-
  0

## 2.6 HL paid 12 month Continuous--------------------------
dev_data$Var_DL_HL_paid_GE_12mon_C1 <-
  ifelse(dev_data$Var_DL_HL_paid_GE_12mon > 4,
         4,
         dev_data$Var_DL_HL_paid_GE_12mon)
oot_data$Var_DL_HL_paid_GE_12mon_C1 <-
  ifelse(oot_data$Var_DL_HL_paid_GE_12mon > 4,
         4,
         oot_data$Var_DL_HL_paid_GE_12mon)

dev_data$Var_DL_HL_paid_GE_12mon_C1[is.na(dev_data$Var_DL_HL_paid_GE_12mon_C1)] <-
  0
oot_data$Var_DL_HL_paid_GE_12mon_C1[is.na(oot_data$Var_DL_HL_paid_GE_12mon_C1)] <-
  0




subset1 <- dev_data %>% filter(Var_DL_HL_paid_GE_12mon_C1 == 0)
subset2 <- dev_data %>% filter(Var_DL_HL_paid_GE_12mon_C1 == 1)
subset3 <- dev_data %>% filter(Var_DL_HL_paid_GE_12mon_C1 >= 2)


mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )


dev_data$Var_DL_HL_paid_GE_12mon_woe1 <-
  ifelse(
    dev_data$Var_DL_HL_paid_GE_12mon_C1 == 0,
    woe1,
    ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C1 == 1, woe2, woe3)
  )


oot_data$Var_DL_HL_paid_GE_12mon_woe1 <-
  ifelse(
    oot_data$Var_DL_HL_paid_GE_12mon_C1 == 0,
    woe1,
    ifelse(dev_data$Var_DL_HL_paid_GE_12mon_C1 == 1, woe2, woe3)
  )


rm(subset1, subset2, subset3, woe1, woe2, woe3)



## 2.7 Education loan------------------
## Education paid 12mon flag
dev_data$Var_DL_Education_paid_GE_12mon_flag_var <-
  dev_data$Var_DL_Education_paid_GE_12mon_flag
oot_data$Var_DL_Education_paid_GE_12mon_flag_var <-
  oot_data$Var_DL_Education_paid_GE_12mon_flag

dev_data$Var_DL_Education_paid_GE_12mon_flag_var[is.na(dev_data$Var_DL_Education_paid_GE_12mon_flag_var)] <-
  0
oot_data$Var_DL_Education_paid_GE_12mon_flag_var[is.na(oot_data$Var_DL_Education_paid_GE_12mon_flag_var)] <-
  0


## Education paid 12 mon continuous

dev_data$Var_DL_Education_paid_GE_12mon_C1 <-
  ifelse(
    dev_data$Var_DL_Education_paid_GE_12mon > 2,
    2,
    dev_data$Var_DL_Education_paid_GE_12mon
  )
oot_data$Var_DL_Education_paid_GE_12mon_C1 <-
  ifelse(
    oot_data$Var_DL_Education_paid_GE_12mon > 2,
    2,
    oot_data$Var_DL_Education_paid_GE_12mon
  )

dev_data$Var_DL_Education_paid_GE_12mon_C1[is.na(dev_data$Var_DL_Education_paid_GE_12mon_C1)] <-
  0
oot_data$Var_DL_Education_paid_GE_12mon_C1[is.na(oot_data$Var_DL_Education_paid_GE_12mon_C1)] <-
  0




subset1 <-
  dev_data %>% filter(Var_DL_Education_paid_GE_12mon_C1 == 0)
subset2 <-
  dev_data %>% filter(Var_DL_Education_paid_GE_12mon_C1 == 1)
subset3 <-
  dev_data %>% filter(Var_DL_Education_paid_GE_12mon_C1 >= 2)


mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )


dev_data$Var_DL_Education_paid_GE_12mon_woe1 <-
  ifelse(
    dev_data$Var_DL_Education_paid_GE_12mon_C1 == 0,
    woe1,
    ifelse(dev_data$Var_DL_Education_paid_GE_12mon_C1 == 1, woe2, woe3)
  )


oot_data$Var_DL_Education_paid_GE_12mon_woe1 <-
  ifelse(
    oot_data$Var_DL_Education_paid_GE_12mon_C1 == 0,
    woe1,
    ifelse(dev_data$Var_DL_Education_paid_GE_12mon_C1 == 1, woe2, woe3)
  )


rm(subset1, subset2, subset3, woe1, woe2, woe3)



## 2.8 Gold loan ------------------------
## Gold paid 12 mon flag
dev_data$Var_DL_Gold_paid_GE_12mon_flag_var <-
  dev_data$Var_DL_Gold_paid_GE_12mon_flag
oot_data$Var_DL_Gold_paid_GE_12mon_flag_var <-
  oot_data$Var_DL_Gold_paid_GE_12mon_flag

dev_data$Var_DL_Gold_paid_GE_12mon_flag_var[sapply(dev_data$Var_DL_Gold_paid_GE_12mon_flag_var,
                                                   Negate(is.finite))] <- 0
oot_data$Var_DL_Gold_paid_GE_12mon_flag_var[sapply(oot_data$Var_DL_Gold_paid_GE_12mon_flag_var,
                                                   Negate(is.finite))] <- 0

dev_data$Var_DL_Gold_paid_GE_12mon_flag_var[is.na(dev_data$Var_DL_Gold_paid_GE_12mon_flag_var)] <-
  0
oot_data$Var_DL_Gold_paid_GE_12mon_flag_var[is.na(oot_data$Var_DL_Gold_paid_GE_12mon_flag_var)] <-
  0


# Gold paid 6 mon continuous

dev_data$Var_DL_Gold_paid_GE_6mon_C1 <-
  dev_data$Var_DL_Gold_paid_GE_6mon
oot_data$Var_DL_Gold_paid_GE_6mon_C1 <-
  oot_data$Var_DL_Gold_paid_GE_6mon

dev_data$Var_DL_Gold_paid_GE_6mon_C1[is.na(dev_data$Var_DL_Gold_paid_GE_6mon_C1)] <-
  0
oot_data$Var_DL_Gold_paid_GE_6mon_C1[is.na(oot_data$Var_DL_Gold_paid_GE_6mon_C1)] <-
  0




subset1 <- dev_data %>% filter(Var_DL_Gold_paid_GE_6mon_C1 == 0)
subset2 <- dev_data %>% filter(Var_DL_Gold_paid_GE_6mon_C1 == 1)
subset3 <- dev_data %>% filter(Var_DL_Gold_paid_GE_6mon_C1 >= 2)


mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )


dev_data$Var_DL_Gold_paid_GE_6mon_woe1 <-
  ifelse(
    dev_data$Var_DL_Gold_paid_GE_6mon_C1 == 0,
    woe1,
    ifelse(dev_data$Var_DL_Gold_paid_GE_6mon_C1 == 1, woe2, woe3)
  )


oot_data$Var_DL_Gold_paid_GE_6mon_woe1 <-
  ifelse(
    oot_data$Var_DL_Gold_paid_GE_6mon_C1 == 0,
    woe1,
    ifelse(dev_data$Var_DL_Gold_paid_GE_6mon_C1 == 1, woe2, woe3)
  )


rm(subset1, subset2, subset3, woe1, woe2, woe3)







# 2.9 Outstanding amount in live loans----------------
dev_data$Var_outstanding_amount_live_loans_C1 <-
  dev_data$Var_outstanding_amount_live_loans
oot_data$Var_outstanding_amount_live_loans_C1 <-
  oot_data$Var_outstanding_amount_live_loans

dev_data$Var_outstanding_amount_live_loans_C1[is.na(dev_data$Var_outstanding_amount_live_loans_C1)] <-
  0
oot_data$Var_outstanding_amount_live_loans_C1[is.na(oot_data$Var_outstanding_amount_live_loans_C1)] <-
  0


## woe1
subset1 <-
  dev_data %>% filter(Var_outstanding_amount_live_loans_C1 <= 15000)
subset2 <-
  dev_data %>% filter((Var_outstanding_amount_live_loans_C1 > 15000) &
                        (Var_outstanding_amount_live_loans_C1 <= 150000)
  )
subset3 <-
  dev_data %>% filter((Var_outstanding_amount_live_loans_C1 > 150000) &
                        (Var_outstanding_amount_live_loans_C1 <= 250000)
  )
subset4 <-
  dev_data %>% filter(Var_outstanding_amount_live_loans_C1 > 250000)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)

woe1 <-
  get_4bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    bin4 = subset4,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_4bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    bin4 = subset4,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_4bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    bin4 = subset4,
    dependent_feature = 'bad_loan'
  )
woe4 <-
  get_4bin_woe(
    focus_bin = subset4,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    bin4 = subset4,
    dependent_feature = 'bad_loan'
  )



dev_data$Var_outstanding_amount_live_loans_woe1 <-
  ifelse(
    dev_data$Var_outstanding_amount_live_loans_C1 <= 15000,
    woe1,
    ifelse(
      dev_data$Var_outstanding_amount_live_loans_C1 > 15000 &
        dev_data$Var_outstanding_amount_live_loans_C1 <= 150000,
      woe2,
      ifelse(
        dev_data$Var_outstanding_amount_live_loans_C1 > 150000 &
          dev_data$Var_outstanding_amount_live_loans_C1 <= 250000,
        woe3,
        woe4
      )
    )
  )


oot_data$Var_outstanding_amount_live_loans_woe1 <-
  ifelse(
    oot_data$Var_outstanding_amount_live_loans_C1 <= 15000,
    woe1,
    ifelse(
      oot_data$Var_outstanding_amount_live_loans_C1 > 15000 &
        oot_data$Var_outstanding_amount_live_loans_C1 <= 150000,
      woe2,
      ifelse(
        oot_data$Var_outstanding_amount_live_loans_C1 > 150000 &
          oot_data$Var_outstanding_amount_live_loans_C1 <= 250000,
        woe3,
        woe4
      )
    )
  )


rm(subset1, subset2, subset3, subset4, woe1, woe2, woe3, woe4)





## woe2
subset1 <-
  dev_data %>% filter(Var_outstanding_amount_live_loans_C1 <= 150000)
subset2 <-
  dev_data %>% filter((Var_outstanding_amount_live_loans_C1 >  150000) &
                        (Var_outstanding_amount_live_loans_C1 <= 400000)
  )
subset3 <-
  dev_data %>% filter(Var_outstanding_amount_live_loans_C1 > 400000)


mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )




dev_data$Var_outstanding_amount_live_loans_woe2 <-
  ifelse(
    dev_data$Var_outstanding_amount_live_loans_C1 <= 150000,
    woe1,
    ifelse(
      dev_data$Var_outstanding_amount_live_loans_C1 > 150000 &
        dev_data$Var_outstanding_amount_live_loans_C1 <= 400000,
      woe2,
      woe3
    )
  )


oot_data$Var_outstanding_amount_live_loans_woe2 <-
  ifelse(
    oot_data$Var_outstanding_amount_live_loans_C1 <= 150000,
    woe1,
    ifelse(
      oot_data$Var_outstanding_amount_live_loans_C1 > 150000 &
        oot_data$Var_outstanding_amount_live_loans_C1 <= 400000,
      woe2,
      woe3
    )
  )

rm(subset1, subset2, subset3, woe1, woe2, woe3)





### SENP_SEP Flag
dev_data$category_flag_SENP_SEP <-
  ifelse(dev_data$Category %in% c('SENP', 'SEP'), 1, 0)
oot_data$category_flag_SENP_SEP <-
  ifelse(oot_data$Category %in% c('SENP', 'SEP'), 1, 0)



## 2.10 Sanctioned amount by outstanding amount in live loans---------------

dev_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 <-
  dev_data$Var_sanctioned_by_outstanding_amount_live_loans
oot_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 <-
  oot_data$Var_sanctioned_by_outstanding_amount_live_loans

dev_data$Var_sanctioned_by_outstanding_amount_live_loans_C1[is.na(dev_data$Var_sanctioned_by_outstanding_amount_live_loans_C1)] <-
  1.5
oot_data$Var_sanctioned_by_outstanding_amount_live_loans_C1[is.na(oot_data$Var_sanctioned_by_outstanding_amount_live_loans_C1)] <-
  1.5




subset1 <-
  dev_data %>% filter(Var_sanctioned_by_outstanding_amount_live_loans_C1 <= 1)
subset2 <-
  dev_data %>% filter((Var_sanctioned_by_outstanding_amount_live_loans_C1 > 1) &
                        (Var_sanctioned_by_outstanding_amount_live_loans_C1 <= 9)
  )
subset3 <-
  dev_data %>% filter(Var_sanctioned_by_outstanding_amount_live_loans_C1 > 9)


mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)

woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )




dev_data$Var_sanctioned_by_outstanding_amount_live_loans_woe1 <-
  ifelse(
    dev_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 <= 1,
    woe1,
    ifelse(
      dev_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 > 1 &
        dev_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 <= 9,
      woe2,
      woe3
    )
  )


oot_data$Var_sanctioned_by_outstanding_amount_live_loans_woe1 <-
  ifelse(
    oot_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 <= 1,
    woe1,
    ifelse(
      oot_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 > 1 &
        oot_data$Var_sanctioned_by_outstanding_amount_live_loans_C1 <= 9,
      woe2,
      woe3
    )
  )


rm(subset1, subset2, subset3, woe1, woe2, woe3)







## 2.11 Outstanding amount by sanctioned amount in live loans------


dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <-
  dev_data$Var_outstanding_by_sanctioned_amount_live_loans
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <-
  oot_data$Var_outstanding_by_sanctioned_amount_live_loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1[is.na(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1)] <-
  0.05
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1[is.na(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1)] <-
  0.05




subset1 <-
  dev_data %>% filter(Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.25)
subset2 <-
  dev_data %>% filter((Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.25) &
                        (Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.9)
  )
subset3 <-
  dev_data %>% filter(Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.9)


mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)

woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )




dev_data$Var_outstanding_by_sanctioned_amount_live_loans_woe1 <-
  ifelse(
    dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.25,
    woe1,
    ifelse(
      dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.25 &
        dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.9,
      woe2,
      woe3
    )
  )


oot_data$Var_outstanding_by_sanctioned_amount_live_loans_woe1 <-
  ifelse(
    oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.25,
    woe1,
    ifelse(
      oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.25 &
        oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.9,
      woe2,
      woe3
    )
  )


rm(subset1, subset2, subset3, woe1, woe2, woe3)






# ### Enquiry variables----------------------------------------------------------------------------
#
#
### Enquiry count 3m------------
dev_data$Var_EN_enquiry_count_3m_C1 <-
  dev_data$Var_EN_enquiry_count_3m
oot_data$Var_EN_enquiry_count_3m_C1 <-
  oot_data$Var_EN_enquiry_count_3m

dev_data$Var_EN_enquiry_count_3m_C1[is.na(dev_data$Var_EN_enquiry_count_3m_C1)] <-
  0
oot_data$Var_EN_enquiry_count_3m_C1[is.na(oot_data$Var_EN_enquiry_count_3m_C1)] <-
  0

dev_data$Var_EN_enquiry_count_3m_C2 <-
  ifelse(dev_data$Var_EN_enquiry_count_3m_C1 >= 3,
         3,
         dev_data$Var_EN_enquiry_count_3m_C1)
oot_data$Var_EN_enquiry_count_3m_C2 <-
  ifelse(oot_data$Var_EN_enquiry_count_3m_C1 >= 3,
         3,
         oot_data$Var_EN_enquiry_count_3m_C1)

dev_data$Var_EN_enquiry_count_3m_C3 <-
  ifelse(dev_data$Var_EN_enquiry_count_3m_C1 >= 4,
         4,
         dev_data$Var_EN_enquiry_count_3m_C1)
oot_data$Var_EN_enquiry_count_3m_C3 <-
  ifelse(oot_data$Var_EN_enquiry_count_3m_C1 >= 4,
         4,
         oot_data$Var_EN_enquiry_count_3m_C1)




### Enquiry count 3m non CC------------
dev_data$Var_EN_enquiry_count_3m_non_CC_C1 <-
  dev_data$Var_EN_enquiry_count_3m_non_CC
oot_data$Var_EN_enquiry_count_3m_non_CC_C1 <-
  oot_data$Var_EN_enquiry_count_3m_non_CC

dev_data$Var_EN_enquiry_count_3m_non_CC_C1[is.na(dev_data$Var_EN_enquiry_count_3m_non_CC_C1)] <-
  0
oot_data$Var_EN_enquiry_count_3m_non_CC_C1[is.na(oot_data$Var_EN_enquiry_count_3m_non_CC_C1)] <-
  0

dev_data$Var_EN_enquiry_count_3m_non_CC_C2 <-
  ifelse(
    dev_data$Var_EN_enquiry_count_3m_non_CC_C1 >= 3,
    3,
    dev_data$Var_EN_enquiry_count_3m_non_CC_C1
  )
oot_data$Var_EN_enquiry_count_3m_non_CC_C2 <-
  ifelse(
    oot_data$Var_EN_enquiry_count_3m_non_CC_C1 >= 3,
    3,
    oot_data$Var_EN_enquiry_count_3m_non_CC_C1
  )

dev_data$Var_EN_enquiry_count_3m_non_CC_C3 <-
  ifelse(
    dev_data$Var_EN_enquiry_count_3m_non_CC_C1 >= 4,
    4,
    dev_data$Var_EN_enquiry_count_3m_non_CC_C1
  )
oot_data$Var_EN_enquiry_count_3m_non_CC_C3 <-
  ifelse(
    oot_data$Var_EN_enquiry_count_3m_non_CC_C1 >= 4,
    4,
    oot_data$Var_EN_enquiry_count_3m_non_CC_C1
  )



### Enquiry count 6m -------------
dev_data$Var_EN_enquiry_count_6m_C1 <-
  dev_data$Var_EN_enquiry_count_6m
oot_data$Var_EN_enquiry_count_6m_C1 <-
  oot_data$Var_EN_enquiry_count_6m

dev_data$Var_EN_enquiry_count_6m_C1[is.na(dev_data$Var_EN_enquiry_count_6m_C1)] <-
  0
oot_data$Var_EN_enquiry_count_6m_C1[is.na(oot_data$Var_EN_enquiry_count_6m_C1)] <-
  0


dev_data$Var_EN_enquiry_count_6m_C2 <-
  ifelse(dev_data$Var_EN_enquiry_count_6m_C1 >= 7,
         7,
         dev_data$Var_EN_enquiry_count_6m_C1)
oot_data$Var_EN_enquiry_count_6m_C2 <-
  ifelse(oot_data$Var_EN_enquiry_count_6m_C1 >= 7,
         7,
         oot_data$Var_EN_enquiry_count_6m_C1)



###  Enquiry count 6m non CC ------------
dev_data$Var_EN_enquiry_count_6m_non_CC_C1 <-
  dev_data$Var_EN_enquiry_count_6m_non_CC
oot_data$Var_EN_enquiry_count_6m_non_CC_C1 <-
  oot_data$Var_EN_enquiry_count_6m_non_CC

dev_data$Var_EN_enquiry_count_6m_non_CC_C1[is.na(dev_data$Var_EN_enquiry_count_6m_non_CC_C1)] <-
  0
oot_data$Var_EN_enquiry_count_6m_non_CC_C1[is.na(oot_data$Var_EN_enquiry_count_6m_non_CC_C1)] <-
  0


dev_data$Var_EN_enquiry_count_6m_non_CC_C2 <-
  ifelse(
    dev_data$Var_EN_enquiry_count_6m_non_CC_C1 >= 5,
    5,
    dev_data$Var_EN_enquiry_count_6m_non_CC_C1
  )
oot_data$Var_EN_enquiry_count_6m_non_CC_C2 <-
  ifelse(
    oot_data$Var_EN_enquiry_count_6m_non_CC_C1 >= 5,
    5,
    oot_data$Var_EN_enquiry_count_6m_non_CC_C1
  )

dev_data$Var_EN_enquiry_count_6m_non_CC_C3 <-
  ifelse(
    dev_data$Var_EN_enquiry_count_6m_non_CC_C1 >= 4,
    4,
    dev_data$Var_EN_enquiry_count_6m_non_CC_C1
  )
oot_data$Var_EN_enquiry_count_6m_non_CC_C3 <-
  ifelse(
    oot_data$Var_EN_enquiry_count_6m_non_CC_C1 >= 4,
    4,
    oot_data$Var_EN_enquiry_count_6m_non_CC_C1
  )



###  Enquiry count 6m non CC CD
dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 <-
  dev_data$Var_EN_enquiry_count_6m_non_CC_CD
oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 <-
  oot_data$Var_EN_enquiry_count_6m_non_CC_CD

dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1[is.na(dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1)] <-
  0
oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1[is.na(oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1)] <-
  0


dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C2 <-
  ifelse(
    dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 >= 3,
    3,
    dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1
  )
oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C2 <-
  ifelse(
    oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 >= 3,
    3,
    oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1
  )

dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C3 <-
  ifelse(
    dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 >= 4,
    4,
    dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1
  )
oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C3 <-
  ifelse(
    oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 >= 4,
    4,
    oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1
  )

dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C4 <-
  ifelse(
    dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 >= 2,
    2,
    dev_data$Var_EN_enquiry_count_6m_non_CC_CD_C1
  )
oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C4 <-
  ifelse(
    oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1 >= 2,
    2,
    oot_data$Var_EN_enquiry_count_6m_non_CC_CD_C1
  )


### Enquiry count woe 
dev_data$Var_EN_enquiry_count_6m_C1  <- dev_data$Var_EN_enquiry_count_6m    
oot_data$Var_EN_enquiry_count_6m_C1  <- oot_data$Var_EN_enquiry_count_6m

dev_data$Var_EN_enquiry_count_6m_C1[is.na(dev_data$Var_EN_enquiry_count_6m_C1)] <-
  0
oot_data$Var_EN_enquiry_count_6m_C1[is.na(oot_data$Var_EN_enquiry_count_6m_C1)] <-
  0

dev_data$Var_EN_enquiry_count_6m_flag <-
  ifelse(dev_data$Var_EN_enquiry_count_6m_C1 < 3, 0, 1)
oot_data$Var_EN_enquiry_count_6m_flag <-
  ifelse(oot_data$Var_EN_enquiry_count_6m_C1 < 3 , 0, 1)




## WOE 1
subset1 <- dev_data %>% filter(Var_EN_enquiry_count_6m_C1 == 0)
subset2 <- dev_data %>% filter(Var_EN_enquiry_count_6m_C1 == 1)
subset3 <- dev_data %>% filter(Var_EN_enquiry_count_6m_C1 > 1 & Var_EN_enquiry_count_6m_C1 <= 4)
subset4 <- dev_data %>% filter(Var_EN_enquiry_count_6m_C1 > 4)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)


woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')



dev_data$Var_EN_enquiry_count_6m_C1_woe1 <-
  ifelse(
    dev_data$Var_EN_enquiry_count_6m_C1 == 0, woe1,
    ifelse(dev_data$Var_EN_enquiry_count_6m_C1 == 1, woe2,
    ifelse(dev_data$Var_EN_enquiry_count_6m_C1 > 1 & dev_data$Var_EN_enquiry_count_6m_C1 <= 4,
           woe3, woe4)
  ))

oot_data$Var_EN_enquiry_count_6m_C1_woe1 <-
  ifelse(
    oot_data$Var_EN_enquiry_count_6m_C1 == 0, woe1,
    ifelse(oot_data$Var_EN_enquiry_count_6m_C1 == 1, woe2,
           ifelse(oot_data$Var_EN_enquiry_count_6m_C1 > 1 & oot_data$Var_EN_enquiry_count_6m_C1 <= 4,
                  woe3, woe4)
    ))
rm(subset1, subset2, subset3,subset4, woe1, woe2, woe3, woe4)

## WOE 2
subset1 <- dev_data %>% filter(Var_EN_enquiry_count_6m_C2 == 0)
subset2 <- dev_data %>% filter(Var_EN_enquiry_count_6m_C2 == 1)
subset3 <- dev_data %>% filter(Var_EN_enquiry_count_6m_C2 > 1 & Var_EN_enquiry_count_6m_C2 <= 5)
subset4 <- dev_data %>% filter(Var_EN_enquiry_count_6m_C2 > 5)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)


woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')



dev_data$Var_EN_enquiry_count_6m_C1_woe2 <-
  ifelse(
    dev_data$Var_EN_enquiry_count_6m_C1 == 0, woe1,
    ifelse(dev_data$Var_EN_enquiry_count_6m_C1 == 1, woe2,
           ifelse(dev_data$Var_EN_enquiry_count_6m_C1 > 1 & dev_data$Var_EN_enquiry_count_6m_C1 <= 5,
                  woe3, woe4)
    ))

oot_data$Var_EN_enquiry_count_6m_C1_woe2 <-
  ifelse(
    oot_data$Var_EN_enquiry_count_6m_C1 == 0, woe1,
    ifelse(oot_data$Var_EN_enquiry_count_6m_C1 == 1, woe2,
           ifelse(oot_data$Var_EN_enquiry_count_6m_C1 > 1 & oot_data$Var_EN_enquiry_count_6m_C1 <= 5,
                  woe3, woe4)
    ))
rm(subset1, subset2, subset3,subset4, woe1, woe2, woe3, woe4)





##### Delinquency variables --------


## 30 DPD in 3 mon ------------
dev_data$Var_DL_all_30dpd_3mon_C1 <- dev_data$Var_DL_all_30dpd_3mon
oot_data$Var_DL_all_30dpd_3mon_C1 <- oot_data$Var_DL_all_30dpd_3mon

dev_data$Var_DL_all_30dpd_3mon_C1[is.na(dev_data$Var_DL_all_30dpd_3mon_C1)] <-
  0
oot_data$Var_DL_all_30dpd_3mon_C1[is.na(oot_data$Var_DL_all_30dpd_3mon_C1)] <-
  0

dev_data$Var_DL_all_30dpd_3mon_flag <-
  ifelse(dev_data$Var_DL_all_30dpd_3mon_C1 == 0, 0, 1)
oot_data$Var_DL_all_30dpd_3mon_flag <-
  ifelse(oot_data$Var_DL_all_30dpd_3mon_C1 == 0, 0, 1)



## WOE 1
subset1 <- dev_data %>% filter(Var_DL_all_30dpd_3mon_C1 == 0)
subset2 <- dev_data %>% filter(Var_DL_all_30dpd_3mon_C1 == 1)
subset3 <- dev_data %>% filter(Var_DL_all_30dpd_3mon_C1 >= 2)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
# 
# dev_data$Var_DL_all_30dpd_3mon_C1_woe1 <-
#   ifelse(
#     dev_data$Var_DL_all_30dpd_3mon_C1 == 0,
#     woe1,
#     ifelse(dev_data$Var_DL_all_30dpd_3mon_C1 == 1, woe2, woe3)
#   )
# 
# oot_data$Var_DL_all_30dpd_3mon_C1_woe1 <-
#   ifelse(
#     oot_data$Var_DL_all_30dpd_3mon_C1 == 0,
#     woe1,
#     ifelse(oot_data$Var_DL_all_30dpd_3mon_C1 == 1, woe2, woe3)
#   )
# 
rm(subset1, subset2, subset3, woe1, woe2, woe3)


## 30 DPD in 6 mon --------------
dev_data$Var_DL_all_30dpd_6mon_C1 <- dev_data$Var_DL_all_30dpd_6mon
oot_data$Var_DL_all_30dpd_6mon_C1 <- oot_data$Var_DL_all_30dpd_6mon

dev_data$Var_DL_all_30dpd_6mon_C1[is.na(dev_data$Var_DL_all_30dpd_6mon_C1)] <-
  0
oot_data$Var_DL_all_30dpd_6mon_C1[is.na(oot_data$Var_DL_all_30dpd_6mon_C1)] <-
  0

dev_data$Var_DL_all_30dpd_6mon_flag <-
  ifelse(dev_data$Var_DL_all_30dpd_6mon_C1 == 0, 0, 1)
oot_data$Var_DL_all_30dpd_6mon_flag <-
  ifelse(oot_data$Var_DL_all_30dpd_6mon_C1 == 0, 0, 1)

## WOE 1
subset1 <- dev_data %>% filter(Var_DL_all_30dpd_6mon_C1 == 0)
subset2 <- dev_data %>% filter(Var_DL_all_30dpd_6mon_C1 == 1)
subset3 <- dev_data %>% filter(Var_DL_all_30dpd_6mon_C1 >= 2)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )

dev_data$Var_DL_all_30dpd_6mon_C1_woe1 <-
  ifelse(
    dev_data$Var_DL_all_30dpd_6mon_C1 == 0,
    woe1,
    ifelse(dev_data$Var_DL_all_30dpd_6mon_C1 == 1, woe2, woe3)
  )

oot_data$Var_DL_all_30dpd_6mon_C1_woe1 <-
  ifelse(
    oot_data$Var_DL_all_30dpd_6mon_C1 == 0,
    woe1,
    ifelse(oot_data$Var_DL_all_30dpd_6mon_C1 == 1, woe2, woe3)
  )

rm(subset1, subset2, subset3, woe1, woe2, woe3)




## 30 DPD in 3m mon CC CD ------
dev_data$Var_DL_non_CC_CD_30dpd_3mon_C1 <-
  dev_data$Var_DL_non_CC_CD_30dpd_3mon
oot_data$Var_DL_non_CC_CD_30dpd_3mon_C1 <-
  oot_data$Var_DL_non_CC_CD_30dpd_3mon

dev_data$Var_DL_non_CC_CD_30dpd_3mon_C1[is.na(dev_data$Var_DL_non_CC_CD_30dpd_3mon_C1)] <-
  0
oot_data$Var_DL_non_CC_CD_30dpd_3mon_C1[is.na(oot_data$Var_DL_non_CC_CD_30dpd_3mon_C1)] <-
  0


dev_data$Var_DL_non_CC_CD_30dpd_3mon_flag <-
  ifelse(dev_data$Var_DL_non_CC_CD_30dpd_3mon_C1 == 0, 0, 1)
oot_data$Var_DL_non_CC_CD_30dpd_3mon_flag <-
  ifelse(oot_data$Var_DL_non_CC_CD_30dpd_3mon_C1 == 0, 0, 1)



## 30 DPD in 6m mon CC CD ------------
dev_data$Var_DL_non_CC_CD_30dpd_6mon_C1 <-
  dev_data$Var_DL_non_CC_CD_30dpd_6mon
oot_data$Var_DL_non_CC_CD_30dpd_6mon_C1 <-
  oot_data$Var_DL_non_CC_CD_30dpd_6mon

dev_data$Var_DL_non_CC_CD_30dpd_6mon_C1[is.na(dev_data$Var_DL_non_CC_CD_30dpd_6mon_C1)] <-
  0
oot_data$Var_DL_non_CC_CD_30dpd_6mon_C1[is.na(oot_data$Var_DL_non_CC_CD_30dpd_6mon_C1)] <-
  0


dev_data$Var_DL_non_CC_CD_30dpd_6mon_flag <-
  ifelse(dev_data$Var_DL_non_CC_CD_30dpd_6mon_C1 == 0, 0, 1)
oot_data$Var_DL_non_CC_CD_30dpd_6mon_flag <-
  ifelse(oot_data$Var_DL_non_CC_CD_30dpd_6mon_C1 == 0, 0, 1)










## 60 DPD in 3 mon
dev_data$Var_DL_all_60dpd_3mon_C1 <- dev_data$Var_DL_all_60dpd_3mon
oot_data$Var_DL_all_60dpd_3mon_C1 <- oot_data$Var_DL_all_60dpd_3mon

dev_data$Var_DL_all_60dpd_3mon_C1[is.na(dev_data$Var_DL_all_60dpd_3mon_C1)] <-
  0
oot_data$Var_DL_all_60dpd_3mon_C1[is.na(oot_data$Var_DL_all_60dpd_3mon_C1)] <-
  0

dev_data$Var_DL_all_60dpd_3mon_flag <-
  ifelse(dev_data$Var_DL_all_60dpd_3mon_C1 == 0, 0, 1)
oot_data$Var_DL_all_60dpd_3mon_flag <-
  ifelse(oot_data$Var_DL_all_60dpd_3mon_C1 == 0, 0, 1)



## 60 DPD in 6 mon
dev_data$Var_DL_all_60dpd_6mon_C1 <- dev_data$Var_DL_all_60dpd_6mon
oot_data$Var_DL_all_60dpd_6mon_C1 <- oot_data$Var_DL_all_60dpd_6mon

dev_data$Var_DL_all_60dpd_6mon_C1[is.na(dev_data$Var_DL_all_60dpd_6mon_C1)] <-
  0
oot_data$Var_DL_all_60dpd_6mon_C1[is.na(oot_data$Var_DL_all_60dpd_6mon_C1)] <-
  0

dev_data$Var_DL_all_60dpd_6mon_flag <-
  ifelse(dev_data$Var_DL_all_60dpd_6mon_C1 == 0, 0, 1)
oot_data$Var_DL_all_60dpd_6mon_flag <-
  ifelse(oot_data$Var_DL_all_60dpd_6mon_C1 == 0, 0, 1)



## 60 DPD in 3m mon CC CD
dev_data$Var_DL_non_CC_CD_60dpd_3mon_C1 <-
  dev_data$Var_DL_non_CC_CD_60dpd_3mon
oot_data$Var_DL_non_CC_CD_60dpd_3mon_C1 <-
  oot_data$Var_DL_non_CC_CD_60dpd_3mon

dev_data$Var_DL_non_CC_CD_60dpd_3mon_C1[is.na(dev_data$Var_DL_non_CC_CD_60dpd_3mon_C1)] <-
  0
oot_data$Var_DL_non_CC_CD_60dpd_3mon_C1[is.na(oot_data$Var_DL_non_CC_CD_60dpd_3mon_C1)] <-
  0


dev_data$Var_DL_non_CC_CD_60dpd_3mon_flag <-
  ifelse(dev_data$Var_DL_non_CC_CD_60dpd_3mon_C1 == 0, 0, 1)
oot_data$Var_DL_non_CC_CD_60dpd_3mon_flag <-
  ifelse(oot_data$Var_DL_non_CC_CD_60dpd_3mon_C1 == 0, 0, 1)



## 60 DPD in 6m non CC CD
dev_data$Var_DL_non_CC_CD_60dpd_6mon_C1 <-
  dev_data$Var_DL_non_CC_CD_60dpd_6mon
oot_data$Var_DL_non_CC_CD_60dpd_6mon_C1 <-
  oot_data$Var_DL_non_CC_CD_60dpd_6mon

dev_data$Var_DL_non_CC_CD_60dpd_6mon_C1[is.na(dev_data$Var_DL_non_CC_CD_60dpd_6mon_C1)] <-
  0
oot_data$Var_DL_non_CC_CD_60dpd_6mon_C1[is.na(oot_data$Var_DL_non_CC_CD_60dpd_6mon_C1)] <-
  0


dev_data$Var_DL_non_CC_CD_60dpd_6mon_flag <-
  ifelse(dev_data$Var_DL_non_CC_CD_60dpd_6mon_C1 == 0, 0, 1)
oot_data$Var_DL_non_CC_CD_60dpd_6mon_flag <-
  ifelse(oot_data$Var_DL_non_CC_CD_60dpd_6mon_C1 == 0, 0, 1)


## WOE 1
subset1 <- dev_data %>% filter(Var_DL_non_CC_CD_60dpd_6mon_C1 == 0)
subset2 <- dev_data %>% filter(Var_DL_non_CC_CD_60dpd_6mon_C1 == 1)
subset3 <- dev_data %>% filter(Var_DL_non_CC_CD_60dpd_6mon_C1 >= 2)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


# woe1 <-
#   get_3bin_woe(
#     focus_bin = subset1,
#     bin1 = subset1,
#     bin2 = subset2,
#     bin3 = subset3,
#     dependent_feature = 'bad_loan'
#   )
# woe2 <-
#   get_3bin_woe(
#     focus_bin = subset2,
#     bin1 = subset1,
#     bin2 = subset2,
#     bin3 = subset3,
#     dependent_feature = 'bad_loan'
#   )
# woe3 <-
#   get_3bin_woe(
#     focus_bin = subset3,
#     bin1 = subset1,
#     bin2 = subset2,
#     bin3 = subset3,
#     dependent_feature = 'bad_loan'
#   )
# 
# dev_data$Var_DL_non_CC_CD_60dpd_6mon_C1_woe1 <-
#   ifelse(
#     dev_data$Var_DL_non_CC_CD_60dpd_6mon_C1 == 0,
#     woe1,
#     ifelse(dev_data$Var_DL_non_CC_CD_60dpd_6mon_C1 == 1, woe2, woe3)
#   )
# 
# oot_data$Var_DL_non_CC_CD_60dpd_6mon_C1_woe1 <-
#   ifelse(
#     oot_data$Var_DL_non_CC_CD_60dpd_6mon_C1 == 0,
#     woe1,
#     ifelse(oot_data$Var_DL_non_CC_CD_60dpd_6mon_C1 == 1, woe2, woe3)
#   )
# 
# rm(subset1, subset2, subset3, woe1, woe2, woe3)

## 30 DPD in 12m mon-------------

dev_data$Var_DL_30dpd_12mon_C1 <- dev_data$Var_DL_all_30dpd_12mon
oot_data$Var_DL_30dpd_12mon_C1 <- oot_data$Var_DL_all_30dpd_12mon

dev_data$Var_DL_30dpd_12mon_C1[is.na(dev_data$Var_DL_30dpd_12mon_C1)] <-
  0
oot_data$Var_DL_30dpd_12mon_C1[is.na(oot_data$Var_DL_30dpd_12mon_C1)] <-
  0

dev_data$Var_DL_30dpd_12mon_flag <-
  ifelse(dev_data$Var_DL_30dpd_12mon_C1 == 0, 0, 1)
oot_data$Var_DL_30dpd_12mon_flag <-
  ifelse(oot_data$Var_DL_30dpd_12mon_C1 == 0, 0, 1)



## Var_DL_secured_30dpd_12mon

dev_data$Var_DL_secured_30dpd_12mon_C1 <- dev_data$Var_DL_secured_30dpd_12mon
oot_data$Var_DL_secured_30dpd_12mon_C1 <- oot_data$Var_DL_secured_30dpd_12mon

dev_data$Var_DL_secured_30dpd_12mon_C1[is.na(dev_data$Var_DL_secured_30dpd_12mon_C1)] <-
  0
oot_data$Var_DL_secured_30dpd_12mon_C1[is.na(oot_data$Var_DL_secured_30dpd_12mon_C1)] <-
  0

dev_data$Var_DL_secured_30dpd_12mon_flag <-
  ifelse(dev_data$Var_DL_secured_30dpd_12mon_C1 == 0, 0, 1)
oot_data$Var_DL_secured_30dpd_12mon_flag <-
  ifelse(oot_data$Var_DL_secured_30dpd_12mon_C1 == 0, 0, 1)


## WOE 1
subset1 <- dev_data %>% filter(Var_DL_secured_30dpd_12mon_C1 == 0)
subset2 <- dev_data %>% filter(Var_DL_secured_30dpd_12mon_C1 > 0 & Var_DL_secured_30dpd_12mon_C1 < 3)
subset3 <- dev_data %>% filter(Var_DL_secured_30dpd_12mon_C1 >= 3)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )

dev_data$Var_DL_secured_30dpd_12mon_C1_woe1 <-
  ifelse(
    dev_data$Var_DL_secured_30dpd_12mon_C1 == 0,
    woe1,
    ifelse(dev_data$Var_DL_secured_30dpd_12mon_C1 > 0 & dev_data$Var_DL_secured_30dpd_12mon_C1 < 3, woe2, woe3)
  )

oot_data$Var_DL_secured_30dpd_12mon_C1_woe1 <-
  ifelse(
    oot_data$Var_DL_secured_30dpd_12mon_C1 == 0,
    woe1,
    ifelse(oot_data$Var_DL_secured_30dpd_12mon_C1 > 0 & oot_data$Var_DL_secured_30dpd_12mon_C1 < 3, woe2, woe3)
  )

rm(subset1, subset2, subset3, woe1, woe2, woe3)



### Var_PO_months_12_PL_live


dev_data$Var_PO_months_12_PL_live_C1 <- dev_data$Var_PO_months_12_PL_live
oot_data$Var_PO_months_12_PL_live_C1 <- oot_data$Var_PO_months_12_PL_live

dev_data$Var_PO_months_12_PL_live_C1[is.na(dev_data$Var_PO_months_12_PL_live_C1)] <-
  0
oot_data$Var_PO_months_12_PL_live_C1[is.na(oot_data$Var_PO_months_12_PL_live_C1)] <-
  0

dev_data$Var_PO_months_12_PL_live_C2 <- ifelse(dev_data$Var_PO_months_12_PL_live > 2, 2, dev_data$Var_PO_months_12_PL_live ) 
oot_data$Var_PO_months_12_PL_live_C2 <- ifelse(oot_data$Var_PO_months_12_PL_live > 2, 2, oot_data$Var_PO_months_12_PL_live)

dev_data$Var_PO_months_12_PL_live_flag <-
  ifelse(dev_data$Var_PO_months_12_PL_live_C1 == 0, 0, 1)
oot_data$Var_PO_months_12_PL_live_flag <-
  ifelse(oot_data$Var_PO_months_12_PL_live_C1 == 0, 0, 1)


## WOE 1
subset1 <- dev_data %>% filter(Var_PO_months_12_PL_live_C1 == 0)
subset2 <- dev_data %>% filter(Var_PO_months_12_PL_live_C1 == 1)
subset3 <- dev_data %>% filter(Var_PO_months_12_PL_live_C1 >= 2)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )

dev_data$Var_PO_months_12_PL_live_C1_woe1 <-
  ifelse(
    dev_data$Var_PO_months_12_PL_live_C1 == 0,
    woe1,
    ifelse(dev_data$Var_PO_months_12_PL_live_C1 == 1, woe2, woe3)
  )

oot_data$Var_PO_months_12_PL_live_C1_woe1 <-
  ifelse(
    oot_data$Var_PO_months_12_PL_live_C1 == 0,
    woe1,
    ifelse(oot_data$Var_PO_months_12_PL_live_C1 == 1, woe2, woe3)
  )

rm(subset1, subset2, subset3, woe1, woe2, woe3)



#----------------------------
# crosstab-----------------

dev_data$Var_PO_closed_sanction_amount_excl_CC_bin <-
  ifelse(
    dev_data$Var_PO_closed_sanction_amount_excl_CC <= 35000,
    'bin_LE_35k',
    ifelse(
      dev_data$Var_PO_closed_sanction_amount_excl_CC > 35000 &
        dev_data$Var_PO_closed_sanction_amount_excl_CC <= 300000,
      'bin_35k_to_3L',
        'bin_GE_3L'
      )
    )
  


oot_data$Var_PO_closed_sanction_amount_excl_CC_bin <-
  ifelse(
    oot_data$Var_PO_closed_sanction_amount_excl_CC <= 35000,
    'bin_LE_35k',
    ifelse(
      oot_data$Var_PO_closed_sanction_amount_excl_CC > 35000 &
        oot_data$Var_PO_closed_sanction_amount_excl_CC <= 300000,
      'bin_35k_to_3L',
      'bin_GE_3L'
    )
  )


dev_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <-
  ifelse(
    dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.1,
    'val_LE_point1',
    ifelse(
      dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.1 &
        dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 1,
      'val_point1_to_1',
      'val_GE_1'
    )
  )


oot_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <-
  ifelse(
    oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 0.1,
    'val_LE_point1',
    ifelse(
      oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 > 0.1 &
        oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C1 <= 1,
      'val_point1_to_1',
      'val_GE_1'
    )
  )


dev_data %>% group_by(
  Var_PO_closed_sanction_amount_excl_CC_bin,
  Var_outstanding_by_sanctioned_amount_live_loans_bin
)  %>% summarise(n = n(),
                 default = mean(bad_loan))



subset1 <- dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_point1')))
subset2 <- dev_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_point1_to_1','val_GE_1'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_35k_to_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_point1'))))
subset3 <- dev_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_35k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_point1','val_point1_to_1'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_35k_to_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_point1_to_1'))))
subset4 <- dev_data %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_35k_to_3L','bin_LE_35k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_1')))

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)

woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')


subset1_oot <- oot_data %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_point1')))
subset2_oot <- oot_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_GE_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_point1_to_1','val_GE_1'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_35k_to_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_point1'))))
subset3_oot <- oot_data %>% filter(((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_LE_35k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_point1','val_point1_to_1'))) | ((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_35k_to_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_point1_to_1'))))
subset4_oot <- oot_data %>% filter((Var_PO_closed_sanction_amount_excl_CC_bin %in% c('bin_35k_to_3L','bin_LE_35k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_1')))


dev_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed <- ifelse(dev_data$deal_no %in% unique(subset1$deal_no), woe1,
                                                                        ifelse(dev_data$deal_no %in% unique(subset2$deal_no), woe2,
                                                                               ifelse(dev_data$deal_no %in% unique(subset3$deal_no), woe3,
                                                                                      woe4)))

oot_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_closed <- ifelse(oot_data$deal_no %in% unique(subset1_oot$deal_no), woe1,
                                                                        ifelse(oot_data$deal_no %in% unique(subset2_oot$deal_no), woe2,
                                                                               ifelse(oot_data$deal_no %in% unique(subset3_oot$deal_no), woe3,
                                                                                      woe4)))


rm(subset1,subset2,subset3,subset4,subset1_oot,subset2_oot,subset3_oot,subset4_oot, woe1, woe2, woe3, woe4)













## crosstab sanctioned amount


dev_data$Var_sanctioned_amount_live_loans_C <- dev_data$Var_sanctioned_amount_live_loans
oot_data$Var_sanctioned_amount_live_loans_C <- oot_data$Var_sanctioned_amount_live_loans


dev_data$Var_sanctioned_amount_live_loans_C[is.na(dev_data$Var_sanctioned_amount_live_loans_C)] <- 0
oot_data$Var_sanctioned_amount_live_loans_C[is.na(oot_data$Var_sanctioned_amount_live_loans_C)] <- 0

subset1 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C <= 60000 )
subset2 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C > 60000 & 
                                 Var_sanctioned_amount_live_loans_C <= 300000 )
subset3 <- dev_data %>% filter(Var_sanctioned_amount_live_loans_C > 300000)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


## 2.9.2 Outstanding amount by sanctioned amount in live loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- dev_data$Var_outstanding_by_sanctioned_amount_live_loans
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <- oot_data$Var_outstanding_by_sanctioned_amount_live_loans

dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 0.05
oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C[is.na(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C)] <- 0.05



dev_data$Var_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_sanctioned_amount_live_loans_C <= 60000, 'bin_LE_60k', 
                                                        ifelse(dev_data$Var_sanctioned_amount_live_loans_C > 60000 & dev_data$Var_sanctioned_amount_live_loans_C <= 300000, 'bin_60k_to_3L', 'bin_GE_3L'))


oot_data$Var_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_sanctioned_amount_live_loans_C <= 60000, 'bin_LE_60k', 
                                                        ifelse(oot_data$Var_sanctioned_amount_live_loans_C > 60000 & oot_data$Var_sanctioned_amount_live_loans_C <= 300000, 'bin_60k_to_3L', 'bin_GE_3L'))



dev_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.25 & dev_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.9, 'val_25_to_90_pct','val_GE_90_pct'))


oot_data$Var_outstanding_by_sanctioned_amount_live_loans_bin <- ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.25, 'val_LE_25_pct', 
                                                                       ifelse(oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C > 0.25 & oot_data$Var_outstanding_by_sanctioned_amount_live_loans_C <= 0.9, 'val_25_to_90_pct','val_GE_90_pct'))


dev_data %>% group_by(
  Var_sanctioned_amount_live_loans_bin,
  Var_outstanding_by_sanctioned_amount_live_loans_bin
)  %>% summarise(n = n(),
                 default = mean(bad_loan))



subset1 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_60k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_90_pct')))
subset2 <- dev_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_60k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_90_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_60k_to_3L','bin_GE_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_90_pct'))))
subset3 <- dev_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_60k_to_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_90_pct'))) |
                                 ((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_90_pct'))) | 
                                    ((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_60k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct'))))
subset4 <- dev_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct')))
                               
mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)
mean(subset4$bad_loan)

woe1 <- get_4bin_woe(focus_bin = subset1, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe2 <- get_4bin_woe(focus_bin = subset2, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe3 <- get_4bin_woe(focus_bin = subset3, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')
woe4 <- get_4bin_woe(focus_bin = subset4, bin1 = subset1, bin2 = subset2, bin3 = subset3, bin4 = subset4,dependent_feature = 'bad_loan')

subset1_oot <- oot_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_60k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_90_pct')))
subset2_oot <- oot_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_60k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_90_pct'))) | ((Var_sanctioned_amount_live_loans_bin %in% c('bin_60k_to_3L','bin_GE_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_GE_90_pct'))))
subset3_oot <- oot_data %>% filter(((Var_sanctioned_amount_live_loans_bin %in% c('bin_60k_to_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct','val_25_to_90_pct'))) |
                                 ((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_25_to_90_pct'))) | 
                                 ((Var_sanctioned_amount_live_loans_bin %in% c('bin_LE_60k')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct'))))
subset4_oot <- oot_data %>% filter((Var_sanctioned_amount_live_loans_bin %in% c('bin_GE_3L')) & (Var_outstanding_by_sanctioned_amount_live_loans_bin %in% c('val_LE_25_pct')))



dev_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live <- ifelse(dev_data$deal_no %in% unique(subset1$deal_no), woe1,
                                                                        ifelse(dev_data$deal_no %in% unique(subset2$deal_no), woe2,
                                                                               ifelse(dev_data$deal_no %in% unique(subset3$deal_no), woe3,
                                                                                      woe4)))

oot_data$cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live <- ifelse(oot_data$deal_no %in% unique(subset1_oot$deal_no), woe1,
                                                                        ifelse(oot_data$deal_no %in% unique(subset2_oot$deal_no), woe2,
                                                                               ifelse(oot_data$deal_no %in% unique(subset3_oot$deal_no), woe3,
                                                                                      woe4)))


rm(subset1,subset2,subset3,subset4,subset1_oot,subset2_oot,subset3_oot,subset4_oot, woe1, woe2, woe3, woe4)





### total sanctioned amount
dev_data$Var_total_sanctioned_amount <- dev_data$Var_sanctioned_amount_live_loans_C1 + dev_data$Var_PO_closed_sanction_amount_excl_CC
oot_data$Var_total_sanctioned_amount <- oot_data$Var_sanctioned_amount_live_loans_C1 + oot_data$Var_PO_closed_sanction_amount_excl_CC

subset1 <- dev_data %>% filter(Var_total_sanctioned_amount <= 250000 )
subset2 <- dev_data %>% filter(Var_total_sanctioned_amount > 250000 & 
                                 Var_total_sanctioned_amount < 600000 )
subset3 <- dev_data %>% filter(Var_total_sanctioned_amount > 600000)

mean(subset1$bad_loan)
mean(subset2$bad_loan)
mean(subset3$bad_loan)


woe1 <-
  get_3bin_woe(
    focus_bin = subset1,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe2 <-
  get_3bin_woe(
    focus_bin = subset2,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )
woe3 <-
  get_3bin_woe(
    focus_bin = subset3,
    bin1 = subset1,
    bin2 = subset2,
    bin3 = subset3,
    dependent_feature = 'bad_loan'
  )

dev_data$Var_total_sanctioned_amount_woe1 <-
  ifelse(
    dev_data$Var_total_sanctioned_amount <= 250000,
    woe1,
    ifelse(dev_data$Var_total_sanctioned_amount > 250000 & dev_data$Var_total_sanctioned_amount <= 600000, woe2, woe3)
  )

oot_data$Var_total_sanctioned_amount_woe1 <-
  ifelse(
    oot_data$Var_total_sanctioned_amount <= 250000,
    woe1,
    ifelse(oot_data$Var_total_sanctioned_amount > 250000 & oot_data$Var_total_sanctioned_amount <= 600000, woe2, woe3)
  )

rm(subset1, subset2, subset3, woe1, woe2, woe3)




########################################################################################################################

selected_features <- c(
   'Var_credit_vintage_C1',
   'Var_credit_vintage_C2',
   'Var_credit_vintage_C3',
   'Var_credit_vintage_woe1',
  
   'Var_PO_closed_sanction_amount_excl_CC_woe1',
  'Var_PO_closed_sanction_amount_excl_CC_woe2',

   'Var_sanctioned_amount_live_loans_C1',
  'Var_sanctioned_amount_live_loans_C2',
  'Var_sanctioned_amount_live_loans_C3',
  # 'Var_sanctioned_amount_live_loans_woe1',
  'Var_sanctioned_amount_live_loans_woe2',

  'Var_PO_closed_excl_CC_C1',
  'Var_PO_closed_excl_CC_C2',
  'Var_PO_closed_excl_CC_C3',
  'Var_PO_closed_excl_CC_woe1',
  
   'Var_DL_HL_paid_GE_12mon_flag_var',
   'Var_DL_HL_paid_GE_12mon_C1',
  'Var_DL_HL_paid_GE_12mon_woe1',
  
  
  "Var_DL_Education_paid_GE_12mon_flag_var",
  "Var_DL_Education_paid_GE_12mon_C1",
  # "Var_DL_Education_paid_GE_12mon_woe1",
  
  "Var_DL_Gold_paid_GE_12mon_flag_var",
   "Var_DL_Gold_paid_GE_6mon_C1",
   "Var_DL_Gold_paid_GE_6mon_woe1",
  
  "Var_sanctioned_by_outstanding_amount_live_loans_C1",
  "Var_sanctioned_by_outstanding_amount_live_loans_woe1",

  "Var_outstanding_by_sanctioned_amount_live_loans_C1",
  "Var_outstanding_by_sanctioned_amount_live_loans_woe1",

  'Var_outstanding_amount_live_loans_C1',
  'Var_outstanding_amount_live_loans_woe1',
  'Var_outstanding_amount_live_loans_woe2',
  
  
  'Var_EN_enquiry_count_3m_C1',
  'Var_EN_enquiry_count_3m_C2',
  'Var_EN_enquiry_count_3m_C3',
  
  'Var_EN_enquiry_count_3m_non_CC_C1',
  'Var_EN_enquiry_count_3m_non_CC_C2',
  'Var_EN_enquiry_count_3m_non_CC_C3',
  
  'Var_EN_enquiry_count_6m_C1',
  'Var_EN_enquiry_count_6m_C2',
  
  'Var_EN_enquiry_count_6m_non_CC_C1',
  'Var_EN_enquiry_count_6m_non_CC_C2',
  'Var_EN_enquiry_count_6m_non_CC_C3',

  'Var_EN_enquiry_count_6m_non_CC_CD_C1',
  'Var_EN_enquiry_count_6m_non_CC_CD_C2',
  'Var_EN_enquiry_count_6m_non_CC_CD_C3',
  'Var_EN_enquiry_count_6m_non_CC_CD_C4',
  
  "Var_EN_enquiry_count_6m_C1_woe1",
  "Var_DL_30dpd_12mon_C1",
  "Var_DL_30dpd_12mon_flag",
  
  "Var_DL_secured_30dpd_12mon_C1_woe1",

  'Var_DL_all_30dpd_3mon_C1',
  'Var_DL_all_30dpd_3mon_flag',
  # 'Var_DL_all_30dpd_3mon_C1_woe1',

  'Var_DL_all_30dpd_6mon_C1',
  'Var_DL_all_30dpd_6mon_flag',
  # 'Var_DL_all_30dpd_6mon_C1_woe1',

  'Var_DL_non_CC_CD_30dpd_3mon_C1',
  'Var_DL_non_CC_CD_30dpd_3mon_flag',

  'Var_DL_non_CC_CD_30dpd_6mon_C1',
  'Var_DL_non_CC_CD_30dpd_6mon_flag',

  'Var_DL_all_60dpd_3mon_C1',
  'Var_DL_all_60dpd_3mon_flag',

  'Var_DL_all_60dpd_6mon_C1',
  'Var_DL_all_60dpd_6mon_flag',
  # 'Var_DL_all_60dpd_6mon_C1_woe1',

  'Var_DL_non_CC_CD_60dpd_3mon_C1',
  'Var_DL_non_CC_CD_60dpd_3mon_flag',
  'Var_DL_non_CC_CD_60dpd_6mon_C1',
  'Var_DL_non_CC_CD_60dpd_6mon_flag',
  # 'Var_DL_non_CC_CD_60dpd_6mon_C1_woe1',
  "Var_DL_secured_30dpd_1mon",
  "Var_DL_secured_30dpd_3mon",
  "Var_DL_secured_30dpd_6mon",
  "Var_DL_secured_30dpd_12mon",
  "Var_DL_secured_30dpd_18mon",
  "Var_DL_secured_30dpd_24mon",
  "Var_DL_secured_60dpd_1mon",
  "Var_DL_secured_60dpd_3mon",
  "Var_DL_secured_60dpd_6mon",
  "Var_DL_secured_60dpd_12mon",
  "Var_DL_secured_60dpd_18mon",
  "Var_DL_secured_60dpd_24mon",
  "Var_DL_secured_90dpd_1mon",
   "Var_DL_secured_90dpd_3mon",
   "Var_DL_secured_90dpd_6mon",
   "Var_DL_secured_90dpd_12mon",
   "Var_DL_secured_90dpd_18mon",
   "Var_DL_secured_90dpd_24mon",
  "Var_PO_months_1_PL_live",
  "Var_PO_months_3_PL_live",
  "Var_PO_months_6_PL_live",
  "Var_PO_months_12_PL_live",
  "Var_DL_secured_30dpd_12mon_C1_woe1",
  "Var_PO_months_12_PL_live_C1_woe1",
   
   "Var_EN_enquiry_count_6m_flag",
    "Var_DL_secured_30dpd_12mon_C1",
    "Var_DL_secured_30dpd_12mon_flag",
    "Var_PO_months_12_PL_live_C1",
    "Var_PO_months_12_PL_live_C2",
    "Var_PO_months_12_PL_live_flag",
    "Var_sanctioned_amount_live_loans_C",
    # "Var_outstanding_by_sanctioned_amount_live_loans_C",

  "category_flag_SENP_SEP",
  "agri_profile_flag"
  

)

for (variable in selected_features) {
  # if (variable %notin% colnames(dev_data))
  # {print(variable)}
  if (variable %notin% colnames(dev_data)){
  print(variable)}
  

}

# dev_data[is.na(dev_data)] <- 0
# oot_data[is.na(oot_data)] <- 0
# 
# dev_data[sapply(dev_data, Negate(is.finite))] <- 0
# oot_data[sapply(oot_data, Negate(is.finite))] <- 0

## Stepwise model ----------------------------------------------------------------------------------------------------------
set.seed(1)
split <- sample.split(dev_data$bad_loan, SplitRatio = 0.99)
train_data <- data.frame(subset(dev_data, split == TRUE))
test_data <- data.frame(subset(dev_data, split == FALSE))


loop_model <-
  glm(bad_loan ~ .,
      data = train_data %>% dplyr::select(c(selected_features, 'bad_loan')),
      family = binomial("logit"))

qchisq(0.05, 1, lower.tail = F)
# 3.841459

loop_model_selected <- stepAIC(
  loop_model,
  family = binomial,
  data = train_data,
  k = qchisq(0.05, 1, lower.tail = F)
)

# loop_model_tidy <- tidy(loop_model_selected)
summary(loop_model_selected)


train_data_stepwise <-
  get_predictions(loop_model_selected, train_data)
test_data_stepwise <-
  get_predictions(loop_model_selected, test_data)
oot_data_stepwise <- get_predictions(loop_model_selected, oot_data)



model_performance_stepwise <-
  get_model_performance(train_data_stepwise, test_data_stepwise, oot_data_stepwise)
model_performance_stepwise


## get prediction deciles
train_data_stepwise$prediction_decile <-
  ntile(train_data_stepwise$predictions, 10)
test_data_stepwise$prediction_decile <-
  ntile(test_data_stepwise$predictions, 10)
oot_data_stepwise$prediction_decile <-
  ntile(oot_data_stepwise$predictions, 10)


## get rank ordering

RO_train_stepwise <-
  train_data_stepwise %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                    total = length(bad_loan)) %>% mutate(event_rate = event_count /
                                                                                                           total)

RO_test_stepwise <-
  test_data_stepwise %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                   total = length(bad_loan)) %>% mutate(event_rate = event_count /
                                                                                                          total)

RO_oot_stepwise <-
  oot_data_stepwise %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                  total = length(bad_loan)) %>% mutate(event_rate = event_count /
                                                                                                         total)


colnames(RO_train_stepwise) <-
  c('prediction_decile',
    'train_events',
    'train_obs',
    'train_event_rate')
colnames(RO_test_stepwise) <-
  c('prediction_decile',
    'test_events',
    'test_obs',
    'test_event_rate')
colnames(RO_oot_stepwise) <-
  c('prediction_decile',
    'oot_events',
    'oot_obs',
    'oot_event_rate')

final_RO_stepwise <-
  left_join(RO_train_stepwise, RO_test_stepwise, by = 'prediction_decile')
final_RO_stepwise <-
  left_join(final_RO_stepwise, RO_oot_stepwise, by = 'prediction_decile')

rm(RO_train_stepwise, RO_test_stepwise, RO_oot_stepwise)
final_RO_stepwise

### Multiple seed stepwise-----------



selected_features <- c (
  "Var_EN_enquiry_count_6m_C1",
  "Var_EN_enquiry_count_6m_C2",
  "Var_EN_enquiry_count_6m_non_CC_C1",
  "Var_EN_enquiry_count_6m_non_CC_C3",
  "Var_EN_enquiry_count_6m_non_CC_C2",
  "Var_EN_enquiry_count_6m_C1_woe1",
  "Var_EN_enquiry_count_3m_C1",
  "Var_EN_enquiry_count_3m_C3",
  "Var_EN_enquiry_count_3m_C2",
  "Var_EN_enquiry_count_3m_non_CC_C1",
  "Var_EN_enquiry_count_3m_non_CC_C2",
  "Var_EN_enquiry_count_3m_non_CC_C3",
  "Var_credit_vintage_C1",
  "Var_credit_vintage_C3",
  "Var_credit_vintage_C2",
  "Var_sanctioned_by_outstanding_amount_live_loans_C1",
  "Var_credit_vintage_woe1",
  "Var_PO_closed_excl_CC_woe1",
  "Var_PO_closed_sanction_amount_excl_CC_woe2",
  "Var_DL_30dpd_12mon_flag",
  "Var_DL_30dpd_12mon_C1",
  "agri_profile_flag",
  "Var_DL_all_30dpd_6mon_flag",
  "Var_PO_closed_sanction_amount_excl_CC_woe1",
  "Var_outstanding_amount_live_loans_woe1",
  "Var_outstanding_by_sanctioned_amount_live_loans_C1",
  "Var_sanctioned_by_outstanding_amount_live_loans_woe1",
  "category_flag_SENP_SEP",
  "Var_outstanding_by_sanctioned_amount_live_loans_woe1",
  "Var_PO_months_6_PL_live",
  "Var_outstanding_amount_live_loans_woe2"
)

seed_list <- c(1:10)
seed_iterations_df <- list()
parameters_df <- list()
for (seed_temp in seed_list) {
  print(seed_temp)
  print("#############################################################################")
  set.seed(seed_temp)
  split <- sample.split(dev_data$bad_loan, SplitRatio = 0.99)
  train_data <- data.frame(subset(dev_data, split == TRUE))
  test_data <- data.frame(subset(dev_data, split == FALSE))
  
  
  loop_model <-
    glm(bad_loan ~ .,
        data = train_data %>% dplyr::select(c(selected_features, 'bad_loan')),
        family = binomial("logit"))
  
  qchisq(0.05, 1, lower.tail = F)
  # 3.841459
  
  loop_model_selected <- stepAIC(
    loop_model,
    family = binomial,
    data = train_data,
    k = qchisq(0.05, 1, lower.tail = F)
  )
  
  train_data_stepwise <-
    get_predictions(loop_model_selected, train_data)
  test_data_stepwise <-
    get_predictions(loop_model_selected, test_data)
  oot_data_stepwise <-
    get_predictions(loop_model_selected, oot_data)
  
  model_performance_stepwise <-
    get_model_performance(train_data_stepwise, test_data_stepwise, oot_data_stepwise)
  rank_order_check <-
    get_rank_order(train_data_stepwise, test_data_stepwise, oot_data_stepwise)
  model_performance_stepwise <-
    cbind(model_performance_stepwise, rank_order_check)
  
  model_performance_stepwise$seed <- seed_temp
  seed_iterations_df[[seed_temp]] <- model_performance_stepwise
  
  
  parameters <-
    data.frame(coef(summary(loop_model_selected))[, 'Pr(>|z|)'])
  parameters$seed <- seed_temp
  parameters$variables_selected <- rownames(parameters)
  parameters_df[[seed_temp]] <- parameters
  print("*********************************************************************************")
  print("*********************************************************************************")
  
  
}

model_performance <- data.frame(rbindlist(l = seed_iterations_df))
model_parameters <- data.frame(rbindlist(l = parameters_df))

fwrite(
  model_performance,
  "data//output//model//2W_RF//step-wise_model_summary_v3.csv"
)
fwrite(
  model_parameters,
  "data//output//model//2W_RF//step-wise_model_parameters_v3.csv"
)





## single variable model --------------------------------------------------------------------------------------------------
set.seed(100)
split <- sample.split(dev_data$bad_loan, SplitRatio = 0.99)
train_data_temp <- data.frame(subset(dev_data, split == TRUE))
test_data_temp <- data.frame(subset(dev_data, split == FALSE))


## 3.2 single feature model - iteration
single_variable_models <-
  get_single_variable_models(
    train = train_data_temp,
    test = test_data_temp,
    oot = oot_data,
    feature_list = selected_features
  )

rm(train_data_temp, test_data_temp)




## seed wise iterations model ---------------------------------------------------------------------------------------------------------

shortlisted_var <- c(
  # "Var_EN_enquiry_count_6m_C2",
  "Var_EN_enquiry_count_6m_C1_woe1",
  "Var_DL_30dpd_12mon_flag",
  # "Var_EN_enquiry_count_3m_non_CC_C1",
  # "Var_DL_all_30dpd_6mon_flag",
  # "Var_DL_all_60dpd_6mon_flag",
  "agri_profile_flag",
  # "category_flag_SENP_SEP",
  # "Var_total_sanctioned_amount_woe1",
  # "Var_PO_months_3_live_unsec",
  # "Var_DL_secured_30dpd_12mon_flag",
  # "Var_DL_secured_30dpd_3mon",
  # "Var_PO_closed_excl_CC_woe1",
  # "Var_DL_HL_paid_GE_12mon_flag_var",
  # "Var_PO_months_12_PL_live_C2",
   "cross_tab_out_by_sanc_amt_live_x_total_sanc_amt_live",
  # "Var_outstanding_by_sanctioned_amount_live_loans_C1",
  # "Var_PO_closed_sanction_amount_excl_CC_woe1",
  # "Var_PO_closed_sanction_amount_excl_CC_woe2",
  "Var_credit_vintage_C2"
  # "Var_outstanding_amount_live_loans_woe2"
   # "Var_DL_Gold_paid_GE_12mon_flag_var"
  # "Var_sanctioned_by_outstanding_amount_live_loans_woe1"

  
)



seed_iterations_output <- get_seed_iterations_no_test(
  seed_list = c(1:100),
  dev_data = dev_data,
  oot = oot_data,
  feature_list = shortlisted_var,
  p_value_threshold = 0.05)



selected_seeds <-
  seed_iterations_output %>% filter((p_value_check <= 1) &
                                      (RO_decile_overall >= 0) &
                                      (RO_pentile_overall >= 1) 
                                      )
write.csv(
  seed_iterations_output,
  "data//output//model//2W_RF//seed_iterations_model_2_2W_RF.csv"
)

selected_seed <- 98
# selected_seed <- 62

set.seed(selected_seed)


split <- sample.split(dev_data$bad_loan, SplitRatio = 0.999)
train_data_final <- data.frame(subset(dev_data, split == TRUE))
test_data_final <- data.frame(subset(dev_data, split == FALSE))


final_model <-
  glm(bad_loan ~ .,
      data = train_data_final %>% dplyr::select(c(shortlisted_var, 'bad_loan')),
      family = binomial("logit"))

summary(final_model)


train_data_final <- get_predictions(final_model, train_data_final)
test_data_final <- get_predictions(final_model, test_data_final)
oot_data_final <- get_predictions(final_model, oot_data)



model_performance_final <-
  get_model_performance(train_data_final, test_data_final, oot_data_final)
model_performance_final


RO_overall <-
  data.frame(get_RO(train_data_final, test_data_final, oot_data_final))

RO_pentile <-
  data.frame(get_RO_pentile(train_data_final, test_data_final, oot_data_final))


RO_scored_decile <-
  data.frame(get_RO_scored(train_data_final, test_data_final, oot_data_final))

RO_scored_pentile <-
  data.frame(get_RO_scored_pentile(train_data_final, test_data_final, oot_data_final))

# RO_SENP_SEP <-  data.frame(get_segment_wise_rank_ordering(train = train_data_final, test = test_data_final, oot = oot_data_final, segment = c('SENP','SEP')))
# RO_SAL <-  data.frame(get_segment_wise_rank_ordering(train = train_data_final, test = test_data_final, oot = oot_data_final, segment = c('SAL')))

RO_SENP_SEP <-
  data.frame(
    get_RO(
      train_data_final %>% filter(Category %in% c('SENP', 'SEP')),
      test_data_final %>% filter(Category %in% c('SENP', 'SEP')),
      oot_data_final %>% filter(Category %in% c('SENP', 'SEP'))
    )
  )

RO_SAL <-
  data.frame(
    get_RO(
      train_data_final %>% filter(Category %in% c('SAL')),
      test_data_final %>% filter(Category %in% c('SAL')),
      oot_data_final %>% filter(Category %in% c('SAL'))
    )
  )








## save outputs

output_list <- list(
  "model_summary" = data.frame(tidy(final_model)),
  "model_performance" = model_performance_final,
  "RO_overall" = RO_overall,
  "RO_Pentile" = RO_pentile,
  "RO_scored_deciles" = RO_scored_decile,
  "RO_scored_pentile" = RO_scored_pentile,
  "RO_SENP_SEP" = RO_SENP_SEP,
  "RO_SAL" = RO_SAL,
  "Var_Imp" = get_variable_importance(final_model),
  "VIF" = get_vif(final_model),
  "wald_chi_sq" = get_wald_chi_sq(final_model),
  "seed" = data.frame(selected_seed)
)

saveRDS(final_model, file = file.path("output//model_2W_RF_updated.rds"))


write_xlsx(
  output_list,
  "data//output//model//2W_RF//model_output_v5.xlsx"
)

# rm(oot_data_final, train_data_final, test_data_final)



assert_data_non_empty(train_data_final)
save(train_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "2W_RF",
       "train_data.rdata"
     )
)

assert_data_non_empty(test_data_final)
save(test_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "2W_RF",
       "test_data.rdata"
     )
)


assert_data_non_empty(oot_data_final)
save(oot_data_final,
     file = file.path(
       get_data_path()$data$output,
       "model",
       "2W_RF",
       "oot_data.rdata"
     )
)



save(oot_data_final,
     file = file.path(
       "data//output//model//2W_RF//oot_data.rdata"
     )
)








