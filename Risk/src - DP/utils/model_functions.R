#############################################################################################
################## Modeling Functions  ######################################################
#############################################################################################


## 1. Select & rename columns in dataframe --------------------------------------------------
get_cleaned_df <- function(df, feature, column_name){
  output <- df %>% dplyr::select(c(feature))
  colnames(output) <- column_name
  return(output)
}


## 2. WOE Calculation - 2 bins --------------------------------------------------------------
get_2bin_woe <- function(focus_bin, bin1, bin2, dependent_feature){
  
  focus_bin <- get_cleaned_df(focus_bin, dependent_feature, 'default')
  bin1 <- get_cleaned_df(bin1, dependent_feature, 'default')
  bin2 <- get_cleaned_df(bin2, dependent_feature, 'default')
  
  non_event_count <- nrow(focus_bin) - sum(focus_bin$default)
  non_event_total <- (nrow(bin1) - sum(bin1$default)) + (nrow(bin2) - sum(bin2$default))
  non_event_pct <- non_event_count/non_event_total
  
  event_count <- sum(focus_bin$default)
  event_total <- sum(bin1$default) + sum(bin2$default)
  event_pct <- event_count/event_total
  
  return(log(non_event_pct/event_pct))
}

## 3. WOE Calculation - 3 bins --------------------------------------------------------------
get_3bin_woe <- function(focus_bin, bin1, bin2, bin3, dependent_feature){
  
  focus_bin <- get_cleaned_df(focus_bin, dependent_feature, 'default')
  bin1 <- get_cleaned_df(bin1, dependent_feature, 'default')
  bin2 <- get_cleaned_df(bin2, dependent_feature, 'default')
  bin3 <- get_cleaned_df(bin3, dependent_feature, 'default')
  
  non_event_count <- nrow(focus_bin) - sum(focus_bin$default)
  non_event_total <- (nrow(bin1) - sum(bin1$default)) + (nrow(bin2) - sum(bin2$default)) + (nrow(bin3) - sum(bin3$default))
  non_event_pct <- non_event_count/non_event_total
  
  event_count <- sum(focus_bin$default)
  event_total <- sum(bin1$default) + sum(bin2$default) + sum(bin3$default)
  event_pct <- event_count/event_total
  
  return(log(non_event_pct/event_pct))
}

## 4. WOE Calculation - 4 bins --------------------------------------------------------------
get_4bin_woe <- function(focus_bin, bin1, bin2, bin3, bin4, dependent_feature){
  
  focus_bin <- get_cleaned_df(focus_bin, dependent_feature, 'default')
  bin1 <- get_cleaned_df(bin1, dependent_feature, 'default')
  bin2 <- get_cleaned_df(bin2, dependent_feature, 'default')
  bin3 <- get_cleaned_df(bin3, dependent_feature, 'default')
  bin4 <- get_cleaned_df(bin4, dependent_feature, 'default')
  
  non_event_count <- nrow(focus_bin) - sum(focus_bin$default)
  non_event_total <- (nrow(bin1) - sum(bin1$default)) + (nrow(bin2) - sum(bin2$default)) + (nrow(bin3) - sum(bin3$default)) + (nrow(bin4) - sum(bin4$default))
  non_event_pct <- non_event_count/non_event_total
  
  event_count <- sum(focus_bin$default)
  event_total <- sum(bin1$default) + sum(bin2$default) + sum(bin3$default) + sum(bin4$default)
  event_pct <- event_count/event_total
  
  return(log(non_event_pct/event_pct))
}





get_12bin_woe <- function(focus_bin, bin1, bin2, bin3, bin4, bin5, bin6, bin7, bin8,bin9, bin10, bin11, bin12,dependent_feature){
  
  focus_bin <- get_cleaned_df(focus_bin, dependent_feature, 'default')
  bin1 <- get_cleaned_df(bin1, dependent_feature, 'default')
  bin2 <- get_cleaned_df(bin2, dependent_feature, 'default')
  bin3 <- get_cleaned_df(bin3, dependent_feature, 'default')
  bin4 <- get_cleaned_df(bin4, dependent_feature, 'default')
  bin5 <- get_cleaned_df(bin5, dependent_feature, 'default')
  bin6 <- get_cleaned_df(bin6, dependent_feature, 'default')
  bin7 <- get_cleaned_df(bin7, dependent_feature, 'default')
  bin8 <- get_cleaned_df(bin8, dependent_feature, 'default')
  bin9 <- get_cleaned_df(bin9, dependent_feature, 'default')
  bin10 <- get_cleaned_df(bin10, dependent_feature, 'default')
  bin11 <- get_cleaned_df(bin11, dependent_feature, 'default')
  bin12 <- get_cleaned_df(bin12, dependent_feature, 'default')
  
  
  
  non_event_count <- nrow(focus_bin) - sum(focus_bin$default)
  non_event_total <- (nrow(bin1) - sum(bin1$default)) + 
                      (nrow(bin2) - sum(bin2$default)) + 
                      (nrow(bin3) - sum(bin3$default)) + 
                      (nrow(bin4) - sum(bin4$default)) + 
                      (nrow(bin5) - sum(bin5$default)) + 
                      (nrow(bin6) - sum(bin6$default)) + 
                      (nrow(bin7) - sum(bin7$default)) + 
                      (nrow(bin8) - sum(bin8$default)) + 
                      (nrow(bin9) - sum(bin9$default)) + 
                      (nrow(bin10) - sum(bin10$default)) + 
                      (nrow(bin11) - sum(bin11$default)) + 
                      (nrow(bin12) - sum(bin12$default))
  
  non_event_pct <- non_event_count/non_event_total
  
  event_count <- sum(focus_bin$default)
  event_total <- sum(bin1$default) +
                 sum(bin2$default) + 
                 sum(bin3$default) + 
                 sum(bin4$default) + 
                  sum(bin5$default) +
                  sum(bin6$default) + 
                  sum(bin7$default) + 
                  sum(bin8$default) + 
                  sum(bin9$default) +
                  sum(bin10$default) + 
                  sum(bin11$default) + 
                  sum(bin12$default) 
  
  event_pct <- event_count/event_total
  
  return(log(non_event_pct/event_pct))
}



## 5. WOE Calculation - 5 bins --------------------------------------------------------------
get_5bin_woe <- function(focus_bin, bin1, bin2, bin3, bin4, bin5, dependent_feature){
  
  focus_bin <- get_cleaned_df(focus_bin, dependent_feature, 'default')
  bin1 <- get_cleaned_df(bin1, dependent_feature, 'default')
  bin2 <- get_cleaned_df(bin2, dependent_feature, 'default')
  bin3 <- get_cleaned_df(bin3, dependent_feature, 'default')
  bin4 <- get_cleaned_df(bin4, dependent_feature, 'default')
  bin5 <- get_cleaned_df(bin5, dependent_feature, 'default')
  
  non_event_count <- nrow(focus_bin) - sum(focus_bin$default)
  non_event_total <- (nrow(bin1) - sum(bin1$default)) + (nrow(bin2) - sum(bin2$default)) + (nrow(bin3) - sum(bin3$default)) + (nrow(bin4) - sum(bin4$default)) + (nrow(bin5) - sum(bin5$default))
  non_event_pct <- non_event_count/non_event_total
  
  event_count <- sum(focus_bin$default)
  event_total <- sum(bin1$default) + sum(bin2$default) + sum(bin3$default) + sum(bin4$default) + sum(bin5$default)
  event_pct <- event_count/event_total
  
  return(log(non_event_pct/event_pct))
}


get_6bin_woe <- function(focus_bin, bin1, bin2, bin3, bin4, bin5, bin6, dependent_feature){
  
  focus_bin <- get_cleaned_df(focus_bin, dependent_feature, 'default')
  bin1 <- get_cleaned_df(bin1, dependent_feature, 'default')
  bin2 <- get_cleaned_df(bin2, dependent_feature, 'default')
  bin3 <- get_cleaned_df(bin3, dependent_feature, 'default')
  bin4 <- get_cleaned_df(bin4, dependent_feature, 'default')
  bin5 <- get_cleaned_df(bin5, dependent_feature, 'default')
  bin6 <- get_cleaned_df(bin6, dependent_feature, 'default')
  
  
  non_event_count <- nrow(focus_bin) - sum(focus_bin$default)
  non_event_total <- ( nrow(bin1) - sum(bin1$default)) + (nrow(bin2) - sum(bin2$default)) + (nrow(bin3) - sum(bin3$default))
  + (nrow(bin4) - sum(bin4$default)) + (nrow(bin5) - sum(bin5$default)) + (nrow(bin6) - sum(bin6$default))
  non_event_pct <- non_event_count/non_event_total
  
  event_count <- sum(focus_bin$default)
  event_total <- sum(bin1$default) + sum(bin2$default) + sum(bin3$default) + sum(bin4$default) + sum(bin5$default) + sum(bin6$default)
  event_pct <- event_count/event_total
  
  return(log(non_event_pct/event_pct))
}





get_predictions <- function(model_object, data_set) {
  data_set$predictions <-
    predict(model_object, type = "response", newdata = data_set)
  return(data_set)
}



get_KS <- function(input_data, model_prediction_col, dependent_variable_col){

  temp_data <- input_data %>% dplyr::select(c(model_prediction_col,dependent_variable_col))
  colnames(temp_data) <- c('model_prediction','dependent_variable')
  
  temp_data <- temp_data %>% arrange(desc(model_prediction)) %>% mutate(bad_cumsum = cumsum(dependent_variable))
  temp_data <- temp_data %>% mutate(filtered = ifelse(dependent_variable == 1, NA, dependent_variable)) %>% mutate(good_cumsum = row_number(filtered))
  
  if(is.na(temp_data[1,"good_cumsum"])){
    temp_data[1,"good_cumsum"] <- 0
  }
  
  temp_data$good_cumsum <- na.locf(temp_data$good_cumsum)
  temp_data$filtered <- NULL
  
  max_bad <- as.numeric(max(temp_data$bad_cumsum))
  max_good <- as.numeric(max(temp_data$good_cumsum))
  
  temp_data$bad_prob <- temp_data$bad_cumsum / max_bad
  temp_data$good_prob <- temp_data$good_cumsum / max_good
  
  temp_data$ks <- temp_data$bad_prob - temp_data$good_prob
  ks <- max(temp_data$ks)
  
  return(ks)    
}

get_performance_statistics <- function(input_data, model_prediction_col, dependent_variable_col, data_type){
  
  gini_value <- Gini(input_data[[model_prediction_col]], input_data[[dependent_variable_col]])
  ks_value <- get_KS(input_data, model_prediction_col, dependent_variable_col)
  
  temp_data <- input_data %>% dplyr::select(c(model_prediction_col,dependent_variable_col))
  colnames(temp_data) <- c('model_prediction','dependent_variable')
  
  row_count <- nrow(temp_data)
  default_count <- nrow(temp_data %>% filter(dependent_variable == 1))
  default_pct <- mean(temp_data$dependent_variable)
  
  
  output_df <- data.frame(gini = gini_value,
                          ks = ks_value)
  
  colnames(output_df)[colnames(output_df) == 'gini'] <- paste0("gini_",data_type)
  colnames(output_df)[colnames(output_df) == 'ks'] <- paste0("ks_",data_type)
  
  return(output_df)
  
}



get_model_performance <- function(train,test,oot){
  
  performance_train <- get_performance_statistics(input_data = train,
                                                  model_prediction_col = 'predictions',
                                                  dependent_variable_col = 'bad_loan',
                                                  data_type = 'train')
  
  
  performance_test <- get_performance_statistics(input_data = test,
                                                 model_prediction_col = 'predictions',
                                                 dependent_variable_col = 'bad_loan',
                                                 data_type = 'test')
  
  performance_oot <- get_performance_statistics(input_data = oot,
                                                model_prediction_col = 'predictions',
                                                dependent_variable_col = 'bad_loan',
                                                data_type = 'oot')
  
  performance_df <- cbind(performance_train,performance_test)
  performance_df <- cbind(performance_df,performance_oot)
  
  performance_df <- performance_df %>% dplyr::select(gini_train,
                                                     gini_test,
                                                     gini_oot,
                                                     ks_train,
                                                     ks_test,
                                                     ks_oot)
  
  return(performance_df)
  
}



get_model_performance_no_test <- function(train_data_subset,oot_data_subset){
  
  performance_train <- get_performance_statistics(input_data = train_data_subset,
                                                  model_prediction_col = 'predictions',
                                                  dependent_variable_col = 'bad_loan',
                                                  data_type = 'train')
  
  performance_oot <- get_performance_statistics(input_data = oot_data_subset,
                                                model_prediction_col = 'predictions',
                                                dependent_variable_col = 'bad_loan',
                                                data_type = 'oot')
  
  performance_df <- cbind(performance_train,performance_oot)
  
  performance_df <- performance_df %>% dplyr::select(gini_train,
                                                     gini_oot,
                                                     ks_train,
                                                     ks_oot)
  
  return(performance_df)
  
}








get_single_variable_models <- function(train, test, oot, feature_list){
  
  single_variable_models <- list()
  i <- 0
  
  for(feature in feature_list){
    
    model_temp <- glm(bad_loan ~., data = train %>% dplyr::select(c(feature,'bad_loan')), family = binomial("logit"))
    
    train_temp <- get_predictions(model_temp, train)
    test_temp <- get_predictions(model_temp, test)
    oot_temp <- get_predictions(model_temp, oot)
    
    performance_train <- get_performance_statistics(input_data = train_temp,
                                                    model_prediction_col = 'predictions',
                                                    dependent_variable_col = 'bad_loan',
                                                    data_type = 'train')
    
    
    performance_test <- get_performance_statistics(input_data = test_temp,
                                                   model_prediction_col = 'predictions',
                                                   dependent_variable_col = 'bad_loan',
                                                   data_type = 'test')
    
    performance_oot <- get_performance_statistics(input_data = oot_temp,
                                                  model_prediction_col = 'predictions',
                                                  dependent_variable_col = 'bad_loan',
                                                  data_type = 'oot')
    
    performance_df <- cbind(performance_train,performance_test)
    performance_df <- cbind(performance_df,performance_oot)
    rm(performance_oot,performance_test,performance_train)
    
    performance_df$variable <- feature
    
    rank_order_check <- get_rank_order(train_temp,test_temp,oot_temp)

    performance_df <- cbind(performance_df,rank_order_check)
    
    performance_df <- performance_df %>% dplyr::select(variable,
                                                       gini_train,
                                                       gini_test,
                                                       gini_oot,
                                                       RO_decile_overall,
                                                       RO_pentile_overall,
                                                       RO_decile_train,
                                                       RO_decile_test,
                                                       RO_decile_oot,
                                                       RO_pentile_train,
                                                       RO_pentile_test,
                                                       RO_pentile_oot,
                                                       ks_train,
                                                       ks_test,
                                                       ks_oot)
    
    i <- i+1
    single_variable_models[[i]] <- performance_df
  }
  
  single_variable_models <- rbindlist(l = single_variable_models)
  
  return(single_variable_models)
}






get_model_performance_quarter <- function(train,test){
  
  performance_train <- get_performance_statistics(input_data = train,
                                                  model_prediction_col = 'predictions',
                                                  dependent_variable_col = 'bad_loan',
                                                  data_type = 'train')
  
  
  performance_test <- get_performance_statistics(input_data = test,
                                                 model_prediction_col = 'predictions',
                                                 dependent_variable_col = 'bad_loan',
                                                 data_type = 'test')
  
  
  performance_df <- cbind(performance_train,performance_test)
  
  performance_df <- performance_df %>% dplyr::select(gini_train,
                                                     gini_test,
                                                     ks_train,
                                                     ks_test)
  
  return(performance_df)
  
}

get_rank_order <- function(train, test, oot){
  
  final_RO <- get_RO(train, test, oot)
  
  pentile_RO <- final_RO
  pentile_RO$pentile <- ifelse(final_RO$prediction_decile <= 2, 1, 
                               ifelse(final_RO$prediction_decile <= 4, 2,
                                      ifelse(final_RO$prediction_decile <= 6, 3,
                                             ifelse(final_RO$prediction_decile <= 8, 4, 5))))
  
  
  pentile_RO <- pentile_RO %>% group_by(pentile) %>% summarise(train_events = sum(train_events),
                                                               train_obs = sum(train_obs),
                                                               test_events = sum(test_events),
                                                               test_obs = sum(test_obs),
                                                               oot_events = sum(oot_events),
                                                               oot_obs = sum(oot_obs)
  )
  
  
  pentile_RO$train_event_rate <- pentile_RO$train_events / pentile_RO$train_obs
  pentile_RO$test_event_rate <- pentile_RO$test_events / pentile_RO$test_obs
  pentile_RO$oot_event_rate <- pentile_RO$oot_events / pentile_RO$oot_obs
  
  
  output_df <- data.frame(RO_decile_train = allIncreasing(final_RO$train_event_rate),
                          RO_decile_test = allIncreasing(final_RO$test_event_rate),
                          RO_decile_oot = allIncreasing(final_RO$oot_event_rate),
                          RO_pentile_train = allIncreasing(pentile_RO$train_event_rate),
                          RO_pentile_test = allIncreasing(pentile_RO$test_event_rate),
                          RO_pentile_oot = allIncreasing(pentile_RO$oot_event_rate)
  )
  
  output_df$RO_decile_overall <- output_df$RO_decile_train + output_df$RO_decile_test + output_df$RO_decile_oot
  output_df$RO_pentile_overall <- output_df$RO_pentile_train + output_df$RO_pentile_test + output_df$RO_pentile_oot
  
  return(output_df)
  
  
}


allIncreasing <- function(x){
  check <- all(diff(x)>0)
  return(check)
} 



get_p_value_flag <- function(model_object, p_value_threshold) {
  list_p <- c(coef(summary(model_object))[, 'Pr(>|z|)'])
  p_flag <- sum(list_p > p_value_threshold)
  return(p_flag)
}


get_seed_iterations <- function(seed_list, dev_data, oot, feature_list, p_value_threshold){
  
  seed_iterations_df <- list()
  
  for(seed_temp in seed_list){
    set.seed(seed_temp)
    
    split <- sample.split(dev_data$bad_loan, SplitRatio = 0.80)
    train_data_seed <- data.frame(subset(dev_data, split == TRUE))
    test_data_seed <- data.frame(subset(dev_data, split == FALSE))
    
    seed_model <- glm(bad_loan ~., data = train_data_seed %>% dplyr::select(c(feature_list,'bad_loan')), family = binomial("logit"))
    
    train_data_seed <- get_predictions(seed_model, train_data_seed)
    test_data_seed <- get_predictions(seed_model, test_data_seed)
    oot_data_seed <- get_predictions(seed_model, oot)
    
    ## get performance
    model_performance <- get_model_performance(train_data_seed,test_data_seed,oot_data_seed)
    model_performance$seed <- seed_temp
    
    ## get rank order
    rank_order_check <- get_rank_order(train_data_seed,test_data_seed,oot_data_seed)
    model_performance <- cbind(model_performance, rank_order_check)
    
    ## check p value
    model_performance$p_value_check <- get_p_value_flag(model_object = seed_model, p_value_threshold = p_value_threshold)
    
    model_performance <- model_performance %>% dplyr::select(seed,
                                                             p_value_check,
                                                             RO_decile_overall,
                                                             RO_pentile_overall,
                                                             gini_train,
                                                             gini_test,
                                                             gini_oot,
                                                             RO_decile_train,
                                                             RO_decile_test,
                                                             RO_decile_oot,
                                                             RO_pentile_train,
                                                             RO_pentile_test,
                                                             RO_pentile_oot,
                                                             ks_train,
                                                             ks_test,
                                                             ks_oot)
    list_p <- c(coef(summary(seed_model))[, 'Pr(>|z|)'])
    max_p_vlaue <- max(list_p)
    model_performance$max_p_value <- max_p_vlaue
    
    seed_iterations_df[[seed_temp]] <- model_performance
    print(seed_temp)
    
  }
  
  seed_iterations_df <- rbindlist(l = seed_iterations_df)
  return(seed_iterations_df)
}


get_RO <- function(train,test,oot){
  train$prediction_decile <- ntile(train$predictions, 10)
  test$prediction_decile <- ntile(test$predictions, 10)
  oot$prediction_decile <- ntile(oot$predictions, 10)
  
  
  ## get rank ordering
  RO_train <- train %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                  total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  RO_test <- test %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  RO_oot <- oot %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                              total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  
  colnames(RO_train) <- c('prediction_decile','train_events', 'train_obs','train_event_rate')
  colnames(RO_test) <- c('prediction_decile','test_events', 'test_obs','test_event_rate')
  colnames(RO_oot) <- c('prediction_decile','oot_events', 'oot_obs','oot_event_rate')
  
  final_RO <- left_join(RO_train, RO_test, by = 'prediction_decile')
  final_RO <- left_join(final_RO, RO_oot, by = 'prediction_decile')
  
  return(final_RO)
  
}


get_RO_pentile <- function(train,test,oot){
  
  train$prediction_decile <- ntile(train$predictions, 5)
  test$prediction_decile <- ntile(test$predictions, 5)
  oot$prediction_decile <- ntile(oot$predictions, 5)
  
  
  ## get rank ordering
  RO_train <- train %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                  total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  RO_test <- test %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  RO_oot <- oot %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                              total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  
  colnames(RO_train) <- c('prediction_pentile','train_events', 'train_obs','train_event_rate')
  colnames(RO_test) <- c('prediction_pentile','test_events', 'test_obs','test_event_rate')
  colnames(RO_oot) <- c('prediction_pentile','oot_events', 'oot_obs','oot_event_rate')
  
  final_RO <- left_join(RO_train, RO_test, by = 'prediction_pentile')
  final_RO <- left_join(final_RO, RO_oot, by = 'prediction_pentile')
  
  
  return(final_RO)
}



get_segment_wise_rank_ordering <- function(train,test,oot, segment){
  
  train_segment <- train %>% filter(Category %in% segment)
  test_segment <- test %>% filter(Category %in% segment)
  oot_segment <- oot %>% filter(Category %in% segment)
  
  final_RO <- get_RO(train_segment,test_segment,oot_segment)
  return(final_RO)
}




get_variable_importance <- function(model_object) {
  variable_importance <- varImp(model_object)
  variable_importance$pct_value <-  variable_importance$Overall / sum(variable_importance$Overall)
  variable_importance$feature <- row.names(variable_importance)
  rownames(variable_importance) <- NULL
  variable_importance <- variable_importance %>% dplyr::select(feature,Overall,pct_value) %>% arrange(desc(pct_value))
  return(variable_importance)
}

get_wald_chi_sq <- function(model_object) {
  wald_chi_square_value <-
    data.frame(Anova(model_object, type = "II", test = "Wald"))
  wald_chi_square_value$feature <- row.names(wald_chi_square_value)
  rownames(wald_chi_square_value) <- NULL
  wald_chi_square_value <- wald_chi_square_value %>% dplyr::select(feature,Df,Chisq,Pr..Chisq.)
  return(wald_chi_square_value)
}


get_vif <- function(model_object) {
  vif_value <- data.frame(vif(model_object))
  vif_value$feature <- row.names(vif_value)
  rownames(vif_value) <- NULL
  colnames(vif_value) <- c('VIF','feature')
  vif_value <- vif_value %>% dplyr::select(feature,VIF)
  return(vif_value)
}

get_cut_off_min <- function(cut_offs, decile){
  min <- (cut_offs %>% filter(prediction_decile == decile))$min
  return(min)
}

get_cut_off_max <- function(cut_offs, decile){
  max <- (cut_offs %>% filter(prediction_decile == decile))$max
  return(max)
}

get_cut_off_min_pentile <- function(cut_offs, pentile){
  min <- (cut_offs %>% filter(prediction_pentile == pentile))$min
  return(min)
}

get_cut_off_max_pentile <- function(cut_offs, pentile){
  max <- (cut_offs %>% filter(prediction_pentile == pentile))$max
  return(max)
}

get_RO_scored <- function(train,test,oot){
  
  train$prediction_decile <- ntile(train$predictions, 10)
  
  cut_offs <- train %>% group_by(prediction_decile) %>% summarise(min = min(predictions), max = max(predictions))
  
  
  
  test$prediction_decile <- ifelse(test$predictions <= get_cut_off_max(cut_offs, 1), 1,
                                   ifelse(test$predictions >= get_cut_off_min(cut_offs,2) & test$predictions <= get_cut_off_max(cut_offs,2), 2,
                                          ifelse(test$predictions >= get_cut_off_min(cut_offs,3) & test$predictions <= get_cut_off_max(cut_offs,3), 3,
                                                 ifelse(test$predictions >= get_cut_off_min(cut_offs,4) & test$predictions <= get_cut_off_max(cut_offs,4), 4,
                                                        ifelse(test$predictions >= get_cut_off_min(cut_offs,5) & test$predictions <= get_cut_off_max(cut_offs,5), 5,
                                                               ifelse(test$predictions >= get_cut_off_min(cut_offs,6) & test$predictions <= get_cut_off_max(cut_offs,6), 6,
                                                                      ifelse(test$predictions >= get_cut_off_min(cut_offs,7) & test$predictions <= get_cut_off_max(cut_offs,7), 7,
                                                                             ifelse(test$predictions >= get_cut_off_min(cut_offs,8) & test$predictions <= get_cut_off_max(cut_offs,8), 8,
                                                                                    ifelse(test$predictions >= get_cut_off_min(cut_offs,9) & test$predictions <= get_cut_off_max(cut_offs,9), 9,10)))))))))
  
  
  
  oot$prediction_decile <- ifelse(oot$predictions <= get_cut_off_max(cut_offs, 1), 1,
                                  ifelse(oot$predictions >= get_cut_off_min(cut_offs,2) & oot$predictions <= get_cut_off_max(cut_offs,2), 2,
                                         ifelse(oot$predictions >= get_cut_off_min(cut_offs,3) & oot$predictions <= get_cut_off_max(cut_offs,3), 3,
                                                ifelse(oot$predictions >= get_cut_off_min(cut_offs,4) & oot$predictions <= get_cut_off_max(cut_offs,4), 4,
                                                       ifelse(oot$predictions >= get_cut_off_min(cut_offs,5) & oot$predictions <= get_cut_off_max(cut_offs,5), 5,
                                                              ifelse(oot$predictions >= get_cut_off_min(cut_offs,6) & oot$predictions <= get_cut_off_max(cut_offs,6), 6,
                                                                     ifelse(oot$predictions >= get_cut_off_min(cut_offs,7) & oot$predictions <= get_cut_off_max(cut_offs,7), 7,
                                                                            ifelse(oot$predictions >= get_cut_off_min(cut_offs,8) & oot$predictions <= get_cut_off_max(cut_offs,8), 8,
                                                                                   ifelse(oot$predictions >= get_cut_off_min(cut_offs,9) & oot$predictions <= get_cut_off_max(cut_offs,9), 9,10)))))))))
  
  
  ## get rank ordering
  RO_train <- train %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                  total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  RO_test <- test %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  RO_oot <- oot %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                              total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  
  colnames(RO_train) <- c('prediction_decile','train_events', 'train_obs','train_event_rate')
  colnames(RO_test) <- c('prediction_decile','test_events', 'test_obs','test_event_rate')
  colnames(RO_oot) <- c('prediction_decile','oot_events', 'oot_obs','oot_event_rate')
  
  final_RO <- left_join(RO_train, RO_test, by = 'prediction_decile')
  final_RO <- left_join(final_RO, RO_oot, by = 'prediction_decile')
  
  return(final_RO)
  
}

get_RO_scored_pentile <- function(train,test,oot){
  
  train$prediction_pentile <- ntile(train$predictions, 5)
  
  cut_offs <- train %>% group_by(prediction_pentile) %>% summarise(min = min(predictions), max = max(predictions))
  
  
  
  test$prediction_pentile <- ifelse(test$predictions <= get_cut_off_max_pentile(cut_offs, 1), 1,
                                   ifelse(test$predictions >= get_cut_off_min_pentile(cut_offs,2) & test$predictions <= get_cut_off_max_pentile(cut_offs,2), 2,
                                          ifelse(test$predictions >= get_cut_off_min_pentile(cut_offs,3) & test$predictions <= get_cut_off_max_pentile(cut_offs,3), 3,
                                                 ifelse(test$predictions >= get_cut_off_min_pentile(cut_offs,4) & test$predictions <= get_cut_off_max_pentile(cut_offs,4), 4,5))))
                                                               
  
  oot$prediction_pentile <- ifelse(oot$predictions <= get_cut_off_max_pentile(cut_offs, 1), 1,
                                  ifelse(oot$predictions >= get_cut_off_min_pentile(cut_offs,2) & oot$predictions <= get_cut_off_max_pentile(cut_offs,2), 2,
                                         ifelse(oot$predictions >= get_cut_off_min_pentile(cut_offs,3) & oot$predictions <= get_cut_off_max_pentile(cut_offs,3), 3,
                                                ifelse(oot$predictions >= get_cut_off_min_pentile(cut_offs,4) & oot$predictions <= get_cut_off_max_pentile(cut_offs,4), 4,5))))
                                                      
  
  ## get rank ordering
  RO_train <- train %>% group_by(prediction_pentile) %>% summarise(event_count = sum(bad_loan),
                                                                  total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  RO_test <- test %>% group_by(prediction_pentile) %>% summarise(event_count = sum(bad_loan),
                                                                total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  RO_oot <- oot %>% group_by(prediction_pentile) %>% summarise(event_count = sum(bad_loan),
                                                              total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  
  colnames(RO_train) <- c('prediction_pentile','train_events', 'train_obs','train_event_rate')
  colnames(RO_test) <- c('prediction_pentile','test_events', 'test_obs','test_event_rate')
  colnames(RO_oot) <- c('prediction_pentile','oot_events', 'oot_obs','oot_event_rate')
  
  final_RO <- left_join(RO_train, RO_test, by = 'prediction_pentile')
  final_RO <- left_join(final_RO, RO_oot, by = 'prediction_pentile')
  
  return(final_RO)
  
}


get_segment_wise_rank_ordering_scored <- function(train,test,oot, segment){
  
  train_segment <- train %>% filter(Category %in% segment)
  test_segment <- test %>% filter(Category %in% segment)
  oot_segment <- oot %>% filter(Category %in% segment)
  
  final_RO <- get_RO_scored(train_segment,test_segment,oot_segment)
  return(final_RO)
}


get_RO_validation_decile <- function(train){
  train$prediction_decile <- ntile(train$predictions, 10)
  
  ## get rank ordering
  RO_train <- train %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                  total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  colnames(RO_train) <- c('prediction_decile','train_events', 'train_obs','train_event_rate')
  
  return(RO_train)
  
}


get_RO_validation_pentile <- function(train){
  train$prediction_decile <- ntile(train$predictions, 5)
  
  ## get rank ordering
  RO_train <- train %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                  total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  colnames(RO_train) <- c('prediction_decile','train_events', 'train_obs','train_event_rate')
  
  return(RO_train)
  
}




###############################################################################################
get_RO_3bin <- function(train,test,oot){
  train$prediction_decile <- ntile(train$predictions, 3)
  test$prediction_decile <- ntile(test$predictions, 3)
  oot$prediction_decile <- ntile(oot$predictions, 3)
  
  
  ## get rank ordering
  RO_train <- train %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                  total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  RO_test <- test %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  RO_oot <- oot %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                              total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  
  colnames(RO_train) <- c('prediction_decile','train_events', 'train_obs','train_event_rate')
  colnames(RO_test) <- c('prediction_decile','test_events', 'test_obs','test_event_rate')
  colnames(RO_oot) <- c('prediction_decile','oot_events', 'oot_obs','oot_event_rate')
  
  final_RO <- left_join(RO_train, RO_test, by = 'prediction_decile')
  final_RO <- left_join(final_RO, RO_oot, by = 'prediction_decile')
  
  colnames(final_RO)[colnames(final_RO) == 'prediction_decile'] <- 'prediction_bin'
  return(final_RO)
  
}


get_RO_scored_3bin <- function(train,test,oot){
  
  train$prediction_pentile <- ntile(train$predictions, 3)
  
  cut_offs <- train %>% group_by(prediction_pentile) %>% summarise(min = min(predictions), max = max(predictions))
  
  
  
  test$prediction_pentile <- ifelse(test$predictions <= get_cut_off_max_pentile(cut_offs, 1), 1,
                                    ifelse(test$predictions >= get_cut_off_min_pentile(cut_offs,2) & test$predictions <= get_cut_off_max_pentile(cut_offs,2), 2,3))
  
  
  oot$prediction_pentile <- ifelse(oot$predictions <= get_cut_off_max_pentile(cut_offs, 1), 1,
                                   ifelse(oot$predictions >= get_cut_off_min_pentile(cut_offs,2) & oot$predictions <= get_cut_off_max_pentile(cut_offs,2), 2, 3))
  
  
  ## get rank ordering
  RO_train <- train %>% group_by(prediction_pentile) %>% summarise(event_count = sum(bad_loan),
                                                                   total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  RO_test <- test %>% group_by(prediction_pentile) %>% summarise(event_count = sum(bad_loan),
                                                                 total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  RO_oot <- oot %>% group_by(prediction_pentile) %>% summarise(event_count = sum(bad_loan),
                                                               total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  
  colnames(RO_train) <- c('prediction_pentile','train_events', 'train_obs','train_event_rate')
  colnames(RO_test) <- c('prediction_pentile','test_events', 'test_obs','test_event_rate')
  colnames(RO_oot) <- c('prediction_pentile','oot_events', 'oot_obs','oot_event_rate')
  
  final_RO <- left_join(RO_train, RO_test, by = 'prediction_pentile')
  final_RO <- left_join(final_RO, RO_oot, by = 'prediction_pentile')
  
  colnames(final_RO)[colnames(final_RO) == 'prediction_pentile'] <- 'prediction_bin'
  
  return(final_RO)
  
}


get_RO_validation_3bin <- function(train){
  train$prediction_decile <- ntile(train$predictions, 3)
  
  ## get rank ordering
  RO_train <- train %>% group_by(prediction_decile) %>% summarise(event_count = sum(bad_loan),
                                                                  total = length(bad_loan)) %>% mutate(event_rate = event_count/total)
  
  colnames(RO_train) <- c('prediction_decile','train_events', 'train_obs','train_event_rate')
  
  colnames(final_RO)[colnames(final_RO) == 'prediction_pentile'] <- 'prediction_bin'
  
  return(RO_train)
  
}


get_seed_iterations_no_test <- function(seed_list, dev_data, oot_data, feature_list, p_value_threshold){
  
  seed_iterations_df <- list()
  
  for(seed_temp in seed_list){
    set.seed(seed_temp)
    
    split <- sample.split(dev_data$bad_loan, SplitRatio = 0.999)
    train_data_seed <- data.frame(subset(dev_data, split == TRUE))
    test_data_seed <- data.frame(subset(dev_data, split == FALSE))
    oot_data_seed <- oot_data %>% dplyr::select(c(feature_list,'bad_loan'))
    
    seed_model <- glm(bad_loan ~., data = train_data_seed %>% dplyr::select(c(feature_list,'bad_loan')), family = binomial("logit"))
    
    train_data_seed <- get_predictions(seed_model, train_data_seed %>% dplyr::select(c(feature_list,'bad_loan')))
    test_data_seed <- get_predictions(seed_model, test_data_seed %>% dplyr::select(c(feature_list,'bad_loan')))
    oot_data_seed <- get_predictions(seed_model, oot_data_seed %>% dplyr::select(c(feature_list,'bad_loan')))
    
    ## get performance
    model_performance <- get_model_performance_no_test(train_data_seed,oot_data_seed)
    model_performance$seed <- seed_temp
    
    ## get rank order
    rank_order_check <- get_rank_order(train_data_seed,test_data_seed,oot_data_seed)
    model_performance <- cbind(model_performance, rank_order_check)
    
    ## check p value
    model_performance$p_value_check <- get_p_value_flag(model_object = seed_model, p_value_threshold = p_value_threshold)
    
    list_p <- c(coef(summary(seed_model))[, 'Pr(>|z|)'])
    max_p_vlaue <- max(list_p)
    
    model_performance <- model_performance %>% dplyr::select(seed,
                                                             p_value_check,
                                                             RO_decile_overall,
                                                             RO_pentile_overall,
                                                             gini_train,
                                                             # gini_test,
                                                             gini_oot,
                                                             RO_decile_train,
                                                             RO_decile_test,
                                                             RO_decile_oot,
                                                             RO_pentile_train,
                                                             RO_pentile_test,
                                                             RO_pentile_oot,
                                                             ks_train,
                                                             # ks_test,
                                                             ks_oot)
    model_performance$max_p_value <- max_p_vlaue
    
    seed_iterations_df[[seed_temp]] <- model_performance
    print(seed_temp)
    
  }
  
  seed_iterations_df <- rbindlist(l = seed_iterations_df)
  return(seed_iterations_df)
}
