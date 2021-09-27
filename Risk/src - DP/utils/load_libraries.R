# ################################################################################################
# ##################### Load Libraries ###########################################################
# ################################################################################################
# 
# package_list <- 
#   c(
#     'vctrs',
#     'magrittr',
#     'lifecycle',
#     'plyr',
#     'generics',
#     'R6',
#     'glue',
#     'reshape2',
#     'data.table',
#     'stringi',
#     'stringr',
#     'zoo',
#     'reshape2',
#     'lubridate',
#     
#     'broom',
#     'ggplot2',
#     'InformationValue',
#     'MASS',
#     'dummies',
#     'woeBinning',
#     'rpart',
#     'caTools',
#     'ModelMetrics',
#     'caret',
#     'SciViews',
#     'fmsb',
#     'MLmetrics',
#     'survey',
#     'aod',
#     'mitools',
#     'randomForest',
#     'scorecard',
#     'carData',
#     'car',
#     'yaml',
#     'validate',
#     'matrixStats',
#     'MazamaCoreUtils',
#     'rlang',
#     'Information',
#     'tidyr',
#     'writexl',
#     'rattle',
#     'rpart.plot',
#     'RColorBrewer',
#    # 'CHAID',
#     # 'purrr',
#     # 'C50',
#     # 'ROSE',
#    'dplyr'
#     # 'partykit'
#   )
# 
# 
# load_libraries <- function(package_list){
#   for (package in package_list){
#     tryCatch(
#       {
#         library(package, character.only = TRUE, lib.loc = "./libraries/")
#         print(paste0(package, " - loaded"))
#       }, error = function(e){
#         # install.packages("ROSE", lib = "./libraries/")
#         install.packages(package, lib = "./libraries/")
#         
#         print(paste0(package, " - installed"))
#       }
#     )
#     # library(package, character.only = TRUE, lib.loc = "./libraries/")
#   }
#     
# }
# 
# load_libraries(package_list)



package_list <- 
  c(
    'crayon',
    'vctrs',
    'data.table',
    'stringi',
    'stringr',
    'zoo',
    'reshape2',
    'lubridate',
    'withr',
    'backports',
    'broom',
    # 'ggplot2',
    'InformationValue',
    'MASS',
    'dummies',
    'woeBinning',
    'rpart',
    'caTools',
    'ModelMetrics',
    'ggplot2',
    'caret',
    'foreach',
    'SciViews',
    'fmsb',
    'MLmetrics',
    'survey',
    'aod',
    'mitools',
    'randomForest',
    'scorecard',
    'rio',
    'abind',
    'Formula',
    'carData',
    'car',
    'yaml',
    # 'validate',
    'matrixStats',
    # 'MazamaCoreUtils',
    # 'rlang',
    'Information',
    'tidyr',
    'writexl',
    # 'rattle',
    'rpart.plot',
    'RColorBrewer',
    'mvtnorm',
    # 'CHAID',
    # 'purrr',
    # 'C50',
    # 'ROSE',
    'bit',
    'bit64',
    'Rcpp',
    'dplyr'
  )


# install_libraries <- function(package_list){
#   for (package in package_list){
#     install.packages(package, lib = "./libraries/")
#       }
# }
# 
# 
# install_libraries(package_list)

load_libraries <- function(package_list){
  for(package in package_list){
    library(package, character.only = TRUE, lib.loc = "./libraries/")
    print(paste0(package, " - loaded"))
  }
}


load_libraries(package_list = package_list)


