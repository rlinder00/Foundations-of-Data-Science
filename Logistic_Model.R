#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("smbinning", repos = "https://cran.r-project.org")
library(smbinning)
#install.packages("caTools")
library(caTools)
#install.packages("ROCR")
library(ROCR)
#install.packages("scales")
library(scales)
#install.packages("Information")
library(Information)
#install.packages("gridExtra")
#library(gridExtra)
#install.packages("ClustOfVar")
#library(ClustOfVar)
#install.packages("reshape2")
library(reshape2)
#install.packages("plyr")
library(plyr)
#install.packages("devtools")
#library(devtools)
#install.packages("woe")
#library(woe)
#install.packages("InformationValue")  # For stable CRAN version
library(InformationValue)
#devtools::install_github("InformationValue")  # For latest dev version.
#install.packages("car")
library(car)
#install.packages("caret")
#install.packages("varhandle")
library(varhandle)
#library(caret) # 3 main functions - pre-process, train, predict

###################################################################

###### PREPARE DATA

###################################################################

setwd("/users/teams/nad/ryhl/Capstone/")
xtract_data("select * from [MSS_U_NADBI_S].[dbo].[RYHL_CAPSTONE_DATA]", "lasr", "/users/teams/nad/ryhl/Capstone/Content_Rec_Data_Raw.xdf")
xtract_data("select * from [MSS_U_NADBI_S].[dbo].[RYHL_CAPSTONE_DATA_CLEAN]", "lasr", "/users/teams/nad/ryhl/Capstone/Content_Rec_Data.xdf")
#rxReadXdf is the main function to use. rxDataStep is another possibility which has a few more options for transforming the data as 
#it's reading it before loading everything into memory
data <- rxReadXdf("/users/teams/nad/ryhl/Capstone/Content_Rec_Data.xdf", stringsAsFactors = T) 
data$AGE_BEEN_REP <- as.numeric(data$AGE_BEEN_REP)
data$YRS_BUS_W_CG <- as.numeric(data$YRS_BUS_W_CG)
data$FINCL_ITRMY_PRSON_UID <- as.factor(data$FINCL_ITRMY_PRSON_UID)
data$DIGITAL_ENGAGEMENT_FLAG <- as.numeric(as.character(data$DIGITAL_ENGAGEMENT_FLAG))
data$TXN_CNT <- as.numeric(data$TXN_CNT)
data <- subset(data, !is.na(data$RTL_SLS_TIER_DESC))
data <- subset(data, !is.na(data$ASSET_CLASS_2))

# COVERAGE: 164,490 advisors

data_new <- data[,-c(2:3,5:15)]
logistic_model(data_new, "INVESTMENTS_FLAG", 10, 1, 0.02, 0.7, 0.001, 5, 0.7)

data_new <- data[,-c(2,4:15)]
logistic_model(data_new, "ACTIVE_ADVANTAGE_FLAG", 10, 1, 0.02, 0.7, 0.001, 5, 0.7)

data_clean <- data[,-c(3:15)]       
logistic_model(data_clean, "DIGITAL_ENGAGEMENT_FLAG", 10, 1, 0.02 , 0.7, 0.001, 4, 0.65)

#######
#seed <- 1
#bin_cnt <- 10
#binary_flag <- "DIGITAL_ENGAGEMENT_FLAG"
#split_ratio <- .7
#p_value_max <- .001
#vif_value_max <- 5
#response_min <- .5
#IV_min <- 0.02
####### 

logistic_model <- function(data, binary_flag, bin_cnt, seed, IV_min, split_ratio, p_value_max, vif_value_max, response_min){
  
  set.seed(seed)
  data_clean <- data
  
  #create new directory for files
  
  if(binary_flag %in% dir("/users/teams/nad/ryhl/Capstone/") == FALSE) {
        dir.create(paste0("/users/teams/nad/ryhl/Capstone/", binary_flag)) 
  } 

  ###################################################################
  
  ###### DETERMINE IVs FOR ALL VARS AND REPLACE CATEGORICAL WITH WoEs
  
  ###################################################################
  
  #builds a table of IVs for all vars -- you can customize bin amt
  IV <- create_infotables(data=data_clean[,-1], y=binary_flag, bins = bin_cnt) 
  write.csv(IV$Summary, paste0("/users/teams/nad/ryhl/Capstone/", binary_flag, "/", "IV_summary.csv")) 
  
  
  for(var in colnames(data_clean[,-1])){
    if(var %in% split(names(data_clean),sapply(data_clean, function(x) paste(class(x), collapse=" ")))$numeric[-1]){
    write.csv(smbinning(data_clean, binary_flag, var, p = 0.1)$ivtable, paste0("/users/teams/nad/ryhl/Capstone/", binary_flag, "/", var, "_WoE.csv")) 
  }
      
   if(var %in% split(names(data_clean),sapply(data_clean, function(x) paste(class(x), collapse=" ")))$factor[-1]){
      write.csv(smbinning.factor(data_clean, binary_flag, var)$ivtable, paste0("/users/teams/nad/ryhl/Capstone/", binary_flag, "/", var, "_WoE.csv")) 
    }
  }
  
  #filters down to only vars with IV higher than 0.02
  model_ind_vars <- IV$Summary$Variable[IV$Summary$IV > IV_min]
  data_clean <- data_clean[,names(data_clean) %in% c("FINCL_ITRMY_PRSON_UID", binary_flag, model_ind_vars)]
  
  #replace all vars with WoEs 

  data_clean <- replace_with_WOE_smb(data_clean, binary_flag) #numeric indep vars 
  data_clean <- replace_with_WOE_smb_cat(data_clean, binary_flag) #categorical indep vars 
  
  ###################################################################
  
  ###### CREATE TRAINING and TESTING SUBSETS
  
  ###################################################################
  
  #split entire data set into train/ test subsets

  #split <- sample.split(data_clean[,2], SplitRatio = 0.7)
  train_t <- dplyr::sample_frac(data_clean, split_ratio)
  test_t <- dplyr::setdiff(data_clean, train_t)
  
  #reduce train to have comparable number of dig engs and not dig engs
  n_bin_flag <- nrow(train_t[train_t[[binary_flag]] == 1,])
  train_not_eng <- dplyr::sample_n(train_t[train_t[[binary_flag]] == 0,], n_bin_flag, replace = FALSE)
  train_t <- rbind(train_t[train_t[[binary_flag]] == 1,], train_not_eng)
  test <- dplyr::setdiff(data_clean, train_t)
  
  data_clean <- train_t
  
  ###################################################################
  
  ###### BUILD MODEL ON TRAINING SUBSET
  
  ###################################################################
  
  #build model -- first part allows us to use binary_flag string as dependent variable
  dep_var <- as.formula(paste0(binary_flag, " ~ ", paste(colnames(data_clean[,-which(names(data_clean) %in% c("FINCL_ITRMY_PRSON_UID",binary_flag))]), sep = "+", collapse = "+"))) 
  model1 <- glm(dep_var, as.data.frame(data_clean), family = 'binomial')

  #filter down by p-value 
  ind_vars <- rownames(summary(model1)$coef)[summary(model1)$coef[,4] < p_value_max]
  ind_vars_low_p_val <- ind_vars[2:length(ind_vars)]

  #rebuild model
  data_clean <- data_clean[,names(data_clean) %in% c("FINCL_ITRMY_PRSON_UID", binary_flag, ind_vars_low_p_val)]
  dep_var <- as.formula(paste0(binary_flag, " ~ ", paste(colnames(data_clean[,-which(names(data_clean) %in% c("FINCL_ITRMY_PRSON_UID",binary_flag))]), sep = "+", collapse = "+"))) 
  model2 <- glm(dep_var, as.data.frame(data_clean), family = 'binomial')
  
  write.csv(cor(data_clean[,-c(1:2)]), paste0("/users/teams/nad/ryhl/Capstone/", binary_flag, "/model_2_cor_matrix.csv"))
  write.csv(data_clean, paste0("/users/teams/nad/ryhl/Capstone/", binary_flag, "/temp_model_pre_vif.csv"))
  
  #filter down to those with low variance inflation factors (VIFs) -- proxy for colineariy -- car::vif(model2)
  #ind_vars <- names(car::vif(model2)[unname(car::vif(model2)) <= vif_value_max])
  
  #ordered by IV
  ind_vars_pre_multi_co_ord <- setdiff(IV$Summary$Variable, setdiff(IV$Summary$Variable, colnames(data_clean[,-c(1:2)])))
  bad <- NULL
  i <- 1
  while(i <= length(ind_vars_pre_multi_co_ord))
  {
    j <- i + 1
    while(j <= length(ind_vars_pre_multi_co_ord)){
      if(cor(data_clean[[which(names(data_clean) == ind_vars_pre_multi_co_ord[i])]], data_clean[[which(names(data_clean) == ind_vars_pre_multi_co_ord[j])]]) >= .8){
        bad <- c(bad, ind_vars_pre_multi_co_ord[j])
      }
    j <- j + 1
    }
    i <- i + 1
  }

  ind_vars <- setdiff(ind_vars_pre_multi_co_ord, bad)[1:10]
  
  data_clean <- data_clean[,names(data_clean) %in% c("FINCL_ITRMY_PRSON_UID", binary_flag, ind_vars)]

  write.csv(colnames(data_clean), paste0("/users/teams/nad/ryhl/Capstone/", binary_flag, "/model_2_col_names.csv"))
  
  #rebuild model
  dep_var <- as.formula(paste0(binary_flag, " ~ ", paste(colnames(data_clean[,-which(names(data_clean) %in% c("FINCL_ITRMY_PRSON_UID",binary_flag))]), sep = "+", collapse = "+"))) 
  model3 <- glm(dep_var, as.data.frame(data_clean), family = 'binomial')

  print(summary(model3))
  
  write.csv(car::vif(model3), paste0("/users/teams/nad/ryhl/Capstone/", binary_flag, "/model_VIFs.csv"))
  write.csv(model3$coefficients, paste0("/users/teams/nad/ryhl/Capstone/", binary_flag, "/model_coefs.csv"))
  
  ###################################################################
  
  ###### TEST MODEL ON TESTING SUBSET
  
  ###################################################################
  
  write.csv(data_clean, paste0("/users/teams/nad/ryhl/Capstone/", binary_flag, "/model_data_clean.csv"))
  
  #Ensure test subset has same columns as training subset
  test_t <- test_t[,colnames(data_clean)]
  
  predictTest = predict(model3, type = "response", newdata = test_t)
  print(table(test_t[,binary_flag], predictTest > response_min))
  write.csv(table(test_t[,binary_flag], predictTest > response_min), paste0("/users/teams/nad/ryhl/Capstone/", binary_flag, "/model_conf_matrix.csv"))
  
  ###
  
  ROCRpred = prediction(predictTest, test_t[,binary_flag])
  print(as.numeric(performance(ROCRpred, "auc")@y.values))
  write.csv(as.numeric(performance(ROCRpred, "auc")@y.values), paste0("/users/teams/nad/ryhl/Capstone/", binary_flag, "/model_AUC.csv"))
  
  model3
}


##########################################################################################################################################################
##########################################################################################################################################################
######################################################## OTHER FUNCTIONS #################################################################################
##########################################################################################################################################################
##########################################################################################################################################################


replace_with_WOE_smb <- function(data_clean, binary_flag){
  numeric_ind_vars <- split(names(data_clean),sapply(data_clean, function(x) paste(class(x), collapse=" ")))$numeric
  for(var in numeric_ind_vars[-1]){
    woe <- smbinning(data_clean, binary_flag, var, p = 0.05)
    for(bin_n in 1:length(woe$bands)-1){
      data_clean[data_clean[[var]] >= woe$bands[bin_n] & data_clean[[var]] <= woe$bands[bin_n+1],var] <- woe$ivtable$WoE[bin_n]
    }
    data_clean[[var]] <- as.numeric(data_clean[[var]])
  }
  data_clean 
}


replace_with_WOE_smb_cat <- function(data_clean, binary_flag){
  categ_ind_vars <- split(names(data_clean),sapply(data_clean, function(x) paste(class(x), collapse=" ")))$factor 
  for(var in categ_ind_vars[-1]){
    woe <- smbinning.factor(data_clean, binary_flag, var)
    data_clean[[var]] <- unfactor(data_clean[[var]])
    for(n in 1:length(woe$cuts)){
      data_clean[data_clean[[var]] == woe$cuts[n], var] <- woe$ivtable$WoE[n]
    }
    data_clean[[var]] <- as.numeric(data_clean[[var]])
  }
  data_clean 
}

xtract_data <- function(sql_file, db_nm, output_file_nm) {
  # Extracts data from DB based on sql_file or query string
  # Args: 
  #   sql_file: file name with path or query string
  #   db_nm   : self-defined DB names, "mssbi5", "mssbi6", "prd1", "prd2" 
  #   output_file_name: outfile name with path
  # Returns:
  #   the queried data as in list
  
  
  
  
  connection_string <-
    switch(db_nm, 
           mssbi5 = paste0("DSN=afsdwp5;uid=", Sys.getenv("oracle_uid"),";pwd=", Sys.getenv("oracle_pwd")), 
           mssbi6 = paste0("DSN=afsdwp6;uid=", Sys.getenv("oracle_uid"),";pwd=", Sys.getenv("oracle_pwd")),
           prd1  = paste0("DSN=afbi_prd1;uid=", Sys.getenv("oracle_uid"),";pwd=", Sys.getenv("oracle_pwd")),
           prd2  = paste0("DSN=fbi_prd2;uid=", Sys.getenv("oracle_uid"),";pwd=", Sys.getenv("oracle_pwd")),
           lasr  = paste0("DSN=lasr_uat;uid=", Sys.getenv("lasr_uid"),";pwd=", Sys.getenv("lasr_pwd")),
           stop("invalid DB name"))
  len <- nchar(sql_file)
  if(substr(sql_file, len-3, len) == '.sql'){
    # is .sql file
    query_string <- readChar(sql_file, file.info(sql_file)$size) 
    # cat(query_string)
  } else {
    query_string <- sql_file
  }
  datasource   <- RxOdbcData(sqlQuery = query_string, connectionString = connection_string, rowBuffering = F)
  xdf_path     <- file.path(output_file_nm)
  xdf          <- rxImport(datasource, xdf_path, overwrite=TRUE)
  res          <- rxReadXdf(xdf)
  res
}



##########################################################################################################################################################
##########################################################################################################################################################

#replace_with_WOE <- function(data_clean){
#  numeric_ind_vars <- split(names(data_clean),sapply(data_clean, function(x) paste(class(x), collapse=" ")))$numeric 
#  for(var in numeric_ind_vars[-1]){
#    IV$Tables[[var]][["new_range"]] <- substring(IV$Tables[[var]][[var]], 2, nchar(IV$Tables[[var]][[var]])-1)
#    IV$Tables[[var]][["num_1"]] <-  unlist(strsplit(IV$Tables[[var]][["new_range"]], ","))[seq_along(unlist(strsplit(IV$Tables[[var]][["new_range"]], ","))) %% 2 == 1]                              
#    IV$Tables[[var]][["num_2"]] <- unlist(strsplit(IV$Tables[[var]][["new_range"]], ","))[1:(2)==(2)]
#    IV$Tables[[var]][["num_1"]] <- as.numeric(IV$Tables[[var]][["num_1"]])
#    IV$Tables[[var]][["num_2"]] <- as.numeric(IV$Tables[[var]][["num_2"]])
    
    #dublicate var column 
#    var_temp <- paste0(var, "_temp")
#    data_clean[[var_temp]] <- NULL
#    data_clean[[var_temp]] <- data_clean[[var]]
    
    #then assign woe
#    for(bin_n in 1:nrow(IV$Tables[[var]])){
#      data_clean[data_clean[[var_temp]] >= IV$Tables[[var]][["num_1"]][bin_n] & data_clean[[var_temp]] <= IV$Tables[[var]][["num_2"]][bin_n],][[var]] <- IV$Tables[[var]]$WOE[bin_n]
#    }
    
#    data_clean <- data_clean[,!(names(data_clean) == var_temp)]
#  }
#  data_clean 
#}


#replace_cat_with_WOE <- function(data_clean){
  
#  categ_ind_vars <- split(names(data_clean),sapply(data_clean, function(x) paste(class(x), collapse=" ")))$factor 
#  factor_vars <- categ_ind_vars[-1]
  
#  for(factor_var in factor_vars){
#    data_clean[[factor_var]] <- unfactor(data_clean[[factor_var]])
#    for(bin_n in 1:nrow(IV$Tables[[factor_var]])){
#      data_clean[data_clean[[factor_var]] == IV$Tables[[factor_var]][[factor_var]][bin_n] & !is.na(data_clean[[factor_var]]), factor_var] <- IV$Tables[[factor_var]]$WOE[bin_n]  #& !is.na(data_clean[[factor_var]])
#    }
#  }
#  data_clean
#}

############################# VIF CALCS


#identify vars with high VIFs
#high_vif_vars <- names(car::vif(model2)[unname(car::vif(model2)) >= vif_value_max])

#remove those from ordered list
#ind_vars_pre_multi_co_ord <- setdiff(ind_vars_pre_multi_co_ord, high_vif_vars)

#bad <- NULL
#i <- 1 

#first eliminate any vars with high VIFs correlated to any vars without high VIFs 
#while(i <= length(ind_vars_pre_multi_co_ord)){
#  j <- 1
#  while(j <= length(high_vif_vars)){
#          if( ind_vars_pre_multi_co_ord[i] != high_vif_vars[j]  & cor(data_clean[[which(names(data_clean) == ind_vars_pre_multi_co_ord[i])]], data_clean[[which(names(data_clean) == high_vif_vars[j])]]) >= .8){
#      #temp <- temp[-which(names(data_clean) %in% high_vif_vars[j])]
#      bad <- c(bad, high_vif_vars[j])
#    }
#    j <- j+1   
#  }
#  i <- i+1 
#}

#then check within just high VIF vars
#high_vif_vars <- setdiff(IV$Summary$Variable, setdiff(IV$Summary$Variable, high_vif_vars)) #this orders them by IV
#i <- 1 
#while(i <= length(high_vif_vars)){
#  j <- i+1
#  while(j <= length(high_vif_vars)){
#    if( cor(data_clean[[which(names(data_clean) == high_vif_vars[i])]], data_clean[[which(names(data_clean) == high_vif_vars[j])]]) >= .8){
#      bad <- c(bad, high_vif_vars[j])
#    }
#    j <- j+1   
#  }
#  i <- i+1 
#}

#print(ind_vars_pre_multi_co_ord)
#print(bad)
#print(high_vif_vars)

#ind_vars <- c(ind_vars_pre_multi_co_ord, temp)


#data_clean[[factor_var]] <- InformationValue::WOE(X = data_clean[[factor_var]], Y = data_clean[[binary_flag]])