

calcMSE <- function( dt_data,s_var1, s_var2 ){
  # calculates the mean square error
  # input:
  #     dt_data: a data.table
  #     s_var1: is the string name of the real value
  #     s_var2: is the string name of the estimated value
  # output: a numeric vector length one of the MSE vaue
  # 
  
  return(sum((dt_data[[s_var1]] - dt_data[[s_var2]])^2)/nrow(dt_data))
  
}

#-------------------------------------------------------------------------------------------------


makeLatexNewCommand <- function( k_name, k_content ){
  # makes a string latex macro
  # k_name: string name of new macro
  # k_content: string with content of new macro
  # ouput: string 
  k_out <- paste0("\\newcommand{", "\\",k_name,  "}{" ,k_content ,"}")
  
  return(k_out)
  
}


#-------------------------------------------------------------------------------------------------

calcPropCasesLogChange <- function( dt_data,s_var1, s_var2, n_size= 0.1 ){
  # calculates the proportion of cases with that a log change lower than n_size
  # input:
  #     dt_data: a data.table
  #     s_var1: is the string name of the real value
  #     s_var2: is the string name of the estimated value
  # output: a numeric vector length one of cases with log change lower than n_size
  # observation: although  dt_data[[s_var1]] and  dt_data[[s_var2]] should both be positive 
  #   if for a row one is negative we say that observation had an infinite log change
  
  v_ratios <- dt_data[[s_var1]] / dt_data[[s_var2]]
  v_ratios[v_ratios< 0] <- Inf
  return(mean( abs(log(v_ratios)) <= n_size ))
  
}

#-------------------------------------------------------------------------------------------------

makeModelAnalysis <- function( s_y,
                               v_x,
                               l_data,
                               s_new_col,
                               funModel=glm,
                               funPredict=predict,
                               funTransPredict=identity,
                               ... ){
  
  # input:
  #   s_y: character length one of name of target variable
  #   v_x: character vector of names of explanatory variables
  #   l_data: list
  #            l_data[["dt_train"]]: data.table train data 
  #            l_data[["dt_test"]]: data.table test data 
  #            l_data[["dt_indi"]]: data.table of performance indicators 
  #   s_new_col: character length one of name of new field in dt_train, and dt_test of estimated value
  #   funModel=glm: function of model to be applied
  #   funPredict=predict: function used to predict values
  #   funTransPredict=identity: function to be applied to predict
  #   ...: additional parameters to be pased on to funModel
  # ouput: 
  #   l_data: list
  #            l_data[["dt_train"]]: data.table train data with new column s_new_col
  #            l_data[["dt_test"]]: data.table test data with new column s_new_col
  #            l_data[["dt_indi"]]: data.table of performance indicators with new row for this model
  
  
  dt_train <- l_data[["dt_train"]]
  dt_test <- l_data[["dt_test"]]
  dt_indi <- l_data[["dt_indi"]]
  
  # make model
  s_fromala <-paste0( s_y ," ~ ", paste0( v_x, collapse = " + "))
  model <- funModel(formula(s_fromala), data=dt_train , ...)
  
  # make estimated field in train and test data
  dt_train[, new_col_name:= funTransPredict(funPredict(model,  dt_train))]
  setnames(dt_train, "new_col_name",s_new_col )
  dt_test[, new_col_name:= funTransPredict(funPredict(model,  dt_test))]
  setnames(dt_test, "new_col_name",s_new_col )
  
  # calculate MSE
  n_mse_train <- calcMSE( dt_train,s_y, s_new_col )
  n_mse_est <- calcMSE( dt_test,s_y, s_new_col )
  
  # if object is of class glm get the aic of the model
  if( "glm" %in% class(model) ){
    n_aic <- AIC(model)
  } else {
    n_aic <- NA_real_
  }
  
  
  # get proportion of cases with in a certain log change
  n_prop_05_train <- calcPropCasesLogChange( dt_train,s_y, s_new_col, n_size= 0.05)
  n_prop_05_test <- calcPropCasesLogChange( dt_test,s_y, s_new_col, n_size= 0.05)
  
  n_prop_10_train <- calcPropCasesLogChange( dt_train,s_y, s_new_col, n_size= 0.1)
  n_prop_10_test <- calcPropCasesLogChange( dt_test,s_y, s_new_col, n_size= 0.1)
  
  # combine performance statistics into one data.table
  dt_indi_local <- data.table( model=s_new_col,
                               aic_train = n_aic,
                               mse_train = n_mse_train, 
                               mse_test= n_mse_est,
                               prop_05_train = n_prop_05_train,
                               prop_05_test = n_prop_05_test,
                               prop_10_train = n_prop_10_train,
                               prop_10_test = n_prop_10_test
  )
  
  # combine existing dt_indi with new performnce statistics
  dt_indi <- rbindlist(l=list(dt_indi, dt_indi_local),use.names = T,fill = T)
  
  l_data <- list( dt_train = dt_train, dt_test= dt_test, dt_indi=dt_indi)
  
  return(l_data)
}

#-------------------------------------------------------------------------------------------------

makeLatexParameters <- function( dt_sum_test){
  l_param <- list()
  
  setorder(dt_sum_test, mse)
  l_param[["sBestModel"]] <- dt_sum_test[  1 , model]
  l_param[["sSecondBestModel"]] <- dt_sum_test[  2 , model]
  
  l_param[["sBestModelMSE"]] <- round(dt_sum_test[  1 , mse ],2)
  l_param[["sBestModelPropFive"]] <-  round(dt_sum_test[  1 , prop_log_chage_05 ],2)
 
  l_param[["sBestChangeMSE"]] <-  round((dt_sum_test[  2 , mse ] -
                                           dt_sum_test[  1 , mse ])/
                                          dt_sum_test[  1 , mse ]*100)
  
  s_out <- character()
  
  for( s_name in names(l_param)){
    
    s_out <- paste0(s_out ,makeLatexNewCommand(s_name, l_param[[s_name]]) ,"\n")
  }
  return(s_out)
}

#-------------------------------------------------------------------------------------------------

makeModelBenchmarking <- function( dt_data,  s_y, v_x ,i_num,  k_include_forrest = T){
  # input:
  #   dt_data: data.table with explanatory and target variable
  #   s_y: character length one of name of target variable
  #   v_x: character vector of names of explanatory variables
  #   i_num: number of simulations
  #   k_include_forrest = T: if we also want a random forrest model. 
  #     This is because random forrest take far longer to estimate.
  # output: NULL
  # details: for each iteration sepeartes dt_data into train and test samples,
  #   performs several supervised learning models and captures performance indicators.
  #   After the iterations summary performance indicators and graphs are created to 
  #   be passed on to documentation/documentation.tex
  
  library(data.table)
  library(partykit)
  library(e1071)
  library(reshape2)
  library(ggplot2)
  library(xtable)
  
  # we check that all values of target variable are positive 
  if( any( dt_data[[s_y]]<=0)){
    stop( paste0("Target variable ", s_y, " has non positive values") )
  }
  
  # initialize table of performance statistics
  dt_indi <- data.table()
  
  # number of rows in data
  k_len <- nrow(dt_data)
  
  # make groups for cross validation
  dt_data[ , group:= sample(1:i_num, size = k_len ,replace = T)]
  
  for(i_iter in 1:i_num){
    
    print(i_iter)
    
    # assign group to dt_test and all others to dt_train
    
    dt_train <- dt_data[ group != i_iter ] 
    dt_test <-  dt_data[ group == i_iter ] 
    
    l_data <- list(dt_train = dt_train, dt_test= dt_test, dt_indi=data.table())
    
    # make model estimation and perofrmance statistics of several supervised models
    l_data <- makeModelAnalysis(s_y = s_y,
                                v_x = v_x,
                                l_data = l_data,
                                s_new_col = "Linear"
    )
    
    
    
    l_data <- makeModelAnalysis(s_y = s_y,
                                v_x = v_x,
                                l_data = l_data,
                                s_new_col = "Gamma1", 
                                funTransPredict =  function(x) 1/x,
                                family = Gamma()
    )
    
    l_data <- makeModelAnalysis(s_y = s_y,
                                v_x = v_x,
                                l_data = l_data,
                                s_new_col = "Gamma2", 
                                funTransPredict =  exp,
                                family = Gamma(link="log")
    )
    

    
    l_data <- makeModelAnalysis(s_y = s_y,
                                v_x = v_x,
                                l_data = l_data,
                                s_new_col = "Ctree", 
                                funModel = partykit::ctree)
    
    
    if( k_include_forrest == TRUE){
      
      l_data <- makeModelAnalysis(s_y = s_y,
                                  v_x = v_x,
                                  l_data = l_data,
                                  s_new_col = "Cforest",
                                  funModel = partykit::cforest,
                                  ntree = 100L)
    }
    
    
    l_data <- makeModelAnalysis(s_y = s_y,
                                v_x = v_x,
                                l_data = l_data,
                                s_new_col = "SMV1", 
                                funModel = svm
    )
    
    l_data <- makeModelAnalysis(s_y = s_y,
                                v_x = v_x,
                                l_data = l_data,
                                s_new_col = "SMV2", 
                                funModel = svm,
                                cost = 5
    )
    
    # combine dt_indi of this iteration with dt_indi of all iterations
    dt_indi_local <- l_data$dt_indi
    dt_indi_local[, inter:=i_iter]
    dt_indi <- rbindlist(l=list(dt_indi,dt_indi_local),use.names = T,fill = T)
  }
  
  # summarize the indicators by taking the mean
  dt_sum_train <- dt_indi[, .(aic = mean(aic_train),
                        mse = mean(mse_train),
                        prop_log_chage_05 = mean(prop_05_train),
                        prop_log_chage_10 = mean(prop_10_train)
                        ),by=.(model)]
  
  dt_sum_test <- dt_indi[, .(mse = mean(mse_test),
                              prop_log_chage_05 = mean(prop_05_test),
                              prop_log_chage_10 = mean(prop_10_test)
                            ),by=.(model)]
  
  dt_sum_train_table <- copy(dt_sum_train)
  dt_sum_test_table <- copy(dt_sum_test)
  
  v_sum_names <- c("Model", "AIC", "MSE", "PCALD\\textsubscript{0.05}",
                   "PCALD\\textsubscript{0.1}")
  setnames(dt_sum_train_table, v_sum_names)
  setnames(dt_sum_test_table, setdiff(v_sum_names, "AIC") )
  
  x_dt_sum_train <- xtable(dt_sum_train_table,
                           caption = "Mean Summary Statistics of Train Sample",
                           label = "ta:sumTrain")
  print.xtable(x_dt_sum_train, 
               file= "./documentation/tables/summaryTrain.tex",
               table.placement = "h",
               size = "small",
               include.rownames = F,
               booktabs = T,
               sanitize.colnames.function = identity)
  
  x_dt_sum_test <- xtable(dt_sum_test_table,
                          caption = "Mean Summary Statistics of Test Sample",
                          label = "ta:sumTest")
  
  print.xtable(x_dt_sum_test, 
               file= "./documentation/tables/summaryTest.tex",
               table.placement = "h",
               size = "small",
               include.rownames = F,
               booktabs = T,
               sanitize.colnames.function = identity)
  
  # give an order to models in dt_indi 
  v_models <- dt_sum_train$model
  dt_indi[, model:= factor(model, levels = v_models)]
  
  # graph with boxplot of mse per model
  g_mse <- ggplot(data = dt_indi, aes(x=model, y= mse_test ))+
    geom_boxplot()+ theme_bw() + xlab("Model")+ ylab("MSE") +
    theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
  
  pdf(file= "./documentation/figures/mse.pdf",width = 5,height = 4)
  print(g_mse)
  dev.off()
  
  # graph with boxplot of proporiton log change <=0.05 per model
  g_prop_05 <- ggplot(data = dt_indi, aes(x=model, y= prop_05_test ))+ 
    geom_boxplot()+theme_bw() + xlab("Model")+ ylab(expression(PCALD[0.05]))+
    theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
  
  pdf(file= "./documentation/figures/prop05.pdf",width = 5,height = 4)
  print(g_prop_05)
  dev.off()
  
  
  # en example of estimated vs real values per model
  dt_test <- l_data$dt_test
  i_max_axis <- max(dt_test[[s_y]])*1.2
  
  for(s_model in v_models){
    
    g_model <- ggplot(dt_test, aes_string( x = s_model , y=s_y)) +
      geom_point() +ylim(0,i_max_axis) +xlim(0,i_max_axis) +  geom_abline(alpha=0.5)+theme_bw()+ 
      xlab(paste0("Estimate by ",s_model, " model")) + ylab("Observed value")
    
    pdf(file=paste0( "./documentation/figures/",s_model,".pdf"), width = 4,height = 4)
    print(g_model)
    dev.off()
    
  }
  
  # get model with best adnd second best
  setorder( dt_sum_test, mse)
  v_x_max <- dt_sum_test[ 1:2, model]
  
  dt_graph <- dt_test[ , c(s_y, v_x_max), with=F]
  
  dt_graph <- melt(dt_graph, id.vars = s_y, measure.vars = v_x_max, variable.name = "Model",value.name = "Estimate")
  
  # make a scatter plot with best worst mse and target variable
  g_comparison <- ggplot(dt_graph, aes_string( x = "Estimate" , y=s_y, color = "Model")) +
    geom_point()  + scale_colour_brewer( palette = "Set1") +
    ylim(0,i_max_axis) +xlim(0,i_max_axis) + geom_abline(alpha=0.5)+theme_bw() + ylab("Observed value")
 
  pdf(file=paste0( "./documentation/figures/","comparison",".pdf"), width = 6.5,height = 4)
  print(g_comparison)
  dev.off()
  
  # pass to latex values of best and se cond best models
  s_param <- makeLatexParameters(dt_sum_test)
  cat(s_param, file=paste0( "./documentation/parameters/","parameters",".sty"))
  
}

