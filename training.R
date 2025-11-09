
# ------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, lubridate, data.table, tictoc, countrycode,
               ggplot2, scales, geomtextpath, foreach, ggpp,
               caret, doParallel, randomForest,
               yuima, xts, corrplot, ggdendro, cowplot,dendextend,
               ggpubr, glmnet
               )

# set working dir to script location (optional)
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

source("functions.r")

# output dir
out_dir <- file.path(getwd(), "outputs") 
if(dir.exists(out_dir)) unlink(out_dir, recursive = T)
dir.create(out_dir, recursive=TRUE)

# ------------------------------------------------------------------------
# Tuning
# ------------------------------------------------------------------------
# get file names in the partitioned data folder
files <- list.files(
    file.path(getwd(),"partition_df"),
    pattern = "^preproc.*\\.csv$",
    full.names = T)

# -------- Loop
# parallel by country
cl <- makeCluster(detectCores()-6)
registerDoParallel(cl)

foreach(
    f=seq_along(files),.packages=c("data.table", "glmnet","foreach","doParallel", "xts", "yuima","tidyverse")
) %dopar% {
    df <- fread(files[f])
     # path to store training results
    cty <- sub(".*_(.*)\\.csv$", "\\1",basename(files[f]))

    # --- split parameters
    min_tr_leng <- 32
    testing.periods <- 6
    max_lag <- 6

    # --- split data
    df$month_id <- seq_along(df$year.mon)
    te_date <- df[month_id %in% (max(df$month_id)-testing.periods+1):max(df$month_id)]$year.mon
    tr_date <- df[month_id %in% 1:(max(df$month_id)-testing.periods)]$year.mon

    df_tr_y <- df[year.mon %in% tr_date, c("year.mon","outcome"), with=F]
    df_te_y <- df[year.mon %in% te_date, c("year.mon","outcome"), with=F]

    x_var <- setdiff(names(df), c("outcome","year.mon","month_id"))
    df_tr_x <- df[year.mon %in% tr_date, c("year.mon",x_var), with=F]

    # --- cv scheme
    cv_list <- list()
    for (split in 1:(length(tr_date)-min_tr_leng)) {
    cv_list[[paste0("spl_",split)]] <- tr_date[split:length(tr_date)]
    }
    cv_list <- cv_list[seq(1,length(cv_list), 1)] # subset cv list by every 12 months
    saveRDS(cv_list, file.path(out_dir, paste0("cv_", cty, ".rds")))

    # --- Training 
    # loop over cv splits
    tune_res <- foreach(
        sp = seq_along(cv_list), .combine = rbind) %do% {
            
            # data
            mat_tr_x <- df_tr_x[year.mon %in% cv_list[[sp]]]
            names(mat_tr_x) <- make.names(names(mat_tr_x))
            num_tr_y <- df_tr_y[year.mon %in% cv_list[[sp]]]

            # llag
            yuima_est <- yuima_est_fun(mat_tr_x,num_tr_y, max_lag)
            mat_tr_x <- xlag_df_fun(mat_tr_x,yuima_est)
            yuima_df <- copy(mat_tr_x)
            num_tr_y <- num_tr_y[year.mon %in% unique(mat_tr_x$year.mon)]$outcome

            # sparse matrix
            mat_tr_x <- Matrix::sparse.model.matrix(~ . -year.mon -1, data = mat_tr_x)

            # train
            fit <- tryCatch({
            glmnet(
                mat_tr_x, num_tr_y, 
                family = "gaussian",
                alpha = 0.5, 
                lambda = exp(seq(0,-5,length.out = 50)),    # pass full sequence
                standardize = TRUE,
                intercept = TRUE,
                maxit = 10000)  
            }, error = function(e) {
            message("Error in glmnet: ", e$message)
            return(NULL)
            })

            # record result
            res <- data.table()
            if(!is.null(fit) && fit$jerr==0){
            res <- data.table(
                place=cty,
                spl=names(cv_list)[sp],
                tune_len=length(cv_list[[sp]]),
                yuima_est=list(yuima_est),
                yuima_df=list(yuima_df),
                mod = list(fit),
                converg=fit$jerr)
            } 
            res
        }

    if(!all(is.null(tune_res$mod))){
      # --- Best tune model
      temp <- tune_res
      temp[, n := sapply(mod, function(m) m$nobs)]
      temp[, k := sapply(mod, function(m) list(m$df))]
      temp[, lambda := sapply(mod, function(m) list(m$lambda))]
      temp[, dev := sapply(mod, function(m) list(m$dev.ratio))]
      # temp <- temp[,c("spl","tune_len","n","k","lambda","dev")]
      
      temp <- unnest(temp, cols = c(k, lambda, dev))
      temp <- temp |> mutate(p_dev=dev*((n-1-k)/(n-1)))
      
      tune_par <- temp |> gather(metric,value,c(dev,p_dev))
      saveRDS(
          subset(tune_par,select = -c(yuima_df)), 
          file.path(out_dir, paste0("tune_", cty, ".rds")))

      best_tune_par <- tune_par |> group_by(metric) |> 
          dplyr::mutate(best_val=max(value)) |> 
          filter(value==best_val)
      saveRDS(best_tune_par, file.path(out_dir, paste0("best_mod_", cty, ".rds")))

      # --- Forecaster
      pred_df <- c()
      for (i in 1:nrow(best_tune_par)) {
          # main data
          input <- best_tune_par[i,]
          temp <- df[year.mon %in% c(cv_list[[input$spl]], te_date)]
          names(temp) <- make.names(names(temp))

          # x data
          yuima_est <- input$yuima_est[[1]]
          temp_x <- temp[, c("year.mon",yuima_est$var), with=F]
          temp_x <- xlag_df_fun(temp_x, yuima_est)
          mat_tr_x <- temp_x[year.mon %in% cv_list[[input$spl]]]
          mat_tr_x <- Matrix::sparse.model.matrix(~ . -year.mon -1, data=mat_tr_x)
          mat_te_x <- temp_x[year.mon %in% te_date]
          mat_te_x <- Matrix::sparse.model.matrix(~ . -year.mon -1, data=mat_te_x)

          # y data
          temp_y <- temp[year.mon %in% temp_x$year.mon, c("year.mon","outcome"), with=F]
          num_tr_y <- temp_y[year.mon %in% cv_list[[input$spl]]]
          num_te_y <- temp_y[year.mon %in% te_date]

          # pred 
          pred_tr <- as.numeric(
              predict(
                  input$mod[[1]], 
                  newx = mat_tr_x, s=input$lambda, type="response"))

          pred_te <- as.numeric(
              predict(
                  input$mod[[1]], 
                  newx = mat_te_x, s=input$lambda, type="response"))

          # store results
          temp <- input[,c("place","spl","n","k","lambda", "metric")] 
          temp$tr_date <- list(num_tr_y$year.mon)
          temp$true_tr <- list(num_tr_y$outcome)
          temp$pred_tr <- list(pred_tr)
          temp$te_date <- list(num_te_y$year.mon)
          temp$true_te <- list(num_te_y$outcome)
          temp$pred_te <- list(pred_te)

          pred_df <- rbind(pred_df, temp)
      }
      saveRDS(pred_df, file.path(out_dir, paste0("forecast_", cty, ".rds")))
    }
    

} # end par loop for each country
stopCluster(cl)
registerDoSEQ()

