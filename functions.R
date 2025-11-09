
#--- fun: yuima est
yuima_est_fun <- function(data_x, data_y, max_lag){
  #: define llag pars
  delta.n <- 1
  delta <- 1/delta.n

  est <- data.table()
  var_names <- colnames(data_x[, !"year.mon"])
  for(c in var_names){
    xcol <- c("year.mon",c)
    yuima_df <- merge(data_y,data_x[,..xcol],by="year.mon",all.x = T)
    yuima_df <- xts(
      x=yuima_df[,-c("year.mon")],order.by=as.Date(yuima_df$year.mon))
    yuima_set <- setYuima(setData(yuima_df,delta=delta))
    result <- try(
        llag(
            yuima_set, 
            grid=seq(-max_lag*delta, max_lag*delta, by=delta), ci=TRUE, plot=F), 
        silent = T)
      if (inherits(result, "try-error")) NULL else result
    if(is.null(result)){
      temp <- data.table(var=c,lag=NA,p.val=NA,llr=NA, corr=NA)
    } else{
      temp <- data.table(
        var=c,
        lag = result$lagcce[1, 2] * delta.n,
        p.val = result$p.values[1, 2],
        llr = result$LLR[1, 2],
        corr=result$cormat[1,2]
      )
    }
    est <- rbind(est,temp)
  }
  est[
    lag>0 & !is.na(lag) & 
      p.val<0.05 & !is.na(p.val) &
      llr>=1 & !is.na(llr)
  ]

}

#--- Fun: map yuima par to time-series, create dyn data
xlag_df_fun <- function(data_x, yuima_est){

  xlag <- data_x[,c("year.mon")]
  for (c in 1:nrow(yuima_est)) {
    x_var <- yuima_est$var[c]
    l <- yuima_est$lag[c]
    temp <- xts(x = data_x[, ..x_var], order.by = as.Date(data_x$year.mon) )
    temp <- lag.xts(temp, l)
    xlag <- cbind(xlag,temp)
  }
  xlag[(max(yuima_est$lag)+1):nrow(xlag),]

}



