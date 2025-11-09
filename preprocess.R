
# ------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, lubridate, data.table, tictoc, countrycode,
               ggplot2, scales, geomtextpath, foreach, ggpp,
               caret, doParallel, randomForest,
               yuima, xts, corrplot, ggdendro, cowplot,dendextend,
               ggpubr
               )

# set working dir to script location (optional)
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# ------------------------------------------------------------------------
# add recg rate and IBC
# ------------------------------------------------------------------------

# load data
df <- fread("cleaned.Mast.df.csv") 
df$destination <- countrycode(df$destination, origin = "country.name", destination = "iso3c")

# create year month
df$year.mon <- as.Date(as.yearmon(sprintf("%s/%s",df$month,df$year), "%m/%Y"), frac=1.)

temp <- fread("estat_recrate.csv")
df <- merge(df,temp, by=c("destination","year.mon"), all.x=T)

temp <- fread("ibc.csv")
df <- merge(df,temp, by=c("year.mon"), all.x=T)

# outcome var
df$outcome <- log(df$asr)
df <- df[,-c("id", "year", "month", "asr", "som_asylum_requests")] 

plot_df <- df[,c("year.mon","outcome","destination")] |> 
    mutate(outcome=exp(outcome)*1000) |> 
    group_by(destination) |> dplyr::mutate(m=mean(outcome))

ggplot(plot_df, aes(x=year.mon, y=outcome) ) +
    geom_textline(
        data=filter(plot_df, m>=quantile(plot_df$m,0.80)),
        aes(label=destination, color=destination),
            show.legend=F,  vjust=.0, hjust="ymid", text_smoothing=30 ) + 
    geom_line(
        data=filter(plot_df, m<quantile(plot_df$m,0.80)),
        aes(group=destination),color="grey80")+
    geom_vline(
        xintercept=max(plot_df$year.mon) %m-% months(6), 
        color="grey20", linetype="dashed") +
    theme_bw() +
    theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() ) +
    scale_fill_brewer(palette = "Dark2") +
    scale_x_date(labels = date_format("%Y-%m")) +
    ylab("Asyl. Seekers per 1000 Origin Pop.") + xlab("Month") 
ggsave(file.path(getwd(),"figs","som_eu.png"),width=8,height=6 )

# ------------------------------------------------------------------------
# Partition data
# ------------------------------------------------------------------------
part_df_dir <- file.path(getwd(), "partition_df")
if(dir.exists(part_df_dir)) {unlink(part_df_dir,recursive = T)} 
    
dir.create(part_df_dir, showWarnings = FALSE)

df[, fwrite(.SD, file = file.path(part_df_dir, paste0("cty_",destination, ".csv"))), by = destination]

# ------------------------------------------------------------------------
# Preprocessing Training data
# Note: If change vars here, re-run llag analysis below
# ------------------------------------------------------------------------

# get file names in the partitioned data folder
files <- list.files(
    part_df_dir,
    pattern = "^cty_.*\\.csv$",
    full.names = T)

for (f in seq_along(files)) {
    cty <- tools::file_path_sans_ext(basename(files[f]))

    temp <- fread(files[f])
    setorder(temp, year.mon)
    df_time <- temp[,c("year.mon")]
    df_y <- temp[,c("year.mon","outcome")]
    df_x <- temp[, -c("outcome")]

    # gdelt
    sel_cols <- c(grep("gdelt.*mean",names(df_x),value=T))
    temp <- df_x[, ..sel_cols]
    pp.mod <- preProcess(temp, method = c("nzv"))
    temp <- predict(pp.mod, newdata = temp)

    temp[, (names(temp)) := lapply(.SD, function(x) {
        x_adj <- x - min(x, na.rm = TRUE)
        x_final <- (x_adj / x_adj[1]) - 1
        return(x_final)
    }), .SDcols = names(temp)]
    pp.mod <- preProcess(temp, method = c("nzv"))
    temp <- predict(pp.mod, newdata = temp)
    # bind all data
    gd <- temp

    # spei
    sel_cols <- c(grep("SPEI.mean",names(df_x),value=T))
    temp <- df_x[, ..sel_cols]
    pp.mod <- preProcess(temp, method = c("nzv"))
    temp <- predict(pp.mod, newdata = temp)

    temp[, (names(temp)) := lapply(.SD, function(x) {
        x_adj <- x - min(x, na.rm = TRUE)
        x_adj <- x_adj + max(x_adj,na.rm=T)*0.01 # add a constant to avoid initial zero
        x_final <- (x_adj / x_adj[1]) - 1
        return(x_final)
    }), .SDcols = names(temp)]
    pp.mod <- preProcess(temp, method = c("nzv"))
    temp <- predict(pp.mod, newdata = temp)
    # bind all data
    spei <- temp

    # smi
    sel_cols <- c(grep("Soil moisture index.mean",names(df_x),value=T))
    temp <- df_x[, ..sel_cols]
    pp.mod <- preProcess(temp, method = c("nzv"))
    temp <- predict(pp.mod, newdata = temp)

    temp[, (names(temp)) := lapply(.SD, function(x) {
        x_adj <- x - min(x, na.rm = TRUE)
        x_adj <- x_adj + max(x_adj,na.rm=T)*0.01 # add a constant to avoid initial zero
        x_final <- (x_adj / x_adj[1]) - 1
        return(x_final)
    }), .SDcols = names(temp)]
    pp.mod <- preProcess(temp, method = c("nzv"))
    temp <- predict(pp.mod, newdata = temp)
    # bind all data
    smi <- temp

    # ibc
    sel_cols <- c(grep("ibc",names(df_x),value=T))
    temp <- df_x[, ..sel_cols]
    pp.mod <- preProcess(temp, method = c("nzv"))
    temp <- predict(pp.mod, newdata = temp)

    temp[, (names(temp)) := lapply(.SD, function(x) {
        x_adj <- x - min(x, na.rm = TRUE)
        x_adj <- x_adj + max(x_adj,na.rm=T)*0.01 # add a constant to avoid initial zero
        x_final <- (x_adj / x_adj[1]) - 1
        return(x_final)
    }), .SDcols = names(temp)]
    pp.mod <- preProcess(temp, method = c("nzv"))
    temp <- predict(pp.mod, newdata = temp)
    # bind all data
    ibc <- temp
    
    # rec_rate
    sel_cols <- c(grep("rec_rate",names(df_x),value=T))
    temp <- df_x[, ..sel_cols]
    pp.mod <- preProcess(temp, method = c("nzv"))
    temp <- predict(pp.mod, newdata = temp)

    temp[, (names(temp)) := lapply(.SD, function(x) {
        x_adj <- x - min(x,na.rm=TRUE) 
        x_adj <- x_adj + max(x_adj,na.rm=T)*0.01 # add a constant to avoid initial zero
        x_final <- (x_adj / x_adj[1]) - 1
        return(x_final)
    }), .SDcols = names(temp)]
    pp.mod <- preProcess(temp, method = c("nzv"))
    temp <- predict(pp.mod, newdata = temp)
    # bind all data
    rec_rate <- temp
    
    # bind x datasets
    df_x <- cbind(df_time, gd, spei, smi, ibc, rec_rate)

    #save data
    df <- merge(df_y,df_x,by="year.mon", all.x = T)
    fwrite(df, file.path(part_df_dir,paste0("preproc_",cty,".csv")))

}

# check plot
files <- list.files(
    part_df_dir,
    pattern = "^preproc_.*\\.csv$",
    full.names = T)
plot_df <- fread(files[1])
sel_cols <- c("year.mon",grep("gdelt",names(plot_df),value=T))
plot_df <- plot_df[, ..sel_cols]

plot.df <- plot_df %>% 
    gather(var,value,-c(year.mon)) %>%
  mutate(loc=word(var,2,2,sep="-"))
ggplot(plot.df, aes(x=year.mon, y=value, color=loc, group=var) ) +
  geom_line(show.legend = F)+
  theme_minimal() +
  scale_x_date(labels = date_format("%Y-%m"))


