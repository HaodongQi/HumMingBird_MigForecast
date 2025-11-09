

# ------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, lubridate, data.table, tictoc, countrycode,
               ggplot2, scales, geomtextpath, foreach, ggpp,
               doParallel, randomForest,
               yuima, xts, corrplot, ggdendro, cowplot,dendextend,sf,
               ggpubr, glmnet
               )

# set working dir to script location (optional)
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

in_dir <- file.path(getwd(), "outputs")
out_dir <- file.path(getwd(), "figs")
dir.create(out_dir, recursive=TRUE)


# ------------------------------------------------------------------------
# cv list
# ------------------------------------------------------------------------
tr_date <- readRDS(file.path(in_dir, "cv_AUT.rds"))

tr_date <- lapply(names(tr_date), function(name) {
  data.table(date = tr_date[[name]], split = name)
})
tr_date <- rbindlist(tr_date)
tr_date$set <- "train"

last_dates <- tr_date[, .(last_date = max(date)), by = split]
te_date <- last_dates[, 
  .(date = seq(last_date %m+% months(1), last_date %m+% months(6), by = "1 month")), 
  by = split]
  te_date$set <- "test"

plot_df <- rbind(tr_date, te_date)
plot_df$split <- as.numeric(gsub("spl_", "", plot_df$split))
ggplot(plot_df, aes(date, y=as.factor(split))) +
  geom_tile(aes(fill=set), color="white") +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank() ) +
  ylab("Folds")
ggsave(file.path(out_dir, "cv.png"), height=3, width=8)

# ------------------------------------------------------------------------
# explore k-n-metric relation
# ------------------------------------------------------------------------
# get file names in the partitioned data folder
files <- list.files(
  in_dir,
  pattern = "^tune.*\\.rds$",
  full.names = T)
plot_df <- rbindlist(
  lapply(files, readRDS),use.names = TRUE, fill = TRUE)

files <- list.files(
  in_dir,
  pattern = "^best.*\\.rds$",
  full.names = T)
best_tune_par <- rbindlist(
  lapply(files, readRDS),use.names = TRUE, fill = TRUE)

ggplot(plot_df, aes(x=lambda,y=value)) +
  facet_wrap(.~place, ncol=4)+
  geom_point(
      aes(color=metric),
      shape=1, alpha=0.3
  ) +
  geom_point(
      data=best_tune_par,
      aes(x=lambda,y=best_val,fill=metric),shape=22,size=3) +
  theme_bw() + ylab("Dev. Ratio") +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    legend.position="bottom")
  
ggsave(file.path(out_dir, "lambdaProc.png"),width=8, height=10)

# # ------------------------------------------------------------------------
# # yuima lags
# # ------------------------------------------------------------------------
# files <- list.files(
#   in_dir,
#   pattern = "^best_mod_.*\\.rds$",
#   full.names = T)
# df <- rbindlist(
#   lapply(files, readRDS),use.names = TRUE, fill = TRUE)
# lags <- df[metric=="p_dev",c("place","yuima_est")] |> 
#     unnest(cols = yuima_est) |> select(place, var, lag)
# x <- df[metric=="p_dev",c("place","yuima_df")] |> 
#     unnest(cols = yuima_df) |> gather(var, value, -c(place,year.mon))
# df <- left_join(x,lags)

# plot_df <- c()
# # gdelt
# temp <- df[grepl("gdelt", df$var),]
# parts <- strsplit(temp$var, "\\.")
# temp$what <- sapply(parts, function(x) x[length(x)-1])
# plot_df <- rbind(plot_df,temp)
# # spei
# temp <- df[grepl("SPEI", df$var),]
# parts <- strsplit(temp$var, "\\.")
# temp$what <- sapply(parts, function(x) x[length(x)-1])
# plot_df <- rbind(plot_df,temp)
# # smi
# temp <- df[grepl("moist", df$var),]
# parts <- strsplit(temp$var, "\\.")
# temp$what <- sapply(parts, function(x) x[(length(x)-3)])
# plot_df <- rbind(plot_df,temp)
# # ibc
# temp <- df[grepl("n_ibc", df$var),]
# parts <- strsplit(temp$var, "\\.")
# temp$what <- sapply(parts, function(x) x[1])
# plot_df <- rbind(plot_df,temp)
# # ibc
# temp <- df[grepl("rec_rate", df$var),]
# parts <- strsplit(temp$var, "\\.")
# temp$what <- sapply(parts, function(x) x[1])
# plot_df <- rbind(plot_df,temp)

# # # line plot
# # ggplot(plot_df |> filter(!is.na(value) ), 
# #     aes(x=year.mon, y=value,group=var) ) +
# #   facet_wrap(.~place,nrow=6)+
# #   geom_line(aes(color=what,alpha=lag), size=.6,
# #           show.legend=T)+
# #   scale_color_brewer(palette = "Set1")+
# #   theme_bw() +
# #   theme(
# #     legend.title = element_blank(),
# #     legend.text = element_text(size=12),
# #     legend.position="bottom")+
# #   ylab(" ") + xlab("")
# # ggsave(file.path(out_dir, "predictors.png"),width=8, height=10)

# # tile polot
# ggplot(
#   plot_df |> filter(!is.na(value) ) |> 
#     group_by(place,what, lag) |> 
#     dplyr::summarise(value=mean(value)) |> 
#     mutate(lag=paste0("L",round(lag,0)),what=paste(what,lag,sep="-")), 
#     aes(x=place, y=what) ) +
#   # facet_wrap(.~place,nrow=2)+
#   geom_tile(aes(fill=value),color="grey50") +
#   scale_fill_gradient2(
#     low = "darkred",     # color for low values
#     mid = "yellow",    # color for midpoint
#     high = "darkgreen",     # color for high values
#     midpoint = 0      # adjust based on your data
#   ) +
#   theme_bw() +
#   theme(
#     legend.title = element_blank(),
#     legend.text = element_text(size=12),
#     legend.position="right",
#     panel.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank())+
#   ylab(" ") + xlab("")
# ggsave(file.path(out_dir, "predictors.png"),width=10, height=9)

# ------------------------------------------------------------------------
# pre-selected x-y corr
# ------------------------------------------------------------------------

files <- list.files(
  in_dir,
  pattern = "^tune.*\\.rds$",
  full.names = T)
df <- rbindlist(
  lapply(files, readRDS),use.names = TRUE, fill = TRUE)
df <- df[metric=="p_dev",c("place","yuima_est")] |> 
    unnest(cols = yuima_est) |> select(place, var, lag, llr, corr)

plot_df <- c()
# gdelt
temp <- df[grepl("gdelt", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[length(x)-1])
plot_df <- rbind(plot_df,temp)
# spei
temp <- df[grepl("SPEI", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[length(x)-1])
plot_df <- rbind(plot_df,temp)
# smi
temp <- df[grepl("moist", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[(length(x)-3)])
plot_df <- rbind(plot_df,temp)
# ibc
temp <- df[grepl("n_ibc", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[1])
plot_df <- rbind(plot_df,temp)
# ibc
temp <- df[grepl("rec_rate", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[1])
plot_df <- rbind(plot_df,temp)

# tile polot
ggplot(
  plot_df |> 
    group_by(place,what, lag) |> 
    dplyr::summarise(value=mean(corr, na.rm=T)) |> 
    mutate(lag=paste0("L",round(lag,0)),what=paste(what,lag,sep="-")), 
    aes(x=place, y=what) ) +
  # facet_wrap(.~place,nrow=2)+
  geom_tile(aes(fill=value),color="grey50") +
  scale_fill_gradient2(
    low = "darkred",     # color for low values
    mid = "yellow",    # color for midpoint
    high = "darkgreen",     # color for high values
    midpoint = 0      # adjust based on your data
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    legend.position="right",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  ylab(" ") + xlab("")
ggsave(file.path(out_dir, "x_y_corr.png"),width=10, height=9)


# ------------------------------------------------------------------------
# forecast performance
# ------------------------------------------------------------------------
files <- list.files(
  in_dir,
  pattern = "^forecast.*\\.rds$",
  full.names = T)
pred_df <- rbindlist(
  lapply(files, readRDS),use.names = TRUE, fill = TRUE)

# -------- Country
perf <- c()
for (i in 1:nrow(pred_df)) {
  input <- pred_df[i,]

  # --- train
  true <- input$true_tr[[1]] 
  pred <- input$pred_tr[[1]]
  err_tr <- true-pred

  # --- test
  true <- input$true_te[[1]] 
  pred <- input$pred_te[[1]]
  err_te <- true-pred

  # --- boot confidence interval for all
  tr <- unnest(
    input[,c("tr_date","true_tr","pred_tr")],
    cols = c(tr_date, true_tr, pred_tr))
  colnames(tr) <- c("date", "true", "pred")
  tr$set <- "tr"

  te <- unnest(
    input[,c("te_date","true_te","pred_te")],
    cols = c(te_date, true_te, pred_te))
  colnames(te) <- c("date", "true", "pred")
  te$set <- "te"

  tr_te <- rbind(tr,te)

  boot_times <- 1000
  sim_df <- expand.grid(date=tr_te$date,boot_id=seq_len(boot_times))
  set.seed(123)
  # note!!! use train error for boot
  sim_df$draw <- sample(err_tr, nrow(sim_df), replace = T) 

  # --- Conf. interval
  sim_df <- sim_df |> group_by(date) |> 
    summarise(lwr = quantile(draw, 0.025), upr = quantile(draw, 0.975)) 
  sim_df <- merge(sim_df, tr_te, by="date", all = T)
  sim_df <- sim_df |> 
    mutate(
      lwr=pred+lwr, upr=pred+upr,
    )
  conf <- sim_df |> as.data.table()

  # store results
  out <- input[,c("place","spl","n","k","lambda", "metric")] 
  out$fcst <- list(conf)

  perf <- rbind(perf,out)
}

# --- plot forecast
fcst_df <- perf[,c("place","fcst", "metric")] 
fcst_df <- unnest(fcst_df, cols = c(fcst))
start_date <- fcst_df |> group_by(place,metric) |> 
  summarise(min_date=min(date))
start_date <- max(start_date$min_date)
spl_date <- min(tail(sort(unique(fcst_df$date)),7))

fcst_cty <- filter(fcst_df,date>=start_date)
pcty <- ggplot(fcst_cty, aes(x=date, y=exp(true)*1000)) +
  facet_wrap(.~place, scales="free_y",ncol=3) +
  geom_ribbon(
    aes(ymin=exp(lwr)*1000, ymax=exp(upr)*1000, fill=metric),alpha=0.28) +
  geom_point(shape=1) +
  geom_line(
    aes(y=exp(pred)*1000, color=metric, linetype=metric)) +
  geom_vline(
    xintercept=spl_date, color="grey50", linewidth=.8, linetype="dashed") +
  theme_bw() + ylab("Asy. Seekers per 1000 Origin Pop. ") +
  theme(legend.position = "bottom", legend.title = element_blank()) 

fcst_all <- perf[,c("place","metric", "fcst")]
fcst_all <- unnest(fcst_all, cols=c(fcst))
fcst_all <- filter(fcst_all, date>=start_date)
fcst_all <- fcst_all |> group_by(metric,date,set) |> 
  dplyr::summarise(
    lwr=sum(exp(lwr)*1000),upr=sum(exp(upr)*1000),
    pred=sum(exp(pred)*1000),true=sum(exp(true)*1000))
pall <-  ggplot(fcst_all, aes(x=date, y=true)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=metric),alpha=0.28) +
  geom_point(shape=1) +
  geom_line(
    aes(y=pred, color=metric, linetype=metric)) +
  geom_vline(
    xintercept=spl_date, color="grey50", linewidth=.8, linetype="dashed") +
  theme_bw() + ylab("") +
  theme(legend.position = "none") 

plot_grid(pall, pcty, align="v", ncol=1, axis = "bt", 
          rel_heights=c(2,8))
ggsave(file.path(out_dir, "PredTS.agg.png"),width=9, height=12)


# --- plot SSE
fcst_all$place <- "All.Dest"
fcst_df <- rbind(fcst_cty,fcst_all)
sse_df <- fcst_df |> group_by(place,set, metric) |> 
  dplyr::summarise(sse=sum((true-pred)^2)) |> 
  spread(metric,sse) |> 
  mutate(
    metric=(p_dev-dev)/dev*100,
    color=ifelse(place=="All.Dest","1","0"),
    set=ifelse(set=="tr","1. Train", "2. Test")
  )
psse <- ggplot(sse_df, aes(x = place, y = metric)) +
  geom_col(
    position = "dodge", aes(fill=color),show.legend=F,
    color="black", width = .5) +
  facet_wrap(~set, scales="free_x") +
  theme_bw() +
  labs(y = "% Diff. in SSE \n (Ref. Dev.Ratio Model)",x="") +
  geom_hline(yintercept=0, color="black", linewidth=.8)+
  coord_flip() 

# --- plot IS
prob_a <- 0.05
is_df <- fcst_df |> 
  mutate(
    out_upr=ifelse(true>upr,1,0),
    out_lwr=ifelse(true<lwr,1,0),
    score=(upr-lwr) + 
      (2/prob_a)*(lwr-true)*out_lwr + 
      (2/prob_a)*(true-upr)*out_upr
  ) 
is_df <- is_df |> group_by(place,set, metric) |> 
  summarise(score=mean(score)) |> spread(metric,score) |> 
  mutate(
    metric=(p_dev-dev)/dev*100,
    color=ifelse(place=="All.Dest","1","0"),
    set=ifelse(set=="tr","1. Train", "2. Test")
  )
pis <- ggplot(is_df, aes(x = place, y = metric)) +
  geom_col(
    position = "dodge", aes(fill=color),show.legend=F,
    color="black", width = .5) +
  facet_wrap(~set, scales="free_x") +
  theme_bw() +
  labs(y = "% Diff. in Pred. Int. Score \n (Ref. Dev.Ratio Model)",x="") +
  geom_hline(yintercept=0, color="black", linewidth=.8)+
  coord_flip() 

plot_grid(psse, pis, align="v", ncol=1, axis = "bt", 
          labels = c("a", "b"))
ggsave(file.path(out_dir, "metric.png"), height=8, width=6)

# ------------------------------------------------------------------------
# Coef. origin map
# ------------------------------------------------------------------------
files <- list.files(
  in_dir,
  pattern = "^best_mod_.*\\.rds$",
  full.names = T)

df <- rbindlist(
  lapply(files, readRDS),use.names = TRUE, fill = TRUE)
df <- df[metric=="p_dev", c("mod","lambda")]

# get best lambda index
df$which <- mapply(function(model, lam) {
  which(model$lambda == lam)
}, df$mod, df$lambda)

# get coef at best lambda
df$coef <- mapply(
  function(model, w) {
    temp <- coef(model)[,w]
    temp <- coef(model)[,w] 
    temp <- tibble(var=names(temp),coef=temp) |> 
      filter(!grepl("Intercept", var) & coef!=0)
    temp <- list(temp)
  }, 
  df$mod, df$which)
df <- df[,c("coef")] |> unnest(cols = coef)

# get what and loc
my.df <- c()
# gdelt
temp <- df[grepl("gdelt", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[length(x)-1])
temp$loc <- sapply(parts, function(vec) {
  if (length(vec) > 3) {
    paste(vec[2:(length(vec) - 2)], collapse = " ")
  } else {
    NA
  }
})
my.df <- rbind(my.df,temp)
# spei
temp <- df[grepl("SPEI", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[length(x)-1])
temp$loc <- sapply(parts, function(vec) {
  if (length(vec) > 2) {
    paste(vec[1:(length(vec) - 2)], collapse = " ")
  } else {
    NA
  }
})
my.df <- rbind(my.df,temp)
# smi
temp <- df[grepl("moist", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[(length(x)-3)])
temp$loc <- sapply(parts, function(vec) {
  if (length(vec) > 4) {
    paste(vec[1:(length(vec) - 4)], collapse = " ")
  } else {
    NA
  }
})
my.df <- rbind(my.df,temp)

# coef mean
my.df <- my.df |> 
  group_by(loc, what) %>%
  dplyr::summarise(coef=mean(coef, na.rm=T))

# spatial layer
som <- readRDS(file.path(getwd(),"gadm36_SOM_2_sp.rds")) 
map <-st_as_sf(som)

#-- correct inconsistent names
true.name <- sort(unique(map$NAME_2))
temp <- my.df %>% 
  mutate(loc=ifelse(grepl("Adan ",loc), "Aadan",loc),
         loc=ifelse(grepl("Banadir",loc), "Mogadisho",loc),
         loc=ifelse(grepl("Baydhaba",loc), "Baydhabo",loc),
         loc=ifelse(grepl("Belet Weyne",loc), "Beled Weyn",loc),
         loc=ifelse(grepl("Belet Xaawo",loc), "Beled Xaawo",loc),
         loc=ifelse(grepl("Borama",loc), "Boorama",loc),
         loc=ifelse(grepl("Bossaso",loc), "Bosaaso",loc),
         loc=ifelse(grepl("Bulo Burto",loc), "Buulo Burdo",loc),
         loc=ifelse(grepl("Burco",loc), "Burao",loc),
         loc=ifelse(grepl("Buur Hakaba",loc), "Buur Xakaba",loc),
         loc=ifelse(grepl("Cabudwaaq",loc), "Caabudwaaq",loc),
         loc=ifelse(grepl("Caluula",loc), "Calawla",loc),
         loc=ifelse(grepl("Ceel Afweyn",loc), "Ceel-Afwein",loc),
         loc=ifelse(grepl("Doolow",loc), "Dolow",loc),
         loc=ifelse(grepl("Gaalkacyo",loc), "Gaalkacayo",loc),
         loc=ifelse(grepl("Galdogob",loc), "Goldogob",loc),
         loc=ifelse(grepl("Gebiley",loc), "Gabiley",loc),
         loc=ifelse(grepl("Laas Caanood",loc), "Lascaanod",loc),
         loc=ifelse(grepl("Laasqoray",loc), "Badhan",loc),
         loc=ifelse(grepl("Lughaye",loc), "Lughaya",loc),
         loc=ifelse(grepl("Luuq",loc), "Luuk",loc),
         loc=ifelse(grepl("Owdweyne",loc), "Oodweyne",loc),
         loc=ifelse(grepl("Sablaale",loc), "Sablale",loc),
         loc=ifelse(grepl("Sheikh",loc), "Sheekh",loc),
         loc=ifelse(grepl("Tayeeglow",loc), "Tiyeeglow",loc),
         loc=ifelse(grepl("Waajid",loc), "Wajid",loc),)
my.df <- temp
my.df <- my.df %>% dplyr::rename(NAME_2=loc) 

# plot
plot_df <- merge(
  map[,c("NAME_2","geometry")],my.df,by="NAME_2",all.x=T) |> 
  filter(!is.na(coef))
plot_df$cent <- st_centroid(plot_df$geometry)

# compute number of districts
n.dist <- plot_df %>% 
  group_by(what) %>% 
  dplyr::summarise(count=max(row_number()),
                    avg=round(mean(coef), 2),
                    up=round(max(coef), 2),
                    lo=round(min(coef), 2)) %>% 
  mutate(
    lab=paste(
    paste("N.Dist. = ",count,sep=""),
    paste("Avg.Coef. = ",avg,sep=""),
    paste("Coef.Range = ","[",lo, ":", up, "]", sep=""),
    sep="\n")
    )
coords <- sf::st_coordinates(n.dist)
n.dist$max_long <- max(coords[, "X"])
n.dist$min_lat <- min(coords[, "Y"])
    
ggplot() +
  facet_wrap(.~what, nrow=2) +
  geom_sf(data = plot_df, 
    aes(fill=coef), linewidth = 0.1) +
  geom_sf(data = map, fill=NA, color = "grey20", linewidth = 0.1)  +
  scale_fill_gradient2(
    low = "darkred",mid = "lightyellow", high = "darkgreen",
    na.value="white"
    ) +
  theme_map() +
  theme(
    plot.background = element_rect(fill = "#e6f2ff", color = NA),
    panel.background = element_rect(fill = "#e6f2ff", color = NA) ) +
  geom_text(data = n.dist, 
    aes(x = max_long-3, y = min_lat+2, label = lab),
      size= 4, show.legend = F) +
  geom_sf_text(
    data = plot_df, 
    aes(geometry = cent, label = NAME_2, size=abs(coef),alpha=abs(coef)),
    color = "grey20", show.legend = F)
ggsave(file.path(out_dir, "coef.origin.map.png"), height=10, width=15)

# ------------------------------------------------------------------------
# Coef. dest
# ------------------------------------------------------------------------
files <- list.files(
  in_dir,
  pattern = "^tune.*\\.rds$",
  full.names = T)

df <- rbindlist(
  lapply(files, readRDS),use.names = TRUE, fill = TRUE)
df <- df |> filter(metric=="p_dev") |>
  group_by(place, spl) |> filter(value==max(value)) |> 
  select(place, spl, mod, lambda)
  
# get best lambda index
df$which <- mapply(function(model, lam) {
  which(model$lambda == lam)
}, df$mod, df$lambda)

# get coef at best lambda
df$coef <- mapply(
  function(model, w) {
    temp <- coef(model)[,w]
    temp <- coef(model)[,w] 
    temp <- tibble(var=names(temp),coef=temp) |> 
      filter(!grepl("Intercept", var) & coef!=0)
    temp <- list(temp)
  }, 
  df$mod, df$which)
df <- df[,c("place", "spl", "coef")] |> unnest(cols = coef)

# get what and loc
my.df <- c()
# gdelt
temp <- df[grepl("gdelt", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[length(x)-1])
my.df <- rbind(my.df,temp)
# spei
temp <- df[grepl("SPEI", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[length(x)-1])
my.df <- rbind(my.df,temp)
# smi
temp <- df[grepl("moist", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[(length(x)-3)])
my.df <- rbind(my.df,temp)
# smi
temp <- df[grepl("moist", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[(length(x)-3)])
my.df <- rbind(my.df,temp)
# smi
temp <- df[grepl("moist", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[(length(x)-3)])
my.df <- rbind(my.df,temp)
# ibc
temp <- df[grepl("ibc", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[(length(x)-3)])
my.df <- rbind(my.df,temp)
# rec_rate
temp <- df[grepl("rec_rate", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[(length(x)-3)])
my.df <- rbind(my.df,temp)

# coef mean
my.df <- my.df |> 
  group_by(place, spl, what) %>%
  dplyr::summarise(coef=mean(coef, na.rm=T))

# merge cvlist
files <- list.files(
  in_dir,
  pattern = "^cv.*\\.rds$",
  full.names = T)

extract_start_date<- function(file_path) {
  place <- sub(".*cv_(.*)\\.rds", "\\1", file_path)
  data <- readRDS(file_path)
  dt <- data.table(
    place = place,
    spl = names(data),
    start_date = as.Date(sapply(data, min))
  )
  return(dt)
}
cv <- rbindlist(lapply(files, extract_start_date))

plot_df <- merge(my.df,cv, by= c("place","spl"), all.x=T)
ggplot(plot_df, aes(x=start_date,y=what,fill=coef)) +
  geom_tile(color="grey20") +
  facet_wrap(.~place, ncol=4) +
  scale_fill_gradient2(low="darkred",mid="#FFFFCC", high="darkgreen") +
  theme_bw() + xlab("")+ylab("") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank() ) +
  scale_x_date(labels = date_format("%y-%m"), breaks=pretty_breaks())
ggsave(file.path(out_dir, "coef.dest.png"), height=9, width=8)


# ------------------------------------------------------------------------
# IF space
# ------------------------------------------------------------------------
files <- list.files(
  in_dir,
  pattern = "^tune.*\\.rds$",
  full.names = T)

df <- rbindlist(
  lapply(files, readRDS),use.names = TRUE, fill = TRUE)
df <- df |> filter(metric=="p_dev") |>
  group_by(place, spl) |> filter(value==max(value)) |> 
  select(place, spl, mod, lambda)
  
# get best lambda index
df$which <- mapply(function(model, lam) {
  which(model$lambda == lam)
}, df$mod, df$lambda)

# get coef at best lambda
df$coef <- mapply(
  function(model, w) {
    temp <- coef(model)[,w]
    temp <- coef(model)[,w] 
    temp <- tibble(var=names(temp),coef=temp) |> 
      filter(!grepl("Intercept", var) )
    temp <- list(temp)
  }, 
  df$mod, df$which)
df <- df[,c("place", "spl", "coef")] |> unnest(cols = coef)

# get what and loc
my.df <- c()
# gdelt
temp <- df[grepl("gdelt", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[length(x)-1])
temp$loc <- sapply(parts, function(vec) {
  if (length(vec) > 3) {
    paste(vec[2:(length(vec) - 2)], collapse = " ")
  } else {
    NA
  }
})
my.df <- rbind(my.df,temp)
# spei
temp <- df[grepl("SPEI", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[length(x)-1])
temp$loc <- sapply(parts, function(vec) {
  if (length(vec) > 2) {
    paste(vec[1:(length(vec) - 2)], collapse = " ")
  } else {
    NA
  }
})
my.df <- rbind(my.df,temp)
# smi
temp <- df[grepl("moist", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[(length(x)-3)])
temp$loc <- sapply(parts, function(vec) {
  if (length(vec) > 4) {
    paste(vec[1:(length(vec) - 4)], collapse = " ")
  } else {
    NA
  }
})
my.df <- rbind(my.df,temp)
# ibc
temp <- df[grepl("ibc", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[(length(x)-3)])
temp$loc <- sapply(parts, function(vec) {
  if (length(vec) > 4) {
    paste(vec[1:(length(vec) - 4)], collapse = " ")
  } else {
    NA
  }
})
my.df <- rbind(my.df,temp)
# rec_rate
temp <- df[grepl("rec_rate", df$var),]
parts <- strsplit(temp$var, "\\.")
temp$what <- sapply(parts, function(x) x[(length(x)-3)])
temp$loc <- sapply(parts, function(vec) {
  if (length(vec) > 4) {
    paste(vec[1:(length(vec) - 4)], collapse = " ")
  } else {
    NA
  }
})
my.df <- rbind(my.df,temp)
my.df <- my.df |> mutate(fl=paste(place,loc,sep="-"))

#-- var selected frequency
x <- my.df %>% 
  dplyr::mutate(varFreq=ifelse(coef==0,0,1) )  %>% 
  # start aggregate
  group_by(what, fl) %>%
  dplyr::summarise(varFreq=mean(varFreq)) 

#-- vip per model (i.e., per dest and per fold)
y <- my.df %>% filter(coef!=0) |> 
  group_by(place, spl) %>%
  dplyr::mutate(vip=abs(coef),
                vip=vip/sum(vip,na.rm=T)) %>%
  # start aggregate
  group_by(what, fl) %>%
  dplyr::summarise(vip=mean(vip,na.rm=T)) 


#-- plot df
plot.df <- left_join(x,y) |> filter(!is.na(vip))

  ## create thresholds
sigLevel <- .5
# varFreq.m <- quantile(plot.df$varFreq, probs=sigLevel)|> round(2)
# vip.m <- quantile(plot.df$vip, probs=sigLevel) |> round(2)
varFreq.m <- .5
vip.m <- .5

  ## classify quan qual
plot.df <- plot.df %>% 
  mutate(
    qq=ifelse(vip>vip.m & varFreq<=varFreq.m,"Lo.Prob-Hi.Imp",
              ifelse(vip>vip.m & varFreq>varFreq.m,"Hi.Prob-Hi.Imp",
                     ifelse(vip<=vip.m & varFreq>varFreq.m,"Hi.Prob-Lo.Imp", 
                            "Lo.Prob-Lo.Imp")
                     )
              )
    )

  ## something like confusion matrix
confusion <- left_join(
  plot.df %>% group_by(what, qq) %>% 
    dplyr::summarise(s=max(row_number(), na.rm = T)),
  plot.df %>% group_by(what) %>% 
    dplyr::summarise(N=max(row_number(), na.rm=T)) ) %>% 
  mutate(s=round(s/N*100,0),
         s=paste0(s,"%")) %>% 
  mutate(pos.x=case_when(grepl("Hi.Prob",qq) ~ varFreq.m +.1,
                         grepl("Lo.Prob",qq) ~ varFreq.m -.1
                         ),
         pos.y=case_when(grepl("Hi.Imp",qq) ~ vip.m +.03,
                         grepl("Lo.Imp",qq) ~ vip.m -.03
                         ) )

ggplot(plot.df, 
       aes(x=(varFreq), y=(vip))) +
    facet_wrap(.~ what, ncol=3) +
    geom_hline(yintercept = vip.m, linetype="dashed",color="grey60") +
    geom_vline(xintercept = varFreq.m, linetype="dashed",color="grey60")+
    geom_point(aes(color=qq),alpha=.68, size=2, shape=1)+
    geom_text(data=confusion, aes(x=pos.x,y=pos.y,label = s)) +
    ggrepel::geom_text_repel(
      aes(label = paste(fl, sep=""),color=qq),
      size=2.8, alpha=1) +
    theme_bw() + 
    xlab("Frequency Variable Selected (per Fold)")+
    ylab("Relative Rank of Variable Importance (per Fold)") +
    theme(
      legend.position = "bottom", legend.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank() )+
    scale_shape_manual(values=1:length(unique(plot.df$what)) ) +
    scale_color_brewer(palette = "Dark2")
ggsave(file.path(out_dir, "IF.vip.space.png"), width=8, height=8)

