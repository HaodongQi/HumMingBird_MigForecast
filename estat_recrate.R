
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

library(pacman)
pacman::p_load(jsonlite, zoo, tidyverse, countrycode, data.table)

js.url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/migr_asydcfstq/1.0/*.*.*.*.*.*.*?c[freq]=Q&c[unit]=PER&c[citizen]=EXT_EU27_2020&c[sex]=T&c[age]=TOTAL&c[decision]=TOTAL,POS&c[geo]=EU27_2020,BE,BG,CZ,DK,DE,EE,IE,EL,ES,FR,HR,IT,CY,LV,LT,LU,HU,MT,NL,AT,PL,PT,RO,SI,SK,FI,SE,IS,LI,NO,CH,UK,ME&c[TIME_PERIOD]=2025-Q3,2025-Q2,2025-Q1,2024-Q4,2024-Q3,2024-Q2,2024-Q1,2023-Q4,2023-Q3,2023-Q2,2023-Q1,2022-Q4,2022-Q3,2022-Q2,2022-Q1,2021-Q4,2021-Q3,2021-Q2,2021-Q1,2020-Q4,2020-Q3,2020-Q2,2020-Q1,2019-Q4,2019-Q3,2019-Q2,2019-Q1,2018-Q4,2018-Q3,2018-Q2,2018-Q1,2017-Q4,2017-Q3,2017-Q2,2017-Q1,2016-Q4,2016-Q3,2016-Q2,2016-Q1,2015-Q4,2015-Q3,2015-Q2,2015-Q1,2014-Q4,2014-Q3,2014-Q2,2014-Q1,2013-Q4,2013-Q3,2013-Q2,2013-Q1,2012-Q4,2012-Q3,2012-Q2,2012-Q1,2011-Q4,2011-Q3,2011-Q2,2011-Q1,2010-Q4,2010-Q3,2010-Q2,2010-Q1,2009-Q4,2009-Q3,2009-Q2,2009-Q1,2008-Q4,2008-Q3,2008-Q2,2008-Q1&compress=false&format=json&lang=en"
json_data <- fromJSON(js.url, flatten = T)

# values
values <- unlist(json_data$value) %>% as.data.frame()
colnames(values) <- "value"
values$row.id <- as.numeric(rownames(values))

index.df <- names(json_data$dimension[[1]]$category$label) %>% as_tibble()
colnames(index.df) <- json_data$id[[1]]

for (i in 2:length(json_data$dimension)) {
  temp <- names(json_data$dimension[[i]]$category$label) %>% as_tibble()
  colnames(temp) <- json_data$id[[i]]
  index.df <- expand_grid(index.df,temp)
}

index.df <- index.df %>% mutate(row.id=row_number()-1)

# final df
df <- left_join(index.df, values) %>% filter(!is.na(value))
df <- df[!grepl("EU", df$geo),]
df <- df |> select(-row.id) |> spread(decision, value) |> 
  mutate(rec_rate=POS/TOTAL) |> select(geo, time,rec_rate) |> distinct()

# convert quarterly to monthly

# Define quarter to month mapping
quarter_months <- list(
  "Q1" = c("01", "02", "03"),
  "Q2" = c("04", "05", "06"),
  "Q3" = c("07", "08", "09"),
  "Q4" = c("10", "11", "12")
)

# Expand quarterly data to monthly
df_monthly <- df %>%
  rowwise() %>%
  mutate(months = list(quarter_months[[str_extract(time, "Q[1-4]")]]),
         year = str_extract(time, "^\\d{4}")) %>%
  unnest(months) %>%
  mutate(time = paste0(year, "-", months),
         rec_rate = ifelse(is.na(rec_rate), NA, rec_rate)) %>%
  select(-months, -year) 

df_monthly$year.mon <- as.Date(as.yearmon(df_monthly$time, "%Y-%m"), frac = 1)
df_monthly <- df_monthly |> select(-time) |> 
  filter(year.mon> 2010)

df_monthly <- df_monthly |> dplyr::rename(destination=geo) |> 
  mutate(destination=countrycode(destination, origin = "iso2c", destination = "iso3c"))

fwrite(df_monthly, file.path(getwd(), "estat_recrate.csv"))
