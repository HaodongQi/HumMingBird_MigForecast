
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

library(pacman)
pacman::p_load(zoo, tidyverse, countrycode, data.table)


library(readxl)
# Replace "your_file.xlsx" with the actual path to your file
df <- read_excel("ibc.xlsx", sheet = "Detections_of_IBC")

# Assuming df is your original data
df_long <- df |>  filter(Nationality=="Somalia") |> 
  pivot_longer(
    cols = matches("^[A-Z]{3}\\d{4}$"),  # Matches columns like JAN2009
    names_to = "year.mon",
    values_to = "n_ibc"
  ) |> group_by(year.mon) |> 
  dplyr::summarise(n_ibc=sum(n_ibc)) |> 
  mutate(
    # Convert month abbreviations to numbers and build a date string
    year.mon = as.Date(as.yearmon(year.mon, "%b%Y"), frac = 1)
  )

fwrite(df_long, file.path(getwd(), "ibc.csv"))
