library(stringr)
library(rvest)
library(tidyr)
library(dplyr)
library(readr)

url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html"
temp <- read_html(url) %>% html_table()

new_row <- temp[[1]][,1:2] %>%
  pivot_wider(names_from = Bundesland, values_from = `Fälle`) %>%
  mutate(Stand = Sys.Date())
old_data <- read_csv(paste0(Sys.Date()-1, "-infizierte.csv"))

old_data %>%
  bind_rows(new_row) %>%
  write_csv(paste0(Sys.Date(), "-infizierte.csv"))

# todo:
# - old files into archive folder
# - create plots
