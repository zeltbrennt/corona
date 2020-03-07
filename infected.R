library(stringr)
library(rvest)
library(tidyr)
library(dplyr)
library(readr)

temp <- read_html("https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html") %>% 
  html_table()

new_row <- temp[[1]][,1:2] %>%
  pivot_wider(names_from = Bundesland, values_from = `Fälle`) %>%
  mutate(Stand = Sys.Date())
old_data <- read_csv(paste0("archive/", Sys.Date()-1, "-infizierte.csv"))

old_data %>%
  bind_rows(new_row) %>%
  write_csv(paste0(Sys.Date(), "-infizierte.csv"))

# todo:
# - create plots
# - automate git update
