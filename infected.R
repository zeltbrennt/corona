library(rvest)
library(dplyr)
library(readr) 
library(stringr)

setwd("/home/pi/corona")
temp <- read_html("https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html") %>%
  html_table() 
write.csv(temp, paste0("dump/", Sys.Date(), ".csv"))
# as long as format is not consitent through the week, the table will be dumped as is

new_data <- temp[[1]][,c(1,2,5)] 
names(new_data)<- c("Bundesland", "Infizierte", "Tote")
new_data <- new_data %>%
  mutate(Tote = str_replace(Tote, "\\.", ""),
         Tote = ifelse(Tote == "", 0, Tote),
         Tote = as.numeric(Tote),
         Infizierte = str_replace(Infizierte, "\\.", ""),
         Infizierte = as.numeric(Infizierte),
         Datum = Sys.Date()) %>%
  filter(!(Bundesland %in% c("Gesamt", "")))
new_data$Bundesland <- c("Baden-Württemberg",
                         "Bayern",
                         "Berlin",
                         "Brandenburg",
                         "Bremen",
                         "Hamburg",
                         "Hessen",
                         "Mecklenburg-Vorpommern",
                         "Niedersachsen",
                         "Nordrhein-Westfalen",
                         "Rheinland-Pfalz",
                         "Saarland",
                         "Sachsen",
                         "Sachsen-Anhalt",
                         "Schlesweig-Holstein",
                         "Thüringen")
file <- list.files(pattern = ".csv")
file.copy(from = file, to = paste0("archive/", file))
read_csv(file) %>%
  bind_rows(new_data) %>% 
  write_csv(paste0("RKI-", Sys.Date(), ".csv"))
file.remove(file)

# update_csv <- function(pattern, new_row) {
#   file <- list.files(pattern = pattern)
#   if (length(file) == 0){
#     new_data <- new_row
#   } else {
#     old_data = read_csv(file)
#     if (grepl(as.character(Sys.Date()), file)) {
#       new_data <- old_data
#       new_data[nrow(new_data),] <- new_row
#     } else {
#       if (!dir.exists(paste0("archive/", pattern))) dir.create(paste0("archive/", pattern))
#       file.copy(from = file, to = paste0("archive/", pattern, "/", file))
#       file.remove(file)
#       new_data <- bind_rows(old_data, new_row)
#     }
#   }
#   write_csv(new_data, paste0(Sys.Date(), "-", pattern, ".csv"))
#   return(new_data)
# }
# 
