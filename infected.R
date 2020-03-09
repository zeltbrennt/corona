library(ggplot2)
library(rvest)
library(tidyr)
library(dplyr)
library(readr)

temp <- read_html("https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html") %>% 
  html_table() 

new_row <- temp[[1]][,1:2] %>%
  pivot_wider(names_from = Bundesland, values_from = `Fälle`) %>%
  mutate(Datum = Sys.Date())

file <- list.files(pattern = ".csv")
old_data <- read_csv(file)
if (grepl(as.character(Sys.Date()), file)) {
  new_data <- old_data
  new_data[nrow(new_data),] <- new_row
} else {
  file.copy(from = file, to = paste0("archive/", file))
  file.remove(file)
  new_data <- bind_rows(old_data, new_row)
}
# store new data in long format
write_csv(new_data, paste0(Sys.Date(), "-infizierte.csv"))

# create a little plot
pl <- new_data %>%
  pivot_longer(cols = -c(Datum, Gesamt), 
               names_to = "Bundesland", 
               values_to = "Anzahl Infizierte") %>%
  ggplot(aes(x = Datum, y = `Anzahl Infizierte`, color = Bundesland)) +
    geom_line() +
    labs(title = "Covid-19 Infektionen in Deutschland",
         caption = paste("Quelle: RKI | Stand:", Sys.Date()),
         x = "")
ggsave(filename = "verlauf.jpg",
       plot = pl,
       width = 20,
       height = 12,
       units = "cm")

