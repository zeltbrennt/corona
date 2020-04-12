library(rvest)
library(dplyr)
library(readr) 
library(stringr)
library(tidyr) 
library(ggplot2)

setwd("/home/pi/corona")
temp <- read_html("https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html") %>%
  html_table() 
write.csv(temp, paste0("dump/", Sys.Date(), ".csv"))
# census in 100k
bl <- c("Baden-Württemberg"      = 110.70,
        "Bayern"                 = 130.77,
        "Berlin"                 = 36.45,
        "Brandenburg"            = 25.12,
        "Bremen"                 = 6.83,
        "Hamburg"                = 16.41,
        "Hessen"                 = 62.66,
        "Mecklenburg-Vorpommern" = 16.10,
        "Niedersachsen"          = 79.82,
        "Nordrhein-Westfalen"    = 179.33,
        "Rheinland-Pfalz"        = 40.85,
        "Saarland"               = 9.91,
        "Sachsen"                = 40.78,
        "Sachsen-Anhalt"         = 22.08,
        "Schleswig-Holstein"     = 28.97,
        "Thüringen"              = 21.43)

new_data <- temp[[1]][,c(1,2,5)] 
names(new_data) <- c("Bundesland", "Infizierte", "Tote")
new_data <- new_data %>%
  filter(!(Bundesland %in% c("Gesamt", ""))) %>%
  mutate(Tote = as.numeric(str_replace(Tote, "\\.", "")),
         Infizierte = as.numeric(str_replace(Infizierte, "\\.", "")),
         Datum = Sys.Date())
new_data$Bundesland <- names(bl)
file <- list.files(pattern = ".csv")
file.copy(from = file, to = paste0("archive/", file))
history <- read_csv(file) %>%
  bind_rows(new_data)  
write_csv(history, paste0("RKI-", Sys.Date(), ".csv"))
file.remove(file)
 
# plot
plot <- history %>%
  pivot_longer(cols = c(Infizierte, Tote),
               names_to = "Messwert",
               values_to = "Anzahl") %>%
  mutate(Relative_Anzahl = Anzahl / bl[Bundesland]) %>%
  ggplot(aes(x = Datum,
             y = Relative_Anzahl,
             color = Bundesland,
             group = Bundesland)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("#2f4f4f",
                                "#a0522d",
                                "#006400",
                                "#000080",
                                "#ff0000",
                                "#00ced1",
                                "#ffa500",
                                "#ffff00",
                                "#00ff00",
                                "#0000ff",
                                "#ff00ff", 
                                "#1e90ff",
                                "#dda0dd",
                                "#90ee90",
                                "#ff1493",
                                "#ffe4b5")) +
  labs(title = "Kumulierte Covid-19 Infektionen nach Bevölkerung je Bundesland",
       y = "Anzahl pro 100.000 Einwohner",
       caption = paste0("Einwohner: Destatis 2018 | Covid-19: RKI ", 
                        format(Sys.Date() -1, "%d.%m.%Y"))) +
  facet_wrap(~Messwert, nrow = 2, scales = "free_y") 
ggsave("plot.jpg", plot = plot,
       width = 10, height = 7)
