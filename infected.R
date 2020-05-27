library(rvest)
library(dplyr)
library(readr) 
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

new_data <- temp[[1]][,c(1,2,6)] 
names(new_data) <- c("Bundesland", "Infizierte", "Tote")
new_data <- new_data %>%
  filter(!(Bundesland %in% c("Gesamt", ""))) %>%
  mutate(Tote = as.numeric(stringr::str_replace(Tote, "\\.", "")),
         Infizierte = as.numeric(stringr::str_replace(Infizierte, "\\.", "")),
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
  group_by(Bundesland) %>%
  mutate(`Neue Fälle (7 Tage)` = Infizierte - lag(Infizierte, 7)) %>% 
  rename("Bestätigte Fälle" = Infizierte) %>%
  pivot_longer(cols = c(`Neue Fälle (7 Tage)`, `Bestätigte Fälle`, Tote),
               names_to = "Messwert",
               values_to = "absolut") %>%
  mutate(`pro 100k EW` = absolut / bl[Bundesland]) %>%
  pivot_longer(cols = c(absolut, `pro 100k EW`),
               names_to = "count",
               values_to = "Anzahl") %>%
  ggplot(aes(x = Datum,
             y = Anzahl,
             color = Bundesland,
             group = Bundesland)) +
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
  labs(title = "Tägliche und kumulierte Covid-19 Fälle nach Bevölkerung je Bundesland",
       y = "Anzahl",
       caption = paste0("Einwohner: Destatis 2018 | Covid-19: RKI ", 
                        format(Sys.Date(), "%d.%m.%Y"))) +
  facet_wrap(forcats::fct_relevel(Messwert, "Neuinfektionen") ~count, scales = "free_y", ncol = 2) +
  theme_light()
ggsave("plot.jpg", plot = plot,
       width = 12, height = 12)
