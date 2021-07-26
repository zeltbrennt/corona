library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

setwd("/home/pi/corona")

# Neue Daten einlesen
RKI_COVID19 <-  read_csv("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data",
                         col_types = cols(Meldedatum = col_date(format = "%Y/%m/%d %H:%M:%S"), 
                                          Datenstand = col_date(format = "%d.%m.%Y, %H:%M Uhr"), 
                                          Refdatum = col_date(format = "%Y/%m/%d %H:%M:%S")))
zensus <- read_csv("https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.csv",
                   col_types = cols(.default = col_skip(),
                                    BL = col_character(),
                                    county = col_character(),
                                    EWZ = col_integer(),
                                    EWZ_BL = col_integer())) %>%
  mutate_if(is.numeric, .funs = function(x) x / 100000) %>%
  pivot_wider(names_from = BL, values_from = EWZ_BL) %>%
  pivot_wider(names_from = county, values_from = EWZ) %>%
  summarise_all(sum, na.rm = T) %>%
  unlist()

# Tabellen wegschreiben
# write.csv(RKI_COVID19, paste0("dump/", Sys.Date(), ".csv"))

new_data <- RKI_COVID19 %>% 
  group_by(Bundesland, Meldedatum) %>% 
  summarise(Infizierte = sum(AnzahlFall),
            Tote = sum(AnzahlTodesfall)) %>% 
  ungroup() %>%
  complete(Meldedatum = seq(min(Meldedatum), max(Meldedatum), by = "day"),
           Bundesland = sort(names(zensus[1:16])),
           fill = list(Infizierte = 0,
                       Tote = 0)) %>% 
  group_by(Bundesland) %>%
  mutate(Infizierte_gesamt = cumsum(Infizierte),
         Tote_gesamt = cumsum(Tote)) 

file <- list.files(pattern = "RKI-.*\\.csv")
file.copy(from = file, to = paste0("archive/", file))
file.remove(file)


# Tabelle mit neuen Fällen der letzen 7 Tage pro 100.000 Einwohner
c19_woche_rel_gesamt <- new_data %>%
  group_by(Meldedatum) %>%
  summarise(faelle = sum(Infizierte)) %>%
  mutate(kum = cumsum(faelle),
         woche = (kum - coalesce(lag(kum, 7), 0)),
         Gesamt = woche / sum(zensus[1:16]))  %>%
  select(Meldedatum, Gesamt)

c19_woche_rel <- new_data %>%
  mutate(woche = (Infizierte_gesamt - coalesce(lag(Infizierte_gesamt, 7), 0)),
         woche100k = woche / zensus[Bundesland])  %>% 
  select(c(Meldedatum, Bundesland, woche100k)) %>%
  pivot_wider(names_from = Bundesland, values_from = woche100k) %>% 
  arrange(Meldedatum) %>%
  left_join(c19_woche_rel_gesamt) 

write_csv(c19_woche_rel,
                 "corona_relativ.csv",
                 na = "")
write_csv(new_data, paste0("RKI-", Sys.Date(), ".csv"))

# extrawunsch
new_data %>% 
  pivot_longer(cols = -c(Meldedatum, Bundesland)) %>%
  pivot_wider(names_from = Meldedatum, values_from = value) %>%
  arrange(name, Bundesland) %>%
  write_csv("pivot.csv")

# Verlauf
verlauf <- RKI_COVID19 %>%
  group_by(Bundesland, Meldedatum) %>%
  filter(NeuerFall >= 0) %>%
  summarise(faelle = sum(AnzahlFall)) %>%
  complete(Meldedatum = seq(min(Meldedatum), max(Meldedatum), "day"),
           fill = list(faelle = 0)) %>%
  mutate(kum = cumsum(faelle),
         woche = (kum - coalesce(lag(kum, 7), 0)),
         woche100k = woche / zensus[Bundesland])  %>%
  ggplot(aes(x = Meldedatum, y = woche100k, fill = Bundesland)) +
  geom_area(position = position_dodge(width = 0), alpha = 0.7, color = "white") +
  scale_fill_viridis_d() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(expand = c(0,0)) +
  labs(title = "Fallzahlentwicklung nach Bundesländern",
       subtitle = "pro 100.000 Einwohner",
       y = "7 Tage Inzidenzrate",
       caption = paste("Quelle: RKI | Stand:", format(Sys.Date(),
                                                      "%d.%m.%Y"))) +
  theme_classic() +
  theme(legend.position = c(0.01,1),
        legend.justification = c(0,1),
        legend.background = element_rect(color = "black"),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray"))

ggsave(verlauf, filename = "01_verlauf.png", width = 14, height = 6)

# Altersverteilung
sex_m <- paste0("männlich: ",
            format(
              round(
                sum(RKI_COVID19$AnzahlFall[RKI_COVID19$Geschlecht == "M"]) / 
                  sum(RKI_COVID19$AnzahlFall[RKI_COVID19$Geschlecht %in% c("M", "W")]), 
                digits = 4) * 100, 
              decimal.mark = ","), 
            "%")
sex_w <- paste0("weiblich: ",
            format(
              round(
                sum(RKI_COVID19$AnzahlFall[RKI_COVID19$Geschlecht == "W"]) / 
                  sum(RKI_COVID19$AnzahlFall[RKI_COVID19$Geschlecht %in% c("M", "W")]), 
                digits = 4) * 100, 
              decimal.mark = ","), 
            "%")
alter_label = c()
for (a in sort(unique(RKI_COVID19$Altersgruppe))) {
  if (a != "unbekannt") {
    alter_label = c(alter_label, paste0(gsub("A", "", a), "\n(", 
                            format(
                              round(
                                sum(RKI_COVID19$AnzahlFall[RKI_COVID19$Altersgruppe == a]) /
                                  sum(RKI_COVID19$AnzahlFall[RKI_COVID19$Altersgruppe != "unbekannt"]),
                                digits = 4) * 100,
                              decimal.mark = ","), "%)"))
  }
}
alter <- RKI_COVID19 %>%
  group_by(Altersgruppe, Geschlecht, Meldedatum) %>%
  filter(NeuerFall >= 0) %>%
  summarise(faelle = sum(AnzahlFall)) %>%
  complete(Meldedatum = seq(min(Meldedatum), max(Meldedatum), "day"),
           fill = list(faelle = 0)) %>%
  mutate(kum = cumsum(faelle),
         woche = (kum - coalesce(lag(kum, 7), 0))) %>%
  filter(tolower(Geschlecht) != "unbekannt",
         tolower(Altersgruppe) != "unbekannt",
         woche > 10) %>%
  ggplot(aes(x = Meldedatum, y = woche, fill = Altersgruppe)) +
  facet_wrap(~factor(Geschlecht, levels = c("M", "W"), labels = c(sex_m, sex_w))) +
  geom_col(position = "fill", width = 1) +
  theme_classic() +
  theme(legend.position = c(0.5,1),
        legend.justification = c(0.5,1),
        legend.key = element_rect(size = 3, color = "white"),
        legend.key.height = unit(3.5, "lines"),
        panel.spacing.x = unit(5.5, "lines")) +
  scale_fill_viridis_d(label = alter_label) +
  scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  scale_x_date(expand = c(0,0)) +
  labs(title = "Geschlechts- und Altersverteilung",
       subtitle = "Insgesamt und im Pandemieverlauf",
       y = "Anteil an 7 Tage Inzidenz",
       fill = "Altersgruppe",
       caption = paste("Quelle: RKI | Stand:", format(Sys.Date(),
                                                      "%d.%m.%Y")))

ggsave(alter, filename = "02_Alter_Geschlecht.png", width = 14, height = 6)


# Hotspots im zeitlichen Verlauf
landkreis <- RKI_COVID19 %>%
  group_by(Landkreis, Meldedatum) %>%
  filter(NeuerFall >= 0) %>%
  summarise(faelle = sum(AnzahlFall)) %>%
  complete(Meldedatum = seq(min(Meldedatum), max(Meldedatum), "day"),
           fill = list(faelle = 0)) %>%
  mutate(kum = cumsum(faelle),
         woche = (kum - coalesce(lag(kum, 7), 0)),
         woche100k = woche / zensus[Landkreis])

hotspot <- landkreis %>%
  filter(woche100k > 50) %>%
  ungroup() %>%
  count(Meldedatum) %>%
  right_join(c19_woche_rel_gesamt) %>%
  replace_na(list(n = 0)) %>%
  mutate(log_n = ifelse(log(n) == -Inf, 0, log(n))) %>%
  ggplot(aes(x = Meldedatum, y = Gesamt, fill = log_n)) +
  geom_col(width = 1) +
  scale_fill_viridis_c(breaks = c(log(2), log(8),log(32) , log(128)),
                       labels = c(2, 8, 32, 128)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(expand = c(0,0)) +
  labs(title = "Anzahl der Hotspots in Abhängigkeit von Gesamtinfektionszahl",
       subtitle = "Hotspots = Landkreis mit mehr als 50 Neuinfektionen pro 100.000 EW innerhalb einer Woche",
       y = "7 Tage Inzidenz pro 100.000 EW",
       fill = "Anzahl Hotspots",
       caption = paste("Quelle: RKI | Stand:", format(Sys.Date(),
                                                      "%d.%m.%Y"))) +
  theme_classic() +
  theme(legend.position = c(0.01,1),
        legend.justification = c(0,1),
        legend.background = element_rect(color = "black"),
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))

ggsave(hotspot, filename = "03_Hotspots.png", width = 14, height = 6)

# Dispersion
dispersion <- landkreis %>%
  filter(woche100k > 50) %>%
  ungroup() %>%
  count(Meldedatum) %>%
  right_join(c19_woche_rel_gesamt) %>%
  replace_na(list(n = 0)) %>%
  mutate(dispersion = n / Gesamt,
         monat = as.factor(lubridate::month(Meldedatum, label = T))) %>%
  filter(lubridate::year(Meldedatum) == 2020) %>% 
  ggplot(aes(x = Gesamt, y = n,
             color = monat,
             shape = monat,
             group = monat)) +
  geom_point(size = 3) +
  scale_shape_manual(values = sort(unique(as.factor(lubridate::month(RKI_COVID19$Meldedatum))))) +
  stat_smooth(geom = "line", method = "loess", se = F, alpha = 0.8, show.legend = F) +
  scale_color_viridis_d() +
  labs(title = "Verteilungsfaktor im Infektionsverlauf im Jahr 2020",
       subtitle = "Hotspots = Landkreis mit mehr als 50 Neuinfektionen pro 100.000 EW innerhalb einer Woche",
       x = "7 Tage Inzidenz pro 100.000 EW bundesweit",
       y = "Anzahl Hotspots",
       color = "Monat",
       shape = "Monat",
       caption = paste("Quelle: RKI | Stand:", format(Sys.Date(),
                                                      "%d.%m.%Y"))) +
  theme_classic() +
  theme(legend.position = c(0.01,1),
        legend.justification = c(0,1),
        legend.background = element_rect(color = "black"),
        panel.grid.major = element_line(linetype = 'dotted', color = 'gray'))

ggsave(dispersion, filename = "04_Dispersion.png", width = 14, height = 6)

landkreis %>%
filter(Landkreis == "SK Dresden", 
       Meldedatum >= lubridate::today() - 14) %>%
  rename(Neuinfektionen = 'faelle',
         `Fälle insgesamt` = 'kum',
         `7 Tage Inzidenz` = 'woche',
         `7 Tage Inzidenz pro 100.000 EW` = 'woche100k') %>%
  write_csv("lage_dresden.csv")



