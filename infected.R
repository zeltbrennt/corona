library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggridges)

# Download Data #####
## Static Data #####
# Population
if (!file.exists("zensus.csv")) {
  download.file("https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.csv",
                "zensus.csv")
}
zensus <- read_csv("zensus.csv" )

# Shapefiles
if (interactive()) {
  if (!file.exists("Kreisgrenzen_2019.shp")) {
  library(httr)
  GET("https://opendata.arcgis.com/api/v3/datasets/248e105774144a27aca2dfbfe080fc9d_0/downloads/data?format=shp&spatialRefId=4326") %>%
    content() %>%
    writeBin("Kreisgrenzen.zip")
  utils::unzip("Kreisgrenzen.zip")
}
shp_forty <- rgdal::readOGR(dsn = "Kreisgrenzen_2019.shp", stringsAsFactors = F)  %>%
  broom::tidy(shp, region = "RS")
}

## Dynamic Data #####
# Vaccines
if (interactive()) {
  if (!file.exists("impfungen.csv") | Sys.Date() - as.Date(file.info("impfungen.csv")$mtime) >= 1) {
    download.file("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Bundeslaender_COVID-19-Impfungen.csv",
                  "impfungen.csv")
  }
  impf <- read_csv("impfungen.csv")
  
  # ICU
  if (!file.exists("its_betten.csv") | Sys.Date() - as.Date(file.info("its_betten.csv")$mtime) >= 1) {
    download.file("https://diviexchange.blob.core.windows.net/%24web/zeitreihe-deutschland.csv",
                  "its_betten.csv")
  }
  betten <- read_csv("its_betten.csv")
}

# Covid Incideces and deaths
if (!file.exists("RKI_COVID19.csv") | Sys.Date() - as.Date(file.info("RKI_COVID19.csv")$mtime) >= 1) {
  download.file("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data", 
                "RKI_COVID19.csv")
}
RKI_COVID19 <-  read_csv("RKI_COVID19.csv",
                         col_types = cols(Meldedatum = col_date(format = "%Y/%m/%d %H:%M:%S"),
                                          Datenstand = col_date(format = "%d.%m.%Y, %H:%M Uhr"),
                                          Refdatum = col_date(format = "%Y/%m/%d %H:%M:%S"))) 

# Process Data #####
## by state #####
state_day <- RKI_COVID19 %>%
  group_by(Bundesland, Meldedatum) %>%
  summarise(Infizierte = sum(AnzahlFall),
            Tote = sum(AnzahlTodesfall)) %>%
  ungroup() %>%
  complete(Meldedatum = seq(min(Meldedatum), max(Meldedatum), by = "day"),
           Bundesland = sort(unique(Bundesland)),
           fill = list(Infizierte = 0,
                       Tote = 0)) %>%
  group_by(Bundesland) %>%
  mutate(Infizierte_kum = cumsum(Infizierte),
         Tote_kum = cumsum(Tote))

state_week_100k <- state_day %>%
  left_join(zensus %>% distinct(BL, EWZ_BL), by = c("Bundesland" = "BL")) %>% 
  mutate(infiziert_woche = (Infizierte_kum - coalesce(lag(Infizierte_kum, 7), 0)), 
         infizierte_woche100k = infiziert_woche / (EWZ_BL / 100000)) 

## by county ####
if (interactive()) {
  county_day <- RKI_COVID19 %>%
    group_by(IdLandkreis, Meldedatum) %>%
    summarise(Infizierte = sum(AnzahlFall),
              Tote = sum(AnzahlTodesfall)) %>%
    ungroup() %>%
    complete(Meldedatum = seq(min(Meldedatum), max(Meldedatum), by = "day"),
             IdLandkreis = sort(unique(IdLandkreis)),
             fill = list(Infizierte = 0,
                         Tote = 0)) %>%
    group_by(IdLandkreis) %>%
    mutate(Infizierte_kum = cumsum(Infizierte),
           Tote_kum = cumsum(Tote))
  
  # summarise berlin as a city
  berlin <- county_day %>%
    filter(substr(IdLandkreis, 1, 2) == "11") %>%
    group_by(Meldedatum) %>%
    summarise_if(is.numeric, sum) %>%
    mutate(IdLandkreis = "11000")
  
  ewz_lk <- zensus %>%
    select(RS, EWZ) %>%
    filter(substr(RS, 1, 2) == "11") %>%
    summarise(EWZ = sum(EWZ)) %>%
    mutate(RS = "11000") %>%
    bind_rows(zensus %>% select(RS, EWZ) %>% filter(substr(RS, 1, 2) != "11"))
  
  county_week <- county_day %>%
    filter(substr(IdLandkreis, 1, 2) != "11") %>%
    bind_rows(berlin) %>%
    mutate(infiziert_woche = (Infizierte_kum - coalesce(lag(Infizierte_kum, 7), 0)), 
           tote_woche = (Tote_kum - coalesce(lag(Tote_kum, 7), 0))) %>%
    mutate(Kalenderwoche = (paste0(year(Meldedatum), stringr::str_pad(isoweek(Meldedatum), 2, "left", "0"))),
           Kalenderwoche = ifelse(Kalenderwoche == "202153", "202053", Kalenderwoche)) 
  
  county_week_100k <- county_week %>%
    left_join(ewz_lk, by = c("IdLandkreis" = "RS")) %>%
    mutate(infiziert_woche100k = infiziert_woche / (EWZ / 100000))  %>%
    select(c(Meldedatum, IdLandkreis, infiziert_woche100k))
  
  tote_woche <- county_week %>%
    group_by(Kalenderwoche) %>%
    summarise(tote_kw = sum(Tote)) %>%
    right_join(county_week) %>% 
    mutate(Meldedatum = Meldedatum + 7) %>%
    distinct(Kalenderwoche, Meldedatum, tote_kw)
  
  impfung <- impf %>%
    complete(Impfdatum = seq(min(county_week_100k$Meldedatum), max(county_week_100k$Meldedatum), by = "day"),
             Impfserie = 1:3,
             BundeslandId_Impfort = unique(sort(impf$BundeslandId_Impfort)),
             Impfstoff = unique(sort(impf$Impfstoff)),
             fill = list(Anzahl = 0)) %>%
    group_by(Impfdatum, BundeslandId_Impfort, Impfserie) %>%
    summarise(Anzahl = sum(Anzahl)) %>%
    group_by(BundeslandId_Impfort, Impfserie) %>%
    mutate(anz_kum = cumsum(Anzahl)) %>% 
    left_join(zensus %>% distinct(BL, EWZ_BL, BL_ID) %>% mutate(BL_ID = stringr::str_pad(BL_ID, 2, "left", 0)), by = c("BundeslandId_Impfort" = "BL_ID")) %>%
    drop_na() %>%
    mutate(Impfquote = anz_kum / EWZ_BL) %>%
    ungroup() %>%
    select(BL, Impfquote, Impfserie, Impfdatum) 
  
  intensiv <- betten %>%
    filter(Behandlungsgruppe == "ERWACHSENE",
           Datum != min(Datum)) %>%
    mutate(Corona = Aktuelle_COVID_Faelle_ITS / (Belegte_Intensivbetten + Freie_Intensivbetten),
           Sonstige = (Belegte_Intensivbetten - Aktuelle_COVID_Faelle_ITS) / (Belegte_Intensivbetten + Freie_Intensivbetten),
           Datum = as.Date(Datum)) %>%
    pivot_longer(cols = c(Sonstige, Corona)) %>%
    select(Datum, name, value) %>% 
    complete(Datum = seq(min(county_week_100k$Meldedatum), max(county_week_100k$Meldedatum), by = "day"),
             name = c("Sonstige", "Corona")) %>%
    group_by(name) %>%
    mutate(betten_kum = cumsum(coalesce(value, 0)),
           woche = (betten_kum - coalesce(lag(betten_kum, 7), 0)) / 7,
           woche = ifelse(woche == 0, NA, woche))
}
## by age #####
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
  summarise(faelle = sum(AnzahlFall)) %>%
  complete(Meldedatum = seq(min(Meldedatum), max(Meldedatum), "day"),
           fill = list(faelle = 0)) %>%
  mutate(kum = cumsum(faelle),
         woche = (kum - coalesce(lag(kum, 7), 0))) %>%
  filter(tolower(Geschlecht) != "unbekannt",
         tolower(Altersgruppe) != "unbekannt",
         woche > 10) 
# Output #####
# cleanup WD

# Verlauf
# to do: make this semi-3D (ggridges??)
verlauf_plot <- state_week_100k %>%
  ggplot(aes(y = forcats::fct_rev(Bundesland), x = Meldedatum, 
             height = infizierte_woche100k, fill = Bundesland, color = Bundesland)) +
  geom_density_ridges(stat = "identity", alpha = 0.5, ) +
  scale_fill_viridis_d(option = "H") +
  scale_color_viridis_d(option = "H") +
  labs(title = "7-Tage Inzidenz nach Bundesländern",
       subtitle = "pro 100.000 Einwohner",
       caption = paste("Quelle: RKI | Stand:", format(Sys.Date(),
                                                      "%d.%m.%Y"))) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_blank())

ggsave(verlauf_plot, filename = "01_verlauf.png", width = 14, height = 6)

# Altersverteilung

alter_plot <- alter %>%
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

ggsave(alter_plot, filename = "02_Alter_Geschlecht.png", width = 14, height = 6)


# auf besonderen Wunsch...
state_day %>% 
  pivot_longer(cols = -c(Meldedatum, Bundesland)) %>%
  pivot_wider(names_from = Meldedatum, values_from = value) %>%
  arrange(name, Bundesland) %>% 
  write_csv("pivot.csv")

## animated gif of pandemic, only executed on demand ####
if (interactive()) {
  library(gridExtra)
  library(magick)
  lan = ""
  limit_max = 3400
  while (!(lan %in% c("en", "de"))) {
    lan <- readline("lan of labels? Type 'en' for englisch, 'de' for german: ") 
  }
  if (max(county_week_100k$infiziert_woche100k) > limit_max) {
    stop("Maximum Incidence is greater than Limit of colour scale!")
  }
  # don't start from scratch every time
  images_path <- file.path("images", lan)
  
  if (length(list.files(path = images_path)) > 0) {
    a <- readline("Images found. Update? (y/n): ")
    if (a == "y") {
      start_here <- list.files(path = images_path, pattern = paste0(lan, "_copy")) %>% 
        tail(1) %>% 
        substr(6, 15) %>%
        as.Date() - 30
    } else if (a == "n") {
      c <- readline("This will remove all images. Continue? (y/n): ")
      if (c == "y") {
        if (dir.exists(images_path)) {
          file.remove(list.files(path = images_path, full.names = T))
        } else {
          dir.create(images_path, recursive = T)
        }
        start_here = as.Date("2020-02-20")
      }
    }
  }
  
  county_week_100k <- county_week_100k %>% filter(Meldedatum >= start_here)
  tote_woche <- tote_woche %>% filter(Meldedatum >= "2020-02-20")
  
  # Create Labels for plots
  if (lan == "en") {
    label_title = "Covid19-Pandemic in Germany since February 2020"
    label_inc =   "7-day incidence"
    label_death = "deaths per week"
    label_vacc =  c("first dose", "second dose", "booster")
    label_vac_t = c("vaccines by states")
    label_hosp =  "occupied beds in ICU"
    label_copy = "(c) /u/zeltbrennt | sources: RKI, DIVI, ESRI"
    intensiv <- intensiv %>%
      mutate(name = case_when(name == "Sonstige" ~ "other",
                              name == "Corona" ~ "Covid",
                              TRUE ~ name))
    impfung <- impfung %>%
      mutate(BL = case_when(BL == "Niedersachsen"         ~ "Lower Saxony",
                            BL == "Nordrhein-Westfalen"   ~ "North Rhine-Westphalia",
                            BL == "Hessen"                ~ "Hesse", 
                            BL == "Rheinland-Pfalz"       ~ "Rhineland-Palatinate",
                            BL == "Baden-Württemberg"     ~ "Baden-Wuerttemberg",
                            BL == "Bayern"                ~ "Bavaria",
                            BL == "Mecklenburg-Vorpommern"~ "Mecklenburg-Western Pomerania",
                            BL == "Sachsen"               ~ "Saxony",
                            BL == "Sachsen-Anhalt"        ~ "Saxony-Anhalt",
                            BL == "Thüringen"             ~ "Thuringia",
                            TRUE ~ BL))
  } else if (lan == "de") {
    label_title = "Corona-Pandemie in Deutschland seit Februar 2020"
    label_inc =   "7-Tage Inzidenz"
    label_death = "Tote pro Woche"
    label_vacc =  c("Erstimpfung", "Zweitimpfung", "Auffrischung")
    label_vac_t = c("Impftatus nach Bundesland")
    label_hosp =  "Bettenbelegung in ITS"
    label_copy = "(c) /u/zeltbrennt | Quellen: RKI, DIVI, ESRI"
    intensiv <- intensiv %>%
      mutate(name = case_when(name == "other" ~ "Sonstige",
                              name == "Covid" ~ "Corona",
                              TRUE ~ name))
  } else {
    stop("Language??")
  }
  
  tote_breaks = tote_woche %>% filter(Meldedatum == quarter(Meldedatum, type = "date_first")) %>%
    mutate(label = ifelse(year(Meldedatum) != lag(year(Meldedatum)) | is.na(lag(Meldedatum)),
                          paste0("Q", quarter(Meldedatum), "\n", year(Meldedatum)),
                          paste0("Q", quarter(Meldedatum)))) 
  ### create single frames ####
  # create plots for each day: 
  # 1) 7-day incidence map my county
  # 2) deaths per calendar week
  # 3) vaccine status by state
  # 4) occupied beds in ICU by type
  
  # pull these plots together into a grid, save to disk, load and animate

  system.time({
    m = n_distinct(county_week_100k$Meldedatum)
    n = 0
    for (day in as.character(unique(county_week_100k$Meldedatum))) {
      #for (day in as.character(range(county_week_100k$Meldedatum))) { # debug
      n = n + 1
      cat("Day:",day, "\tImage:\t", n, "/", m, "\n")
      map <- county_week_100k %>%
        filter(Meldedatum == day) %>%
        bind_rows(list(IdLandkreis = "16056", infiziert_woche100k = filter(., IdLandkreis == "16063")$infiziert_woche100k)) %>%
        left_join(shp_forty, by = c("IdLandkreis" = "id")) %>%
        ggplot(aes(x = long, y = lat, group = group, fill = infiziert_woche100k)) +
        geom_polygon() +
        scale_fill_viridis_c(option = "A", direction = -1, name = label_inc, 
                             limits = c(4.5, 3400),
                             breaks = c(5, 10, 20, 40, 80, 150, 300, 600, 1200, 2600),
                             #breaks = c(5, 15, 45, 135, 400, 1200, 3400),
                             trans = "log",
                             guide = guide_colorbar(
                               direction = "horizontal",
                               barheight = unit(2, units = "mm"),
                               barwidth = unit(100, units = "mm"),
                               draw.ulim = FALSE,
                               title.position = 'top',
                               title.hjust = 0.5,
                               title.vjust = 0.5
                             )) + 
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        theme_void() + 
        theme(legend.position = "bottom",
              plot.caption = element_text(color = "gray", size =7, hjust = 0),
              plot.caption.position = "plot") +
        labs(title = label_title, 
             subtitle = ifelse(lan == "en", 
                               paste("day:", format(as.Date(day), "%Y/%m/%d")),
                               paste("Tag:", format(as.Date(day), "%d.%m.%Y"))),
             caption = label_copy) +
        coord_quickmap() 
      
      plot1 <- tote_woche %>%
        mutate(tote_kw = ifelse(Meldedatum > day, 0, tote_kw)) %>%
        distinct(Kalenderwoche, tote_kw) %>%
        ggplot(aes(x = Kalenderwoche, y = tote_kw)) +
        geom_col(width = 1, fill = "#000004FF") +
        scale_y_continuous(limits = c(0, plyr::round_any(max(tote_woche$tote_kw), 1000, f = ceiling)),
                           expand = c(0,0)) +
        scale_x_discrete(breaks = tote_breaks$Kalenderwoche,
                         labels = tote_breaks$label) +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        labs(y = label_death)
      
      
      plot2 <- impfung %>%
        filter(Impfdatum == day) %>%
        ggplot(aes(x = forcats::fct_rev(BL), y = Impfquote, fill = as.character(Impfserie))) +
        geom_col(position = "dodge") +
        scale_y_continuous(labels = scales::label_percent(),
                           limits = c(0, 1),
                           expand = expansion(mult = c(0, 0.05))) +
        scale_fill_manual(values = c("#1D1147FF", "#B63679FF", "#FEC287FF"),
                          name = label_vac_t,
                          limits = factor(c(1, 2, 3)),
                          labels = label_vacc) +
        coord_flip() +
        guides(fill = guide_legend(label.position = "right",
                                   direction = "vertical",
                                   label.theme = element_text(angle = 90, hjust = 0.5, size =9),
                                   title.theme = element_text(angle = 90, hjust = 0.5),
                                   title.position = "left",
                                   reverse = T)) + 
        theme_minimal() +
        theme(legend.position = "right",
              legend.key.height = unit(27, "mm"),
              legend.key.width = unit(3, "mm"),
              panel.grid.major.y = element_blank(),
              axis.title = element_blank())
      
      plot3 <- intensiv %>%
        filter(Datum == day) %>% 
        ggplot(aes(x = 1, y = woche, fill = name)) +
        geom_col() +
        geom_label(aes(label = name, color = name), 
                   fill = "#FFFFFFDD", 
                   position = position_stack(vjust = 0.5),
                   label.padding = unit(0.15, "lines")) +
        coord_flip() +
        scale_y_continuous(label = scales::label_percent(),
                           limits = c(0, 1)) +
        labs(title = label_hosp) +
        scale_fill_manual(values = c("#FB8861", "#51127C")) +
        scale_color_manual(values = c("#FB8861", "#51127C")) +
        theme_minimal() +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.y = element_blank(),
              axis.title = element_blank(),
              legend.position = "none",
              axis.text.x =element_text(size = 8),
              plot.title = element_text(size = 10, hjust = 0.05))
      
      grid <- grid.arrange(map, plot1, plot2, plot3,
                           layout_matrix = rbind(c(1,1,1,1,2,2,2,2),
                                                 c(1,1,1,1,2,2,2,2),
                                                 c(1,1,1,1,2,2,2,2),
                                                 c(1,1,1,1,3,3,3,3),
                                                 c(1,1,1,1,3,3,3,3),
                                                 c(1,1,1,1,3,3,3,3),
                                                 c(1,1,1,1,3,3,3,3),
                                                 c(1,1,1,1,4,4,4,4)))
      dev.off()
      ggsave(file.path("images", lan, paste0("grid_", day,"_",lan,".png")), 
             plot = grid, width = 10, height = 7, dpi = 72)
    }
  })
  ggsave(paste0("corona_", lan, ".png"),
         plot = grid, width = 10, height = 7, dpi = 72)
  ### pull frames together into GIF
  # add extra 4 seconds 
  file.remove(list.files(path = images_path, pattern = "copy", full.names = T))
  for (i in 1:40) {
    file.copy(from= rev(list.files(path = images_path, pattern = "*.png", full.names = T))[1],
              to = file.path("images", lan, paste0("grid_", day, "_", lan,"_copy_", i,".png")))
  }

  
  system.time({list.files(images_path, full.names = TRUE) %>%
      image_read() %>% 
      image_join() %>% 
      image_animate(fps=10) %>% 
      image_write(paste0("corona_animated_",lan,".gif"))
  })
  
}  
