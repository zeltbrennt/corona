library(rvest)

setwd("/home/pi/corona")
temp <- read_html("https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html") 
temp <- html_table(temp) 

# as long as format is not consitent through the week, the table will be dumped as is

write.csv(temp[[1]], paste0("dump/", Sys.Date(), ".csv"))

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
# new_infected <- temp[[1]][,1:2] %>%
#   pivot_wider(names_from = Bundesland, values_from = `Fälle`) %>%
#   mutate(Datum = Sys.Date())
# new_dead <- temp[[1]][,c(1,3)] %>%
#   pivot_wider(names_from = Bundesland, values_from = `Todesfälle`) %>%
#   mutate(Datum = Sys.Date())
# 
# # write csv and keep results for plotting
# new_infected <- update_csv("infizierte", new_infected)
# new_dead <- update_csv("tote", new_dead)
# 
# pl <- new_infected %>%
#   pivot_longer(cols = -c(Datum, Gesamt), 
#                names_to = "Bundesland", 
#                values_to = "Anzahl Infizierte") %>%
#   ggplot(aes(x = Datum, y = `Anzahl Infizierte`, color = Bundesland)) +
#   geom_line() +
#   labs(title = "Covid-19 Infektionen in Deutschland",
#        caption = paste("Quelle: RKI | Letzte Abfrage:", Sys.time()),
#        x = "")
# ggsave(filename = "verlauf.jpg",
#        plot = pl,
#        width = 20,
#        height = 12,
#        units = "cm")

