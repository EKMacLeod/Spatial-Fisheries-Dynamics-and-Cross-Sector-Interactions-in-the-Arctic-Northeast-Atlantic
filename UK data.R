## UK fleet ##
## E MacLeod ##
## August 2025 ##

rm(list = ls())

## libraries and setwd -----

library (sf)
library (tidyverse)
library (reshape2)
library (marmap)
library (rnaturalearth)
library (vmstools)
library (viridis)
library (icesVocab)
library(RColorBrewer)

## load in datasets required -----

load ("data/spatial data/spatial_data.Rdata")

MMO_data <- readRDS ("data/UK/MMO_data.Rdata")

## make dataset a spatial object and limit to "Arctic" areas -----

MMO_data <- MMO_data %>% 
  mutate (lat_ct = st_coordinates(st_centroid(MMO_data))[,2]) %>% 
  mutate (lon_ct = st_coordinates(st_centroid(MMO_data))[,1])

MMO_data$cs_code0.05 <- CSquare(MMO_data$lon_ct, MMO_data$lat_ct,degrees = 0.05 )

MMO_data <- MMO_data %>% 
  as.data.frame () %>% select (-Shape)

MMO_data <- left_join (MMO_data, Csquare_0.05, by = "cs_code0.05")

MMO_data <- left_join (MMO_data, Csquare_0.05)
MMO_data <- st_as_sf (MMO_data)

MMO_data <- MMO_data %>% filter (lat_ct > 57.9999999) %>% filter (lon_ct <45)

# convert effort to days
MMO_data$tottime_hours <- MMO_data$tottime/60
MMO_data$tottime_days <- MMO_data$tottime_hours/24

MMO_data$mobtime_hours <- MMO_data$mobtime/60
MMO_data$mobtime_days <- MMO_data$mobtime_hours/24

MMO_data$passtime_hours <- MMO_data$passtime/60
MMO_data$passtime_days <- MMO_data$passtime_hours/24

## Distribution across all years and gears -----

overall_distribution <- MMO_data %>% 
  select (cs_code0.05, year, tottime_days, x)

overall_distribution

overall_distribution <- overall_distribution %>% 
  group_by (cs_code0.05) %>% 
  summarise (Tot_tottime_days = sum (tottime_days))

summary (overall_distribution$Tot_tottime_days)
max <- 20 # keep same as EU equivalent

breakk <- pretty (seq (0, max))

max <- breakk[length(breakk)]

label <- breakk
label <- label[-length (label)]
label <- c(label, paste0(">", max))

mediann <- median (breakk)

# for visuals cap max
overall_distribution$Tot_tottime_days[overall_distribution$Tot_tottime_days > (max)] <- max

p <- ggplot () +
  geom_sf (data = overall_distribution, aes (fill = Tot_tottime_days), colour = NA) +
  scale_fill_viridis_c (name = "Fishing Effort \n(days)", labels = label) +
  geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-100, -500),
               linewidth=c(0.1), colour="grey50",) +
  geom_sf (data = eez, aes (), fill = NA, colour = "black", linewidth = 0.1) +
  # geom_sf (data = eez2, aes (), fill = NA, colour = "black") +  
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) + xlab ("Longitude") + ylab ("Latitude") +
  theme_bw() + theme (panel.background = element_rect (fill = "aliceblue")) +
  
  theme (legend.text = element_text(size=5)) +
  theme (legend.title = element_text(size = 6)) +
  
  theme (axis.text.x = element_text(size = 5), 
         axis.title.x = element_text(size = 6), 
         axis.ticks.length.x = unit(0.5, "mm")) +
  theme (axis.text.y = element_text(size = 5), 
         axis.title.y = element_text(size = 6), 
         axis.ticks.length.y = unit(0.5, "mm"))

ggsave(p, filename ="output/UK/overall_distribution2.png", width = 5, height = 5)

## Yearly distribution of effort -----

yearly_effort <- MMO_data %>% 
  group_by (year, cs_code0.05) %>% 
  summarise (TOTAL_effort_days = sum (tottime_days))

yearly_effort2 <- MMO_data %>% 
  group_by (year) %>% 
  summarise (TOTAL_effort_days2 = sum (tottime_days)) %>% 
  as.data.frame() %>% select (-x)

summary (yearly_effort$TOTAL_effort_days)
max <- 10 # keep same as EU equivalent

breakk <- pretty (seq (0, max))

max <- breakk[length(breakk)]

label <- breakk
label <- label[-length (label)]
label <- c(label, paste0(">", max))

mediann <- median (breakk)

# for visuals cap max
yearly_effort$TOTAL_effort_days[yearly_effort$TOTAL_effort_days > (max)] <- max

p <- ggplot () +
  geom_sf (data = yearly_effort, aes (fill = TOTAL_effort_days), colour = NA) + 
  facet_wrap (~year, ncol = 2) +
  scale_fill_gradient2 (name = "Fishing Effort \n(days)",
                        low = "white", mid = "yellow", high = "red",
                        midpoint = mediann,
                        labels = label,
                        breaks = breakk, 
                        limits = c(0, breakk[length(breakk)])
  ) + 
  # geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-50, -100, -150, -500, -1000),
  #              linewidth=c(0.1), colour="lightblue") +
  geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-100, -500),
               linewidth=c(0.3), colour="grey50") +
  geom_sf (data = eez, aes (), fill = NA, colour = "black") +  
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) +
  theme_bw() + theme (panel.background = element_rect (fill = "lightblue"))

ggsave(p, filename ="output/UK/effort_year.png", width = 6, height= 12)

write.csv (yearly_effort2, file = "output/UK/effort_year.csv")

rm (yearly_effort, yearly_effort2)

## effort/value/weight split between passive and mobile gears -----

MMO_data_mobile <- MMO_data %>% 
  select (cs_code0.05, mobtime_days) %>% 
  rename (time_days = mobtime_days) %>% 
  group_by (cs_code0.05) %>% 
  summarise (total_time_days = sum (time_days)) %>% 
  mutate (temp = "Mobile") 
  
MMO_data_passive <- MMO_data %>% 
  select (cs_code0.05, passtime_days) %>% 
  rename (time_days = passtime_days) %>% 
  group_by (cs_code0.05) %>% 
  summarise (total_time_days = sum (time_days)) %>% 
  mutate (temp = "Passive")

mob_pass <- rbind (MMO_data_mobile, MMO_data_passive) 
# mob_pass <- melt (mob_pass, id = c("cs_code0.05", "type"))

mob_pass$total_time_days[mob_pass$total_time_days == 0] <- NA
mob_pass <- mob_pass %>% filter (!is.na (total_time_days))

summary (mob_pass$total_time_days)
max <- 20 # keep same as EU equivalent

breakk <- pretty (seq (0, max))

max <- breakk[length(breakk)]

label <- breakk
label <- label[-length (label)]
label <- c(label, paste0(">", max))

mediann <- median (breakk)

# for visuals cap max
mob_pass$total_time_days[mob_pass$total_time_days > (max)] <- max

p <- ggplot () +
  geom_sf (data = mob_pass, aes (fill = total_time_days), colour = NA) +
  facet_wrap (~temp) +
  # scale_fill_gradient2 (name = "Fishing Effort \n(days)",
  #                       low = "white", mid = "yellow", high = "red",
  #                       midpoint = mediann,
  #                       labels = label,
  #                       breaks = breakk,
  #                       limits = c(0, breakk[length(breakk)])
  # ) +
  scale_fill_viridis_c (name = "Fishing Effort \n(days)", labels = label) +
  geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-100, -500),
               linewidth=c(0.3), colour="grey50") +
  geom_sf (data = eez, aes (), fill = NA, colour = "black") +
  # geom_sf (data = eez2, aes (), fill = NA, colour = "black") +  
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) + xlab ("Longitude") + ylab ("Latitude") +
  theme_bw() + theme (panel.background = element_rect (fill = "aliceblue"))

ggsave(p, filename ="output/UK/mob_pass3.png", width = 6, height= 3)

## effort/value/weight split between gear groups for years where we have information -----

time_names <- names (MMO_data[grep ("time",names(MMO_data))])

MMO_data_gears <- MMO_data %>% 
  select (year, cs_code0.05, demtime, peltime, dredtime, linetime, netstime, peltime) %>% 
  as.data.frame() %>% 
  select (-x)

MMO_data_gears <- melt (MMO_data_gears, id.vars = c("cs_code0.05", "year"))
MMO_data_gears <- MMO_data_gears %>% filter (!is.na (value))

MMO_data_gears <- MMO_data_gears %>% 
  group_by (variable, cs_code0.05) %>% 
  summarise (Tot_Time_mins = sum (value)) %>% 
  mutate (Tot_Time_Hours = Tot_Time_mins/60) %>% 
  mutate (Tot_Time_Days = Tot_Time_Hours/24) %>% 
  filter (Tot_Time_Days >0)

total_gears <- MMO_data_gears %>% 
  group_by (variable) %>% 
  summarise (SUM = sum (Tot_Time_Days))

MMO_data_gears$variable2 <- NA
MMO_data_gears$variable2[MMO_data_gears$variable == "demtime"] <- "Demersal"
MMO_data_gears$variable2[MMO_data_gears$variable == "peltime"] <- "Pelagic"
MMO_data_gears$variable2[MMO_data_gears$variable == "dredtime"] <- "Dredges"
MMO_data_gears$variable2[MMO_data_gears$variable == "linetime"] <- "Lines"
MMO_data_gears$variable2[MMO_data_gears$variable == "netstime"] <- "Nets"

MMO_data_gears_plot <- merge (Csquare_0.05, MMO_data_gears, all.y = TRUE)

MMO_data_gears_plot$Tot_Time_Days[MMO_data_gears_plot$Tot_Time_Days == 0] <- NA
MMO_data_gears_plot <- MMO_data_gears_plot %>% filter (!is.na (Tot_Time_Days))

summary (MMO_data_gears_plot$Tot_Time_Days)
max <- 25 # keep same as EU equivalent

breakk <- pretty (seq (0, max))

max <- breakk[length(breakk)]

label <- breakk
label <- label[-length (label)]
label <- c(label, paste0(">", max))

mediann <- median (breakk)

# for visuals cap max
MMO_data_gears_plot$Tot_Time_Days[MMO_data_gears_plot$Tot_Time_Days > (max)] <- max

p <- ggplot () +
  geom_sf (data = MMO_data_gears_plot, aes (fill = Tot_Time_Days), colour = NA) +
  facet_wrap (~variable2, ncol = 2) +
  scale_fill_viridis_c (name = "Fishing Effort \n(days)", labels = label) +
  geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-100, -500),
               linewidth=c(0.05), colour="grey50") +
  geom_sf (data = eez, aes (), fill = NA, colour = "black", linewidth = 0.075) +
  # geom_sf (data = eez2, aes (), fill = NA, colour = "black") +  
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) + xlab ("Longitude") + ylab ("Latitude") +
  theme_bw() + theme (panel.background = element_rect (fill = "aliceblue")) +
  
  theme (legend.text = element_text(size=6)) +
  theme (legend.title = element_text(size = 7)) +
  
  theme (axis.text.x = element_text(size = 5), 
         axis.title.x = element_text(size = 6), 
         axis.ticks.length.x = unit(0.5, "mm")) +
  theme (axis.text.y = element_text(size = 5), 
         axis.title.y = element_text(size = 6), 
         axis.ticks.length.y = unit(0.5, "mm"))

ggsave(p, filename ="output/UK/gears2.png", width = 6, height= 8)

## percentage effort/landings/weight in neafc areas -----

total_effort <- MMO_data %>% 
  group_by (year) %>% 
  summarise (TOTAL_effort_days = sum (tottime_days)) %>% 
  as.data.frame () %>% 
  select (-x)

# total effort UK in neafc
total_effort_Neafc <- MMO_data %>%
  mutate (totfishdaysNEAFC = tottime_days * prop_neafc) %>% 
  group_by (year) %>% 
  summarise (TOTAL_effort_days_neafc = sum (totfishdaysNEAFC)) %>% 
  as.data.frame () %>% 
  select (-x)
  
percentage_neafc <- merge (total_effort, total_effort_Neafc) %>% 
  mutate (prop_neafc = (TOTAL_effort_days_neafc/TOTAL_effort_days)*100)

percentage_neafc <- percentage_neafc %>% 
  select (-TOTAL_effort_days_neafc) %>% 
  mutate (prop_neafc = round (prop_neafc, digits = 2)) %>% 
  mutate (TOTAL_effort_days = round (TOTAL_effort_days, digits = 2))

write.csv (percentage_neafc, file = "output/UK/percentage neafc effort.csv")

rm (total_effort, total_effort_Neafc, percentage_neafc)

## percentage effort/landings/weight in offshore wind farm areas -----

total_effort <- MMO_data %>% 
  group_by (year) %>% 
  summarise (TOTAL_effort_days = sum (tottime_days)) %>% 
  as.data.frame () %>% 
  select (-x)

# total effort UK in neafc
total_effort_windfarm <- MMO_data %>%
  mutate (totfishdays_windfarm = tottime_days * prop_windfarm) %>% 
  group_by (year) %>% 
  summarise (totfishdays_windfarm = sum (totfishdays_windfarm)) %>% 
  as.data.frame () %>% 
  select (-x)

percentage_windfarm <- merge (total_effort, total_effort_windfarm) %>% 
  mutate (prop_windfarm = (totfishdays_windfarm/TOTAL_effort_days)*100)

percentage_windfarm <- percentage_windfarm %>% 
  select (-totfishdays_windfarm) %>% 
  mutate (prop_windfarm = round (prop_windfarm, digits = 2)) %>% 
  mutate (TOTAL_effort_days = round (TOTAL_effort_days, digits = 2))

write.csv (percentage_windfarm, file = "output/UK/percentage windfarm effort.csv")

rm (total_effort, total_effort_windfarm, percentage_windfarm)

## percentage effort/landings/weight in offshore wind zones -----

total_effort <- MMO_data %>% 
  group_by (year) %>% 
  summarise (TOTAL_effort_days = sum (tottime_days)) %>% 
  as.data.frame () %>% 
  select (-x)

# total effort UK in neafc
total_effort_windzone <- MMO_data %>%
  mutate (totfishdays_windzone = tottime_days * prop_windZone) %>% 
  group_by (year) %>% 
  summarise (totfishdays_windzone = sum (totfishdays_windzone)) %>% 
  as.data.frame () %>% 
  select (-x)

percentage_windzone <- merge (total_effort, total_effort_windzone) %>% 
  mutate (prop_windzone = (totfishdays_windzone/TOTAL_effort_days)*100)

percentage_windzone <- percentage_windzone %>% 
  select (-totfishdays_windzone) %>% 
  mutate (prop_windzone = round (prop_windzone, digits = 2)) %>% 
  mutate (TOTAL_effort_days = round (TOTAL_effort_days, digits = 2))

write.csv (percentage_windzone, file = "output/UK/percentage windzone effort.csv")

rm (total_effort, total_effort_windzone, percentage_windzone)

## percentage effort/landings/weight in mining areas -----

total_effort <- MMO_data %>% 
  group_by (year) %>% 
  summarise (TOTAL_effort_days = sum (tottime_days)) %>% 
  as.data.frame () %>% 
  select (-x)

# total effort UK in neafc
total_effort_mining <- MMO_data %>%
  mutate (totfishdays_mining = tottime_days * prop_mining) %>% 
  group_by (year) %>% 
  summarise (totfishdays_mining = sum (totfishdays_mining)) %>% 
  as.data.frame () %>% 
  select (-x)

percentage_mining <- merge (total_effort, total_effort_mining) %>% 
  mutate (prop_mining = (totfishdays_mining/TOTAL_effort_days)*100)

percentage_mining <- percentage_mining %>% 
  select (-totfishdays_mining) %>% 
  mutate (prop_mining = round (prop_mining, digits = 2)) %>% 
  mutate (TOTAL_effort_days = round (TOTAL_effort_days, digits = 2))

write.csv (percentage_mining, file = "output/UK/percentage mining effort.csv")

rm (total_effort, total_effort_mining, percentage_mining)
