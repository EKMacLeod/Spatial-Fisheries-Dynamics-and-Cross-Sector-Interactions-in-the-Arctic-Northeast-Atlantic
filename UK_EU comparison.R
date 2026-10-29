## Compare between UK and EU fleets ##
## E MacLeod ##
## June 2025 ##

rm(list = ls())

## libraries and setwd -----

library (sf)
library (tidyverse)
library (reshape2)
library (marmap)
library (rnaturalearth)
library (vmstools)

## load in datasets required -----

load ("data/spatial data/spatial_data.Rdata")
combined_EU_UK_data <- readRDS ("data/combined EU_UK_data.Rdata")
# Csquare_0.5 <- readRDS ("data/spatial data/0.5 csquare_grid.Rdata")

## make dataset a spatial object and limit to "Arctic" areas -----

EU_UK_comparison <- left_join (combined_EU_UK_data, Csquare_0.5)
EU_UK_comparison_allAreas <- st_as_sf (EU_UK_comparison)

EU_UK_comparison <- EU_UK_comparison %>% filter (lat_ct > 57.9999999) %>% filter (lon_ct <45)
EU_UK_comparison <- st_as_sf(EU_UK_comparison)

EU_UK_comparison_orig <- EU_UK_comparison

## plot to compare the two datasets over time -----

summary (EU_UK_comparison$Fishing_effort_days)
max <- 50

breakk <- pretty (seq (0, max))

max <- breakk[length(breakk)]

label <- breakk
label <- label[-length (label)]
label <- c(label, paste0(">", max))

mediann <- median (breakk)

# for visuals cap max
EU_UK_comparison$Fishing_effort_days[EU_UK_comparison$Fishing_effort_days > (max)] <- max

p <- ggplot () + 
  # geom_sf (data = offshore_windfarms, aes (), fill = "pink", colour = "hotpink") +
  # geom_sf (data = ices_areas, aes (), fill = NA, colour = "black") +
  # geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-50, -100, -150, -500, -1000),
  #                linewidth=c(0.1), colour="lightblue") +
  geom_sf (data = EU_UK_comparison, aes (fill = Fishing_effort_days), colour = NA) +
  geom_sf (data = eez, aes (), fill = NA, colour = "black") +
  scale_fill_gradient2 (name = "Fishing Effort \n(days)",
                        low = "white", mid = "yellow", high = "red",
                        midpoint = mediann,
                        labels = label,
                        breaks = breakk, 
                        limits = c(0, breakk[length(breakk)])
  ) +  
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  facet_grid (year ~ country) +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) +
  theme_bw() + theme (panel.background = element_rect (fill = "aliceblue"))

ggsave(p, file = "output/UK_EU comparison/comparison years.png", width = 10, height= 17)

## summary across years -----

EU_UK_comparison_sum <- EU_UK_comparison_orig %>% 
  group_by (cs_code0.5, country) %>% 
  summarise (sum_effort = sum (Fishing_effort_days))

summary (EU_UK_comparison_sum$sum_effort)
max <- 40

breakk <- pretty (seq (0, max))

max <- breakk[length(breakk)]

label <- breakk
label <- label[-length (label)]
label <- c(label, paste0(">", max))

mediann <- median (breakk)

# for visuals cap max
EU_UK_comparison_sum$sum_effort[EU_UK_comparison_sum$sum_effort > (max)] <- max

p <- ggplot () + 
  # geom_sf (data = offshore_windfarms, aes (), fill = "pink", colour = "hotpink") +
  # geom_sf (data = ices_areas, aes (), fill = NA, colour = "black") +

  geom_sf (data = EU_UK_comparison_sum, aes (fill = sum_effort), colour = NA) +
  # geom_sf (data = eez, aes (), fill = NA, colour = "black") +
  # scale_fill_gradient2 (name = "Fishing Effort \n(days)",
  #                       low = "white", mid = "yellow", high = "red",
  #                       midpoint = mediann,
  #                       labels = label,
  #                       breaks = breakk, 
  #                       limits = c(0, breakk[length(breakk)])
  # ) +  
  scale_fill_viridis_c (name = "Fishing Effort \n(days)", labels = label, breaks = breakk) +
  geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-100, -500),
               linewidth=c(0.2), colour="grey50") +
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  facet_wrap (~country, ncol = 1) + 
  xlab ("Longitude") + ylab ("Latitude") +
  geom_sf (data = eez, aes (), fill = NA, colour = "black", linewidth = 0.2) +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) +
  theme_bw() + theme (panel.background = element_rect (fill = "aliceblue")) +
  
  theme (legend.text = element_text(size=6)) +
  theme (legend.title = element_text(size = 7)) +
  
  theme (axis.text.x = element_text(size = 6), 
         axis.title.x = element_text(size = 6), 
         axis.ticks.length.x = unit(0.5, "mm")) +
  theme (axis.text.y = element_text(size = 6), 
         axis.title.y = element_text(size = 6), 
         axis.ticks.length.y = unit(0.5, "mm"))

ggsave(p, file = "output/UK_EU comparison/comparison all years2.png", width = 6, height= 6)

## What percentage of activity takes place in the NEAFC regulatory areas? -----

# total effort of UK and EU across whole area
total_effort <- EU_UK_comparison %>% 
  group_by (year, country) %>% 
  summarise (TOTAL_effort_days = sum (Fishing_effort_days)) %>% 
  as.data.frame () %>% 
  select (-x)


# total effort of UK and EU in neafc
total_effort_Neafc <- EU_UK_comparison %>%
  mutate (Fishing_effort_days_neafc = Fishing_effort_days * prop_neafc) %>% 
  group_by (year, country) %>% 
  summarise (TOTAL_effort_days_neafc = sum (Fishing_effort_days_neafc)) %>% 
  as.data.frame () %>% 
  select (-x)

percentage_neafc <- merge (total_effort, total_effort_Neafc) %>% 
  mutate (prop_neafc = (TOTAL_effort_days_neafc/TOTAL_effort_days)*100)

scalefactor <- max(percentage_neafc$TOTAL_effort_days)/max(percentage_neafc$prop_neafc)

ggplot () +
  geom_bar (data = percentage_neafc, aes (x = year, y = TOTAL_effort_days), stat = "identity", fill = "grey50", colour = "black") +
  # geom_point (data = percentage_neafc, aes (x = year, y = prop_neafc*scalefactor), colour = "red") +
  geom_line (data = percentage_neafc, aes (x = year, y = prop_neafc*scalefactor), colour = "red", linewidth = 1.5) +
  # geom_bar (data = percentage_neafc, aes (x = year, y = prop_neafc*scalefactor), stat = "identity", fill = "blue", width = 0.5) +
  facet_wrap (~country, scales = "free_y", ncol = 1) +
  ylab ("Total Effort (days)") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / scalefactor, name = "Percentage in NEAFC regulated area (%)")
  ) +
  theme_bw () + theme (axis.text.y.right = element_text(colour = "red"), axis.title.y.right = element_text(colour = "red"),  axis.ticks.y.right = element_line(colour = "red"))

ggsave("output/UK_EU comparison/comparison percentage NEAFC.png", width = 12, height= 6)

percentage_neafc <- percentage_neafc %>% 
  select (-TOTAL_effort_days_neafc) %>% 
  mutate (prop_neafc = round (prop_neafc, digits = 2)) %>% 
  mutate (TOTAL_effort_days = round (TOTAL_effort_days, digits = 2))

write.csv (dcast (data = percentage_neafc, year~country, value.var = "prop_neafc"), file = "output/UK_EU comparison/comparison percentage neafc.csv")
write.csv (dcast (data = percentage_neafc, year~country, value.var = "TOTAL_effort_days"), file = "output/UK_EU comparison/comparison total effort.csv")

## difference in % conflicting with infrastructure between the two countries -----

UK_files <- list.files ("output/UK", pattern = "percentage")

for (f in 1:length (UK_files)) {
  
  file <- read_csv (paste0("output/UK/", UK_files[f]))
  
  if (f == 1) {UK_percentages <- file} else {
    UK_percentages <- left_join (UK_percentages, file)
  }
  
}

UK_percentages <- UK_percentages %>% mutate (country = "UK")
rm (UK_files, f)

EU_files <- list.files ("output/EU", pattern = "percentage effort")

for (f in 1:length (EU_files)) {
  
  file <- read_csv (paste0("output/EU/", EU_files[f]))
  
  if (f == 1) {EU_percentages <- file} else {
    EU_percentages <- left_join (EU_percentages, file)
  }
  
}

EU_percentages <- EU_percentages %>% mutate (country = "EU")
rm (EU_files, f)

percentages <- rbind (UK_percentages, EU_percentages)
colnames (percentages) <- c("...1","year", "TOTAL_effort_days", "Areas of Mining Interest", "NEAFC Regulatory Area", "Installed/Planned Wind Farms", "Wind Development Zones", "country")

percentages2 <- melt (percentages, id.vars = c("year", "country"))

scalefactor <- max(percentages$TOTAL_effort_days/1000)/max(percentages$`Wind Development Zones`)

percentages2$variable <- factor(percentages2$variable, levels = c("NEAFC Regulatory Area", "Wind Development Zones", "Areas of Mining Interest", "Installed/Planned Wind Farms", "TOTAL_effort_days", "...1"))

ggplot () +
  geom_bar (data = percentages, aes (x = as.character (year), y = TOTAL_effort_days/1000),
            stat = "identity", fill = "grey80", colour = "grey50") +
  geom_line (data = subset (percentages2, variable %notin%  c("TOTAL_effort_days", "...1")), 
             aes (x = as.character (year), y = value*scalefactor,
                  colour = variable, group = variable), linewidth = 1.5) +
  facet_wrap (~country, nrow = 2) + 
  scale_colour_manual (values = c("orange", "darkgreen", "#E41A1C","darkorchid4"), name = "") +
  scale_y_continuous(sec.axis = sec_axis(~ . / scalefactor, name = "Spatial overlap (%)")) +
  theme_bw () + 
  xlab ("Year") + ylab ("Effort (thousand days)") + 
  theme (legend.position = "bottom") +
  
  theme (legend.text = element_text(size=10)) +
  guides(colour=guide_legend(nrow=2,byrow=TRUE)) +
  theme (strip.text.x = element_text(size = 10)) +

  theme (axis.text.x = element_text(size = 10), 
         axis.title.x = element_text(size = 10), 
         axis.ticks.length.x = unit(0.5, "mm")) +
  
  theme (axis.text.y = element_text(size = 10), 
         axis.title.y = element_text(size = 10), 
         axis.ticks.length.y = unit(0.5, "mm"))

ggsave("output/UK_EU comparison/comparison percentage devs3.png", width = 8, height= 7)

