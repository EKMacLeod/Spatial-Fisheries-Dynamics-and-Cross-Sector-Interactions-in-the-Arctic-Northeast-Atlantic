## Import VMS summary data for Arctic Project ##
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
library(geojsonsf)

`%notin%` <- Negate(`%in%`)

## import spatial information -----

# ices subareas from ICES
ices_areas <- read_sf ("data/spatial data/ICES_areas (3)/ICES_Areas_20160601_cut_dense_3857.shp")
ices_areas <- st_transform (ices_areas, 4326)

# set bounding box for analyses around ICES areas (for now, might want to filter to Arctic later)
big.bbox <- st_bbox(ices_areas, crs = st_crs(ices_areas))

# offshore wind developments
planned_windfams <- st_read ("data/offshore renewables/powerplant_windfarm_planned.gpkg/powerplant_windfarm_planned.gpkg")
planned_windfams <- st_make_valid(planned_windfams)
planned_windfams <- st_crop (planned_windfams, big.bbox)

installed_windfarms <- st_read ("data/offshore renewables/powerplant_windfarm_installed.gpkg/powerplant_windfarm_installed.gpkg")
installed_windfarms <- st_make_valid(installed_windfarms)
installed_windfarms <- st_crop (installed_windfarms, big.bbox)


installed_windfarms <- installed_windfarms %>% select (admin_area_name, description, installation_year, name, owner, round_name, 
                                                       status, windfarm_type, geom) %>% 
  mutate (source = "installed")
planned_windfams <- planned_windfams %>% select (admin_area_name, description, installation_year, name, owner, round_name, 
                                                       status, windfarm_type, geom) %>% 
  mutate (source = "planned")

all_windfarms <- rbind (installed_windfarms, planned_windfams) 
all_windfarms <- all_windfarms %>% 
  filter (windfarm_type %in% c("OFFSHORE_FIXED", "DEMONSTRATION_SITE", "OFFSHORE_FLOATING", "OFFSHORE_MIXED"))

all_windfarms <- st_union (all_windfarms, )

rm (installed_windfarms, planned_windfams)

wind_development_zone <- st_read ("data/offshore renewables/wind_development_zone.gpkg/wind_development_zone.gpkg")
wind_development_zone <- st_make_valid(wind_development_zone)
wind_development_zone <- st_crop (wind_development_zone, big.bbox)

# ggplot () +
#   geom_sf (data = wind_development_zone, aes (), fill = "lightblue") +
#   geom_sf (data = all_windfarms, aes (fill = source))

## norway deep sea mining areas -----

geo <- geojson_sf("data/mining areas/Play resource estimate area.geojson")
geo <- st_make_valid(geo)

mining_areas <- geo; rm (geo)

# ggplot () +
#   geom_sf (data = wind_development_zone, aes (), fill = "lightblue") +
#   geom_sf (data = all_windfarms, aes (fill = source)) + 
#   geom_sf (data = mining_areas, aes (), fill = "darkblue")

# coastline
countries <- ne_countries (type = "countries", scale = "large")
countries <- st_as_sf (countries)
countries <- st_transform (countries, 4326)

countries <- countries %>% filter (subregion %notin% c("Micronesia", "Polynesia", "Western Africa", "Australia and New Zealand", "Caribbean", "South-Eastern Asia", "Southern Africa", "Middle Africa", 
                                                       "South America", "Central America", "Southern Asia"))

# bathymetric information from NOAA
bathy = getNOAA.bathy(lon1 = -50, lon2 = 50, lat1 = 50, lat2 = 90,
                      resolution = 1)
bathy = fortify.bathy(bathy)

# exclusive economic zone - downloaded from https://www.marineregions.org/downloads.php
eez <- read_sf ("data/spatial data/World_EEZ_v12_20231025/World_EEZ_v12_20231025/eez_v12.shp")
eez <- st_transform (eez, 4326)
eez <- st_crop (st_make_valid (eez), big.bbox)

# c square grid and res
lon = c(big.bbox[1], big.bbox[3])
lat = c(big.bbox[2], big.bbox[4])

offset <- 0.05/2
lon_ct = seq(lon[1] + offset, lon[2],0.05)
lat_ct = seq(lat[1] + offset, lat[2], 0.05)

spatial_grid_ct = expand.grid( lon_ct = lon_ct, lat_ct = lat_ct)
spatial_grid_ct$ICES_rectangle = mapplots::ices.rect2(lon = lon_ct, lat = lat_ct)

spatial_grid_ct2 <- spatial_grid_ct %>%
  mutate (cs_code0.05 = CSquare(spatial_grid_ct$lon_ct, spatial_grid_ct$lat_ct,degrees =  0.05)) %>%
  mutate (cs_code0.1 = CSquare(spatial_grid_ct$lon_ct, spatial_grid_ct$lat_ct,degrees =  0.1)) %>%
  mutate (cs_code0.5 = CSquare(spatial_grid_ct$lon_ct, spatial_grid_ct$lat_ct,degrees =  0.5))

# data frame with point positions in the middle of 0.05 degree grid cell with the corresponding ICES rectangle and c square codes (various resolutions)
Csquares <- spatial_grid_ct2; rm (spatial_grid_ct, lon_ct, lat_ct, lon, lat, offset, spatial_grid_ct2)

# plot to make sure the spatial data makes sense
ggplot () +
  geom_sf (data = all_windfarms, aes (), fill = "pink", colour = "hotpink") +
  geom_sf (data = wind_development_zone, aes (), fill = NA, colour = "hotpink") +
  geom_sf (data = mining_areas, aes (), fill =  NA, colour = "red") +
  geom_sf (data = ices_areas, aes (), fill = NA, colour = "black") +
  # geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-50, -100, -150, -500, -1000),
  #              linewidth=c(0.1), colour="lightblue") +
  geom_sf (data = eez, aes (), fill = NA, colour = "blue") +
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  coord_sf (xlim = c(-12, 12), ylim = c(49, 75)) +
  theme_bw()

# designate areas as within NEAFC regulatory area or not

# ggplot () +
#   geom_sf (data = ices_areas, aes (), fill = NA, colour = "black") +
#   geom_sf_label (data = ices_areas, aes (label = Area_Full)) +
#   theme_bw() + coord_sf (xlim = c(-25, -10), ylim = c(50, 65))

Neafc_Reg_areas <- ices_areas %>%
  filter (Area_Full %in% c("27.1.a", "27.2.b.1", "27.2.a.1",
                           "27.10.a.1", "27.9.b.1", "27.10.b", "27.8.e.1", "27.8.d.1", "27.7.j.1",
                           "27.12.c", "27.12.b", "27.7.k.1", "27.12.a.1", "27.7.c.1", "27.12.b.1", "27.6.b.1",
                           "27.14.b.1", "27.12.a.2", "27.5.b.1.a"))

# ggplot () +
#   geom_sf (data = ices_areas, aes (), fill = NA, colour = "black") +
#   geom_sf (data = Neafc_Reg_areas2, aes (), fill = "orange", colour = "orange") +
#   # geom_sf_label (data = ices_areas, aes (label = Area_Full)) +
#   theme_bw()

Neafc_Reg_areas <- st_union (Neafc_Reg_areas)

# # create csquare grids over the whole extent
# # set grid boundaries
# lon = c(-45,45)
# lat = c(55,89)
# 
# # create grid and assign c-square notation
# 
# # set grid cell resolution and calculate offset value
# res = c(0.05, 0.1, 0.2, 0.25, 0.5)
# 
# for (r in 1:length (res)) {
# 
#   print (res[r])
# 
#   # create offset
#   offset <- res[r]/2
# 
#   # create grid centroids
#   lon_ct = seq(lon[1] + offset, lon[2],res[r])
#   lat_ct = seq(lat[1] + offset, lat[2], res[r])
# 
#   spatial_grid_ct = expand.grid( lon_ct = lon_ct, lat_ct = lat_ct)
#   # head (spatial_grid_ct)
# 
#   # Add ICES rectangles using centroids
#   spatial_grid_ct$ICES_rectangle = mapplots::ices.rect2(lon = lon_ct, lat = lat_ct)
# 
#   # add grid notation - to csquare if applicable
#   if (res[r] %in% c(0.05, 0.1, 0.5)) {
# 
#     csquare_resolution = c(res[r])
# 
#     for ( i in unique (csquare_resolution)) {
# 
#       print(i)
#       cs_code  = CSquare(spatial_grid_ct$lon_ct, spatial_grid_ct$lat_ct,degrees =  i )
# 
#       spatial_grid_ct = spatial_grid_ct %>% mutate ( cs_code = cs_code   )
# 
#       colnames (spatial_grid_ct) [ ncol(spatial_grid_ct) ] = paste0("cs_code", i )
#     }
#   }
# 
#   if (res[r] %in% c(0.2, 0.25)) {
# 
#     spatial_grid_ct = spatial_grid_ct %>%
#       mutate ("temp" = 1:nrow (spatial_grid_ct))
# 
#     names (spatial_grid_ct) <- c("lon_ct", "lat_ct", "ICES_rectangle", paste0 ("grid_cell_", res[r]))
#   }
# 
#   # transform to geom object
#   spatial_grid_geom = spatial_grid_ct %>%
#     sf::st_as_sf(coords = c('lon_ct', 'lat_ct' ))%>%
#     st_set_crs(4326)
# 
#   # create bounding box from geom object
#   bbox = spatial_grid_geom %>% st_bbox()   %>% st_as_sfc() %>%
#     st_sf( id  = 1, label = 'bbox' )  %>%
#     st_set_crs(4326)
# 
#   # transform into grid using bounding box and centroids
#   gridp = st_make_grid(bbox, cellsize = res[r], crs = 4326  ,
#                        offset =  c( min(spatial_grid_ct$lon_ct) - offset,
#                                     min(spatial_grid_ct$lat_ct)-  offset)
#   ) %>% st_as_sf()
# 
#   # Add table information to the grid (c square codes and ICES rectangle information)
#   spatial_grid_cells = gridp %>% cbind(spatial_grid_ct)
# 
#   # # plot grid to check
#   # ggplot(spatial_grid_cells)   + geom_sf (fill = "pink", colour = "blue") +
#   #   # geom_polygon (data = countries, aes (x = long, y = lat, group = group), fill = "grey50", colour = "grey30") +
#   #   coord_sf (xlim = c(-10, 5), ylim = c(55, 62)) +
#   #   theme_bw () + xlab ("Longitude") + ylab ("Latitude")
# 
#   saveRDS(spatial_grid_cells, file = paste0 ("data/spatial data/", res[r], " csquare_grid.Rdata"))
# 
# }
# 
# rm (bbox, gridp, spatial_grid_cells, spatial_grid_ct, spatial_grid_geom)
# rm (cs_code, csquare_resolution, i, lat, lat_ct, lon, lon_ct, offset, r)

# load in 0.05 and 0.5 csquare spatial grid
Csquare_0.05 <- readRDS ("data/spatial data/0.05 csquare_grid.Rdata")
Csquare_0.5 <- readRDS ("data/spatial data/0.5 csquare_grid.Rdata")

## for each csquare calculate the % area in the neafc reg area -----
Csquare_0.5_neafc <- st_intersection (Csquare_0.5, Neafc_Reg_areas)
Csquare_0.05_neafc <- st_intersection (Csquare_0.05, Neafc_Reg_areas)

Csquare_0.05$TotalArea <- as.numeric (st_area(Csquare_0.05))
Csquare_0.05_neafc$NeafcArea <- as.numeric (st_area (Csquare_0.05_neafc))
Csquare_0.05_neafc <- Csquare_0.05_neafc %>% as.data.frame() %>% select (cs_code0.05, NeafcArea)

Csquare_0.5$TotalArea <- as.numeric (st_area(Csquare_0.5))
Csquare_0.5_neafc$NeafcArea <- as.numeric (st_area (Csquare_0.5_neafc))
Csquare_0.5_neafc <- Csquare_0.5_neafc %>% as.data.frame() %>% select (cs_code0.5, NeafcArea)

Csquare_0.5 <- left_join (Csquare_0.5, Csquare_0.5_neafc) %>% mutate (NeafcArea = replace_na(NeafcArea, 0)) %>% mutate (prop_neafc = NeafcArea/TotalArea)
Csquare_0.05 <- left_join (Csquare_0.05, Csquare_0.05_neafc)  %>% mutate (NeafcArea = replace_na(NeafcArea, 0)) %>% mutate (prop_neafc = NeafcArea/TotalArea)

rm (Csquare_0.05_neafc, Csquare_0.5_neafc)

ggplot () +
  # geom_sf (data = Csquare_0.05, aes (fill = prop_neafc), colour = NA) + 
  geom_sf (data = Csquare_0.5, aes (fill = prop_neafc), colour = NA)

# save.image ("data/spatial_temp.Rdata")

## for each csquare calculate the % area in planned or installed windfarm -----
Csquare_0.5_windfarm <- st_intersection (Csquare_0.5, all_windfarms)
Csquare_0.05_windfarm <- st_intersection (Csquare_0.05, all_windfarms) ; Csquare_0.05_windfarm <- st_make_valid(Csquare_0.05_windfarm)

Csquare_0.05_windfarm$windfarmArea <- as.numeric (st_area (Csquare_0.05_windfarm))
Csquare_0.05_windfarm <- Csquare_0.05_windfarm %>% as.data.frame() %>% select (cs_code0.05, windfarmArea)

Csquare_0.5_windfarm$windfarmArea <- as.numeric (st_area (Csquare_0.5_windfarm))
Csquare_0.5_windfarm <- Csquare_0.5_windfarm %>% as.data.frame() %>% select (cs_code0.5, windfarmArea)

Csquare_0.5 <- left_join (Csquare_0.5, Csquare_0.5_windfarm) %>% mutate (windfarmArea = replace_na(windfarmArea, 0)) %>% mutate (prop_windfarm = windfarmArea/TotalArea)
Csquare_0.05 <- left_join (Csquare_0.05, Csquare_0.05_windfarm) %>% mutate (windfarmArea = replace_na(windfarmArea, 0)) %>% mutate (prop_windfarm = windfarmArea/TotalArea)

rm (Csquare_0.5_windfarm, Csquare_0.05_windfarm)

ggplot () +
  # geom_sf (data = Csquare_0.05, aes (fill = prop_windfarm), colour = NA) + 
  geom_sf (data = Csquare_0.5, aes (fill = prop_windfarm), colour = NA)

## for each csquare calculate the % area in windfarm leased area

Csquare_0.5_windzone <- st_intersection (Csquare_0.5, wind_development_zone)
Csquare_0.05_windzone <- st_intersection (Csquare_0.05, wind_development_zone); Csquare_0.05_windzone <- st_make_valid(Csquare_0.05_windzone)

Csquare_0.5_windzone$windzoneArea <- as.numeric (st_area (Csquare_0.5_windzone))
Csquare_0.5_windzone <- Csquare_0.5_windzone %>% as.data.frame() %>% select (cs_code0.5, windzoneArea)

Csquare_0.05_windzone$windzoneArea <- as.numeric (st_area (Csquare_0.05_windzone))
Csquare_0.05_windzone <- Csquare_0.05_windzone %>% as.data.frame() %>% select (cs_code0.05, windzoneArea)

Csquare_0.5 <- left_join (Csquare_0.5, Csquare_0.5_windzone) %>% mutate (windzoneArea = replace_na(windzoneArea, 0)) %>% mutate (prop_windZone = windzoneArea/TotalArea)
Csquare_0.05 <- left_join (Csquare_0.05, Csquare_0.05_windzone) %>% mutate (windzoneArea = replace_na(windzoneArea, 0)) %>% mutate (prop_windZone = windzoneArea/TotalArea)

rm (Csquare_0.5_windzone, Csquare_0.05_windzone)

ggplot () +
  geom_sf (data = Csquare_0.05, aes (fill = prop_windZone), colour = NA) + 
  geom_sf (data = Csquare_0.5, aes (fill = prop_windZone), colour = NA)

## for each csquare calculate the % area in mining areas 

Csquare_0.5_mining <- st_intersection (Csquare_0.5, mining_areas)
Csquare_0.05_mining <- st_intersection (Csquare_0.05, mining_areas); Csquare_0.05_mining <- st_make_valid(Csquare_0.05_mining)

Csquare_0.5_mining$miningArea <- as.numeric (st_area (Csquare_0.5_mining))
Csquare_0.5_mining <- Csquare_0.5_mining %>% as.data.frame() %>% select (cs_code0.5, miningArea)

Csquare_0.05_mining$miningArea <- as.numeric (st_area (Csquare_0.05_mining))
Csquare_0.05_mining <- Csquare_0.05_mining %>% as.data.frame() %>% select (cs_code0.05, miningArea)

Csquare_0.5 <- left_join (Csquare_0.5, Csquare_0.5_mining) %>% mutate (miningArea = replace_na(miningArea, 0)) %>% mutate (prop_mining = miningArea/TotalArea)
Csquare_0.05 <- left_join (Csquare_0.05, Csquare_0.05_mining) %>% mutate (miningArea = replace_na(miningArea, 0)) %>% mutate (prop_mining = miningArea/TotalArea)

rm (Csquare_0.5_mining, Csquare_0.05_mining)

ggplot () +
  geom_sf (data = Csquare_0.05, aes (fill = prop_mining), colour = NA) + 
  geom_sf (data = Csquare_0.5, aes (fill = prop_mining), colour = NA)

# save.image ("data/spatial_temp.Rdata")

# save.image ("data/spatial data/spatial_data.Rdata")

load ("data/spatial data/spatial_data.Rdata")

## infrastructure/neafc plot -----

mining_areas2 <- st_as_sf (mining_areas) %>% mutate (test = "Areas of Mining Interest")
wind_development_zone2 <- st_as_sf (wind_development_zone) %>% mutate (test = "Wind Development Zones")
all_windfarms2 <- st_as_sf (all_windfarms) %>% mutate (test = "Installed/Planned Wind Farms")
Neafc_Reg_areas2 <- st_as_sf (Neafc_Reg_areas) %>% mutate (test = "NEAFC Regulatory Area")

infrastructure <- rbind (mining_areas2, wind_development_zone2, all_windfarms2, Neafc_Reg_areas2)
infrastructure$test <- factor(infrastructure$test, levels = c("NEAFC Regulatory Area", "Wind Development Zones", "Areas of Mining Interest", "Installed/Planned Wind Farms"))

ggplot () + 
  # geom_sf (data = infrastructure, aes (fill = test, colour = test), size = 3) +
  geom_sf (data = subset (infrastructure, test != "NEAFC Regulatory Area"), aes (fill = test), colour = NA) +
  # scale_colour_manual (values = c("#377EB8", "#E41A1C","#984EA3"), name = "")+
  geom_sf (data = eez, aes (), fill = NA, colour = "black") +
  geom_sf (data = subset (infrastructure, test == "NEAFC Regulatory Area"), aes (colour = test), , fill = NA, linewidth = 2) +
  scale_colour_manual (values = c("orange"), name = "") +
  scale_fill_manual (values = c("#377EB8", "#E41A1C","#984EA3"), name = "")+
  geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-50, -100, -150, -500, -1000),
                 linewidth=c(0.1), colour="lightblue4") +
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) + xlab ("Longitude") + ylab ("Latitude") +
  theme_bw() + theme (panel.background = element_rect (fill = "aliceblue"), legend.position = "bottom")

ggsave("output/infrastructure_management.png", width = 9, height= 5)

bathy2 <- bathy %>% filter (z <0)
bathy2$z_abs <- abs (bathy2$z)
bathy2$z[bathy2$z < -2000] <- -2000

library (ggnewscale)

ggplot () + 
  # geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-50, -100, -150, -500, -1000),
  #              linewidth=c(0.1), colour="lightblue4") +
  geom_tile(data = subset (bathy2, z< 0), aes(x = x, y = y, fill = z)) +
  scale_fill_distiller (name = "Depth (m)", 
                        breaks = c(-1, -1000, -2000), 
                        labels = c("0", "-1000","-2000+")) +
  new_scale_fill() +
  geom_sf (data = subset (infrastructure, test != "NEAFC Regulatory Area"),
           aes (fill = test), colour = NA, alpha = 0.6) +
  scale_fill_manual (values = c("darkgreen", "#E41A1C","darkorchid4"), name = "") +
  geom_sf (data = subset (infrastructure, test == "NEAFC Regulatory Area"), aes (colour = test), fill = NA, linewidth = 2) +
  scale_colour_manual (values = c("orange"), name = "") +
  geom_sf (data = eez, aes (), fill = NA, colour = "black") +
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) + xlab ("Longitude") + ylab ("Latitude") +
  theme_bw() + theme (panel.background = element_rect (fill = "aliceblue"), 
                      legend.position = "bottom", 
                      # legend.text = element_text(size = 3), 
                      # legend.key.size = unit(1, 'cm')
                      ) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))

ggsave("output/infrastructure_management_bathymetry4.png", width = 9, height= 5)

# rm (infrastructure, mining_areas2, wind_development_zone2, all_windfarms2, Neafc_Reg_areas2)

## import UK MMO summarised dataNULL## import UK MMO summarised data -----

# https://environment.data.gov.uk/dataset/bf55694c-3330-4c1d-a48e-db6bbe82ad77 - dowloaded June 2023 for MASTS 

mmo.dat <- "data/UK/MMO VMS Summaries"

years <- c(2011:2020) # years where summary data include effort information (kw hours). Effort info available from 2007 as minutes 
# years <- c(2016:2020) # years with information on vessel number and gear specific info

for (i in 1:length(years)) {
  
  print (years[i])
  
  # read in data file for that year
  dat.temp <- read_sf (paste0(mmo.dat, "/", years[i], ".gdb"))
  
  # add year column
  dat.temp$year <- years[i]
  
  # prior to 2016 no gear specific columns, need to add columns as NAs for prior years
  
  if (years[i] < 2016) {
    
    gears <- c("dem", "dred", "line", "nets", "other", "pel", "trap", "drb", "drh", 
               "fix", "fpo", "gen", "gn", "gnc", "gnd", "gns", "gtr", "hf", "hmd", "lhp", 
               "ll", "lld", "lls", "ltl", "lx", "mis", "ot", "otb", "otm", "ott", "ps",
               "pt", "ptb", "ptm", "sdn", "spr", "ssc", "sx", "tb", "tbb", "tbn", "tm",
               "tms", "tx"
    )
    
    for (g in unique (gears)) {
      
      print (g)
      varname_num <- paste0 (g, "num")
      varname_time <- paste0 (g, "time")
      varname_qty <- paste0 (g, "qty")
      varname_val <- paste0 (g, "val")
      varname_kwh <- paste0 (g, "kwh")
      
      dat.temp[[varname_num]] <- NA
      dat.temp[[varname_time]] <- NA
      dat.temp[[varname_qty]] <- NA
      dat.temp[[varname_val]] <- NA
      dat.temp[[varname_kwh]] <- NA
      
    }
    
    rm (gears, g, varname_kwh, varname_num, varname_val, varname_qty, varname_time)
  }
  
  # also need to rename some column names for earlier years and remove unwanted columns
  if (years[i] %in% c(2011)) {
    dat.temp <- dat.temp %>% 
      rename (Shape_Length = shape_Length, Shape_Area = shape_Area, Shape = shape, 
              linesnum = linenum, 
              othtime = othertime, othqty = otherqty, othernum = othernum,
              othval = otherval, othkwh = otherkwh) %>%
      select (-shape_leng)  %>% 
      mutate (allvessnum = NA, mobnum = NA, passnum = NA)
    
    
  }
  
  if (years[i] %in% c(2012, 2013, 2014, 2015)) {
    dat.temp <- dat.temp %>% 
      rename (Shape_Length = shape_Length, Shape_Area = shape_Area, Shape = shape, 
              linesnum = linenum, 
              othtime = othertime, othqty = otherqty, othernum = othernum,
              othval = otherval, othkwh = otherkwh) %>% 
      mutate (allvessnum = NA, mobnum = NA, passnum = NA)
    
    
  }
  
  if (years[i] %in% c(2016, 2017, 2018, 2019, 2020)) {
    dat.temp <- dat.temp %>% 
      rename (trapkwh = trapskwh)
  }
  
  if (years[i] %in% c(2018)) {
    dat.temp <- dat.temp %>% 
      select (-objectid, -st_area_sh, -st_perimet)
  }
  
  if (years[i] %in% c(2019)) {
    dat.temp <- dat.temp %>% 
      select (-objectid_1, -st_area_sh, -st_perimet)
  }
  
  if (years[i] %in% c(2020)) {
    dat.temp <- dat.temp %>% 
      select (-objectid_1, -shape_leng)
  }
  
  print ("making distinct")
  dat.temp <- distinct (dat.temp)
  
  # bind together
  if (i == 1) {
    mmo_summaries <- dat.temp
  } else (mmo_summaries <- rbind(mmo_summaries, dat.temp))
}

# names_dattemp <- names (dat.temp) 
# name_mmo <- names (mmo_summaries)
# 
# setdiff(names_dattemp, name_mmo) # columns in recent years not in early years
# setdiff(name_mmo, names_dattemp) # columns in early years not in recent years

rm (dat.temp, i)
rm (name_mmo, names_dattemp)

summary (mmo_summaries)
names (mmo_summaries)
head (mmo_summaries)

boxplot (otbval~year, data = mmo_summaries)# only have gear specific information for 2016 onwards

boxplot (allvessnum~year, data = mmo_summaries) 
# only have all vessel numbers for 2016 onwards

boxplot (totkwh~year, data = mmo_summaries) 
# but have total effort from 2011 onwards

boxplot (tottime~year, data = mmo_summaries) 
# and total time from 2011 onwards

boxplot (passnum~year, data = mmo_summaries)
# info on numbers for mobile vs passive from 2016 only

boxplot (mobkwh~year, data = mmo_summaries)
# info on effort for mobile vs passive from 2011

boxplot (passqty~year, data = mmo_summaries)
# info on quantity for mobile vs passive from 2011 onwards

boxplot (pelqty~year, data = mmo_summaries)
# info on quantity demersal vs pelagic from 2016 only 

boxplot (demnum~year, data = mmo_summaries)
# info on number demersal vs pelagic from 2016 only 

boxplot (pelkwh~year, data = mmo_summaries)
# info on effort demersal vs pelagic from 2016 only

# Gear specific information only available 2016 onwards
# Pelagic vs Demersal information only available 2016 onwards
# Mobile vs Passive informtion available from 2011 onwards
# Total effort and time available from 2011 onwards
# TOtal vessel numbers only available 2016 onwards

saveRDS (mmo_summaries, file = "data/UK/MMO_data.Rdata")

## import newest MMO summarised data -----

# https://environment.data.gov.uk/dataset/bf55694c-3330-4c1d-a48e-db6bbe82ad77

# Are only downloadable as LYR files which can only be opened using ArcGIS...

# new.mmo.dat <- "data/UK/new MMO VMS Summaries"
# 
# years <- c(2014:2016)
# 
# for (i in 1:length(years)) {
#   
#   print (years[i])
#   
#   # read in data file for that year
#   dat.temp <-  rgdal::readOGR(dsn = new.mmo.dat, layer = paste0("MMO_UK_Fishing_Activity_", years[i], ".lyr"))
#   st_read (paste0(new.mmo.dat, "/MMO_UK_Fishing_Activity_2014.lyr"))
#   
#   # add year column
#   dat.temp$year <- years[i]
#   
#   # prior to 2016 no gear specific columns, need to add columns as NAs for prior years
#   
#   if (years[i] < 2016) {
#     
#     gears <- c("dem", "dred", "line", "nets", "other", "pel", "trap", "drb", "drh", 
#                "fix", "fpo", "gen", "gn", "gnc", "gnd", "gns", "gtr", "hf", "hmd", "lhp", 
#                "ll", "lld", "lls", "ltl", "lx", "mis", "ot", "otb", "otm", "ott", "ps",
#                "pt", "ptb", "ptm", "sdn", "spr", "ssc", "sx", "tb", "tbb", "tbn", "tm",
#                "tms", "tx"
#     )
#     
#     for (g in unique (gears)) {
#       
#       print (g)
#       varname_num <- paste0 (g, "num")
#       varname_time <- paste0 (g, "time")
#       varname_qty <- paste0 (g, "qty")
#       varname_val <- paste0 (g, "val")
#       varname_kwh <- paste0 (g, "kwh")
#       
#       dat.temp[[varname_num]] <- NA
#       dat.temp[[varname_time]] <- NA
#       dat.temp[[varname_qty]] <- NA
#       dat.temp[[varname_val]] <- NA
#       dat.temp[[varname_kwh]] <- NA
#       
#     }
#     
#     rm (gears, g, varname_kwh, varname_num, varname_val, varname_qty, varname_time)
#   }
#   
#   # also need to rename some column names for earlier years and remove unwanted columns
#   if (years[i] %in% c(2011)) {
#     dat.temp <- dat.temp %>% 
#       rename (Shape_Length = shape_Length, Shape_Area = shape_Area, Shape = shape, 
#               linesnum = linenum, 
#               othtime = othertime, othqty = otherqty, othernum = othernum,
#               othval = otherval, othkwh = otherkwh) %>%
#       select (-shape_leng)  %>% 
#       mutate (allvessnum = NA, mobnum = NA, passnum = NA)
#     
#     
#   }
#   
#   if (years[i] %in% c(2012, 2013, 2014, 2015)) {
#     dat.temp <- dat.temp %>% 
#       rename (Shape_Length = shape_Length, Shape_Area = shape_Area, Shape = shape, 
#               linesnum = linenum, 
#               othtime = othertime, othqty = otherqty, othernum = othernum,
#               othval = otherval, othkwh = otherkwh) %>% 
#       mutate (allvessnum = NA, mobnum = NA, passnum = NA)
#     
#     
#   }
#   
#   if (years[i] %in% c(2016, 2017, 2018, 2019, 2020)) {
#     dat.temp <- dat.temp %>% 
#       rename (trapkwh = trapskwh)
#   }
#   
#   if (years[i] %in% c(2018)) {
#     dat.temp <- dat.temp %>% 
#       select (-objectid, -st_area_sh, -st_perimet)
#   }
#   
#   if (years[i] %in% c(2019)) {
#     dat.temp <- dat.temp %>% 
#       select (-objectid_1, -st_area_sh, -st_perimet)
#   }
#   
#   if (years[i] %in% c(2020)) {
#     dat.temp <- dat.temp %>% 
#       select (-objectid_1, -shape_leng)
#   }
#   
#   print ("making distinct")
#   dat.temp <- distinct (dat.temp)
#   
#   # bind together
#   if (i == 1) {
#     mmo_summaries <- dat.temp
#   } else (mmo_summaries <- rbind(mmo_summaries, dat.temp))
# }
# 
# # names_dattemp <- names (dat.temp) 
# # name_mmo <- names (mmo_summaries)
# # 
# # setdiff(names_dattemp, name_mmo) # columns in recent years not in early years
# # setdiff(name_mmo, names_dattemp) # columns in early years not in recent years
# 
# rm (dat.temp, i)
# rm (name_mmo, names_dattemp)
# 
# summary (mmo_summaries)
# names (mmo_summaries)
# head (mmo_summaries)
# 
# boxplot (otbval~year, data = mmo_summaries)# only have gear specific information for 2016 onwards
# 
# boxplot (allvessnum~year, data = mmo_summaries) 
# # only have all vessel numbers for 2016 onwards
# 
# boxplot (totkwh~year, data = mmo_summaries) 
# # but have total effort from 2011 onwards
# 
# boxplot (tottime~year, data = mmo_summaries) 
# # and total time from 2011 onwards
# 
# boxplot (passnum~year, data = mmo_summaries)
# # info on numbers for mobile vs passive from 2016 only
# 
# boxplot (mobkwh~year, data = mmo_summaries)
# # info on effort for mobile vs passive from 2011
# 
# boxplot (passqty~year, data = mmo_summaries)
# # info on quantity for mobile vs passive from 2011 onwards
# 
# boxplot (pelqty~year, data = mmo_summaries)
# # info on quantity demersal vs pelagic from 2016 only 
# 
# boxplot (demnum~year, data = mmo_summaries)
# # info on number demersal vs pelagic from 2016 only 
# 
# boxplot (pelkwh~year, data = mmo_summaries)
# info on effort demersal vs pelagic from 2016 only

# Gear specific information only available 2016 onwards
# Pelagic vs Demersal information only available 2016 onwards
# Mobile vs Passive informtion available from 2011 onwards
# Total effort and time available from 2011 onwards
# TOtal vessel numbers only available 2016 onwards

## import EU STECF summarised data -----

STECF_effort <- read_csv("data/EU/EU27/spatial_effort_tableau_pts_EU27.csv")

STECF_landings.files <- list.files ("data/EU/EU27", pattern = "landings_tab")

for (s in 1:length (STECF_landings.files)) {
  
  print (STECF_landings.files[s])
  
  csv <- read.csv (paste0("data/EU/EU27/", STECF_landings.files[s]))
  
  if (s == 1) {STECF_landings <- csv} 
  else {STECF_landings <- rbind (csv, STECF_landings)}
  
}

rm (csv, s, STECF_landings.files)

head (STECF_effort)
head (STECF_landings)

# landings dataset is much larger than the effort dataset? Is detailing the species caught and assocated value/weight

boxplot (quarter~year, data = STECF_effort); table (STECF_effort$year, STECF_effort$quarter) # each quarter covered over the time period (2013 - 2022), looks fairly even

boxplot (year ~ vessel_length, data = STECF_effort) ; summary (as.factor(STECF_effort$vessel_length)) # quite a few not known, but most are assigned a vessel length

boxplot (year ~ fishing_tech, data = STECF_effort) ; summary (as.factor(STECF_effort$fishing_tech)) # no NAs

boxplot (year ~ gear_type, data = STECF_effort) ; summary (as.factor(STECF_effort$gear_type)) # no NAs

boxplot (year ~ mesh_size_range, data = STECF_effort) ; summary (as.factor(STECF_effort$mesh_size_range)) # here we do have a few NAs

boxplot (year ~ target_assemblage, data = STECF_effort) ; summary (as.factor(STECF_effort$target_assemblage)) # no NAs

boxplot (year ~ metier, data = STECF_effort) ; summary (as.factor(STECF_effort$metier)) # so many different metiers it is difficult to see what is going on

boxplot (year ~ supra_region, data = STECF_effort) ; summary (as.factor(STECF_effort$supra_region)) # what does MBS NAO and OFR mean?

boxplot (year ~ sub_region, data = STECF_effort) ; summary (as.factor(STECF_effort$sub_region)) # lots of different sub regions

summary (STECF_effort$totfishdays) # covering a very wide range, no NAs. Ranges from 0 to a huge maximum but most values are small.

# effort in the effort data is measured in DAYS, differing from the MMO datasets (kw/hour). 
# Data broken down by quarter, data from 2013 to 2022
# Data on gear types and target assemblage are pretty complete.

boxplot (quarter~year, data = STECF_landings); table (STECF_landings$year, STECF_landings$quarter) # each quarter covered over the time period (2013 - 2022), looks fairly even

boxplot (year~vessel_length, data = STECF_landings); summary (as.factor(STECF_effort$vessel_length)) # quite a few not known, but most are assigned a vessel length

boxplot (year~fishing_tech, data = STECF_landings); summary (as.factor(STECF_effort$fishing_tech)) # no NAs

boxplot (year~gear_type, data = STECF_landings); summary (as.factor(STECF_effort$gear_type)) # no NAs

boxplot (year~target_assemblage, data = STECF_landings); summary (as.factor(STECF_effort$target_assemblage)) #  no NAs

boxplot (year~vessel_length, data = STECF_landings); summary (as.factor(STECF_effort$vessel_length)) # quite a few not known, but most are assigned a vessel length

boxplot (year ~ supra_region, data = STECF_landings) ; summary (as.factor(STECF_landings$supra_region)) # what does MBS NAO and OFR mean?

boxplot (year ~ sub_region, data = STECF_landings) ; summary (as.factor(STECF_landings$sub_region))  # lots of different sub regions

summary (STECF_landings$totwghtlandg) # mostly zeros, some very large values
summary (STECF_landings$totvallandg) # overing a very wide range, no NAs. Ranges from 0 to a huge maximum but most values are small.

saveRDS (STECF_landings, file = "data/EU/STECF_landings.Rdata")
saveRDS (STECF_effort, file = "data/EU/STECF_effort.Rdata")

## Are we able to join these datasets together? -----

head (STECF_landings)
head (STECF_effort)
head (mmo_summaries)
head (Csquares)

# STECF effort measured in days
# MMO effort measured in kw/h, but there is time measured in minutes
# MMO value in pounds, EU data in Euros?
# MMO data at the 0.05 degree resolution, STECF data at the 0.5 degree resolution
# MMO data 2011 to 2020, STECF data 2013 to 2022
# MMO data only for vessels over 15m in length, STECF data all vessel lengths but we can filter to larger vessels only
# MMO data whole year only, STECF quarterly
# so, what we can do is filter datasets to corresponding years only, focus on fishing effort (for now)

# get year range consistent between the two datasets
comparison_years <- c(2013:2020)

STECF_effort_comparison <- STECF_effort %>% 
  filter (year %in% c(comparison_years))

mmo_summaries_comparison <- mmo_summaries %>% 
  filter (year %in% c(comparison_years))

# summarise MMO data by coarser 0.5 resolution
mmo_summaries_comparison <- mmo_summaries_comparison %>% 
  mutate (lat_ct = st_coordinates(st_centroid(mmo_summaries_comparison))[,2]) %>% 
  mutate (lon_ct = st_coordinates(st_centroid(mmo_summaries_comparison))[,1])

mmo_summaries_comparison$cs_code0.05 <- CSquare(mmo_summaries_comparison$lon_ct, mmo_summaries_comparison$lat_ct,degrees = 0.05 )
mmo_summaries_comparison <- left_join (mmo_summaries_comparison, Csquares, by = "cs_code0.05")

mmo_summaries_comparison_summ <- mmo_summaries_comparison %>% 
  as.data.frame() %>% 
  select (-Shape) %>% 
  group_by (cs_code0.5, year) %>% 
  summarise (sum_tottime = sum(tottime))

# MMO convert effort from minutes to days
mmo_summaries_comparison_summ$sum_tottime_hours <- mmo_summaries_comparison_summ$sum_tottime/60
mmo_summaries_comparison_summ$sum_tottime_days <- mmo_summaries_comparison_summ$sum_tottime_hours/24

# STECF data filter to vessels over 15 metres - note vessels where length is not known is also removed - will have to do to 12m
unique (STECF_effort$vessel_length)
STECF_effort_comparison <- STECF_effort_comparison %>% 
  filter (vessel_length %in% c("VL2440", "VL40XX", "VL1824", "VL1218") )

# STECF aggregate across the whole year

STECF_effort_comparison_sum <- STECF_effort_comparison %>% 
  group_by(year, cscode) %>% 
  summarise (totfishdaysSum = sum (totfishdays))

# streamline column names and add column with data source/country
STECF_effort_COMPARE <- STECF_effort_comparison_sum %>% 
  rename (cs_code0.5 = cscode, Fishing_effort_days = totfishdaysSum) %>% 
  mutate (year = as.integer (year), source = "STECF", country = "EU")

MMO_effort_COMPARE <- mmo_summaries_comparison_summ %>% 
  rename (Fishing_effort_days = sum_tottime_days) %>% 
  select (-sum_tottime, -sum_tottime_hours) %>% 
  mutate (source = "MMO", country = "UK")

rm (Csquare_0.05_crop, mmo_summaries_comparison, mmo_summaries_comparison_summ, STECF_effort_comparison, STECF_effort_comparison_sum)

combined_EU_UK_data <- rbind (STECF_effort_COMPARE, MMO_effort_COMPARE)

saveRDS (combined_EU_UK_data, file = "data/combined EU_UK_data.Rdata")


