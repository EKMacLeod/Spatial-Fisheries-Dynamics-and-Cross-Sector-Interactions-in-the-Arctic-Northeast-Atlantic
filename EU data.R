## EU fleet analysis ##
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
library (RColorBrewer)
library(treemap)

## functions -----

make_poly <- function(xmin,xmax,ymin,ymax){
  
  st_polygon(list(
    matrix(
      c(
        xmin,ymin,
        xmax,ymin,
        xmax,ymax,
        xmin,ymax,
        xmin,ymin
      ),
      byrow = TRUE,
      ncol = 2
    )
  ))
}

make_cell_treemap_effort <- function(df){
  
  bb <- st_bbox(df)
  
  tm <- treemap(
    st_drop_geometry(df),
    index = "target_assemblage2",
    vSize = "prop",
    draw = FALSE
  )
  
  rects <- tm$tm
  
  rects <- rects %>%
    mutate(
      xmin = bb["xmin"] + x0 * (bb["xmax"] - bb["xmin"]),
      xmax = bb["xmin"] + (x0 + w) * (bb["xmax"] - bb["xmin"]),
      ymin = bb["ymin"] + y0 * (bb["ymax"] - bb["ymin"]),
      ymax = bb["ymin"] + (y0 + h) * (bb["ymax"] - bb["ymin"])
    )
  
  polys <- lapply(
    seq_len(nrow(rects)),
    function(i)
      make_poly(
        rects$xmin[i],
        rects$xmax[i],
        rects$ymin[i],
        rects$ymax[i]
      )
  )
  
  st_sf(
    cscode = unique(df$cscode),
    target_assemblage2 = rects$target_assemblage2,
    geometry = st_sfc(polys, crs = st_crs(df))
  )
}

make_cell_treemap_landings <- function(df){
  
  bb <- st_bbox(df)
  
  tm <- treemap(
    st_drop_geometry(df),
    index = "Common",
    vSize = "prop",
    draw = FALSE
  )
  
  rects <- tm$tm
  
  rects <- rects %>%
    mutate(
      xmin = bb["xmin"] + x0 * (bb["xmax"] - bb["xmin"]),
      xmax = bb["xmin"] + (x0 + w) * (bb["xmax"] - bb["xmin"]),
      ymin = bb["ymin"] + y0 * (bb["ymax"] - bb["ymin"]),
      ymax = bb["ymin"] + (y0 + h) * (bb["ymax"] - bb["ymin"])
    )
  
  polys <- lapply(
    seq_len(nrow(rects)),
    function(i)
      make_poly(
        rects$xmin[i],
        rects$xmax[i],
        rects$ymin[i],
        rects$ymax[i]
      )
  )
  
  st_sf(
    cscode = unique(df$cscode),
    Common = rects$Common,
    geometry = st_sfc(polys, crs = st_crs(df))
  )
}

## load in datasets required -----
load ("data/spatial data/spatial_data.Rdata")

STECF_landings <- readRDS ("data/EU/STECF_landings.Rdata")
STECF_effort <- readRDS ("data/EU/STECF_effort.Rdata")

## make dataset a spatial object and limit to "Arctic" areas -----

Csquare_0.5 <- Csquare_0.5 %>% rename (cscode = cs_code0.5)

STECF_landings <- left_join (STECF_landings, Csquare_0.5)
STECF_landings <- st_as_sf (STECF_landings)

STECF_effort <- left_join (STECF_effort, Csquare_0.5)
STECF_effort <- st_as_sf (STECF_effort)

STECF_landings <- STECF_landings %>% filter (rectangle_lat > 57.9999999) %>% filter (rectangle_lon <45)
STECF_effort <- STECF_effort %>% filter (rectangle_lat > 57.9999999) %>% filter (rectangle_lon <45)

## what is the most common targetassemblage across the Arctic space? EFFORT -----

# for each square, which target assemblage has the highest effort?
# TargetAssemblage <- STECF_effort %>% 
#   group_by (cscode, target_assemblage) %>%
#   summarise (total_fishingDays = sum (totfishdays))

# TargetAssemblage <- TargetAssemblage %>% 
#   group_by (cscode) %>%
#   filter(total_fishingDays == max(total_fishingDays)) 

summary (as.factor(STECF_effort$target_assemblage))

# ANA = anadromous species
# CAT = catadromous species
# CRU = Crustaceans
# DEF = Demersal Fish
# DWS = deep water species
# FWS = Freshwater species
# LPF = Large pelagic fish
# MCD = Mixed crustaceans and demersal fish
# MDD = Mixed demersal species and deep water species
# MOL = Molluscs
# NK = Not known
# SPF = Small pelagic fish

STECF_effort$target_assemblage2 <- NA

STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "CAT"] <-"NK"

STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "ANA"] <- "Other"
STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "CAT"] <- "Other"
STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "CRU"] <- "Crustaceans"
STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "DEF"] <- "Demersal Fish"
STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "DWS"] <- "Deepwater Species"
STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "FWS"] <- "Other"
STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "LPF"] <- "Other"
STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "MCD"] <- "Mixed crustaceans and demersal fish"
STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "MDD"] <- "Other"
STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "MOL"] <- "Molluscs"
STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "NK"] <- "Other"
STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "SPF"] <- "Small Pelagic Fish"
STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "FIF"] <- "Other"
STECF_effort$target_assemblage2[STECF_effort$target_assemblage == "CEP"] <- "Molluscs"

summary (as.factor (STECF_effort$target_assemblage2))
STECF_effort %>% 
  filter (is.na (target_assemblage2)) %>% 
  select (target_assemblage, target_assemblage2)

TargetAssemblage <- STECF_effort %>%
  group_by(cscode, target_assemblage2) %>%
  summarise(
    fishingDays = sum(totfishdays, na.rm = TRUE),
    geometry = first(x),
    .groups = "drop"
  ) %>%
  group_by(cscode) %>%
  mutate(
    prop = fishingDays / sum(fishingDays)
  ) %>%
  ungroup()

TargetAssemblage_tm <- TargetAssemblage %>%
  group_by(cscode) %>%
  group_split() %>%
  lapply(make_cell_treemap_effort) %>%
  do.call(rbind, .)

target_cols <- c(
  "Small Pelagic Fish" = "#66C2A5",
  "Demersal Fish" = "#8DA0CB",
  "Deepwater Species" = "#E78AC3",
  "Crustaceans" = "#A6D854",
  "Mixed crustaceans and demersal fish" = "#1B9E77",
  "Mixed demersal species and deep water species" = "#D95F02",
  "Molluscs" = "#7570B3",
  "Other" = "grey50"
)

TargetAssemblage_tm$target_assemblage2 <- factor (TargetAssemblage_tm$target_assemblage2,
                                                  levels = c("Small Pelagic Fish", 
                                                             "Demersal Fish", 
                                                             "Crustaceans", 
                                                             "Mixed crustaceans and demersal fish", 
                                                             "Mixed demersal species and deep water species", 
                                                             "Deepwater Species", 
                                                             "Molluscs", 
                                                             "Other"))

# plot 
ggplot () +
  geom_sf (data = TargetAssemblage_tm, aes (fill = target_assemblage2), colour = NA) + 
  scale_fill_manual (values = target_cols, name = "", labels = scales::label_wrap(18)) +
  geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-100, -500),
               linewidth=c(0.3), colour="grey50") +
  geom_sf (data = eez, aes (), fill = NA, colour = "black") +
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) +
  theme_bw() + theme (panel.background = element_rect (fill = "aliceblue"), legend.position = "right") + 
  xlab ("Longitude") + ylab ("Latitude") +
  theme (legend.text = element_text(size = 8))

ggsave("output/EU/targetassemblage6.png", width = 6, height= 5)

rm (TargetAssemblage, TargetAssemblage_tm, target_cols)

## what is the most common species caught across the Arctic space? LANDINGS -----

# lots of species codes, need to summarise into simpler descriptions
summary (as.factor(STECF_landings$species))
codes <- getCodeList("IC_Species")
codes <- codes %>% rename (species = Key) %>% select (species, Description, LongDescription)
STECF_landings <- left_join(STECF_landings, codes)

summary (as.factor(STECF_landings$Description))

species_test <- STECF_landings %>% 
  group_by (cscode, Description) %>% 
  summarise(weight_sum = sum(totwghtlandg, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(cscode) %>%
  mutate(
    prop = weight_sum / sum(weight_sum)
  ) %>%
  filter (prop != 0) %>%
  ungroup()

write.csv (species_test, "output/EU/species.test.csv")
rm (species_test)

STECF_landings %>%
  st_drop_geometry() %>%
  group_by(Description) %>%
  summarise(
    weight_sum = sum(totwghtlandg, na.rm = TRUE)
  ) %>%
  mutate(
    prop = weight_sum / sum(weight_sum)
  ) %>%
  arrange(desc(prop))

# Herring
# Mackerel
# Blue Whiting
# Sprat
# Cod
# Norway Pout
# Redfish
# Capelin
# Other Demersal Fishes
# Other Flatfish
# Shrimp
# Crabs
# Other

STECF_landings <- STECF_landings %>%
  mutate(
    Common = case_when(
      
      # Pelagic species
      Description == "Clupea harengus" ~ "Herring",
      Description == "Scomber scombrus" ~ "Mackerel",
      Description == "Micromesistius poutassou" ~ "Blue Whiting",
      Description == "Sprattus sprattus" ~ "Sprat",
      Description == "Mallotus villosus" ~ "Capelin",
      
      # Cod
      Description == "Gadus morhua" ~ "Cod",
      
      # Norway pout
      Description == "Trisopterus esmarkii" ~ "Norway Pout",
      
      # Redfish complex
      Description %in% c("Sebastes mentella", "Sebastes norvegicus", "Sebastes spp"
      ) ~ "Redfish",
      
      # Flatfish complex
      Description %in% c(
        "Pleuronectes platessa","Psetta maxima","Hippoglossus hippoglossus",
        "Reinhardtius hippoglossoides", "Glyptocephalus cynoglossus", "Limanda limanda", 
        "Scophthalmus rhombus", "Lepidorhombus whiffiagonis", "Microstomus kitt", 
        "Lepidorhombus spp", "Solea solea", "Platichthys flesus"
      ) ~ "Flatfish",
      
      # Northern shrimp
      Description == "Pandalus borealis" ~ "Northern Shrimp",
      
      # Nephrops
      Description == "Nephrops norvegicus" ~ "Nephrops",
      
      # Crabs
      Description == "Chaceon affinis" ~ "Crabs",
      
      # Demersal fish complex
      Description %in% c(
        "Melanogrammus aeglefinus",  # Haddock
        "Pollachius virens",         # Saithe/Pollock
        "Pollachius pollachius",     # Pollock
        "Merluccius merluccius",     # Hake
        "Molva molva",               # Ling
        "Molva dypterygia",          # Blue ling
        "Brosme brosme",             # Tusk
        "Lophius piscatorius",       # Anglerfish
        "Lophius spp",
        "Lophiidae", 
        "Merlangius merlangus"       # Whiting
      ) ~ "Demersal Fish",
      
      TRUE ~ "Other"
    )
  )

STECF_landings <- STECF_landings %>%
  mutate(
    Common = case_when(
      species %in% c("CRQ", "CRG", "CRQ", "PCR") ~ "Crabs",
      species == "PLA" ~ "Flatfish",
      TRUE ~ Common
    )
  )

SpeciesLookup <- STECF_landings %>%
  st_drop_geometry() %>%
  distinct(species, Description, Common) %>% 
  arrange (Common)

write_csv (SpeciesLookup, file = "output/EU/landings lookup table.csv")
rm (SpeciesLookup)

# check the proportions of the new grouped species
STECF_landings %>%
  st_drop_geometry() %>%
  group_by(Common) %>%
  summarise(
    weight_sum = sum(totwghtlandg, na.rm = TRUE)
  ) %>%
  mutate(
    prop = weight_sum / sum(weight_sum)
  ) %>%
  arrange(desc(prop))

# and look at how they dominate specific cells
STECF_landings %>%
  group_by(cscode, Common) %>%
  summarise(weight = sum(totwghtlandg), .groups = "drop") %>%
  group_by(Common) %>%
  summarise(max_prop = max(weight / sum(weight)))

# looking at species grouped into other
STECF_landings %>%
  filter(Common == "Other") %>%
  st_drop_geometry() %>%
  group_by(Description) %>%
  summarise(
    weight_sum = sum(totwghtlandg, na.rm = TRUE)
  ) %>%
  arrange(desc(weight_sum))

# looking into species where description was NA (a big component in others)
STECF_landings %>%
  filter (Common == "Other") %>% 
  filter(is.na(Description)) %>%
  st_drop_geometry() %>%
  count(species, sort = TRUE)

# A few freshwater fish in there, SME = smelt, CAT = Atlantic Catfish

temp <- STECF_landings[is.na(STECF_landings$Common),]
rm (temp)

STECF_landings$Common <- factor(
  STECF_landings$Common,
  levels = c(
    "Herring",
    "Mackerel",
    "Blue Whiting",
    "Sprat",
    "Capelin",
    
    "Cod",
    "Demersal Fish",
    "Norway Pout",
    "Redfish",
    "Flatfish",
    
    "Northern Shrimp",
    "Nephrops",
    "Crabs",
    
    "Other"))

# calculate the proportion of species in landings in each cell (value and weight)

# Landings_value <- STECF_landings %>% 
#   group_by (cscode, species) %>% 
#   summarise (TotalVal = sum (totvallandg))
# Landings_value <- Landings_value %>% 
#   group_by (cscode) %>% 
#   filter(TotalVal == max(TotalVal)) %>% 
#   rename (value = TotalVal) %>% 
#   mutate (variable = "Value (Euro)") %>% 
#   select (cscode, value, variable, species)

Landings_value <- STECF_landings %>%
  group_by(cscode, Common) %>%
  summarise(value = sum(totvallandg, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(cscode) %>%
  mutate(
    prop = value / sum(value)
  ) %>%
  filter (prop != 0) %>%
  ungroup() %>% 
  mutate (variable = "Value (Euro)") %>% 
  select (cscode, prop, variable, Common)

# Landings_weight <- STECF_landings %>% 
#   group_by (cscode, species) %>% 
#   summarise (TotalWgt = sum (totwghtlandg))
# Landings_weight <- Landings_weight %>% 
#   group_by (cscode) %>% 
#   filter(TotalWgt == max(TotalWgt)) %>% 
#   rename (value = TotalWgt) %>% 
#   mutate (variable = "Weight (Tonnes)") %>% 
#   select (cscode, value, variable, species)

Landings_weight <- STECF_landings %>%
  group_by(cscode, Common) %>%
  summarise(weight = sum(totwghtlandg, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(cscode) %>%
  mutate(
    prop = weight / sum(weight)
  ) %>%
  filter (prop != 0) %>%
  ungroup() %>% 
  mutate (variable = "Weight (Tonnes)") %>% 
  select (cscode, prop, variable, Common)

Landings_treeplot <- rbind (Landings_value)

# extract bbox for each csquare for tree plot
cell_boxes <- Landings_value %>%
  group_by(cscode) %>%
  summarise(geometry = first(x)) %>%
  rowwise() %>%
  mutate(
    bb = list(st_bbox(geometry)),
    xmin = bb$xmin,
    xmax = bb$xmax,
    ymin = bb$ymin,
    ymax = bb$ymax
  ) %>%
  st_drop_geometry() %>%
  select(-bb)

treemap_dat_value <- Landings_value
treemap_dat_weight <- Landings_weight

treemap_dat_value <- left_join(
  treemap_dat_value,
  cell_boxes,
  by = "cscode"
)

treemap_dat_weight <- left_join(
  treemap_dat_weight,
  cell_boxes,
  by = "cscode"
)

# turn into a treemap sf - creating treemap df and bbox, converting to sf object
treemap_sf_value <- treemap_dat_value %>%
  group_by(cscode) %>%
  group_split() %>%
  lapply(make_cell_treemap_landings) %>%
  do.call(rbind, .) %>% 
  mutate (variable = "Value (Euro)")
  
treemap_sf_weight <- treemap_dat_weight %>%
  group_by(cscode) %>%
  group_split() %>%
  lapply(make_cell_treemap_landings) %>%
  do.call(rbind, .) %>% 
  mutate (variable = "Weight (Tonnes)")

treemap_sf <- rbind (treemap_sf_value, treemap_sf_weight)

# plot
# paired_colors <- brewer.pal(12, "Paired")
# print(paired_colors)

cols <- c(
  
  # Pelagics
  "Herring"         = "#1F77B4",  # blue
  "Mackerel"        = "#2CA02C",  # green
  "Blue Whiting"    = "#17BECF",  # cyan
  "Sprat"           = "#E377C2",  # pink
  "Capelin"         = "#9467BD",  # purple
  
  # Demersals
  "Cod"             = "#D62728",  # red
  "Demersal Fish"   = "#8C564B",  # brown
  "Norway Pout"     = "#BCBD22",  # olive
  "Redfish"         = "#7F0000",  # dark burgundy
  "Flatfish"        = "#332288",  # indigo
  
  # Shellfish
  "Northern Shrimp" = "#FF7F0E",  # orange
  "Nephrops"        = "#F781BF",  # pink-orange
  "Crabs"           = "#006D77",  # dark teal
  
  # Residual
  "Other"           = "grey55"
)

ggplot() +
  geom_sf(data = treemap_sf, aes(fill = Common), colour = NA) +
  scale_fill_manual (values = cols, name = "", labels = scales::label_wrap(8)) +
  facet_wrap (~variable, ncol = 1) +
  geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-100, -500),
               linewidth=c(0.3), colour="grey50") +
  geom_sf (data = eez, aes (), fill = NA, colour = "black") +
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) +
  xlab ("Longitude") + ylab ("Latitude") +
  theme_bw() + theme (panel.background = element_rect (fill = "aliceblue"), legend.position = "right") +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme (legend.text = element_text(size = 6)) +
  
  theme (axis.text.x = element_text(size = 5), 
         axis.title.x = element_text(size = 6), 
         axis.ticks.length.x = unit(0.5, "mm")) +
  theme (axis.text.y = element_text(size = 5), 
         axis.title.y = element_text(size = 6), 
         axis.ticks.length.y = unit(0.5, "mm")) +
  theme (strip.text.x = element_text(size = 6))

ggsave("output/EU/landings13.png", width = 6, height= 6)

# temp_weight <- Landings %>% 
#   filter (Common == "Cod") %>% 
#   filter (variable == "Weight (Tonnes)")
# temp_weight2 <- STECF_landings %>% 
#   filter (cscode %in% unique (temp_weight$cscode)) %>% 
#   filter (species %in% c("COD")) %>% 
#   group_by (gear_type) %>% 
#   summarise (sum_weight = sum(totwghtlandg)) %>% 
#   mutate (total_weight = sum (sum_weight), perc_value = (sum_weight/total_weight) *100) 
# 
# temp_weight <- Landings %>% 
#   filter (Common == "Crabs") %>% 
#   filter (variable == "Weight (Tonnes)") 
# temp_weight %>% 
#   group_by (species) %>% 
#   summarise (sum_weight = sum (value)) %>% 
#   mutate (total_weight = sum (sum_weight), perc_weight = (sum_weight/total_weight)*100)
# 
# temp_value <- Landings %>% 
#   filter (Common == "Crabs") %>% 
#   filter (variable == "Value (Euro)") 
# temp_value %>% 
#   group_by (species) %>% 
#   summarise (sum_value = sum (value)) %>% 
#   mutate (total_value = sum(sum_value), perc_weight = (sum_value/total_value)*100)
# ggplot () +
#   geom_sf (data = temp_value, aes ())

# temp_weight <- Landings %>%
#   filter (Common == "Northern Shrimp") %>%
#   filter (variable == "Weight (Tonnes)")
# temp_weight2 <- STECF_landings %>%
#   filter (cscode %in% unique (temp_weight$cscode)) %>%
#   filter (species %in% c("PRA", "CAS")) %>%
#   group_by (gear_type) %>%
#   summarise (sum_weight = sum(totwghtlandg)) %>%
#   mutate (total_weight = sum (sum_weight), perc_value = (sum_weight/total_weight) *100)

rm (codes, Landings_weight, Landings_value, Landings, temp)
rm (treemap_dat_value, treemap_dat_weight, treemap_sf_value, treemap_sf_weight)
rm (treemap_sf)

## Quarterly distribution of effort -----

effort_quarter <- STECF_effort %>% 
  group_by (cscode, quarter) %>% 
  summarise (Total_effort_days = sum (totfishdays))

effort_quarter$quarter <- paste0 ("Quarter ", effort_quarter$quarter)

summary (effort_quarter$Total_effort_days)
max <- 60

breakk <- pretty (seq (0, max))

max <- breakk[length(breakk)]

label <- breakk
label <- label[-length (label)]
label <- c(label, paste0(">", max))

mediann <- median (breakk)

# for visuals cap max
effort_quarter$Total_effort_days[effort_quarter$Total_effort_days > (max)] <- max

ggplot () +
  geom_sf (data = effort_quarter, aes (fill = Total_effort_days), colour = NA) + 
  facet_wrap (~quarter) +
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
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) + 
  xlab ("Longitude") + ylab ("Latitude") +
  theme_bw() + theme (panel.background = element_rect (fill = "aliceblue"))

ggsave("output/EU/effort_quarter2.png", width = 8, height= 6)

rm (effort_quarter)

## Distribution of effort by mobile/passive -----

head (STECF_effort)
unique(STECF_effort$gear_type); unique (STECF_effort$metier)

STECF_effort$mob_pass <- NA
STECF_effort$mob_pass [STECF_effort$gear_type %in% c("OTB", "OTM", "PTM", "OTT", "SSC", "PS", "SDN", "TBB", "LTL", "SB", "DRH", "DRB", "PTB")] <- "Mobile"
STECF_effort$mob_pass [STECF_effort$gear_type %in% c("GNS", "LLS", "LHP", "GND", "FPO", "GTR", "FYK", "FPN", "LLD")] <- "Passive"

summary (as.factor (STECF_effort$mob_pass))

effort_mob_pass <- STECF_effort %>% 
  group_by (mob_pass, cscode) %>% 
  summarise (Total_effort_days = sum (totfishdays))

summary (effort_mob_pass$Total_effort_days)
max <- 50

breakk <- pretty (seq (0, max))

max <- breakk[length(breakk)]

label <- breakk
label <- label[-length (label)]
label <- c(label, paste0(">", max))

mediann <- median (breakk)

# for visuals cap max
effort_mob_pass$Total_effort_days[effort_mob_pass$Total_effort_days > (max)] <- max

p <- ggplot () +
  geom_sf (data = effort_mob_pass, aes (fill = Total_effort_days), colour = NA) + 
  facet_wrap (~mob_pass, ncol = 2) +
  # scale_fill_gradient2 (name = "Fishing Effort \n(days)",
  #                       low = "white", mid = "yellow", high = "red",
  #                       midpoint = mediann,
  #                       labels = label,
  #                       breaks = breakk, 
  #                       limits = c(0, breakk[length(breakk)])
  # ) + 
  scale_fill_viridis_c (name = "Fishing Effort \n(days)", labels = label, breaks = breakk) +
  geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-50, -100, -150, -500, -1000),
               linewidth=c(0.1), colour="lightblue") +
  geom_sf (data = eez, aes (), fill = NA, colour = "black") +  
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) +
  theme_bw() + theme (panel.background = element_rect (fill = "aliceblue")) +
  xlab ("Longitude") + ylab ("Latitude")

ggsave(p, filename ="output/EU/effort_mob_pass.png", width = 10, height= 5)

rm (effort_mob_pass)

## Yearly distribution of effort -----

effort_year <- STECF_effort %>% 
  group_by (cscode, year) %>% 
  summarise (Total_effort_days = sum (totfishdays))

summary (effort_year$Total_effort_days)
max <- 40

breakk <- pretty (seq (0, max))

max <- breakk[length(breakk)]

label <- breakk
label <- label[-length (label)]
label <- c(label, paste0(">", max))

mediann <- median (breakk)

# for visuals cap max
effort_year$Total_effort_days[effort_year$Total_effort_days > (max)] <- max

p <- ggplot () +
  geom_sf (data = effort_year, aes (fill = Total_effort_days), colour = NA) + 
  facet_wrap (~year, ncol = 2) +
  scale_fill_gradient2 (name = "Fishing Effort \n(days)",
                        low = "white", mid = "yellow", high = "red",
                        midpoint = mediann,
                        labels = label,
                        breaks = breakk, 
                        limits = c(0, breakk[length(breakk)])
  ) + 
  geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-100, -500),
               linewidth=c(0.3), colour="grey50") +
  geom_sf (data = eez, aes (), fill = NA, colour = "black") +  
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) +
  theme_bw() + theme (panel.background = element_rect (fill = "aliceblue"))

ggsave(p, filename ="output/EU/effort_year.png", width = 6, height= 12)

for (y in unique (effort_year$year)) {
  
  print (y)
  
  effort_year_y <- effort_year %>% filter (year == y)
  
  p_y <- ggplot () +
  geom_sf (data = effort_year_y, aes (fill = Total_effort_days), colour = NA) + 
  # facet_wrap (~year, ncol = 2) +
  scale_fill_gradient2 (name = "Fishing Effort \n(days)",
                        low = "white", mid = "yellow", high = "red",
                        midpoint = mediann,
                        labels = label,
                        breaks = breakk, 
                        limits = c(0, breakk[length(breakk)])
  ) + 
  geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-100, -500),
               linewidth=c(0.3), colour="grey50") +
  geom_sf (data = eez, aes (), fill = NA, colour = "black") +  
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) +
  theme_bw() + theme (panel.background = element_rect (fill = "aliceblue"))

ggsave(p_y, filename =paste0("output/EU/year/effort_year_", y, ".png"), width = 6, height= 6)
  
}


rm (effort_year)

## Yearly and quarterly distribution of effort -----

# effort_year_quarter <- STECF_effort %>% 
#   group_by (cscode, year, quarter) %>% 
#   summarise (Total_effort_days = sum (totfishdays))
# 
# summary (effort_year_quarter$Total_effort_days)
# max <- 40
# 
# breakk <- pretty (seq (0, max))
# 
# max <- breakk[length(breakk)]
# 
# label <- breakk
# label <- label[-length (label)]
# label <- c(label, paste0(">", max))
# 
# mediann <- median (breakk)
# 
# # for visuals cap max
# effort_year$Total_effort_days[effort_year$Total_effort_days > (max)] <- max
# 
# ggplot () +
#   geom_sf (data = effort_year, aes (fill = Total_effort_days), colour = NA) + 
#   facet_wrap (~year, ncol = 2) +
#   scale_fill_gradient2 (name = "Fishing Effort \n(days)",
#                         low = "white", mid = "yellow", high = "red",
#                         midpoint = mediann,
#                         labels = label,
#                         breaks = breakk, 
#                         limits = c(0, breakk[length(breakk)])
#   ) + 
#   geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-50, -100, -150, -500, -1000),
#                linewidth=c(0.1), colour="lightblue") +
#   geom_sf (data = eez, aes (), fill = NA, colour = "black") +  
#   geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
#   coord_sf (xlim = c(-40, 40), ylim = c(59, 80)) +
#   theme_bw() + theme (panel.background = element_rect (fill = "aliceblue"))
# 
# ggsave("output/EU/effort_year.png", width = 6, height= 12)
# 
# rm (effort_year

## Overall distribution of effort and landings, can we link to any specific features? -----

# ## Percentage of catches in terms of value and weight in/out of the NEAFC areas -----
# 
# total_value <- STECF_landings %>% 
#   group_by (year) %>% 
#   summarise (total_value = sum (totvallandg)) %>% 
#   as.data.frame () %>% 
#   select (-x)
# total_value_species <- STECF_landings %>% 
#   group_by (species) %>% 
#   summarise (total_value_sp = sum (totvallandg)) %>% 
#   as.data.frame () %>% 
#   select (-x)
# 
# total_weight <- STECF_landings %>% 
#   group_by (year) %>% 
#   summarise (total_weight = sum (totwghtlandg)) %>% 
#   as.data.frame () %>% 
#   select (-x)
# total_weight_species <- STECF_landings %>% 
#   group_by (species) %>% 
#   summarise (total_weight_sp = sum (totwghtlandg)) %>% 
#   as.data.frame () %>% 
#   select (-x)
# 
# # total landings in value/weight in neafc
# 
# total_value_neafc <- STECF_landings %>% 
#   mutate (totvallandgNEAFC = totvallandg * prop_neafc) %>% 
#   group_by (year) %>% 
#   summarise (total_value_neafc = sum (totvallandgNEAFC)) %>% 
#   as.data.frame () %>% 
#   select (-x)
# total_value_species_neafc <- STECF_landings %>% 
#   mutate (totvallandgNEAFC = totvallandg * prop_neafc) %>% 
#   group_by (species) %>% 
#   summarise (total_value_sp_neafc = sum (totvallandgNEAFC)) %>% 
#   as.data.frame () %>% 
#   select (-x)
# 
# total_weight_neafc <- STECF_landings %>% 
#   mutate (totwghtlandgNEAFC = totwghtlandg * prop_neafc) %>% 
#   group_by (year) %>% 
#   summarise (total_weight_neafc = sum (totwghtlandgNEAFC)) %>% 
#   as.data.frame () %>% 
#   select (-x)
# total_weight_species_neafc <- STECF_landings %>% 
#   mutate (totwghtlandgNEAFC = totwghtlandg * prop_neafc) %>% 
#   group_by (species) %>% 
#   summarise (total_weight_sp_neafc = sum (totwghtlandgNEAFC)) %>% 
#   as.data.frame () %>% 
#   select (-x)
# 
# percentage_neafc_landings_val <- merge (total_value, total_value_neafc) %>% 
#   mutate (prop_neafc = (total_value_neafc/total_value)*100)
# percentage_neafc_landings_val_sp <- merge (total_value_species, total_value_species_neafc) %>% 
#   mutate (prop_neafc = (total_value_sp_neafc/total_value_sp)*100)
# 
# percentage_neafc_landings_val <- percentage_neafc_landings_val %>% 
#   mutate (prop_neafc = round (prop_neafc, digits = 2)) %>% 
#   mutate (total_value = round (total_value, digits = 2)) %>% 
#   mutate (total_value_neafc = round (total_value_neafc, digits = 2))
# percentage_neafc_landings_val_sp <- percentage_neafc_landings_val_sp %>% 
#   mutate (prop_neafc = round (prop_neafc, digits = 2)) %>% 
#   mutate (total_value_sp = round (total_value_sp, digits = 2)) %>% 
#   mutate (total_value_sp_neafc = round (total_value_sp_neafc, digits = 2))
# 
# write.csv (percentage_neafc_landings_val, file = "output/EU/comparison percentage neafc landings value.csv")
# write.csv (percentage_neafc_landings_val_sp, file = "output/EU/comparison percentage neafc landings value species.csv")
# 
# rm (percentage_neafc_landings_val, percentage_neafc_landings_val_sp, total_value, total_value_neafc, total_value_species, total_value_species_neafc)
# 
# percentage_neafc_landings_wgt <- merge (total_weight, total_weight_neafc) %>% 
#   mutate (prop_neafc = (total_weight_neafc/total_weight)*100)
# percentage_neafc_landings_wgt_sp <- merge (total_weight_species, total_weight_species_neafc) %>% 
#   mutate (prop_neafc = (total_weight_sp_neafc/total_weight_sp)*100)
# 
# percentage_neafc_landings_wgt <- percentage_neafc_landings_wgt %>% 
#   mutate (prop_neafc = round (prop_neafc, digits = 2)) %>% 
#   mutate (total_weight = round (total_weight, digits = 2)) %>% 
#   mutate (total_weight_neafc = round (total_weight_neafc, digits = 2))
# percentage_neafc_landings_wgt_sp <- percentage_neafc_landings_wgt_sp %>% 
#   mutate (prop_neafc = round (prop_neafc, digits = 2)) %>% 
#   mutate (total_weight_sp_neafc = round (total_weight_sp_neafc, digits = 2)) %>% 
#   mutate (total_weight_sp = round (total_weight_sp, digits = 2))
# 
# write.csv (percentage_neafc_landings_wgt, file = "output/EU/comparison percentage neafc landings weight.csv")
# write.csv (percentage_neafc_landings_wgt_sp, file = "output/EU/comparison percentage neafc landings weight species.csv")
# 
# rm (percentage_neafc_landings_wgt, percentage_neafc_landings_wgt_sp, total_weight, total_weight_neafc, total_weight_species, total_weight_species_neafc)
# 
## Percentage of effort in/out the NEAFC areas -----

total_effort <- STECF_effort %>% 
  group_by (year) %>% 
  summarise (TOTAL_effort_days = sum (totfishdays)) %>% 
  as.data.frame () %>% 
  select (-x)

# total effort EU in neafc
total_effort_Neafc <- STECF_effort %>%
  mutate (totfishdaysNEAFC = totfishdays * prop_neafc) %>% 
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

write.csv (percentage_neafc, file = "output/EU/percentage effort neafc.csv")

rm (total_effort, total_effort_Neafc, percentage_neafc)

## Overlay offshore wind developments with effort and value of EU catches, is there conflict and if so, where -----

# effort <- STECF_effort %>% 
#   group_by (cscode) %>% 
#   summarise (Total_effort_days = sum (totfishdays))
# 
# summary (effort$Total_effort_days)
# max <- 50
# 
# breakk <- pretty (seq (0, max))
# 
# max <- breakk[length(breakk)]
# 
# label <- breakk
# label <- label[-length (label)]
# label <- c(label, paste0(">", max))
# 
# mediann <- median (breakk)
# 
# # for visuals cap max
# effort$Total_effort_days[effort$Total_effort_days > (max)] <- max
# 
# ggplot () +
#   geom_sf (data = effort, aes (fill = Total_effort_days), colour = NA) + 
#   scale_fill_gradient2 (name = "Fishing Effort \n(days)",
#                         low = "white", mid = "yellow", high = "red",
#                         midpoint = mediann,
#                         labels = label,
#                         breaks = breakk, 
#                         limits = c(0, breakk[length(breakk)])
#   ) + 
#   # geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-50, -100, -150, -500, -1000),
#   #              linewidth=c(0.1), colour="lightblue") +
#   geom_sf (data = eez, aes (), fill = NA, colour = "black") +  
#   geom_sf (data = offshore_windfarms, aes (), colour = "blue", fill = "blue") +
#   geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
#   coord_sf (xlim = c(-15, 25), ylim = c(59, 75)) +
#   theme_bw() + theme (panel.background = element_rect (fill = "aliceblue"))
# 
# # ggplot () +
# #   geom_sf (data = offshore_windfarms, aes (), colour = "blue", fill = "blue") +
# #   coord_sf (xlim = c(-15, 25), ylim = c(59, 75)) +
# #   theme_bw() + theme (panel.background = element_rect (fill = "aliceblue"))
# 
# ggsave(filename ="output/EU/effort_offshorewindfarms.png", width = 12, height= 10)
# 
# rm (effort_year)

total_effort <- STECF_effort %>% 
  group_by (year) %>% 
  summarise (TOTAL_effort_days = sum (totfishdays)) %>% 
  as.data.frame () %>% 
  select (-x)

# total effort EU in neafc
total_effort_windfarms <- STECF_effort %>%
  mutate (totfishdays_windfarms = totfishdays * prop_windfarm) %>% 
  group_by (year) %>% 
  summarise (totfishdays_windfarms = sum (totfishdays_windfarms)) %>% 
  as.data.frame () %>% 
  select (-x)

percentage_windfarms <- merge (total_effort, total_effort_windfarms) %>% 
  mutate (prop_windfarm = (totfishdays_windfarms/TOTAL_effort_days)*100)

percentage_windfarms <- percentage_windfarms %>% 
  select (-totfishdays_windfarms) %>% 
  mutate (prop_windfarm = round (prop_windfarm, digits = 2)) %>% 
  mutate (TOTAL_effort_days = round (TOTAL_effort_days, digits = 2))

write.csv (percentage_windfarms, file = "output/EU/percentage effort windfarms.csv")

rm (total_effort, total_effort_windfarms, percentage_windfarms)

## Overlay wind farm zones with effort and value of EU catches, is there conflict -----

total_effort <- STECF_effort %>% 
  group_by (year) %>% 
  summarise (TOTAL_effort_days = sum (totfishdays)) %>% 
  as.data.frame () %>% 
  select (-x)

# total effort EU in neafc
total_effort_windZones <- STECF_effort %>%
  mutate (totfishdays_windZones = totfishdays * prop_windZone) %>% 
  group_by (year) %>% 
  summarise (totfishdays_windZones = sum (totfishdays_windZones)) %>% 
  as.data.frame () %>% 
  select (-x)

percentage_windZones <- merge (total_effort, total_effort_windZones) %>% 
  mutate (prop_windzone = (totfishdays_windZones/TOTAL_effort_days)*100)

percentage_windZones <- percentage_windZones %>% 
  select (-totfishdays_windZones) %>% 
  mutate (prop_windzone = round (prop_windzone, digits = 2)) %>% 
  mutate (TOTAL_effort_days = round (TOTAL_effort_days, digits = 2))

write.csv (percentage_windZones, file = "output/EU/percentage effort wind zones.csv")

rm (total_effort, total_effort_windZones, percentage_windZones)

## Overlay potential mining areas with effort and value of EU catches, is there conflict -----

total_effort <- STECF_effort %>% 
  group_by (year) %>% 
  summarise (TOTAL_effort_days = sum (totfishdays)) %>% 
  as.data.frame () %>% 
  select (-x)

# total effort EU in neafc
total_effort_mining <- STECF_effort %>%
  mutate (totfishdaysMining = totfishdays * prop_mining) %>% 
  group_by (year) %>% 
  summarise (totfishdaysMining = sum (totfishdaysMining)) %>% 
  as.data.frame () %>% 
  select (-x)

percentage_mining <- merge (total_effort, total_effort_mining) %>% 
  mutate (prop_mining = (totfishdaysMining/TOTAL_effort_days)*100)

percentage_mining <- percentage_mining %>% 
  select (-totfishdaysMining) %>% 
  mutate (prop_mining = round (prop_mining, digits = 2)) %>% 
  mutate (TOTAL_effort_days = round (TOTAL_effort_days, digits = 2))

write.csv (percentage_mining, file = "output/EU/percentage effort mining.csv")

rm (total_effort, total_effort_mining, percentage_mining)







