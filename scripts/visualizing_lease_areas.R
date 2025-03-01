#############
## Mapping offshore wind in California and potential intersections
## Zoë Kitchel
## Created Feb 26 2025
## Modified Feb 26 2025
#############

############ Setup ##
library(data.table)
library(ggmap)
library(sf)
library(terra)
library(rasterVis) #for plotting rasters
library(ggplot2)
library(sf)
library(ggnewscale)
library(rnaturalearth)
library(dplyr)
library(ggspatial) #scale bar
#####################

######### Load Data ##
##Substrate hard/soft from https://cmgds.marine.usgs.gov/data-releases/datarelease/10.5066-P9QQZ27U/
##Cal_DIG_I_Induration/Cal_DIG_I_Induration.tif downloaded Feb 26 2025
Morro_substrate_simple <- rast(file.path("data","substrate","Cal_DIG_I_Induration","Cal_DIG_I_Induration.tif"))

##Substrate CMECS from https://cmgds.marine.usgs.gov/data-releases/datarelease/10.5066-P9QQZ27U/
##Cal_DIG_I_CMECS/ downloaded Feb 26 2025
Morro_substrate_CMECS <- read_sf(file.path("data","substrate","Cal_DIG_I_CMECS","Cal_DIG_I_CMECS.shp"))

##Outline of wind lease areas from https://occidental.maps.arcgis.com/home/item.html?id=3c9d6570b5a14f62b837757f1bc2ff60
##Offshore_Wind_Lease_Outlines/Offshore_Wind_Lease_Outlines.shp downloaded Feb 26 2025
Morro_offshore_wind <- read_sf(file.path("data","Offshore_Wind_Lease_Outlines","Offshore_Wind_Lease_Outlines.shp"))

##Load up California Bottom Trawl Data (copied over from Trawl_Spatial_Turnover)
CA_annual_trawl <- fread(file.path("~", "Dropbox", "Repositories", "archive", "trawl_spatial_turnover_git", "data", "FishGlob_wgt_abun_edits", "WCANN_clean.csv"))

############ Convert to same projections GGMAP ("EPSG:3857")

#Reproject raster
Morro_substrate_simple.t <- terra::project(Morro_substrate_simple, "EPSG:3857")

#Convert to df for plotting
Morro_substrate_simple.df <- as.data.frame(Morro_substrate_simple.t, xy = TRUE)
colnames(Morro_substrate_simple.df) <- c("x", "y", "value")

#shapefiles to crs = 26910 (like Morro substrate simple raster)
Morro_substrate_CMECS.t <- st_transform(Morro_substrate_CMECS, crs = 3857)
Morro_offshore_wind.t <- st_transform(Morro_offshore_wind, crs = 3857)

#GPS to projected shapefiles for trawl survey
CA_annual_trawl_lat_lon <- unique(CA_annual_trawl[,.(year, latitude, longitude)])

CA_annual_trawl_lat_lon.sf <- st_as_sf(CA_annual_trawl_lat_lon, coords = c("longitude","latitude"),
                                       crs = 4326)

#Transform trawl points to matching crs
CA_annual_trawl_lat_lon.t <- st_transform(CA_annual_trawl_lat_lon.sf, crs = 3857)

######## Map
#Plot using ggmap (google map) remember to 
#add API here: https://console.cloud.google.com/google/maps-apis/credentials?utm_source=Docs_CreateAPIKey&utm_content=Docs_maps-backend&_gl=1*xzqbpi*_ga*ODI3MDgxMjQ1LjE3MzgyMDI3OTI.*_ga_NRWSTWS78N*MTczODIwMjc5Mi4xLjEuMTczODIwMzE3OS4wLjAuMA..&project=alpine-canto-410820

#once API added, may need to use register_google() again with API key from website above

#California outline
# Load the state boundaries for the USA
states <- ne_states(country = "united states of america", returnclass = "sf")

# Filter to get only California
california <- states[states$name == "California", ]


#Morro bay satellite imagery
MorroBay_basemap <- get_googlemap(center=c(-121.10727255981176,35.40228206171622), zoom = 10, maptype = "satellite")

#Map
ggmap(MorroBay_basemap) 
    # Plot the raster
    #geom_raster(data = Morro_substrate_simple.df, aes(x = x, y = y, fill = value)) +
    #scale_fill_viridis_c() +  # Better color scale for raster
    # New scale for sf points
   # new_scale_fill() +
    # Overlay sfs

#Change order of factors
Morro_substrate_CMECS.t <- Morro_substrate_CMECS.t %>%
    mutate(SubstrDesc = factor(SubstrDesc, levels = c("Coarse Unconsolidated",
                                                      "Rock",
                                                      "Fine Unconsolidated",
                                                      "Sand",
                                                      "Muddy Sand",
                                                      "Mud")))

MorroBay_OSW_map <- ggplot() +     
    geom_sf(data = california, fill = "lightgrey", color = "black") +     
    geom_sf(data = Morro_substrate_CMECS.t, aes(fill = SubstrDesc), color = NA, lwd = 0, alpha = 0.5) +     
    scale_fill_manual(values = c("lightcoral", "lightpink", "lightblue1", "lightskyblue", "royalblue3", "navy")) +     
    geom_sf(data = Morro_offshore_wind.t, fill = NA, color = "black", lwd = 0.7) +     
    geom_sf(data = CA_annual_trawl_lat_lon.t, size = 1.5, color = "gray29", shape = 18) +     
    coord_sf(xlim = c(-122.3, -120.3), ylim = c(34, 36.25), expand = FALSE) +     
    labs(fill = "Substrate type") +     
    annotation_scale(location = "bl", width_hint = 0.3) +  # Adds a scale bar at the bottom left
    theme_minimal() +
    theme(
       # axis.text = element_blank(),  # Remove lat/lon labels
       # axis.ticks = element_blank(),  # Remove axis ticks
        panel.grid = element_blank(),  # Remove grid lines
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Add a box around the plot
        legend.position = c(0.14,0.25),
        legend.background = element_rect(fill = NA, color = NA)
    )

# Create an inset plot for California
california_inset <- ggplot() +
    geom_sf(data = california, lwd = 0.3) +
    geom_rect(aes(xmin = -122.3,
                  xmax = -120.3,
                  ymin = 34,
                  ymax = 36.25),
              color = "yellow",fill = "yellow", linewidth = 0.6, alpha = 0.3) +  # Add rectangle to highlight the area
    theme_void()

# Add the CA outline inset to the main plot
MorroBay_OSW_map_CAinset <- MorroBay_OSW_map +
    annotation_custom(
        grob = ggplotGrob(california_inset), 
        xmin = -121, 
        ymin = 35.7) +
    #Supplemental legend info
    geom_point(aes(x = -122.22, y = 34.3), shape = 18, color = "gray29", size = 3) +
    geom_point(aes(x = -122.22, y = 34.25), shape = 22, fill = NA, color = "black", size = 4, stroke = 1) +
    annotate(geom = "text", x = -121.71, y = 34.3, label = "Groundfish Bottom Trawl Survey Locations", size = 4) +
    annotate(geom = "text", x = -121.91, y = 34.25, label = "Morro Bay Lease Area", size = 4)

# Save the plot
ggsave(MorroBay_OSW_map_CAinset, filename = "MorroBay_OSW_map_CAinset.pdf",
       path = file.path("figures"), width = 10, height =10, units = "in", dpi = 300)
