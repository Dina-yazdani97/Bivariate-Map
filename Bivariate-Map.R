# install and load all packages
pacman::p_load(
  geodata, tidyverse, sf, terra,
  rchelsa, biscale, elevatr, cowplot,
  gridGraphics, rayshader, extrafont, ggspatial
)

# 2. CHELSA DATA
#----------------

# set the working directory
main_dir <- getwd()

# load the raster files
temp <- terra::rast("CHELSA_bio10_01.tif")
prec <- terra::rast("CHELSA_bio10_12.tif")

# average precipitation
prec_average <- prec / 30

# Combine average temperature and precipitation
# into a raster stack
temp_prec <- c(temp, prec_average)

# assign names to each layer in the stack
names(temp_prec) <- c("temperature", "precipitation")

# 3. COUNTRY POLYGON
#-------------------

# Read shapefile and prepare polygon
country_sf <- sf::st_read("/home1/user/Yazdani-NEX-NASA/test/iran.shp") %>% 
  sf::st_transform(crs = sf::st_crs(temp_prec)) %>% 
  sf::st_union() %>% 
  sf::st_as_sf()

# 4. CROP AND RESAMPLE
#---------------------

# define the target CRS
target_crs <- "EPSG:4326"

# crop the input raster to the country's extent and apply a mask
temp_prec_country <- terra::crop(
  temp_prec, country_sf,
  mask = TRUE
)

# Obtain DEM data and project
dem <- terra::rast("/home1/user/Yazdani-NEX-NASA/test/dem_lam/dem_lam/dem_lam_Clip.img")
dem <- terra::project(dem, "EPSG:4326")

# resample the raster to match DEM resolution
# using bilinear interpolation, then reproject
temp_prec_resampled <- terra::resample(
  x = temp_prec_country,
  y = dem, method = "bilinear"
) |> terra::project(target_crs)

# plot the resampled raster
terra::plot(temp_prec_resampled)

# convert the raster to dataframe with coordinates
temp_prec_df <- as.data.frame(
  temp_prec_resampled, xy = TRUE
) %>% na.omit()

# 5. BREAKS, PALETTE AND PLOT THEME
#----------------------------------

# create bivariate classes using biscale
breaks <- biscale::bi_class(
  temp_prec_df, x = temperature,
  y = precipitation, style = "fisher",
  dim = 3
)

# Define the color palette
pal <- "PinkGrn"

# define a custom theme for the map based on your river map style
theme_for_the_win <- function(){
  theme_minimal(base_family = "Times New Roman") +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 12, color = "black", face = "bold", family = "Times New Roman"),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(
        color = "black", hjust = .5, size = 16,
        face = "bold", vjust = -1, family = "Times New Roman"
      ),
      plot.subtitle = element_text(
        hjust = .5, vjust = -1, size = 12, family = "Times New Roman"
      ),
      plot.caption = element_text(
        size = 12, color = "black", family = "Times New Roman",
        hjust = 0, face = "bold", vjust = 1
      ),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      panel.background = element_rect(fill = "white", color = "black", linewidth = 1),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
      panel.grid = element_line(color = "gray90", linewidth = 0.2)
    )
}

# 6. 2D BIVARIATE MAP
#--------------------

theme_set(theme_minimal())

theme_for_the_win <- function(){
  theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 12, color = "black"),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(
        color = "black", hjust = .5, size = 16,
        face = "bold", vjust = -1
      ),
      plot.subtitle = element_text(
        hjust = .5, vjust = -1, size = 12
      ),
      plot.caption = element_text(
        size = 12, color = "black",
        hjust = 0, vjust = 1
      ),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      panel.background = element_rect(fill = "white", color = "black", linewidth = 1),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
      panel.grid = element_line(color = "gray90", linewidth = 0.2)
    )
}

neighbor_countries <- geodata::world(resolution = 5, path = tempdir()) %>%
  sf::st_as_sf() %>%
  filter(NAME_0 %in% c("Turkey", "Iraq", "Pakistan", "Afghanistan", 
                      "Turkmenistan", "Azerbaijan", "Armenia", "Kazakhstan",
                      "Russia", "Uzbekistan", "Kuwait", "Qatar", 
                      "Bahrain", "United Arab Emirates", "Oman", "Saudi Arabia")) %>%
  sf::st_transform(crs = target_crs)

# create the bivariate map using ggplot2
map <- ggplot(breaks) +
  geom_raster(
    aes(x = x, y = y, fill = bi_class),
    show.legend = FALSE
  ) +
  biscale::bi_scale_fill(
    pal = pal, dim = 3,
    flip_axes = TRUE, rotate_pal = FALSE
  ) +
  # Add neighbor countries borders (thin gray lines)
  geom_sf(data = neighbor_countries, fill = NA, color = "black", linewidth = 0.5) +
  # Add country border (thicker black line)
  geom_sf(data = country_sf, fill = NA, color = "black", linewidth = 1) +
  labs(
    title = "Iran: Temperature and Precipitation",
    subtitle = "Average temperature and precipitation (1981-2010)",
    caption = "Data Source: CHELSA | Created by: Dina Yazdani",
    x = "", y = ""
  ) +
  # Set coordinate limits similar to your river map
  coord_sf(
    xlim = c(42, 65),
    ylim = c(23, 41),
    expand = FALSE,
    crs = target_crs
  ) +
  # Add scale bar 
  annotation_scale(
    location = "bl", 
    width_hint = 0.3,
    height = unit(0.18, "cm"),
    text_cex = 0.7
  ) +
  # Add north arrow
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_nautical,
    height = unit(3, "cm"),
    width = unit(3, "cm")
  ) +
  theme_for_the_win()

# create the legend for the bivariate map
legend <- biscale::bi_legend(
  pal = pal,
  flip_axes = TRUE,
  rotate_pal = FALSE,
  dim = 3,
  xlab = "Higher Temperature ",
  ylab = "Higher Precipitation ",
  size = 6 
)

# combine the map and legend using cowplot
full_map <- cowplot::ggdraw() +
  cowplot::draw_plot(
    plot = map, x = 0, y = 0,
    width = 1, height = 1
  ) +
  cowplot::draw_plot(
    plot = legend, 
    x = 0.15,  
    y = 0.15,  
    width = 0.2, 
    height = 0.2  
  )

# display the final map with legend
print(full_map)

# save as PNG file with similar specifications as your river map
ggsave(
  filename = "Iran_Temperature_Precipitation_Bivariate.png",
  plot = full_map,
  width = 12, height = 10, dpi = 300,
  device = "png", bg = "white"
)

# 7. ALTERNATIVE: CREATE SIMPLER VERSION WITHOUT LEGEND INSET
#------------------------------------------------------------

# Create map with legend in the actual legend position
map_with_legend <- ggplot(breaks) +
  geom_raster(
    aes(x = x, y = y, fill = bi_class),
    show.legend = TRUE
  ) +
  biscale::bi_scale_fill(
    pal = pal, dim = 3,
    flip_axes = TRUE, rotate_pal = FALSE,
    name = "Climate Zones"
  ) +
  # Add neighbor countries borders
  geom_sf(data = neighbor_countries, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = country_sf, fill = NA, color = "black", linewidth = 1) +
  labs(
    title = "Iran: Temperature and Precipitation",
    subtitle = "Average temperature and precipitation (1981-2010)",
    caption = "Data Source: CHELSA | Created by: Dina Yazdani",
    x = "", y = ""
  ) +
  coord_sf(
    xlim = c(42, 65),
    ylim = c(23, 41),
    expand = FALSE,
    crs = target_crs
  ) +
  annotation_scale(
    location = "bl", 
    width_hint = 0.3,
    height = unit(0.15, "cm"),
    text_cex = 0.7
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_nautical,
    height = unit(3, "cm"),
    width = unit(3, "cm")
  ) +
  theme_for_the_win() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8)
  )

# Save this version as well
ggsave(
  filename = "Iran_Temperature_Precipitation_Bivariate_v4.png",
  plot = map_with_legend,
  width = 12, height = 10, dpi = 300,
  device = "png", bg = "white"
)
