
# Load Required Packages --------------------------------------------------

library(tidyverse)
library(mapview)
library(sf)
library(giscoR)
library(terra)
library(terrainr)
library(magick)



# 1. WORLD MAP

#-------------

# define projections

longlat_crs <- "+proj=longlat +datum=WGS84 +no_defs"

ortho_crs_old <- "+proj=ortho +lat_0=0 +lon_0=0"

ortho_crs <- "+proj=ortho +lat_0=32.4279 +lon_0=53.688 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs"

get_flat_world_sf <- function() {
  world <- giscoR::gisco_get_countries(
    year = "2016",
    epsg = "4326",
    resolution = "10"
  ) %>%
    sf::st_transform(longlat_crs)

  world_vect <- terra::vect(world)

  return(world_vect)
}


world_vect <- get_flat_world_sf()

# 2. NASA DATA
#-------------

get_nasa_data <- function() {
  ras <- terra::rast("/vsicurl/https://eoimages.gsfc.nasa.gov/images/imagerecords/144000/144898/BlackMarble_2016_01deg_geo.tif")
  rascrop <- terra::crop(x = ras, y = world_vect, snap = "in")
  ras_latlong <- terra::project(rascrop, longlat_crs)
  ras_ortho <- terra::project(ras_latlong, ortho_crs)
  return(ras_ortho)
}

ras_ortho <- get_nasa_data()

r <- ifel(is.na(ras_ortho), 0, ras_ortho)


# 3. MAP NIGHTLIGHTS

#-------------------

make_nighlights_globe <- function() {
  globe <- ggplot() +
    terrainr::geom_spatial_rgb(
      data = r,
      mapping = aes(
        x = x,
        y = y,
        r = red,
        g = green,
        b = blue
      )
    ) +
    theme_void() +
    theme(
      plot.margin = unit(
        c(t = 1, r = -1, b = -1, l = -1), "lines"
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA),
      legend.background = element_rect(fill = "black", color = NA),
      panel.border = element_rect(fill = NA, color = "black"),
    ) +
    labs(
      x = "",
      y = "",
      title = "",
      subtitle = "",
      caption = ""
    )
}


# Globe -------------------------------------------------------------------


globe <- make_nighlights_globe()


# Save the Plot -----------------------------------------------------------


ggsave(
  filename = "globe.png",
  width = 7, height = 7.5, dpi = 600, device = "png", globe,path=here::here('2023/2023-week_01/plots/'))


# 4. ANNOTATE MAP

#----------------

# read image

map <- magick::image_read(here::here('2023/2023-week_01/plots/globe.png/'))


# set font color
clr <- "#FFFFBC"

# Title
map_title <- magick::image_annotate(map, "Map Showing the Situation of the Earth at Night!!!",
 font = "Georgia",color = alpha(clr, .65), size = 150, gravity = "north",location = "+0+80"
)


# Caption

map_final <- magick::image_annotate(map_title, glue::glue(
  "BB1464 | ",
  "Data: NASA Earth Observatory"
),font = "Georgia", location = "+0+50",
color = alpha(clr, .45), size = 75, gravity = "south"
)


## Save the Plot

magick::image_write(map_final, glue::glue(here::here('2023/2023-week_01/plots/globe.png')))

