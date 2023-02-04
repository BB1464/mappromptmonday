
# Load the required Packages ----------------------------------------------

library(tidyverse)
library(giscoR)
library(eurostat)
library(showtext)
library(ggtext)

# EU members plus UK

eu2016 <- c("UK", gisco_countrycode[gisco_countrycode$eu, ]$CNTR_CODE)

nuts2 <- gisco_get_nuts(
  year = "2016",
  epsg = "3035",
  resolution = "3",
  nuts_level = "2",
  country = eu2016
)

# Borders
borders <- gisco_get_countries(
  epsg = "3035",
  year = "2016",
  resolution = "3",
  country = eu2016
)

# Eurostat data - Purchase parity power
pps <- giscoR::tgs00026
pps <- pps[pps$time == 2016, ]

# Breaks
br <- c(0, seq(10, 25, 2.5), 1000) * 1000

nuts2.sf <- merge(nuts2,
                  pps,
                  by.x = "NUTS_ID",
                  by.y = "geo",
                  all.x = TRUE
)


# Cut
nuts2.sf$values_groups <- cut(nuts2.sf$values, breaks = br)

# Labels
labels <- paste0(br / 1000, "k")[-1]
labels[1] <- "<10k"
labels[8] <- ">25k"

# Plot
# pal <- RColorBrewer::brewer.pal(n = 9,name = 'Set1')
#
# pal <- monochromeR::generate_palette(colour = 'green',modification = 'go_both_ways',n_colours = 9)

#pal <- hcl.colors(8, "Spectral", alpha = 0.8)

pal <- colorRampPalette(colors = hcl.colors(n = 8,palette = 'Spectral'))(9)

pal <- rev(pal)


# 5. VISUALIZATION ----
# |- plot aesthetics ----

bkg_col      <- "#555555"
title_col    <-  "white"
subtitle_col <-  "white"
caption_col  <-  "white"


# Plot --------------------------------------------------------------------


ggplot(nuts2.sf) +
  geom_sf(aes(fill = values_groups), color = NA, alpha = 0.9) +
  geom_sf(data = borders, fill = NA, size = 0.1, col = "grey30") +
  # Center in Europe: EPSG 3035
  #coord_sf(xlim = c(-2600000, 3100000), ylim = c(300000, 4500000), crs = "+proj=aea +lat_0=30 +lon_0=10 +lat_1=43 +lat_2=62 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs +type=crs")+
  #coord_sf(xlim = c(2377294, 6500000), ylim = c(1413597, 5228510), crs = "+proj=aea +lat_0=30 +lon_0=10 +lat_1=43 +lat_2=62 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs +type=crs")+
  coord_sf(
    xlim = c(2377294, 6500000),
    ylim = c(1413597, 5228510)
  ) +
  labs(title = str_wrap(string = "Disposable Income in Europe (2016)",width = 40),subtitle = 'NUTS-2 level',caption = "Source:Eurostat Data")+
  #labs(
   # title = "Disposable Incoming Households (2016)",
    #subtitle = "NUTS-2 level",
    #caption = paste0(
     # "Source: Eurostat\n ", gisco_attributions()
    #)
#  ) +
  scale_fill_manual(
    name = "euros",
    values = pal,
    drop = FALSE,
    na.value = "gray95",
    labels = labels,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = 0.5,
      keywidth = 2,
      title.position = "top",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = TRUE,
      reverse = FALSE,
      label.position = "bottom"
    )
  ) +
  theme_void() +
  # Theme
theme(plot.title = element_text(
        color = "grey90",
        hjust = 0.5,
        vjust = -1,
        size=60
      ),
      #panel.background = element_rect(fill = "#555555"),
      plot.background = element_rect(fill = "#555555",colour = '#555555'),
      plot.subtitle = element_text(
        color = "grey90",
        hjust = 0.5,
        vjust = -2,
        face = "bold",
        size=50
      ),
      plot.caption = element_text(
        color = "grey90",
        size = 40,
        hjust = 1,
        margin = margin(b = 2, t = 13)
      ),
      legend.text = element_text(
        size = 26,
        color = "grey90"
      ),
      legend.title = element_text(
        size = 38,
        color = "grey90"
      ),
      legend.position = c(0.5, 0.02),
      panel.grid = element_blank()
    )



# Save the Plot -----------------------------------------------------------


ggsave(filename = 'flowMap.png',plot = last_plot(),path = here::here('2023/2023-week-05/'),width = 13,height = 12,dpi = 200)
