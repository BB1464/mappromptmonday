
# Load the Required Packages ----------------------------------------------


library(tidyverse)
library(sf)
library(afrilearndata)
library(ggsflabel)
library(ggimage)


# Data Wrangling ----------------------------------------------------------


top_prod <- africountries |>
  filter(name %in%c('Nigeria',"Côte d'Ivoire",'Ghana','Cameroon'))



# Data Visualization ------------------------------------------------------


africountries |>
mutate(color=case_when(name=='Nigeria'~'#ee9b00',name=='Ghana'~'#ca1b82',
name=='Cameroon'~'#94d2bd',name=="Côte d'Ivoire"~'green',TRUE~'#fde7dc')) |>
ggplot()+
geom_sf(aes(fill=color),colour='black')+
scale_fill_identity()+
geom_sf_label_repel(aes(label=name),size=25,nudge_x = -0.5,
segment = TRUE, segment.color = "white", segment.size = 0.5,data = top_prod)+
geom_image(aes(x = -10,y = -9,image='2023/2023-week-03/Cocoa.jpg'),size=.4)+
labs(title = 'The top four major Cocoa producers in Africa',caption = 'Data: {afrilearndata} | Graphic: @Oluwafemi Oyedele')+
theme_void()+
theme(plot.background = element_rect(fill = "#2C3E4C", color = "#2C3E4C", size = 6),
plot.caption = element_text(hjust = 1, color = "grey80",size = 80),
plot.margin = margin(1, 1, 1, 1, unit = "cm"),plot.title = element_text(size = 115,family = 'serif',face = 'bold',colour = 'white'))




# Save the Plot -----------------------------------------------------------


ggsave(filename = 'Week3-Colour.png',path = here::here('2023/2023-week-03'),width = 12,height = 12,dpi = 300)
