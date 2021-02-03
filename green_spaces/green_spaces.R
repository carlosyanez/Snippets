# Get plots of green spaces in Santiago and Melbourne

library(tidyverse)
library(osmdata)
library(broom)
library(spdplyr)
library(rgdal)
library(raster)

library(ggmap)

library(showtext)
library(cowplot)
library(patchwork)

### Function to plot green spaces

font_add_google("Titillium Web", "Titilium")
showtext_auto()

green_spaces <- function(boundingbox,map_title){
  
  ### Get font
  
  font_add_google("Titillium Web", "Titilium")
  showtext_auto()
  
  ### Theme
  
  theme_map <-   theme_minimal()+
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          legend.title = element_text(vjust=0.75,hjust=1,size=10,family="Titilium"),
          legend.text=element_text(size=8,family="Titilium"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 14, face = "bold",family="Titilium"),
          plot.subtitle = element_text(size = 12,family="Titilium"),
          legend.position="bottom",
          legend.direction="horizontal"
    )
  
  ## Get green areas
  
  parks1 <-    opq(boundingbox) %>%
    add_osm_feature(key = 'leisure',
                    value = c("garden", "nature_reserve",
                              "park", "common","beach_resort","fishing","bird_hide")) %>%
    osmdata_sp()
  
  Sys.sleep(60)
  parks2 <-    opq(boundingbox) %>%
    add_osm_feature(key="landuse",value=c("flowerbed","forest","meadow","grass",
                                          "recreation_ground", 	"vineyard","farmland")) %>%
    osmdata_sp()
  Sys.sleep(60)
  parks3 <-    opq(boundingbox) %>%
    add_osm_feature(key="tourism",value=c("zoo","picnic_site","camp_site")) %>%
    osmdata_sp()
  Sys.sleep(60)
  parks4 <-    opq(boundingbox) %>%
    add_osm_feature(key="natural",value=c("wood","scrub","grassland")) %>%
    osmdata_sp()
  
  
  parks1 <- tidy(parks1$osm_polygons, group = osm_id)
  parks2 <- tidy(parks2$osm_polygons, group = osm_id)
  parks3 <- tidy(parks3$osm_polygons, group = osm_id)
  parks4 <- tidy(parks4$osm_polygons, group = osm_id)
  
    
  parks <- rbind(parks1,parks2) %>% rbind(parks3) %>% rbind(parks4)
  
  #get map tiles  
  
  map_tiles <-
    get_map(
      boundingbox,
      zoom = 10,
      source = "stamen",
      maptype = "toner",
      color = "bw",
      alpha = 0.5
    )
  
  #plot results 
  
  map_plot <- ggmap(map_tiles)  +
    
    geom_polygon(
      data = parks,
      mapping = aes(y = lat , x = long, group = group),
      fill = "#085202",
      size = 1
    )  + theme_map + labs(title=map_title)
  
  map_plot
  
}

### download metropolitan boundaries
if(!file.exists("cities.json")){
        download.file("https://raw.githubusercontent.com/interline-io/osm-extracts/master/cities.json",
                      "cities.json")
}
metro_cities <- readOGR("cities.json")

melb_box   <-metro_cities %>% filter(name=="Melbourne")
stgo_box   <-metro_cities %>% filter(name=="Santiago")
vienna_box <-metro_cities %>% filter(name=="Vienna")

melb_box   <- bbox(melb_box)
stgo_box   <- bbox(stgo_box)
vienna_box <- bbox(vienna_box)

##create map
melb_map   <- green_spaces(melb_box,"Melbourne, Australia")
Sys.sleep(500)
stgo_map   <- green_spaces(stgo_box,"Santiago, Chile")
Sys.sleep(500)
vienna_map <- green_spaces(vienna_box,"Vienna, Austria")

map <- melb_map + stgo_map  + vienna_map +
      plot_annotation(title="Green Spaces in Selected Cities") & 
  theme(text= element_text(size = 16, face = "bold",family="Titilium"))


## Add twitter logo and handle, Twitter logos available at https://about.twitter.com/en_us/company/brand-resources.html
logo_file <- "../Twitter_Logo_Blue.png"

p2 <- ggdraw() +
  draw_image(
    logo_file, scale = .07, x = 1,
    hjust = 1, halign =0, valign = 0
  ) +  draw_plot(map)  +
  draw_plot_label("@carlosyanez",x=0,y=0,
             fontfamily = "Titilium",
             fontface = "plain",
             size=10,
             hjust=-0.4,vjust=-1.1
  ) +
  draw_plot_label("Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL",
              x=0,y=0,
             fontfamily = "Titilium",
             fontface = "plain",
             size=10,
             hjust=-1.25,vjust=-1.1
  ) 
 
  

#p2

## save into png file
ggsave("green_spaces.png",p2,width = 12,height=6)