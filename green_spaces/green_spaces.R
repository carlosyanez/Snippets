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

plot_polygons <- function(boundingbox,features, map_title){
  
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
  
  feature_keys <- features %>% pull(key)  %>% unique()
  
  
  parks <- map_dfr(1:length(feature_keys), function(x,features,feature_keys,boundingbox) {
                     message(x)
    
                     feature_key <- feature_keys[x]
                     message(feature_key)
                     values <-  features %>% filter(key==feature_key) %>% pull(value)
                     message(values)
                     
                     parks <- opq(boundingbox) %>%
                              add_osm_feature(key = feature_key,
                                               value = values) %>%
                              osmdata_sp()
                     
                     Sys.sleep(60)
                     parks$osm_polygons <- parks$osm_polygons 
                     parks <- tidy(parks$osm_polygons, group = osm_id) %>%
                              nest(polygon=!id) %>%
                              left_join(as.data.frame(parks1$osm_polygons) %>% 
                                        mutate(osm_id=as.numeric(osm_id)),
                                 by=c("id"="osm_id"))
                     
                     parks
                      },features,feature_keys,boundingbox)
  
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
      data = parks %>%  dplyr::select(id,polygon) %>% unnest(polygon),
      mapping = aes(y = lat , x = long, group = group),
      fill = "#085202",
      size = 1
    )  + theme_map + labs(title=map_title)
  
  result <- list()
  
  result$data <- parks
  result$plot <- map_plot
  
  result
}

### download metropolitan boundaries
if(!file.exists("cities.json")){
        download.file("https://raw.githubusercontent.com/interline-io/osm-extracts/master/cities.json",
                      "cities.json")
}
metro_cities <- readOGR("cities.json")

melb_box   <-metro_cities %>% filter(name=="Melbourne")  %>% bbox(.)
stgo_box   <-metro_cities %>% filter(name=="Santiago") %>% bbox(.)
vienna_box <-metro_cities %>% filter(name=="Vienna")  %>% bbox(.)



###List features

features  <- tribble(~key,~value,
                     "leisure","garden",
                     "leisure","nature_reserve",
                     "leisure","park",
                     "leisure","common",
                     "leisure","beach_resort",
                     "leisure","fishing",
                     "leisure","bird_hide",
                     "landuse","flowerbed",
                     "landuse","forest",  
                     "landuse","meadow",  
                     "landuse","grass",  
                     "landuse","recreation_ground",  
                     "landuse","vineyard",  
                     "landuse","farmland",
                     "tourism","zoo",
                     "tourism","picnic_site",
                     "tourism","camp_site",
                     "natural","wood",
                     "natural","scrub",
                     "natural","grassland"
)

##create maps
melb_map   <- plot_polygons(melb_box,features,"Melbourne, Australia")
Sys.sleep(500)
stgo_map   <- plot_polygons(stgo_box,features,"Santiago, Chile")
Sys.sleep(500)
vienna_map <- plot_polygons(vienna_box,features,"Vienna, Austria")

map <- melb_map$plot + stgo_map$plot  + vienna_map$plot +
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

nest(stgo_map$data,data = !group)