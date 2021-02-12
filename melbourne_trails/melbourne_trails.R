# Get plots of green spaces in Santiago and Melbourne

library(tidyverse)
library(osmdata)
library(broom)
library(spdplyr)
library(rgdal)
library(raster)

library(ggmap)

library(showtext)
library(magick)

font_add_google("Titillium Web", "Titilium")
showtext_auto()

plot_polygons <- function(boundingbox,features, map_title,map_title_size=14,
                          map_source="stamen",map_type="toner", map_alpha=0.5,
                          poly_colour="#085202",line_colour="#085202",
                          get_poly=TRUE,get_line=FALSE,
                          include_details=TRUE,
                          named_only=FALSE,
                          saved_data=FALSE,
                          line_size=1
                          ){
  
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
          plot.title = element_text(size = map_title_size, face = "bold",family="Titilium"),
          plot.subtitle = element_text(size = 12,family="Titilium"),
          legend.position="bottom",
          legend.direction="horizontal"
    )
  
  ## Get green areas
  
  feature_keys <- features %>% pull(key)  %>% unique()
  
  if(saved_data==TRUE & file.exists("parks.rds")){
    parks <- readRDS("parks.rds")
  }else{
    parks <- map_dfr(1:length(feature_keys), function(x,features,feature_keys,
                                                      boundingbox,get_poly,get_line,named_only) {
      message(x)
      
      feature_key <- feature_keys[x]
      message(feature_key)
      values <-  features %>% filter(key==feature_key) %>% pull(value)
      message(values)
      
      parks <- opq(boundingbox) %>%
               add_osm_feature(key = feature_key,
                               value = values) %>%
               osmdata_sp()
      message("osm data extracted")
      Sys.sleep(60)
      
      parks_result <- tibble()
      
      if(get_line){
        parks_lines_s <- parks$osm_lines
        if(named_only) parks_lines_s <- parks_lines_s %>% dplyr::filter(!is.na(name))
        
        parks_lines <- tidy(parks_lines_s, group = osm_id) %>%
          nest(polygon=!id) %>% mutate(p_type="line")
        message("p")
        if(include_details){
          details <- as.data.frame(parks_lines_s) %>% 
            mutate(id=as.numeric(osm_id)) %>%
            dplyr::select(id,name)
            
          parks_lines <- parks_lines %>%
            left_join(details,by="id") 
          
          
        }
        parks_result <- rbind(parks_result,parks_lines)
        message("osm lines processed")
      }
      if(get_poly){
        parks_polygons_s <- parks$osm_polygons_s 
        if(named_only) parks_polygons_s  <- parks_polygons_s  %>% dplyr::filter(!is.na(name))
        
        parks_polygons <- tidy(parks_polygons_s, group = osm_id) %>%
          nest(polygon=!id) %>% mutate(p_type="polygon")
        message("p")
        if(include_details){
          
          details <- as.data.frame(parks_polygons_s) %>% 
            mutate(id=as.numeric(osm_id)) %>%
            dplyr::select(id,name)
          
          parks_polygons <- parks_polygons %>%
            left_join(details,by="id") 
          
          
        }
        message("osm polygons processed")
        
        parks_result <- rbind(parks_result,parks_polygons)
        
        
      }
      
      parks_result
      
    },features,feature_keys,boundingbox,get_poly,get_line,named_only)
    
    saveRDS(parks,"parks.rds")
    
  }
  
  result <- list()
  result$data <- parks
  
  
  #get map tiles  
  
  map_tiles <-
    get_map(
      boundingbox,
      zoom = 10,
      source = map_source,
      maptype = map_type,
      color = "bw",
      alpha = map_alpha
    )
  
  map <- ggmap(map_tiles)  +
    theme_map +
    labs(title=map_title)
  
  #plot results 
  if(get_line){
    
    map_geom_lines <- geom_line(
      data = parks %>%  dplyr::filter(p_type=="line") %>% dplyr::select(id,polygon) %>% unnest(polygon),
      mapping = aes(y = lat , x = long, group = group),
      colour = line_colour,
      size = line_size
    )
    
    result$map_lines <- map + map_geom_lines
    
    
  }
  
  if(get_poly){
    
    map_geom_polygon <- geom_polygon(
      data = parks %>% dplyr::filter(p_type=="polygon") %>% dplyr::select(id,polygon) %>% unnest(polygon),
      mapping = aes(y = lat , x = long, group = group),
      colour = poly_colour
    )
    
    result$map_polygons <- map + map_geom_polygon
    
    
  }
  
  if(get_poly & get_line){
    
    result$map_all<-      map + 
      map_geom_polygon +
      map_geom_lines 
    
    
  }
  return(result)
}

### download metropolitan boundaries
if(!file.exists("cities.json")){
  download.file("https://raw.githubusercontent.com/interline-io/osm-extracts/master/cities.json",
                "cities.json")
}
metro_cities <- readOGR("cities.json")

melb_box   <-metro_cities %>% filter(name=="Melbourne")  %>% bbox(.)


###List features

features  <- tibble(key=rep("highway",3),
                    value=c("footway","cycleway"))

##create maps
melb_map   <- plot_polygons(melb_box,features,"Walking and Cycling Trails in Metro Melbourne",saved_data = TRUE,
                            named_only=TRUE,
                            get_poly = FALSE,
                            line_colour="#ff9203",
                            get_line = TRUE)
map <- melb_map$map_lines + 
  labs(caption="Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        plot.caption  = element_text(size = 6,family="Titilium"))


## Add twitter logo and handle, Twitter logos available at https://about.twitter.com/en_us/company/brand-resources.html
logo_file <- "../Twitter_Logo_Blue.png"
plot_file <-"melbourne_trail.png"

# https://stackoverflow.com/questions/41574732/how-to-add-logo-on-ggplot2-footer
# https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/

ggsave(plot_file,map,dpi=320,width = 6.4,height = 6.5)

plot <- image_read(plot_file)
logo_raw <- image_read(logo_file) 
logo <- logo_raw %>% image_resize("60x60") %>%
  image_extent("700x60",gravity="west") %>%
         image_annotate("@carlosyanez", color = "black",
                font="Titilium",
                
                 location="+61+0",
                 size = 35, gravity = "west")
  



plot_height <- magick::image_info(plot)$height
plot_width <- magick::image_info(plot)$width

# get dims of the logo
logo_width <- magick::image_info(logo)$width
logo_height <- magick::image_info(logo)$height

y_pos <-round(plot_height - logo_height ,0)
x_pos <-round(plot_width * 0.01,0)

offset_text <- str_c("+",x_pos,"+",y_pos)


final_plot <-image_composite(plot,logo, offset = offset_text)

final_plot
image_write(final_plot,plot_file)





