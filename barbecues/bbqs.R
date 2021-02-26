# Get plots of barbecue spots in Australian capital cities


# Load/Install pacman
if(!require(pacman)) install.packages("pacman", repos = "http://cran.us.r-project.org")

#use pacman to install all other packages
pacman::p_load("tidyverse","ggplot2","patchwork","broom","showtext","magick","grid","gridExtra","ggpubr",
               "osmdata","spdplyr","sp","rgdal","raster","maptools",
               "rvest","xml2")


font_add_google("Titillium Web", "Titilium")
font_add_google("Roboto", "Roboto")

showtext_auto()


# Melbourne

## get metropolitan area council

melb_lgas_list <- read_html("https://en.wikipedia.org/wiki/Local_government_areas_of_Victoria#Municipalities_of_Greater_Melbourne") %>%
             html_node(xpath="/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>%
             html_table(fill = TRUE) %>% 
             mutate(LGA=str_remove(`Local government area`,"City of"),
                    LGA=str_remove(LGA,"Shire of"),
                    LGA=str_squish(LGA),
                    LGA=str_trim(LGA)) %>%
             rename(Population.2018=`Population (2018)[1][2]`) %>%
             dplyr::select(LGA,Population.2018,Region) %>%
             mutate(ABB_NAME=toupper(LGA))



## download victorian LGAs

if(!file.exists("VIC_LGA_POLYGON_SHP.shp")){
  download.file("https://data.gov.au/data/dataset/bdf92691-c6fe-42b9-a0e2-a4cd716fa811/resource/7b6043d1-76b8-4ea9-b36b-51c61aa740d0/download/vic_lga_polygon_shp.zip",
                "LGAs.zip")
  unzip("LGAs.zip")
  file.remove("LGAs.zip")
}

vic_lga_polygon <- readOGR( 
  dsn="./" , 
  layer="VIC_LGA_POLYGON_SHP"
)

##filter non metropolitan councils and create bounding box

melb_lga_polygon <- vic_lga_polygon %>% filter(ABB_NAME %in% melb_lgas_list$ABB_NAME)
state_pid <- melb_lga_polygon@data$STATE_PID
melb_boundary <-maptools::unionSpatialPolygons(melb_lga_polygon,state_pid)

melb_bb <- sp::bbox(melb_boundary)

## get suburb polygons

if(!file.exists("VIC_LOCALITY_POLYGON_SHP.shp")){
  download.file("https://data.gov.au/data/dataset/af33dd8c-0534-4e18-9245-fc64440f742e/resource/3b946968-319e-4125-8971-2a33d5bf000c/download/vic_locality_polygon_shp.zip",
                "LGAs.zip")
  unzip("LGAs.zip")
  file.remove("LGAs.zip")
}

vic_suburb_polygon <- readOGR( 
  dsn="./" , 
  layer="VIC_LOCALITY_POLYGON_SHP"
)

melb_suburb<-raster::intersect(melb_boundary,vic_suburb_polygon)


## get points with cafes

melb_bbq <- opq(melb_bb) %>%
        add_osm_feature(key = "amenity",
                  value = c("bbq")) %>%
        osmdata_sp()

## extract and reproject
melb_bbq_loc <- spTransform(melb_bbq$osm_points,
                              crs(melb_lga_polygon))

melb_result<-sp::over(melb_bbq_loc,melb_suburb) %>%
         count(NAME) %>%
         filter(!is.na(NAME)) %>%
         rename(`Barbecue Spots`=n) 


## fortify and  attach result to polygon


melb_result_fortified <- tidy(melb_suburb,region="NAME") %>%
                    left_join(melb_result,by=c("id"="NAME"))


# Sydney 

## get metropolitan area council

syd_lgas_list <- read_html("https://en.wikipedia.org/wiki/Local_government_areas_of_New_South_Wales") %>%
  html_node(xpath="/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>%
  html_table(fill = TRUE) 

syd_lgas_list <-  tibble(LGA=syd_lgas_list[,1]) %>%
  filter(!(LGA=="Local government area")) %>%
  mutate(LGA=str_remove(LGA,", City of"),
         LGA=str_remove(LGA,"  City"),
         LGA=str_remove(LGA," Council"),
         LGA=str_remove(LGA,", Municipality of"),
         LGA=str_remove(LGA," Shire"),
         LGA=str_squish(LGA),
         LGA=str_trim(LGA)) %>%
   mutate(NSW_LGA__3=toupper(LGA))


## download NSW LGAs

if(!file.exists("NSW_LGA_POLYGON_shp/NSW_LGA_POLYGON_shp.shp")){
  download.file("https://data.gov.au/data/dataset/f6a00643-1842-48cd-9c2f-df23a3a1dc1e/resource/acd0b143-3616-4144-9ef5-d83a67f84148/download/nsw_lga_polygon_shp.zip",
                "LGAs.zip")
  unzip("LGAs.zip")
  file.remove("LGAs.zip")
}

nsw_lga_polygon <- readOGR( 
  dsn="./NSW_LGA_POLYGON_shp" , 
  layer="NSW_LGA_POLYGON_shp"
)

##filter non metropolitan councils and create bounding box

syd_lga_polygon <- nsw_lga_polygon %>% filter(NSW_LGA__3 %in% syd_lgas_list$NSW_LGA__3)
state_pid <- syd_lga_polygon@data$NSW_LGA__5
syd_boundary <-maptools::unionSpatialPolygons(syd_lga_polygon,state_pid)

syd_bb <- sp::bbox(syd_boundary)

## get suburb polygons

if(!file.exists("NSW_LOC_POLYGON_shp/NSW_LOC_POLYGON_shp.shp")){
  download.file("https://data.gov.au/data/dataset/91e70237-d9d1-4719-a82f-e71b811154c6/resource/5e295412-357c-49a2-98d5-6caf099c2339/download/nsw_loc_polygon_shp.zip",
                "LGAs.zip")
  unzip("LGAs.zip")
  file.remove("LGAs.zip")
}

nsw_suburb_polygon <- readOGR( 
  dsn="./NSW_LOC_POLYGON_shp" , 
  layer="NSW_LOC_POLYGON_shp"
)

syd_suburb<-raster::intersect(syd_boundary,nsw_suburb_polygon)

## get points with cafes

syd_bbq <- opq(syd_bb) %>%
  add_osm_feature(key = "amenity",
                  value = c("bbq")) %>%
  osmdata_sp()

## extract and reproject
syd_bbq_loc <- spTransform(syd_bbq$osm_points,
                            crs(syd_lga_polygon))

syd_result<-sp::over(syd_bbq_loc,syd_suburb) %>%
  count(NSW_LOCA_2) %>%
  filter(!is.na(NSW_LOCA_2)) %>%
  rename(`Barbecue Spots`=n) 

## fortify and  attach result to polygon


syd_result_fortified <- tidy(syd_suburb,region="NSW_LOCA_2") %>%
  left_join(syd_result,by=c("id"="NSW_LOCA_2"))

# map and chart


## map theme

my_map_theme <- theme_set(theme_void())
my_map_theme <- theme_update(plot.margin=grid::unit(c(0,0,0,0), "mm"),
                            #legend.position="right",
                            plot.title = element_blank(),
                            plot.subtitle =element_blank(),
                            plot.caption =  element_blank(),
                            axis.ticks= element_blank(),
                            axis.text = element_blank(),
                            axis.title = element_blank(),
                            legend.text = element_text(size=10,colour = "#272928",family="Roboto"),
                            legend.position="none",
                            strip.text = element_blank(),
                            strip.background = element_blank())

## plot theme

my_chart_theme <- theme_set(theme_minimal())
my_chart_theme <- theme_update(legend.position="right",
                         plot.title = element_text(size=16,face="bold",colour = "#272928",family="Roboto"),
                         plot.subtitle =element_text(size=10,colour = "azure4",family="Roboto"),
                         plot.caption =  element_text(size=10,colour = "azure4",family="Roboto"),
                         axis.text = element_text(size=8,colour = "azure4",family="Roboto"),
                         axis.title = element_text(size=10,colour = "azure4",family="Roboto"),
                         axis.line = element_blank(),
                         panel.border = element_blank(),
                         panel.grid.major.y = element_blank(),
                         panel.grid.minor.y = element_blank(),
                         legend.text = element_text(size=10,colour = "#272928",family="Roboto"),
                         strip.text = element_text(face = "bold", color = "azure4",
                                                   hjust = 0, size = 8,family="Roboto"),
                         strip.background = element_rect(fill = "#fcf8f7",linetype = "blank"))

# cuts for display

breaks_bbq <- c(0,5,10,15,20,50)
labels_bbq <- c("5 or less","6 to 10","11 to 15","16 to 20","more than 20")

# colour palette for maps
my_palette <-c("#B29700", "#D4AF37","#E1C158","#7DAA6A","#619A46","#438029")
names(my_palette)<-c(labels_bbq)
my_scale <- scale_fill_manual(name = "BBQ Spots",values = my_palette,na.value="gray80")

melb_map <- melb_result_fortified %>% mutate(Spots=cut(`Barbecue Spots`,breaks_bbq,labels_bbq)) %>%
  mutate(Spots2=ifelse(is.na(Spots),"NA",Spots)) %>%
  ggplot() +
  geom_polygon(data = , aes(fill = Spots, x = long, y = lat, group = group)) +
  my_map_theme + 
  my_scale+
  coord_map() +
  theme(legend.position = "none") +
  labs(title=NULL,x=NULL,y=NULL)


syd_map <- syd_result_fortified %>% mutate(Spots=cut(`Barbecue Spots`,breaks_bbq,labels_bbq)) %>%
  ggplot() +
  geom_polygon( aes(fill = Spots, x = long, y = lat, group = group)) +
  my_map_theme +
  my_scale +
  coord_map()+
  theme(legend.position = "bottom", legend.box = "horizontal")+
  labs(title=NULL,x=NULL,y=NULL)


map_legend <-as_ggplot(get_legend(syd_map, position = "bottom"))

syd_map <- syd_map + theme(legend.position = "none")


melb_chart <- melb_result %>% mutate(NAME=str_to_title(NAME)) %>%
  slice_max(`Barbecue Spots`,n=10) %>%
  ggplot() +geom_col(aes(x=reorder(NAME, `Barbecue Spots`),y=`Barbecue Spots`),fill="#00843D") +
  my_chart_theme +
  labs(subtitle="Top 10",x="",y="Barbeque Spots") +
  ylim(0,max(melb_result$`Barbecue Spots`+2)) +
  coord_flip() 

syd_chart <- syd_result %>% mutate(NSW_LOCA_2=str_to_title(NSW_LOCA_2)) %>%
  slice_max(`Barbecue Spots`,n=10) %>%
  ggplot() +geom_col(aes(x=reorder(NSW_LOCA_2, `Barbecue Spots`),y=`Barbecue Spots`),fill="#FFCD00") +
  my_chart_theme +
  labs(title="",x="",y="Barbeque Spots") +
  ylim(0,max(melb_result$`Barbecue Spots`+2)) +
  coord_flip()



# create composite chart

p_layout <- "AAAABB
            AAAABB
            AAAABB
            CCCCDD
            CCCCDD
            CCCCDD
            EEEEEE
            EEEEEE"



p <-  melb_map + melb_chart +syd_map + syd_chart + map_legend + plot_layout(design = p_layout) +
      plot_annotation(
    title = 'A barbie in the park, mate? - Living the Australian Dream',
    subtitle = 'Public barbecue spots by suburb in Melbourne and Sydney',
    caption  ="Sources: Open Street Map (ODbL) | data.gov.au (CC BY 4.0)") 


## Add twitter logo and handle, Twitter logos available at https://about.twitter.com/en_us/company/brand-resources.html
logo_file <- "../Twitter_Logo_Blue.png"
plot_file <-"barbeques.png"

# https://stackoverflow.com/questions/41574732/how-to-add-logo-on-ggplot2-footer
# https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/

ggsave(plot_file,p,dpi=500)
#,width = 6.4,height = 6.5)

plot <- image_read(plot_file)
logo_raw <- image_read(logo_file) 
logo <- logo_raw %>% image_resize("80x80") %>%
  image_extent("350x60",gravity="west") %>%
  image_annotate("@carlosyanez", color = "black",
                 font="Roboto",
                 location="+61+0",
                 size = 40, gravity = "west") 


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


