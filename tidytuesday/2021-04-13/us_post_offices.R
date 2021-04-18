# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-13/readme.md

# make sure that {librarian} and {tidytuesdayR} are there
if(!require(librarian)) install.packages("librarian")
if(!require(tidytuesdayR)) install.packages("tidytuesdayR")


# load other libraries


librarian::shelf("tidyverse","sf","patchwork","ggtext",
                 "showtext","sysfonts","units","tinter","here",
                 "yutannihilation/ggsflabel","ggrepel","png",
                 "carlosyanez/customthemes",
                 "maps","tigris","grid")
 
# load tt data
tuesdata <- tidytuesdayR::tt_load(tt_date)
post_offices <- tuesdata$post_offices

# folder reference for here()
tt_date<-"2021-04-13"

#ference year for plots
ref_year <- 1945

#buffer area around route 66
buffer_area <- set_units(5,km)



# create sf object

my_crs <-"+proj=merc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +nadgrids=@null +units=km "
my_crs1 <-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


post_offices_sf <- post_offices %>%
                filter(!is.na(latitude) | !is.na(longitude)) %>%
                st_as_sf(., coords = c("longitude", "latitude"), 
                         crs = 4326, agr = "constant")

post_offices_sf <- st_transform(post_offices_sf,my_crs)

#donwload additional spatial objects

# route 66

temp_zip <- here(tt_date,"temp.zip")

if(!file.exists(temp_zip)){
    download.file("https://opendata.arcgis.com/datasets/adf920cfd4024cb8a90a99bef0d34443_0.zip",temp_zip)
}

unzip(temp_zip,exdir=here(tt_date,"shapefile"))
route66 <- st_read(here(tt_date,"shapefile","ROSI.shp"))
route66 <- st_transform(route66,my_crs)


# create buffer area around route 66 and filter post offices within 

route66_area <- st_buffer(route66,buffer_area)

route66_po <- st_within(post_offices_sf,route66_area,sparse = FALSE)
route66_po <- tibble(
  id = post_offices_sf$id,
  value = route66_po)  %>% filter(value==TRUE)



post_offices_66 <- post_offices_sf %>% filter(id %in% route66_po$id) %>%
                   replace_na(list(established=1600,discontinued=2030)) %>%
                   mutate(opened_after_ref   = if_else(is.na(established),FALSE,(established   > ref_year)),
                          closed_before_ref   = if_else(is.na(discontinued),FALSE,(discontinued < ref_year)),
                          decade_discountinued = if_else(discontinued==2030,"Open",
                                                         str_c(str_sub(as.character(floor((discontinued)/10)*10),3,4),"s"))
                   )

post_offices_66 <- post_offices_66 %>%  
                   left_join(post_offices_66 %>% st_drop_geometry() %>% count(decade_discountinued),
                               by="decade_discountinued") %>%
                      mutate(`Closed on`=str_c(decade_discountinued, " (",n,")")) %>%
                      select(-n)


#get route 66 counties

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- st_transform(counties,my_crs)

counties66 <- st_intersects(counties, route66_area,sparse=FALSE)
counties66 <- tibble(
  ID = counties$ID,
  value = counties66)  %>% filter(value==TRUE)

counties66 <- counties %>% filter(ID %in% counties66$ID)

#get city shapes (from the song) # https://en.wikipedia.org/wiki/(Get_Your_Kicks_on)_Route_66

song_places <-tribble(~NAME,         ~STATE, ~group,
                      "Chicago",       "IL", 1,
                      "St. Louis",     "MO", 2,
                      "Joplin",        "MO", 2,
                      "Oklahoma City", "OK", 3,
                      "Amarillo",      "TX", 2,
                      "Gallup",        "NM", 3,
                      "Flagstaff",     "AZ", 1,
                      "Winona",        "AZ", 1,
                      "Kingman",       "AZ", 3,
                      "Barstow",       "CA", 3, 
                      "San Bernardino","CA", 2,
                      "Los Angeles"   ,"CA", 4)

cities66<- map_df(c("IL","MO","OK","TX","NM","AZ","CA"),function(x){
        a<- places(x)                    # Winona is not longer counted (forgotten)!
        a <- a %>% mutate(STATE=x)
        }) %>%
        inner_join(song_places,
                   by=c("NAME","STATE")) %>%
                   mutate(NAME=if_else(NAME=="Los Angeles","LA",NAME)) %>%
                   st_transform(my_crs) 
  

###PLOTTING                  


#colours and formatting 

county_border <- "gray80"
county_fill   <- "#c4ada7"
city_fill     <- "gray80"
route66_fill  <- "#db6740"
us_post_fill <-"#333366"

text_colour   <- "#1971b1"

city_font <-"Overpass"
city_size <-3.5
sysfonts::font_add_google(city_font, city_font)
showtext::showtext_auto()
winona_colour<-"red"

google_font <-"Roboto"
sysfonts::font_add_google(google_font, google_font)
showtext::showtext_auto()
bg_colour <- lighten(county_fill,0.2)

#arrow shapes

my_arrow <- arrow(angle = 30, length = unit(0.075, "inches"),
                  ends = "first", type = "open")

#plots
set.seed(42)
base_plot <- ggplot()+
             geom_sf(data=counties66 ,aes(geometry=geom),colour=county_border, size=0.5,fill=county_fill,inherit.aes = FALSE) +
             geom_sf(data=route66 ,aes(geometry=geometry),colour=route66_fill, alpha=1,size=2,inherit.aes = FALSE) +
             geom_sf(data=cities66 ,aes(geometry=geometry),fill=city_fill, alpha=1,size=0,inherit.aes = FALSE) +
             geom_sf_text_repel(data = cities66 %>% filter(group==1),
                                aes(geometry=geometry, label = NAME),
                                colour=text_colour,
                                family=city_font,
                                size=city_size,
                                force=10,
                                nudge_x = 1,
                                direction ="both",
                                point.padding = 0.1,
                                box.padding = 0.5,
                                nudge_y = 1.6,
                                segment.curvature = -0.1,
                                segment.ncp = 3,
                                segment.angle = 20) +
            geom_sf_text_repel(data = cities66 %>% filter(group==2),
                     aes(geometry=geometry, label = NAME),
                     colour=text_colour,
                     family=city_font,
                     size=city_size,
                     force=10,
                     nudge_x = 1,
                     direction ="both",
                     point.padding = 0.1,
                     box.padding = 0.5,
                     nudge_y = -1.1,
                     segment.curvature = -0.1,
                     segment.ncp = 3,
                     segment.angle = 20,
                     arrow=my_arrow) +
            geom_sf_text_repel(data = cities66 %>% filter(group==3),
                     aes(geometry=geometry, label = NAME),
                     colour=text_colour,
                     family=city_font,
                     size=city_size,
                     force=10,
                     nudge_x = -1,
                     direction ="both",
                     point.padding = 0.1,
                     box.padding = 0.5,
                     nudge_y = 1.1,
                     segment.curvature = -0.1,
                     segment.ncp = 3,
                     segment.angle = 20,
                     arrow=my_arrow) +
            geom_sf_text_repel(data = cities66 %>% filter(group==4),
                     aes(geometry=geometry, label = NAME),
                     colour=text_colour,
                     family=city_font,
                     size=city_size,
                     force=10,
                     nudge_x = -2,
                     nudge_y = -5,
                     box.padding = 0.5,
                     direction ="both",
                     point.padding = 0.1,
                     segment.curvature = -0.1,
                     segment.ncp = 3,
                     segment.angle = 20,
                     arrow=my_arrow) 


theme_base <-  custom_map_theme_md(legend_pos = "none", plot_margin = c(0,0,0,0)) +
             theme(panel.background = element_rect(fill=lighten(county_fill,0.1),
                                                   colour = lighten(county_border,0.2)))


post_office_winona <- post_offices_66 %>% filter(name=="WINONA") %>%
                      mutate(label_name=str_c(name,
                                         if_else(discontinued<ref_year," - Already closed in "," - Closed in"),
                                                 discontinued)) %>%
                       mutate(lat = sf::st_coordinates(.)[,1],
                              lon = sf::st_coordinates(.)[,2])
           
p1<- base_plot +
        geom_sf(data= (post_offices_66 %>% filter(!opened_after_ref & !closed_before_ref)) ,
        aes(geometry=geometry),fill=us_post_fill, colour=us_post_fill,size=3,
        alpha=0.4,inherit.aes = FALSE) +
        labs(subtitle = str_c("**Active** in ",ref_year))


set.seed(42)
p2<- base_plot +
  geom_sf(data= post_offices_66 %>% filter(!opened_after_ref & !closed_before_ref & !(decade_discountinued=="Open")),
          aes(geometry=geometry, colour=`Closed on`),size=3,alpha=0.5,inherit.aes = FALSE) +
  geom_sf(data = post_office_winona,
          aes(geometry=geometry),colour=winona_colour,size=3) +
  geom_text_repel(data = post_office_winona,
                     aes(label = label_name,x=lat,y=lon),
                     colour="red",
                     family=city_font,
                     fontface="bold",
                     size=city_size,
                     force=10,
                     nudge_x = 0,
                     direction ="y",
                     point.padding = 95,
                     box.padding = 0.5,
                     nudge_y = 10,
                     segment.colour="red",
                     segment.linetype="solid",
                     arrow =my_arrow) +
  theme(legend.position = c(.23,.95)) +
  labs(subtitle = str_c("**Closed** from ",ref_year," onwards"))


#putting plots together

plot_title <- str_c("Winona has been forgotten")
plot_subtitle <- str_c("*Post Office closures along Route 66 from ",ref_year," onwards*")
plot_caption <-str_c(str_c('**Sources:** Cameron Blevins and Richard W. Helbock, US National Park Service, TIGER/Line, {maps}, and Bobby Troup'),
                     '<br><br>',
                    add_social_ref("@carlosyanez"))

## image from https://imgbin.com/png/d72xgBtC/u-s-route-66-in-california-oatman-interstate-40-road-png
logo <- here(tt_date,"route66.png")
logo <- png::readPNG(logo, native = TRUE)

top   <- 24.5
right <- 29.5

plogo <- inset_element(logo, unit(right-2.5,"cm"), unit(top-2.5,"cm"), unit(right,"cm"), unit(top,"cm"), align_to = 'full', on_top=FALSE) 

p <- (p1  / p2) + plogo +  
              plot_annotation(title=plot_title,subtitle = plot_subtitle,caption = plot_caption) & 
                theme(plot.title.position = "plot",
                plot.title = element_markdown(size = 18,family=city_font,face="bold",
                                              hjust=0,colour=darken(text_colour,0.2)),
                plot.subtitle = element_markdown(size = 12,family=city_font,hjust=0,
                                                 colour=darken(text_colour,0.08)),
                plot.caption = element_markdown(size = 10,hjust=0,vjust=0,family=google_font)
)  


p[[1]] <- p[[1]] + theme_base
p[[2]] <- p[[2]] + theme_base + theme(legend.position = c(.23,.95)) 
p[[3]] <- p[[3]] + theme_void()

p <- p & theme(plot.background = element_rect(fill=bg_colour))

p[[3]] <- p[[3]] + theme(plot.background = element_rect(fill="transparent",colour=NA))



##save on file
wv <- 12
save_image(p,here(tt_date,"post_office_route66.png"),width=wv,height=wv*0.85) 
