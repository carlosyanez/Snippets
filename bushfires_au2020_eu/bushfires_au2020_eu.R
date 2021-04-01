

librarian::shelf("tidyverse","sf","giscoR","units","measurements",
                 "labelled","ggmap","carlosyanez/customthemes","ggfx",
                 "carlosyanez/aussiemaps", #just for cleaning up function
                 "ggspatial",
                 "ggtext")

area_burnt_aus <- as_units(10173*10^3*10^4,"m^2") # #from https://www.agriculture.gov.au/abares/forestsaustralia/forest-data-maps-and-tools/fire-data#area-of-native-forest-in-fire-area-by-forest-tenure-and-jurisdiction
area_burnt_us <-  as_units(5299044.8*10^4,"m^2") # https://en.wikipedia.org/wiki/2020_wildfire_season
area_burnt_bra <- as_units((376416)*10^4,"m^2") # https://en.wikipedia.org/wiki/2020_C%C3%B3rdoba_wildfires https://en.wikipedia.org/wiki/2020_Delta_del_Paran%C3%A1_wildfires 

### two functions to get equivalent surfaces
get_contiguous <- function(other,base){
  contiguous <- st_touches(other,base)
  names(contiguous) <- other$NUTS_ID
  contiguous <- Filter(length, contiguous)
  contiguous <- other %>% filter(NUTS_ID %in% names(contiguous))
  return(contiguous)
}
map_fires <- function(eu_nuts3,starting_id,area_burnt_aus){
  
  base <- eu_nuts3 %>%filter(NUTS_ID==starting_id)
  base$area <- st_area(base)
  continue <- TRUE
  
  #add all contiguous areas until exceeding 1.1 of aussie area
  
  while(continue){
    
    cont1 <- get_contiguous(eu_nuts3 %>% filter(!(NUTS_ID %in% base$NUTS_ID)),
                            base)
    
    cont1$area <- st_area(cont1)
    total_area <- sum(base$area,cont1$area)
    continue <- area_burnt_aus > total_area
    
    if(continue) base <- rbind(base,cont1)
  }
  
  renmant <- area_burnt_aus - sum(base$area)
  
  cont1 <- cont1 %>% mutate(area_rel = area/renmant) %>%
    arrange(desc(area_rel)) %>%
    mutate(cumul_area =cumsum(area_rel))
  
  cont1$cumul_area <- drop_units(cont1$cumul_area)  
  
  cont1 <- cont1  %>%
    filter(cumul_area < 1.02) %>%
    select(-cumul_area,-area_rel)
  
  
  base <- rbind(base,cont1)
  
  return(base)
  
}

#get eurostat areas
eu_nuts3 <- gisco_get_nuts(nuts_level = "3")

# find areas
# eu_nuts3 %>% filter(NUTS_NAME %in% c("Stuttgart, Stadtkreis","Paris","Wien"))

aus_fires <- map_fires(eu_nuts3,"DEC01",area_burnt_aus) %>% # Starting point = Stuttgart
             summarise(.groups=drop) %>% clean_polygons()
us_fires <- map_fires(eu_nuts3,"UKD34",area_burnt_us) %>%  #starting point = Camden and City of London
            summarise(.groups=drop) %>% clean_polygons()
bra_fires <- map_fires(eu_nuts3,"AT130",area_burnt_us) %>% #starting point =Vienna
            summarise(.groups=drop) %>% clean_polygons()

#fires <- rbind(aus_fires %>% mutate(Region="Eastern Australia"),
#               us_fires %>% mutate(Region="Western USA")) %>%
#         rbind(bra_fires %>% mutate(Region="Amazon Brazil"))

#rm(aus_fires,us_fires,bra_fires)

colour_aus <- "#d10606"
colour_us <- "#e8931c"
colour_bra <- "#e63760"
sea_colour <- "#ced6d9"
land_colour <- "#c7c4bb"
border_colour <- "#e3e1dc"
  
world <- gisco_get_countries()


title_text <- "**What if the 2020 fires were in Europe?**"
subtitle_text <-str_c("Fires from <span style = 'color:",colour_aus,
                 ";'> Australia</span>, <span style = 'color:",
                 colour_us,";'>USA</span> and <span style = 'color:",
                 colour_bra,";'>Brazil</span>")                        

caption_text <- "**Sources**: Eurostat,Wikipedia <br> <img src='https://upload.wikimedia.org/wikipedia/de/thumb/9/9f/Twitter_bird_logo_2012.svg/200px-Twitter_bird_logo_2012.svg.png'
    height='9' />@carlosyanez"
                  

 p1<-  ggplot()+
  #  annotation_map_tile(type = "stamenwatercolor")+
  with_shadow(geom_sf(data=world,aes(geometry=geometry),fill=land_colour,colour=border_colour),
              colour="azure1",
              x_offset = 0.01,
              y_offset = 0.01) +
  with_bloom(with_outer_glow(
    geom_sf(data=(aus_fires),aes(geometry=geometry),fill=colour_aus,colour=colour_aus,alpha=0.5),
    colour=colour_aus)) +
  with_bloom(with_outer_glow(
    geom_sf(data=(us_fires),aes(geometry=geometry),fill=colour_us,colour=colour_us,alpha=0.5),
    colour=colour_us)) +  
  with_bloom(with_outer_glow(
    geom_sf(data=(bra_fires),aes(geometry=geometry),fill=colour_bra,colour=colour_bra,alpha=0.5),
    colour=colour_bra)) +
  coord_sf(xlim = c(-12, 45), ylim = c(30, 73), expand = FALSE) +
  custom_map_theme(legend_pos = "none") +
  theme(panel.background = element_rect(fill = sea_colour,
                                        colour = NA),
        plot.background = element_rect(fill = sea_colour, 
                                       colour = NA),
        plot.title.position = "plot",
        plot.title = element_markdown(size = 14,hjust=0),
        plot.subtitle = element_markdown(size = 12,hjust=0),
        plot.caption = element_markdown(size = 9,hjust=0,vjust=0)) +
  labs(title=title_text,
       subtitle=subtitle_text,
       caption=caption_text)


ggsave("bushfires2020.png",p1,dpi=400)
  