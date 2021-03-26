
if(!require(librarian)) install.packages("librarian")
librarian::shelf("tidyverse",
                 "ggmap",
                  "sf",
                 "tidygeocoder",
                 "carlosyanez/aussiemaps",
                 "carlosyanez/customthemes")

vienna <- geo("Wien Mitte the Mall, Vienna Austria",method = "osm")
melbourne <- geo("Doncaster Shoppingtown Victoria Australia",method="osm")

difference <- c(vienna[1,]$long-melbourne[1,]$long,
                vienna[1,]$lat-melbourne[1,]$lat) %>%
              st_point() 

difference <- st_sfc(difference)


melb_map <- get_region("Greater Metropolitan Melbourne") %>%
            load_map(aggregation = "LGA")  %>%
            st_transform("WGS84")


melb_map_geom <-st_geometry(melb_map)

mel_translated <-  melb_map %>%
                  mutate(geometry=geometry+ difference)

melbb <- st_bbox(mel_translated)
names(melbb) <- c("left","bottom","right","top")


map_tiles <-
  get_map(
    melbb,
    zoom = 10,
    source = "stamen",
    maptype = "toner",
    color = "bw",
    alpha = 0.95
  )

melb_vienna <- ggmap(map_tiles) +
  geom_sf(data=mel_translated,aes(geometry=geometry),
          alpha=0.2,fill="grey85",
          colour="royalblue3",size=1.1,
          inherit.aes = FALSE) +
#  geom_sf_label(data=mel_translated, aes(label = LGA), colour = "blue",inherit.aes = FALSE,size=0.5) +
  custom_map_theme() +
  labs(title="Melbourne is a big city!",
       subtitle = "Melbourne's area on Eastern Austria and Western Slovakia",
       caption = "Sources: Stamen Maps | data.gov.au")

  
finished_plot <- add_logo(melb_vienna,"../Twitter_Logo_Blue.png","@carlosyanez","melb_vie.png",
                          plot_height = NA, plot_width = NA)
