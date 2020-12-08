## Script to create Map of Melbourne Streets

## Load Libraries

### tidyverse
library(tidyverse)
library(broom)

### spatial manipulation
library(spdplyr)
library(osmdata)

### plotting 
library(ggmap)
library(paletteer)
library(colRoz)
library(showtext)

## Load data from OpenStreetMap

base_all <- opq('Melbourne Australia') %>%
  add_osm_feature(key = 'highway',value=c("motorway","trunk",
                                          "primary","secondary",
                                          "tertiary","unclassified",
                                          "residential"))

base_cbd <- opq('3000 Australia') %>%
  add_osm_feature(key = 'highway',value=c("motorway","trunk",
                                          "primary","secondary",
                                          "tertiary","unclassified",
                                          "residential"))

cway_cbd<- osmdata_sp(base_cbd)
cway_all <- osmdata_sp(base_all)

cbd <- cway_cbd$osm_lines %>% filter(grepl("Flinders?\\b",name)|
                                       grepl("Collins?\\b",name)|
                                       grepl("Bourke?\\b",name)|
                                       grepl("Lonsdale?\\b",name)|
                                       grepl("La Trobe?\\b",name)|
                                       grepl("Spring?\\b",name)|
                                       grepl("Exhibition?\\b",name)|
                                       grepl("Russell?\\b",name)|
                                       grepl("Swanston?\\b",name)|
                                       grepl("Elizabeth?\\b",name)|
                                       grepl("William?\\b",name)|
                                       grepl("Kings?\\b",name)|
                                       grepl("Spencer?\\b",name)) 

others <- cway_all$osm_lines %>% filter(grepl("Flinders?\\b",name)|
                                          grepl("Collins?\\b",name)|
                                          grepl("Bourke?\\b",name)|
                                          grepl("Lonsdale?\\b",name)|
                                          grepl("La Trobe?\\b",name)|
                                          grepl("Spring?\\b",name)|
                                          grepl("Exhibition?\\b",name)|
                                          grepl("Russell?\\b",name)|
                                          grepl("Swanston?\\b",name)|
                                          grepl("Elizabeth?\\b",name)|
                                          grepl("William?\\b",name)|
                                          grepl("Kings?\\b",name)|
                                          grepl("Spencer?\\b",name)) %>%
  filter(!(osm_id %in% cbd$osm_id)) %>%
  filter(!(name %in% c("Kings College Court","King Edward Avenue",
                       "King William Street","Sir William Street",
                       "William Cooper Street")))


###https://www.nceas.ucsb.edu/sites/default/files/2020-04/ggmapCheatsheet.pdf


data_cbd <- tidy(cbd,group=osm_id) %>% mutate(type="CBD")
data_others <- tidy(others,group=osm_id) %>% mutate(type="Others")
data <- rbind(data_cbd,data_others)

data <- data %>% mutate(Location=as.factor(ifelse(type=="CBD","City of Melbourne","Other Suburbs")))

### bounding box using retrieved data
#mybbox <- as.numeric(unlist(strsplit(cway_all$bbox, split=",")))
#names(mybbox) <- c("bottom","left","top","right")
#top left

### manually set bounding box
mybbox <- c(bottom=-38.012574,right=145.1944393,top=-37.626721,left=144.7938753)
map <- get_map(mybbox, zoom=13, source="stamen",maptype="toner",color = "bw",alpha=0.6)


## plotting

font_add_google("Covered By Your Grace", "grace")
showtext_auto()

mapp<- ggmap(map) + geom_line(data=data, 
                              mapping=aes(y=lat , x=long, group=group,colour=Location), 
                              size=1) +
  scale_color_paletteer_d("colRoz::uluru") + 
  theme_minimal() +
  labs(title="Melbourne, a city of many centres",
       subtitle = "Roads sharing names with CBD streets",
       caption = "Source: OpenStreetMap") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        plot.title = element_text(size=12,face="bold",colour = "#272928",family="grace"),
        plot.subtitle =element_text(size=10,colour = "azure4",family="grace"),
        plot.caption =  element_text(size=8,colour = "azure4",family="grace"),
        legend.text = element_text(size=10,colour = "#272928",family="grace"),
        legend.title = element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

#mapp

ggsave("street_map.png",mapp,dpi=400)