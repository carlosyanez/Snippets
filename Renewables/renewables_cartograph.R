###Create plot to show share of RE electricity per country in a map, 
#### adjusting country size to total electicity generation
###Data initially taken from wikipedia- code left here as comments

#library(rvest)
library(tidyverse)
library(maptools)
library(spdplyr)
library(sf)
library(rgdal)

### Get world map and add Mercator Projection

data(wrld_simpl)
simpleworld <- spTransform(wrld_simpl, CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))


##Data source
#wiki_url<-"https://en.wikipedia.org/wiki/List_of_countries_by_renewable_electricity_production"

# download the data from wikipedia and put in table
#data_wiki<-wiki_url %>% read_html() %>%
#  html_node(xpath="/html/body/div[3]/div[3]/div[5]/div[1]/table") %>%
#  html_table(fill = TRUE)

#data_wiki<-data_wiki[2:nrow(data_wiki),1:4]

data_irena <-  readxl::read_xlsx("irena.xlsx")

data_irena <- data_irena %>% filter(Year==2019) 

data <- data_irena %>% group_by(`ISO Code`,`RE or Non-RE`) %>% summarise(Value=sum(Value)) %>%
              ungroup() %>%
              mutate(`RE or Non-RE`=str_remove(`RE or Non-RE`,"Total ")) %>%
                       pivot_wider(values_from = Value, names_from=`RE or Non-RE`) %>%
              mutate(Total.GWh =`Non-Renewable`+Renewable,
                     RE_Percentage=Renewable/Total.GWh) %>%
              select(ISO3=`ISO Code`,Total.RE.GWh=Renewable,Total.GWh,RE_Percentage)
              

# trasform to numbers and calculate percentage 

#data <- data_wiki %>% mutate(Total.GWh= as.numeric(str_remove_all(`Total (GWh)`, ",")),
#               Total.RE.GWh=as.numeric(str_remove_all(`Total RE (GWh)`, ",")),
#               RE_Percentage= Total.RE.GWh/Total.GWh,
#               NAME=`Country or territory`) %>%
#             select(NAME,Total.GWh,Total.RE.GWh,RE_Percentage)

world.total.gwh <- sum(data$Total.GWh,na.rm = TRUE)

data <- data%>% mutate(RE_Percentage=ifelse(is.na(RE_Percentage),0.01,RE_Percentage),
                      Total.GWh=ifelse(is.na(Total.GWh),min(Total.GWh),Total.GWh),
                      energy_weight=Total.GWh/world.total.gwh)




# merge into geo shape 

data_geo <- simpleworld %>% left_join(data, by="ISO3")  %>%
  mutate(energy_weight=ifelse(is.na(energy_weight),min(energy_weight),energy_weight))

# https://www.r-graph-gallery.com/cartogram.html 

##Create cartogram

energy_cartogram <- cartogram_cont(data_geo, "energy_weight", itermax=5)


#plot with ggplot

energy_df <- fortify(energy_cartogram, region = "ISO3")

energy_df <- energy_df %>% left_join(energy_cartogram@data %>% select(-LON,-LAT),
                   by = c("id"="ISO3")) %>%
                  mutate(RE_Percentage=RE_Percentage*100)


p0 <- ggplot(data = energy_df, aes(x = long, y = lat, group = group, fill = RE_Percentage)) +
  geom_polygon() +
  geom_path(color = "white", size = 0.2) +
 # scale_fill_gradient(breaks=c(0.33,0.66,0.99), labels=c("Low","Medium","High")) + 
  scale_fill_gradient(name="Perc. Renewables",low = "red", high = "green", na.value = NA) +  coord_equal() +
  labs(title = "Countries by Renewable Electricity Production - 2019",
       subtitle = "Size adjusted to total electricity production",
       caption="Source: https://www.irena.org/Statistics/Download-Data")+
  theme_void() +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    legend.background = element_rect(fill = "#f5f5f4", color = NA) ,
    legend.key.size = unit(1,"line"),
    legend.title = element_text(size=8),
    plot.title = element_text(size= 12, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 10, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.1, 0.25)
  )
print(p0)
ggsave("renewables.png",p0,dpi=400)


