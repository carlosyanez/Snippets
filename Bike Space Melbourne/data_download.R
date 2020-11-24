library(tidyverse)
library(lubridate)

## download building data
### https://data.melbourne.vic.gov.au/Property/Buildings-with-name-age-size-accessibility-and-bic/pmhb-s6pn

file_buildings <- "buildings_source.csv"
if(!file.exists(file_buildings))
  download.file("https://data.melbourne.vic.gov.au/api/views/pmhb-s6pn/rows.csv?accessType=DOWNLOAD",file_buildings)

filter_year<-2016

source_buildings <- read.csv(file_buildings,stringsAsFactors=FALSE)

excluded_uses <- c("Residential Apartment","Student Accommodation",
                   "Unoccupied - Under Construction","Institutional Accommodation",
                   "Unoccupied - Unused", "House/Townhouse",
                   "Commercial Accommodation",
                   "Unoccupied - Under Renovation",
                   "Storage",
                   "Unoccupied - Under Demolition/Condemned")

source_buildings <- source_buildings %>% 
                    filter(Census.year==2015) %>%
                    filter(!is.na(Bicycle.spaces)) %>%
                    filter(Bicycle.spaces >0) %>%
                    filter(!(Predominant.space.use %in% excluded_uses)) %>%
                    select(Building.name,Street.address,Suburb=CLUE.small.area,
                           Predominant.space.use,Bicycle.spaces,Has.showers,
                           longitude=x.coordinate,latitude=y.coordinate) %>%
                    mutate(Suburb=case_when(Suburb=="Melbourne (CBD)" ~ "Melbourne CBD",
                                             Suburb=="West Melbourne (Residential)" ~ "West Melbourne",
                                             Suburb=="West Melbourne (Industrial)"  ~ "West Melbourne",
                                             TRUE ~ Suburb),
                            Has.showers=case_when(Has.showers==TRUE ~ "Yes",
                                                  TRUE ~ "No"))
write_csv(source_buildings,"Buildings2017.csv")


#download traffic data

file_traffic <- "traffic_source.csv"

if(!file.exists(file_traffic))
  download.file("https://data.melbourne.vic.gov.au/api/views/qksr-hqee/rows.csv?accessType=DOWNLOAD",file_traffic)

source_traffic <- read.csv(file_traffic,stringsAsFactors=FALSE)

source_traffic <- source_traffic %>% mutate(Date=dmy(date),
                                            Year=year(Date)) %>%
                                     mutate(suburb=case_when(suburb=="CARLTON" ~ "Carlton",
                                                             suburb=="Carlton North" ~ "Carlton",
                                                             TRUE ~ suburb))

source_traffic <- source_traffic %>% group_by(Date,suburb)%>% 
  summarise(bike=sum(bike,na.rm = TRUE)) %>%
  ungroup()%>% filter(bike>0) %>%
  group_by(suburb) %>%
  summarise(bike=mean(bike))

write_csv(source_traffic,"Traffic2017.csv")

