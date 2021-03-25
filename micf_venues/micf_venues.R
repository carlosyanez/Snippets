### Map all Venues of Melbourne International Commediy Festival 2021

if(!require(librarian)) install.packages("librarian")
librarian::shelf("tidyverse","rvest",
                 "jessecambon/tidygeocoder",  #version on CRAN does not have reverse_geo
                 "sf",
                 "carlosyanez/customthemes",
                 "carlosyanez/aussiemaps")


# retrieve all venues

url <- "https://www.comedyfestival.com.au/2021/shows?filters[sort]=alpha&filters[category]=&filters[date]=&filters[venue]=all&filters[accessible]="
path <- "li.nav-bar__venues:nth-child(3) > div:nth-child(2) > div:nth-child(1) > ul:nth-child(1)"


venues_html <-read_html(url)
venues <- tibble(venues =venues_html %>% html_element(css = path)  %>%
  html_text2() ) %>% separate_rows(venues,sep="\n") %>%
  slice(4:n()) %>%
  mutate(
         venues=str_remove(venues,"- Chapel Theatre"),
         venues=str_remove(venues,"- The Loft"),
         venues=str_trim(venues),
         venues=str_replace(venues,"Wonthaggi Community Arts Centre","Wonthaggi Union Community Arts Centre"),

  ) %>%
  filter(!(venues %in% c("North","West","Ubar","South-East"))) %>%
  unique(.) 

# Use Google to get all the addresses

##pass 1
venue_loc <- geo(pull(venues,venues),method="google",
                     return_addresses = TRUE, unique_only = TRUE,verbose=TRUE)

## filter na and northern hemisphere results, add VIC Australia
## and try again

venues2 <- venue_loc %>%
           filter(is.na(lat) | lat>-34) %>%
           mutate(venue=str_c(address," Victoria Australia")) 

venue_loc1 <- venue_loc %>%
             filter(!is.na(lat) & lat< -34) 
          
           
venue_loc2 <-  geo(pull(venues2,venue),method="google",
                   return_addresses = TRUE, unique_only = TRUE,verbose=TRUE)

#the ones that didn't work 
venues3 <- venue_loc2 %>%
           filter(is.na(lat) | lat>-34) %>%
           mutate(address=str_replace(address,"Brunswick Ballroom Victoria Australia","Brunswick Ballroom, Sydney Road, Brunswick VIC, Australia"),
                  address=str_replace(address,"Citipointe Church West Victoria Australia","-"),
                  address=str_replace(address,"Nambour State College Victoria Australia","-"),
                  address=str_replace(address,"The Local Taphouse Victoria Australia","The Local Taphouse, Carlisle Street, St Kilda East VIC, Australia")
                  ) %>% filter(!address=="-") %>%
          rbind(tribble(~address, ~lat, ~long,
                        "52 Mitchell Street Brunswick, VIC 3056 Australia, Melbourne, Australia","0","0",
                        "1 Fitzroy St, St Kilda VIC Australia","0","0"))

  
#last retrieval 

venue_loc3 <-  geo(pull(venues3,address),method="google",
                   return_addresses = TRUE, unique_only = TRUE,verbose=TRUE)

#merge

venue_loc <- rbind(venue_loc1,venue_loc2) %>%  
             filter(!(address %in% c("The Leather Works (Dumpster Fire Comedy) Victoria Australia",
                                     "Ellora")))
             rbind(venue_loc3) %>%
             filter(!is.na(lat) & lat< -34) 


#get addresses
venue_addresses <- reverse_geo(lat=venue_loc$lat,long=venue_loc$long,
                       method = 'google', full_results = TRUE, verbose = TRUE)

venue_loc <- venue_loc %>% left_join(venue_addresses, by=c("lat"="lat","long"="long"))



venues_per_suburb <-venue_addresses %>% select(address_components) %>% 
                    unnest(address_components) %>%
                    unnest(types) %>%
                    filter(types=="locality") %>%
                    rename(suburb=long_name) %>%
                    group_by(suburb) %>%
                    summarise(n=n(),.groups="drop")

#get map with aussiemaps     
     
micf_map <- get_region("Greater Metropolitan Melbourne") %>%
            load_map(aggregation = c("LOCALITY","Region")) %>%
            left_join(venues_per_suburb,by=c("LOCALITY"="suburb"))

#plot

p <- micf_map %>% ggplot() +
     geom_sf(fill="gray96",colour="gray92") +
     geom_sf(data=(micf_map %>% filter(!is.na(n))), aes(fill=n))+
    custom_map_theme(legend_pos = "bottom", legend_dir = "horizontal") +
    scale_fill_continuous(low = "#ffca05", high = "#ff7900", name="MICF Venues") +
    labs(title = "MICF 2021 - Venues per Suburb",
         subtitle ="in Greater Metropolitan Melbourne",
         caption="Sources : data.gov.au (maps), www.comedyfestival.com.au (venues)")


finished_plot <- add_logo(p,"../Twitter_Logo_Blue.png","@carlosyanez","micf_venues.png",
                          plot_height = NA, plot_width = NA)

