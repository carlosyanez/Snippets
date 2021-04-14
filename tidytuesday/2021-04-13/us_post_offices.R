# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-13/readme.md

# make sure that {librarian} and {tidytuesdayR} are there
if(!require(librarian)) install.packages("librarian")
if(!require(tidytuesdayR)) install.packages("tidytuesdayR")


# load other libraries
librarian::shelf("tidyverse",                 # no intro required
                 "stringi",
                 "patchwork",                 # plotting aggregation
                 "here",
                 "ggtext",                    # element_markdown()
                 "carlosyanez/customthemes",  # personal library with custom themes
                 "maps",                       #US Maps
                 "osmdata",
                 "sf"
)


tt_date<-"2021-04-13"

# load data
tuesdata <- tidytuesdayR::tt_load(tt_date)
post_offices <- tuesdata$post_offices

# route 66

states_route66 <- c("california", "arizona", "new mexico", "texas", 
                    "oklahoma","kansas", "missouri", "illinois")
us <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))


if(file.exists(here(tt_date,"route66.rds"))){
  route66 <- readRDS(here(tt_date,"route66.rds"))
}else{
  route66_counties <- c("california,los angeles","california,san bernardino",
                        "arizona,mohave","arizona,yavapai","arizona,coconino","arizona,navajo","arizona,apache",
                        "new mexico,mckinley","new mexico,cibola","new mexico,bernalillo","new mexico,santa fe",
                        "new mexico,torrance","new mexico,guadalupe","new mexico,quay",
                        "texas,deaf smith","texas,oldham","texas,potter","texas,carson","texas,gray","texas,donley",
                        "texas,wheeler",
                        "oklahoma,beckham","oklahoma,washita","oklahoma,custer","oklahoma,caddo","oklahoma,canadian",
                        "oklahoma,oklahoma","oklahoma,lincoln","oklahoma,creek",
                        "oklahoma,tulsa","oklahoma,wagoner","oklahoma,rogers","oklahoma,craig",
                        "oklahoma,delaware","oklahoma,ottawa",
                        "kansas,cherokee",
                        "missouri,jasper","missouri,lawrence","missouri,greene","missouri,webster","missouri,laclede",
                        "missouri,pulaski","missouri,phelps","missouri,crawford","missouri,franklin","missouri,st louis",
                        "missouri,st louis city",
                        "illinois,st clair","illinois,madison","illinois,macoupin","illinois,montgomery","illinois,sangamon",
                        "illinois,logan","illinois,mclean","illinois,livingston","illinois,grundy","illinois,will",
                        "illinois,du page","illinois,cook")
  route66 <- NULL
  
  for(x in 1:length(route66_counties)){
      message(str_c(x, ":", route66_counties[x], " started"))
      county <- us %>% filter(ID == route66_counties[x])
      county_box <- st_bbox(county)
      
      route66_osm <- opq(county_box, timeout = 25 * 100) %>%
        add_osm_feature(
          key = "highway",
          value = c("primary", "secondary", "tertiary", "unclassified")
        ) %>%
        osmdata_sf()
        data <- route66_osm$osm_lines %>% 
                filter(grepl(" 66", ref)) %>%
                mutate(term=str_c("ref: ",ref)) %>%
                select(osm_id,term) %>%
                st_drop_geometry()
        
        if ("name" %in% names(route66_osm$osm_lines)) data <- rbind(data,route66_osm$osm_lines %>%
                                                                         filter(grepl(" 66", name)) %>%
                                                                         mutate(term=str_c("name: ",name)) %>%
                                                                         select(osm_id,term) %>%
                                                                         st_drop_geometry())
        
        if ("name_1" %in% names(route66_osm$osm_lines)) data <- rbind(data,route66_osm$osm_lines %>%
                                                                      filter(grepl(" 66", name_1)) %>%
                                                                      mutate(term=str_c("name_1: ",name_1)) %>%
                                                                      select(osm_id,term) %>%
                                                                      st_drop_geometry())
        
        if ("old_name" %in% names(route66_osm$osm_lines)) data <- rbind(data,route66_osm$osm_lines %>%
                                                                      filter(grepl(" 66", old_name)) %>%
                                                                      mutate(term=str_c("old_name: ",old_name)) %>%
                                                                      select(osm_id,term) %>%
                                                                      st_drop_geometry())  
                
        if ("old_ref" %in% names(route66_osm$osm_lines)) data <- rbind(data,route66_osm$osm_lines %>%
                                                                      filter(grepl(" 66", old_ref)) %>%
                                                                      mutate(term=str_c("old_ref: ",old_ref)) %>%
                                                                      select(osm_id,term) %>%
                                                                      st_drop_geometry())
          
        if ("alt_name" %in% names(route66_osm$osm_lines)) data <- rbind(data,route66_osm$osm_lines %>%
                                                                           filter(grepl(" 66", alt_name)) %>%
                                                                           mutate(term=str_c("alt_name: ",alt_name)) %>%
                                                                           select(osm_id,term) %>%
                                                                           st_drop_geometry()) 
        if ("tiger.name_base" %in% names(route66_osm$osm_lines)) data <- rbind(data,route66_osm$osm_lines %>%
                                                                                   filter(grepl(" 66", tiger.name_base)) %>%
                                                                                   mutate(term=str_c("tiger.name_base: ",tiger.name_base)) %>%
                                                                                   select(osm_id,term) %>%
                                                                                   st_drop_geometry())
                  
        if ("tiger.name_base_1" %in% names(route66_osm$osm_lines)) data <- rbind(data,route66_osm$osm_lines %>%
                                                                            filter(grepl(" 66", tiger.name_base_1)) %>%
                                                                            mutate(term=str_c("tiger.name_base_1: ",tiger.name_base_1)) %>%
                                                                            select(osm_id,term) %>%
                                                                            st_drop_geometry())
        
        if ("tiger.name_base_2" %in% names(route66_osm$osm_lines)) data <- rbind(data,route66_osm$osm_lines %>%
                                                                                   filter(grepl(" 66", tiger.name_base_2)) %>%
                                                                                   mutate(term=str_c("tiger.name_base_2: ",tiger.name_base_2)) %>%
                                                                                   select(osm_id,term) %>%
                                                                                   st_drop_geometry())
        
        if ("wikipedia" %in% names(route66_osm$osm_lines)) data <- rbind(data,route66_osm$osm_lines %>%
                                                                                   filter(grepl("Route 66", wikipedia)) %>%
                                                                                   mutate(term=str_c("wikipedia: ",wikipedia)) %>%
                                                                                   select(osm_id,term) %>%
                                                                                   st_drop_geometry())
        
        if ("name.en" %in% names(route66_osm$osm_lines)) data <- rbind(data,route66_osm$osm_lines %>%
                                                                           filter(grepl(" 66", name.en)) %>%
                                                                           mutate(term=str_c("name.en: ",name.en)) %>%
                                                                           select(osm_id,term) %>%
                                                                           st_drop_geometry())
        
        if ("name.full" %in% names(route66_osm$osm_lines)) data <- rbind(data,route66_osm$osm_lines %>%
                                                                         filter(grepl(" 66", name.full)) %>%
                                                                         mutate(term=str_c("name.full: ",name.full)) %>%
                                                                         select(osm_id,term) %>%
                                                                         st_drop_geometry())
        
      data <- data %>% group_by(osm_id) %>%
              summarise(term = paste(term, collapse = ",")) %>%
              left_join(route66_osm$osm_lines,by="osm_id") %>%
        select(osm_id,name,term,highway,surface,geometry) %>%
        mutate(x = route66_counties[x]) %>%
        separate(x, c("state", "county"), sep = ',') %>%
        mutate(across(c(state, county), stri_trans_totitle))
      
      message(str_c(x, ":", route66_counties[x], " ended"))
      
      
      if(is.null(route66)){ route66 <- data
      }else{ route66 <- rbind(route66,data)}
      
      }
  rm(x,data,county,county_box,route66_osm)
  saveRDS(route66, here(tt_date, "route66.rds"))
}

counties66       <- us %>% filter(ID %in% route66_counties) %>%
                    separate(ID, c("state", "county"), sep = ',') %>%
                    filter(state=="oklahoma")
counties66_label <- st_centroid(counties66)


           
ggplot() +
  geom_sf(data=counties66, aes(geometry=geom,colour=state,label=county)) +
  geom_sf_text(data=counties66_label,aes(label=county),size=3) +
  geom_sf(data=route66 ,aes(geometry=geometry),colour="red") 
