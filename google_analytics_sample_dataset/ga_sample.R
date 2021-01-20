#### code to download Google Analytics sample dataset made available through google big query ####
### Data provided by Google under CC0 licence ###

library(tidyverse)

############################### 
### LOAD PACKAGES  - IT WILL INSTALL THEM IF NOT FOUND

packages <- c("bigrquery",  # 
              "DBI",        # 
              "tidyr")      # 

loaded_packages <- paste0(search(),sep=" ",collapse = "")
packages <- tibble(package = packages)
packages <- packages %>% mutate(loaded=str_detect(loaded_packages, package, negate = FALSE)) %>% pull(package)

if(length(packages)>0 ){
  for(i in 1:length(packages)){
    result <- require(packages[i],character.only = TRUE)
    if(!result){
      install.packages(packages[i])
      library(packages[i],character.only = TRUE)
    }
  }
}
rm(packages,i,result)


###Create connection with BigQuery

con <- dbConnect(
  bigrquery::bigquery(),
  project = "bigquery-public-data",
  dataset = "google_analytics_sample",
  billing = "<<PROJECT NAME HERE>>")

#dbListTables(con)

#### list sample to downlad

ga_tables <- c("ga_sessions_20160801","ga_sessions_20160802","ga_sessions_20160803",
               "ga_sessions_20160804","ga_sessions_20160805")
               
               
#               "ga_sessions_20160806",
#               "ga_sessions_20160807","ga_sessions_20160808","ga_sessions_20160809",
#               "ga_sessions_20160810","ga_sessions_20160811","ga_sessions_20160812",
#               "ga_sessions_20160813","ga_sessions_20160814","ga_sessions_20160815")

for(i in 1:length(ga_tables)){
  
  data_i <- tbl(con,ga_tables[i])
  data_i <- data_i %>% collect()
  
  data_i2<-data_i %>% unnest_wider(totals, names_repair="universal") %>%
    unnest_wider(geoNetwork, names_repair="universal") %>%
    unnest_wider(device, names_repair="universal") %>%
    unnest_wider(hits...49, names_repair="universal") %>%
    unnest_wider(trafficSource, names_repair="universal") %>%
    unnest_wider(adwordsClickInfo, names_repair="universal") %>%
    unnest_wider(...25, names_repair="universal")
  
  if(exists("result")){
     result <- rbind(result,data_i2)
     }else{ result<-data_i2}
  
}

result <- as_tibble(result)
write_rds(result,"ga_stats.rds")
