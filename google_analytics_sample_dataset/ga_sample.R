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
  billing = "carlosyanezcl")

dbListTables(con)

#### list sample to download

ga_tables <- dbListTables(con)
ga_tables <- ga_tables[1:10]  

  
###download data

for(i in 1:length(ga_tables)){
  
  data_i <- tbl(con,ga_tables[i])
  data_i2 <- data_i %>% collect()
  

  if(exists("result")){
     result <- rbind(result,data_i2)
     }else{ result<-data_i2}
  
}

stats <- result

##backup
write_rds(stats,"ga_stats.rds")

#expand lists

for(j in 1:5){

##get columns types

col_names <- names(stats)
col_types <- tibble(column=character(),type=character())

for(i in 1:length(col_names)){
  
  col_types<- tibble::add_row(col_types,column=col_names[i],type=typeof(stats[[i]]))
  
}

lists <- col_types %>% filter(type=="list")

if(nrow(lists)>0){
  for(i in 1:nrow(lists)){
    stats <- stats %>% unnest_wider(lists[i,]$column, names_repair="unique")
  }
}

}

#remove residual lists
stats<- stats %>% select(where(function(x) {a<-is.list(x)
!a  
}))

#remove columns with  "not available in demo dataset"


col_names <- names(stats)
col_types <- tibble(column=character(),type=character())

for(i in 1:length(col_names)){
  
  value <- stats[1,i] %>% pull()
  value <- as.character(as.character(value)=="not available in demo dataset")
  col_types<- tibble::add_row(col_types,column=col_names[i],type=value)
  
}

filter <- col_types %>% mutate(type=ifelse(is.na(type),FALSE,type)) %>% filter(type==FALSE) 


stats<- stats %>% select(all_of(filter$column))

#store in csv file

write_csv(stats,"ga_stats.csv")


