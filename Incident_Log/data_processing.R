library(tidyverse)
library(lubridate)

file_name <- "incident_event_log"

if(!file.exists(paste(file_name,".csv",sep=""))){
            download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00498/incident_event_log.zip",
              paste(file_name,".zip",sep=""))
  unzip(paste(file_name,".zip",sep=""))
  
}

inclog <- read_csv(paste(file_name,".csv",sep=""))

### remove attributes

inclog <- inclog %>% select(number, incident_state,active,
                            reassignment_count,made_sla,
                            caller_id,opened_by,opened_at,
                            sys_updated_by, sys_updated_at,
                            category,subcategory,
                            cmdb_ci, impact,urgency,priority,
                            assignment_group,knowledge,
                            closed_code,resolved_by)

### format dates and add row number (index)

reference_time <- dmy_hm("03-04-2016 7:00",tz=Sys.timezone())

inclog <- inclog %>% mutate(
                    opened_at = dmy_hm(opened_at,tz=Sys.timezone()),
                    sys_updated_at=dmy_hm(sys_updated_at,tz=Sys.timezone()),
                    elapsed_time=difftime(reference_time, opened_at, tz=Sys.timezone(),units =  "hours"),
                    last_update_time=difftime(reference_time, sys_updated_at, tz=Sys.timezone(),units =  "hours")
                    )



###filter two months

inclog<- inclog %>% filter(sys_updated_at>=ymd("2016-04-01")) %>%
          filter(sys_updated_at<=ymd("2016-04-02")) %>% unique(.) %>%
          mutate(rowid=row_number())

#### clean-up user names

inclog <- inclog %>% mutate(opened_by=str_replace(opened_by,"Opened by ","Agent "),
                  sys_updated_by=str_replace(sys_updated_by,"Updated by ","Agent "),
                  resolved_by=str_replace(resolved_by,"Resolved by ","Agent ")) 


inclog <- inclog %>% group_by(number) %>% 
  mutate(latest=(sys_updated_at==max(sys_updated_at)) &
                 rowid==max(rowid),
         interactions = n()) %>% ungroup()

write_csv(inclog,"processed_log_incident.csv")