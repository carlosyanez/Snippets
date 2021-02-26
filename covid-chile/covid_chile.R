# Load/Install pacman
if(!require(pacman)) install.packages("pacman", repos = "http://cran.us.r-project.org")

#use pacman to install all other packages
pacman::p_load("tidyverse","ggplot2","patchwork","lubridate","magick")



casos_chile_source <- read.csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto5/TotalesNacionales.csv")

casos_chile <- casos_chile_source %>% 
              pivot_longer(-Fecha,values_to = "Value",names_to="Date") %>%
              mutate(Date=ymd(str_remove_all(Date,"X"))) %>%
              filter(Fecha %in% c("Casos nuevos totales","Fallecidos")) %>%
              mutate(Fecha=ifelse(Fecha=="Fallecidos","Deaths","NewCases")) %>%
              pivot_wider(values_from = Value, names_from = Fecha) %>%
              arrange(Date) %>%
              mutate(Deaths = Deaths-lag(Deaths,1,0))


casos_chile %>% pivot_longer(-Date, values_to="Value",names_to="Metric") %>%
                ggplot(aes(x=Date,y=Value)) + geom_line() + facet_grid(Metric~.,scales="free_y")
