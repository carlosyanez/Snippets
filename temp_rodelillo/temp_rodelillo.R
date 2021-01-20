##### Plot of monthly average of daily max in Rodelillo (Viña del Mar), Chile

## Load libraries

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(showtext)
library(cowplot)

## Load file 
file <- "temp.csv"
download.file("https://climatologia.meteochile.gob.cl/application/productos/gethistoricos/330007_XXXX_Temperatura_",file)
temp <- read_delim(file,delim=";")

## massage data

temp_chart<- temp %>%
              mutate(Fecha=as_date(parse_date_time(str_sub(momento,1,10),"dmY"))) %>%  # Get date into usable format
                group_by(Fecha) %>%                                                    # Group and get daily max, min, median
                summarise(Max_Diaria=max(Ts_Valor,na.rm=TRUE),                   
                          Min_Diaria=min(Ts_Valor,na.rm=TRUE),
                          Med_Diaria=median(Ts_Valor,na.rm=TRUE),
                          .groups = "drop") %>%
                mutate(Mes=month(Fecha, label = TRUE,locale = "es_ES"),                # get months names in spanish
                        Año=year(Fecha)) %>%                                           # get year
                 group_by(Mes,Año) %>%                                                 # Group by (Month,Year) to get monthly averages
                summarise(Promedio_Max=mean(Max_Diaria,na.rm=TRUE),                     
                          Promedio_Min=mean(Min_Diaria,na.rm=TRUE),
                          Promedio_Media=mean(Med_Diaria,na.rm=TRUE),
                          .groups = "drop") %>%
                filter((Año<2020)&(Año>1971))                                          #filter out years with incomplete data 


### Get font

font_add_google("Titillium Web", "Titilium")
showtext_auto()

# based on https://dominicroye.github.io/en/2018/how-to-create-warming-stripes-in-r/, modified

## colour palette
col_strip <- brewer.pal(11, "RdBu")

# plot theming

theme_strip <-   theme_minimal()+
  theme(axis.text.y=element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(vjust=0.75,hjust=1,size=10,family="Titilium"),
        legend.text=element_text(size=8,family="Titilium"),
        axis.text.x = element_text(vjust = 1,size=10,family="Titilium"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold",family="Titilium"),
        plot.subtitle = element_text(size = 12,family="Titilium"),
        legend.position="bottom",
        legend.direction="horizontal"
  )

#plot

p<- temp_chart %>% group_by(Año) %>%
  summarise(`Temperatura (C)`=mean(Promedio_Max)) %>%
  ggplot(aes(x=Año,y=1,fill=`Temperatura (C)`)) +
  geom_tile() +
  scale_y_continuous(expand = c(0, 0))+
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1))+
  scale_fill_gradientn(colors = rev(col_strip)) +
  labs(title="Cambio climático en el Gran Valparaíso",
       subtitle="Rodelillo (330007) - Temperatura Máxima Diaria, Promedio Anual",
       legend="Temperatura (C)",
       caption = "Fuente: Dirección Metereológica de Chile")+
  theme_strip


## Add twitter logo and handle, Twitter logos available at https://about.twitter.com/en_us/company/brand-resources.html
logo_file <- "Twitter_Logo_Blue.png"

p2 <- ggdraw() +
  draw_image(
    logo_file, scale = .08, x = 1,
    hjust = 1, halign =0, valign = 0
  ) +
  draw_plot(p) +
  draw_label("@carlosyanez",x=0,y=0,
             fontfamily = "Titilium",
             size=12,
             hjust=-0.28,vjust=-1.1
  )
  
p2

## save into png file
ggsave("max_temp_rodelillo.png",p2,width = 12,height=4)



