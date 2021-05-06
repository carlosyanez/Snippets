# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-04/readme.md

# make sure that {librarian} and {tidytuesdayR} are there
if(!require(librarian)) install.packages("librarian")

# load other libraries

librarian::shelf("tidyverse",
                 "here",
                 "arrow",
                 "patchwork",
                 "sf",
                 "ggrepel",
                 "gridtext",
                 "grid",
                 "png",
                 "tinter",
                 "showtext","sysfonts",
                  "carlosyanez/customthemes")

# folder reference for here()
tt_date<-"2021-05-04"


# load tt data
if(!file.exists(here(tt_date,"tidytuesday.parquet"))){
  
  if(!require(tidytuesdayR)) install.packages("tidytuesdayR")
  
  tuesdata <- tidytuesdayR::tt_load(tt_date)
  water <- tuesdata$water %>% filter(country_name=="Timor-Leste") 
  write_parquet(water, here(tt_date,"tidytuesday.parquet"),compression = "gzip", compression_level = 7)
}else{
  water <- read_parquet(here(tt_date,"tidytuesday.parquet"))
}


water_timor_leste <- water %>%
                     mutate(installer=case_when(
                                                grepl("DAA",installer)     ~ "Local Government",
                                                installer=="UNKNOWN......" ~ "Unknown",
                                                TRUE                       ~ installer        
                    ))

summary_timor_leste <- water_timor_leste %>% 
  select(row_id,installer,status) %>%
  mutate(dummy=1) %>%
  pivot_wider(names_from = status,values_from = dummy) %>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  ) %>%
  select(-row_id) %>%
  group_by(installer) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
            n=n(),
            .groups = "drop") %>%
  arrange(n) 
            



order <- tibble(n=unique(summary_timor_leste$n)) %>%
         mutate(y_pos=row_number())

summary_timor_leste_plot <- summary_timor_leste %>%
                       left_join(order,by="n") %>%
                       arrange(desc(y_pos)) %>%
                       mutate(x_pos=1.8,             # transformation for plotting
                              bubble_size=sqrt(n/mean(n)))
  

#geoboundaries

if(!file.exists(here(tt_date,"tidytuesday.parquet"))){
  map<-st_read("https://www.geoboundaries.org/data/geoBoundariesCGAZ-3_0_0/ADM1/simplifyRatio_25/geoBoundariesCGAZ_ADM1.geojson")
  map<-map %>% filter(shapeGroup=="TLS")
  write_parquet(map, here(tt_date,"map.parquet"),compression = "gzip", compression_level = 7)
}else{
  map <- read_parquet(here(tt_date,"map.parquet"))
}


text_font <-"Overpass"
text_size <-3.5
drop_colour<-"blue"
text_colour <- darken(drop_colour,0.5)
sysfonts::font_add_google(text_font, text_font)
showtext::showtext_auto()


#arrow shapes

my_arrow <- arrow(angle = 30, length = unit(0.075, "inches"),
                  ends = "first", type = "open")

#base plot
p1 <- summary_timor_leste_plot %>%
mutate(x_pos=2.5,
       y_pos=y_pos,
       label=str_c(installer," (",n,")"),
       selected=TRUE
       ) %>%
  ggplot(aes(x=x_pos,
             size=bubble_size,
             y=y_pos,
             label=label)) +
  geom_point(position=position_jitter(seed = 1),colour=drop_colour,alpha=0.5) +
  geom_text_repel(data = . %>% filter(selected),
                  position=position_jitter(seed = 1),
                  colour=text_colour,
                  family=text_font,
                  size=text_size,
                  force=20,
                  direction ="both",
                  point.padding = 0.1,
                  box.padding = 0.5,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20
                  ) +
  ylim(0,max(summary_timor_leste_plot$y_pos)*1.7) +
  xlim(0,7)  +
  scale_size_area(max_size = 15)+
  theme_void() +
  theme(legend.position = "none")


#count number of dwells per LGA

water_timor_leste <- water_timor_leste %>%
  st_as_sf(., coords = c("lon_deg", "lat_deg"), 
           crs = 4326, agr = "constant") %>%
  st_transform(crs=st_crs(map))


plot_data <- tibble(shapeName=map$shapeName,
                    a=st_contains(map,water_timor_leste,sparse = TRUE)) %>%
             unnest(a) %>%
             count(shapeName) %>%
            left_join(map,by="shapeName") %>%
            select(shapeName,n,geometry)


subtitle_config <- element_text(family=text_font,size=18,hjust=0.5,colour="azure4")

p2 <- plot_data %>%
    ggplot()+
    geom_sf(aes(fill=n,geometry=geometry)) +
  
    custom_map_theme(legend_pos = c(.75,.2),
                     legend_dir = "horizontal",
                     google_font = text_font) +
    scale_fill_viridis_b(name="number",trans="log10",breaks=c(0,10,15,20,30,40,50,100))+
    theme(plot.subtitle = subtitle_config)+
    labs(subtitle="Location of sources")

functioning_status <- c("Not functioning","Partially functioning","Always functioning")

organisations <- tribble(~installer,~type,
                         "Local Government","Government",
                         "NTF","Local NGO",
                         "BESIK","Foreign Governmnent",  #AusAid
                         "FHTL","Local NGO", #Fundasaun Hafoun Timor Lorosa
                         "UNICEF","Intergov. Organisation",
                         "World Vision","International NGO",
                         "WATER AID","International NGO",
                         "MF", "National Government", #Ministry of Finance
                         "ADRA TL","International NGO", #Adventist Development and Relief Agency
                         "TGH","International NGO", #Triangle Generatio Humanitaire
                         "CVTL","International NGO",#Red Cross Timor-Leste
                         "HAMORIS NEON","Local NGO",
                         "Unknown","Unknown",
                         "NATILES","Unknown",
                         "OFI","Unknown",
                         "KaLISE","Local NGO",
                         "FUNDAMOR","Local NGO",
                         "CHILDFUND", "International NGO"
                                           )


p3 <- summary_timor_leste %>%
  left_join(organisations, by="installer") %>%
  group_by(type) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),.groups="drop") %>%
  pivot_longer(c(-type,-n),names_to = "status",values_to = "percentage") %>%
  mutate(status=factor(status,levels=functioning_status)) %>%
  ggplot(aes(x=fct_reorder(type,n),y=percentage,group=status,fill=status)) +
  geom_col()+
  custom_plot_theme(background_colour = "white",
                    legend_pos =  c(.75,.15),
                    legend_dir ="vertical",
                    google_font=text_font) +
  theme(axis.title.y = element_blank(),
        plot.subtitle = subtitle_config,
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_fill_manual(name="",values=c(alpha("azure4",0.5), alpha(drop_colour,0.4), alpha(drop_colour,0.6))) +
  labs(subtitle="Operational status by type of installer",
       x="Percentage") +
  coord_flip()
 

flag_size<-30
plot_title <- richtext_grob("Installers of Rural Water Sources in East Timor ",
                 x=0,y=0,
                 hjust = 0,
                 vjust = 0,
                 gp=gpar(fontsize=30,
                         fontface="bold",
                        fontfamily=text_font,
                        col="black"))
                


plot_caption <- richtext_grob(str_c(str_c('**Sources:** Water Point Data Exchange, geoboundaries.org'),
                                    '<br>',
                                    add_social_ref("@carlosyanez")),
                              x=0,y=0,
                              hjust = 0,
                              vjust = 0,
                              gp=gpar(fontsize=9,
                                      fontfamily=text_font,
                                      col="azure4")) 
 
# blue-water-tap-256x256.png
# from https://www.svgimages.com/png/blue-water-tap.html

water_tap <- here(tt_date,"blue-water-tap-256x256.png")
water_tap <- png::readPNG(water_tap, native = TRUE)


tap_size <- 0.38
tap_left <- 0.01
tap_top <-0.975

flag_TLS <-"https://upload.wikimedia.org/wikipedia/commons/thumb/2/26/Flag_of_East_Timor.svg/320px-Flag_of_East_Timor.svg.png"
download.file(flag_TLS,here(tt_date,"tls.png"))
flag <- png::readPNG(here(tt_date,"tls.png"), native = TRUE)

p <- p1 + inset_element(water_tap, left=tap_left,bottom=tap_top-tap_size,
                   right=tap_left+tap_size,top=tap_top, align_to = 'full', on_top=FALSE) +
     inset_element(p2,left=0.45 ,bottom = 0.55,top=0.9,right=1.02,align_to = "full") +
     inset_element(p3,left=0.5 ,bottom = 0.05,top=0.5,right=0.99,align_to = "full")  +
     inset_element(plot_caption, left =0.01 ,bottom = 0.01,top=0.2,right=0.5,align_to = "full", on_top=FALSE) +
     inset_element(plot_title, left =0.02 ,bottom = 0.95,top=1,right=0.9,align_to = "full", on_top=TRUE) +
     inset_element(flag, left =0.85 ,bottom = 0.85,top=1.05,right=1,align_to = "full", on_top=TRUE) &
     theme(plot.background = element_rect(colour="black"))                                            #add border so save_image knows where to cut

for(i in 1:7){
p[[i]] <- p[[i]] +theme(plot.background = element_blank())             #remove borders in each inset
}
     

##save on file
wv <- 12
save_image(p,here(tt_date,"water_sources_timor_leste.png"),width=wv,height=wv*1.1) 



