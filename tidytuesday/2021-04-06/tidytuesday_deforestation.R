# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-06/readme.md


# make sure that {librarian} and {tidytuesdayR} are there
if(!require(librarian)) install.packages("librarian")
if(!require(tidytuesdayR)) install.packages("tidytuesdayR")


# load other libraries
librarian::shelf("tidyverse",                 # no intro required
                 "rvest",                     # scrapping data from wikipedia
                 "polite",
                 "patchwork",                 # plotting aggregation
                  "here",
                 "ggtext",                    # element_markdown()
                 "gridtext",
                 "sysfonts",
                 "showtext",
                 "ggalt",                     # for Dumbnell chart
                 "wpp2019",                   # world population prospects from United Nations Population Division
                 "giscoR","sf",               # maps
                 "carlosyanez/customthemes",   # personal library with custom themes
                 "mitchelloharawild/icons",
                 "rsvg",
                 "png"
                 )


tt_date<-"2021-04-06"
bg_colour <- "#fffdeb"

google_font <-"Roboto"
sysfonts::font_add_google(google_font, google_font)
showtext::showtext_auto()

# load data
tuesdata <- tidytuesdayR::tt_load(tt_date)
forest_change <- tuesdata$forest
forest_area <- tuesdata$forest_area
brazil_loss <- tuesdata$brazil_loss

# overall forest area estimate from FAO via wikipedia
session <-bow("https://en.m.wikipedia.org/wiki/List_of_countries_by_forest_area", force = TRUE)

forest_area_year <- scrape(session, query=list(t="semi-soft", per_page=100)) %>%
               html_node(xpath="/html/body/div[1]/div/main/div[3]/div[1]/div/section[2]/table") %>%
               html_table(fill = TRUE) %>%
               filter(Region=="World") %>%
               select(-Region) %>%
               mutate(across(where(is.character), 
                             function(x){1000*as.numeric(str_remove_all(x,","))})) %>%
               unlist(., use.names=TRUE) 
               

# world's maps

world <- gisco_get_countries() %>% filter(!(CNTR_NAME =="Antarctica"))

# population data from UN

data(pop)
a <- ls()[grepl("pop",ls())]
a <- tibble(list=a) %>% 
     filter(!(list %in% c("pop"))) %>%
     pull(list)

pop <- pop %>% mutate(across(where(is.numeric),function(x){x*1000})) %>%
       mutate(name = case_when(
         name=="Bolivia (Plurinational State of)" ~ "Bolivia",
         name=="Brunei Darussalam" ~ "Brunei",
         name=="Dem. Republic of the Congo" ~ "Democratic Republic of Congo",
         name=="Iran (Islamic Republic of)" ~"Iran",
         name=="Lao People's Dem. Republic" ~ "Laos",
         name=="Republic of Moldova" ~ "Moldova",
         name=="Dem. People's Rep. of Korea" ~ "North Korea",
         name=="State of Palestine" ~"Palestine",
         name=="Russian Federation" ~ "Russia",
         name=="Republic of Korea" ~ "South Korea",
         name=="Syrian Arab Republic" ~ "Syria",
         name=="United Republic of Tanzania" ~ "Tanzania",
         name=="Timor-Leste" ~ "Timor",
         name=="United States of America" ~ "United States",
         name=="Venezuela (Bolivarian Republic of)" ~ "Venezuela",
         name=="Viet Nam" ~ "Vietnam",
         TRUE ~ name
       ))
#write_csv(pop,here(tt_date,"pop.csv"))  ## to export to use with python

rm(list=a,"a")


forest_apportion <-forest_area %>% 
                   filter(!is.na(code) & year %in% c(2000,2020)) %>%
                   pivot_wider(values_from = forest_area,names_from = year) %>%
                   left_join(pop %>% select(entity=name,pop_2020=`2020`,pop_2000=`2000`),by="entity") %>%
                   mutate(forest_area2020 = `2020`*forest_area_year[grepl("2020", names(forest_area_year))],
                          forest_area2000 = `2000`*forest_area_year[grepl("2000", names(forest_area_year))],
                          pc2020 = forest_area2020/pop_2020,
                          pc2000 = forest_area2000/pop_2000,
                          diffpc = (pc2020 - pc2000)/pc2000,
                          diffpc_abs = (pc2020 - pc2000),
                          area_diff = (`2020` -`2000`)/`2000`,
                          area_diff_abs=(`2020` -`2000`)
                          )

forest_apportion %>% filter(is.na(pop_2020))

scale1 <- scale_fill_gradient2(low = "#d1b152",
                               mid = "#b6d6ab",
                               high = '#164a06',
                               midpoint = quantile(log(forest_apportion$pc2020),0.5, na.rm=TRUE),
                               na.value = '#cccccc',
                               trans = "log",
                               name = "hectares per capita",
                               breaks=c(0,1,20,500)
                               )

plot_map2020 <- world %>% left_join(forest_apportion %>% filter(!(is.nan(diffpc) | is.na(diffpc))),
                    by=c("ISO3_CODE"="code")) %>%
  ggplot() + geom_sf(aes(fill=pc2020)) +
  coord_sf(expand = FALSE)+
  customthemes::custom_map_theme_md(legend_pos = "bottom",
                                    legend_dir = "horizontal",
                                    background_colour =bg_colour,
                                    plot_margin = c(0,0,0,0)) +
  theme(legend.key.width=unit(1,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.justification = "center") +
  scale1 
  

low_colour <-"bisque2"
high_colour <- '#bad744'


# chart with top 10

top10 <- forest_apportion %>% slice_max(pc2020,n=10) %>% mutate(group="a") %>%
  ggplot() + geom_col(aes(x=fct_reorder(entity,pc2020),y=pc2020,fill=pc2020),
                      width=0.88) +
  coord_flip() +
  customthemes::custom_plot_theme_md(background_colour = bg_colour,legend_pos="none",
                                     plot_margin = c(0,0,0,0)) +
  scale1 + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  labs(subtitle = "Forest area per capita - **Top 10**",
       x= "",
       y="Hectares per capita") 


# look at countries with biggest (positive and negative) changes
# (and numberblocks reference)
# reference : https://towardsdatascience.com/create-dumbbell-plots-to-visualize-group-differences-in-r-3536b7d0a19a

extreme14  <- rbind(forest_apportion %>% filter(!is.na(pc2000) | !is.na(pc2020)) %>% slice_max(diffpc_abs,n=7) %>% mutate(group="Top 7"),
                    forest_apportion  %>%  filter(!is.na(pc2000) | !is.na(pc2020)) %>% slice_min(diffpc_abs,n=7) %>% mutate(group="Bottom 7")) %>%
              arrange(desc(area_diff_abs))%>%
              mutate(Country=as_factor(entity))

colour_2000 <-"#76b6cf"
colour_2020 <-"#054963"

chart_title <- str_c("Per capita changes between \n <i  style='color:",
                     colour_2000,"'> 2000</i> and <i style='color:",
                     colour_2020,
                     "'>2020</i>")
x_min<- min(c(extreme14$pc2000,extreme14$pc2020))
x_max<- max(c(extreme14$pc2000,extreme14$pc2020))
x_breaks <- c(0,1000,2000,3000)
my_x_scale <- scale_x_continuous(limits=c(x_min,x_max),breaks = x_breaks,
                                 labels=function(x) format(round(x,1), big.mark = ",", scientific = FALSE)) 

p1 <- extreme14 %>% filter(group=="Top 7") %>%
              ggplot() +
              geom_dumbbell(aes(y=fct_reorder(Country,diffpc_abs), x=pc2000, xend=pc2020),
                            colour = high_colour,
                            size=3,
                            colour_x = colour_2000, colour_xend = colour_2020) +
  geom_text(aes(y=fct_reorder(Country,diffpc_abs),x=pc2020,label=str_c("+",round(diffpc_abs,1))),
            size=2.75, vjust=0.1,hjust=-0.6) +
             customthemes::custom_plot_theme_md(background_colour = bg_colour,
                                                plot_margin = c(0,0,0,0)) +
             theme(legend.position="none",
                   panel.grid.major.y=element_blank(),
                   panel.grid.minor=element_blank(),
                   panel.border=element_blank(),
                   axis.ticks=element_blank(),
                   axis.text.x=element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_textbox_simple(fill=high_colour,orientation = "left-rotated",halign = 0.5)
                   ) +
              my_x_scale +
             labs(y="**Top 7**",subtitle = chart_title) 
             
             

p2 <- extreme14 %>% filter(group=="Bottom 7") %>% ggplot() +
  geom_dumbbell(aes(y=fct_reorder(Country,diffpc_abs), x=pc2000, xend=pc2020),
                colour = low_colour,
                  size=3,
                colour_x = colour_2000, colour_xend = colour_2020) +
  geom_text(aes(y=fct_reorder(Country,diffpc_abs),x=pc2020,label=str_c("",round(diffpc_abs,1))),
            size=2.75, vjust=0.1,hjust=1.2) +
  customthemes::custom_plot_theme_md(background_colour = bg_colour,
                                     plot_margin = c(0,0,0,0)) +
  theme(legend.position="none",
        panel.grid.major.y =element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y = element_textbox_simple(fill=low_colour,orientation = "left-rotated",halign = 0.5)
  ) +
  my_x_scale +
  labs(x="Hectares per capita", y="**<span align='center'>Bottom 7</span>**")

plot_dumbnell <- p1 / p2 + plot_layout(widths=1)  &
  theme(plot.title =  element_markdown(hjust=0))

#put together

layout <- "
AAAAAA#
AAAAAA#
AAAAAA#
AAAAAA#
AAAAAA#
AAAAAA#
AAAAAA#
AAAAAA#
AAAAAA#
#######
BBB#CCC
BBB#CCC
BBB#CCC
BBB#CCC
BBB#CCC
BBB#CCC
BBB#CCC
BBB#CCC
BBB#CCC
"


plot_title <- '**Forests are not equally distributed**'
plot_subtitle <- "Forests per capita in *2020*"


##get social icons
#if(!icon_installed(ionicons)) download_ionicons()
#github_logo  <- ionicons("logo-github")
#writeLines(toString(github_logo), here(tt_date,"github_logo.svg"))
#rsvg::rsvg_png(here(tt_date,"github_logo.svg"), here(tt_date,"github_logo.png"), width = 800)


plot_caption <-str_c('**Sources** : FAO/Our World in Data, Eurostat (maps), UN Population Division, Wikipedia <br>',
                     add_social_ref("@carlosyanez"))#," ",
                     #add_social_ref("/carlosyanez",12,here(tt_date,"github_logo.png")))


deforestation <- plot_map2020 + top10 + plot_dumbnell + 
                  plot_layout(design = layout, widths = 50) +
                  plot_annotation(title=plot_title,
                                  subtitle=plot_subtitle,
                                  caption=plot_caption) &
    theme(panel.background = element_rect(fill = bg_colour, colour = NA),
          plot.background = element_rect(fill = bg_colour, colour = NA),
          plot.margin=grid::unit(c(1,3,1,3), "mm"),   ## top, right, bottom, left)
          plot.title.position = "plot",
          plot.title = element_markdown(size = 24,family=google_font,
                                        hjust=0),
          plot.subtitle = element_markdown(size = 12,family=google_font,hjust=0),
          plot.caption = element_markdown(size = 10,hjust=0,vjust=0,family=google_font)
    )

wv <- 12
save_image(deforestation,here(tt_date,"deforestation.png"),width=wv,height=wv*0.85)

deforestation


