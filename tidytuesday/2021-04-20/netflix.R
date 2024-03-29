# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-20/readme.md

# make sure that {librarian} and {tidytuesdayR} are there
if(!require(librarian)) install.packages("librarian")

# installing webshot if it is not there, to create raster from table
if(!require(webshot)) {
  install.packages("webshot")
  webshot::install_phantomjs()
  
}

# load other libraries

librarian::shelf("tidyverse",
                 "here",
                 "ggbeeswarm",
                 "patchwork",
                 "ggtext",
                 "showtext",
                 "sysfonts",
                 "ggrepel",
                 "tinter",
                 "arrow",
                 "flextable",
                 "plotly",                    #for interactive version
                 "htmlwidgets",
                 "carlosyanez/customthemes")

# folder reference for here()
tt_date<-"2021-04-20"



# load tt data
if(!file.exists(here(tt_date,"tidytuesday.parquet"))){
  
      if(!require(tidytuesdayR)) install.packages("tidytuesdayR")
  
      tuesdata <- tidytuesdayR::tt_load(tt_date)
      write_parquet(tuesdata$netflix_titles, here(tt_date,"tidytuesday.parquet"),compression = "gzip", compression_level = 5)
      netflix_titles <- tuesdata$netflix_titles
}else{
  netflix_titles <- read_parquet(here(tt_date,"tidytuesday.parquet"))
}


#load ratings
if(file.exists(here(tt_date,"ratings.parquet"))){
  #ratings <- readRDS(here(tt_date,"ratings.rds"))
  ratings <- read_parquet(here(tt_date,"ratings.parquet"))
  
}else{
  librarian::shelf("imdbapi")
  ratings <-map_df(netflix_titles$title,function(x){
              message(x)
              result <- find_by_title(title=x)
              if(nrow(result)>0) result
               })
  #saveRDS(ratings,here(tt_date,"ratings.rds"))
  write_parquet(ratings, here(tt_date,"ratings.parquet"),compression = "gzip", compression_level = 5)
}


  
#ratings <- ratings %>% unnest(Ratings)

#just keep imdb ratings and language

ratings <- ratings  %>% select(Title,Language, Country, imdbRating) %>% unique(.)

# join with netflix list, remove NAs

netflix_catalogue <- netflix_titles  %>%
                     left_join(ratings,by=c("title"="Title")) %>%
                     filter(!is.na(imdbRating)) %>%
                     separate(Country, c("country_main","other_countries"), sep = ',') %>%
                     replace_na(list(country_main="NA")) %>%
                     select(-other_countries) %>%
                     separate_rows(listed_in,sep=",") %>%
                     mutate(listed_in=str_remove(listed_in,"TV Shows"),
                            listed_in=str_remove(listed_in,"TV"),
                            listed_in=str_remove(listed_in,"Movies")) %>%
                     mutate(listed_in=str_squish(listed_in)) %>%
                     filter(!(listed_in %in% c("Spanish-Language","International","British","Korean"))) %>%
                     select(-rating) %>%
                     rename(rating=imdbRating)
  
  


country_stats <- netflix_catalogue %>%
                 group_by(country_main,listed_in) %>% 
                 summarise(n=n(),median=median(rating),range=max(rating)-min(rating), .groups = "drop") %>%
                 mutate(group_id=row_number(),
                        label=str_c(country_main,"\n",listed_in),
                        label_inter = str_c("<b>",country_main," - ",listed_in,"\n Titles:</b> ", n,
                                            "\n <b>Median: </b>",median,"\n <b>Range:</b> ", range)) %>%
                 filter(!(country_main=="N/A")) 
                 

#added AFTER initial exploration

categories_to_higlight <-tibble(group_id=c(331,562,691,652,257,701,103)) %>%
                          mutate(order=row_number())



#table with best and worst rated films in selected categories

summary_table <- netflix_catalogue %>%
  left_join(country_stats,by=c("country_main","listed_in")) %>%
  inner_join(categories_to_higlight, by="group_id") %>%
  group_by(order) %>%
  mutate(best_worst=case_when(
    rating==min(rating) ~ "Worst",
    rating==max(rating) ~ "Best",
    TRUE ~ "Other"    
  ))%>% ungroup() %>%  
  filter(best_worst %in% c("Best","Worst"))

summary_table<- summary_table %>% 
  filter(best_worst=="Best") %>%
  select(country_main,listed_in,best=title,best_rating=rating,order) %>%
  group_by(country_main,listed_in,best_rating,order) %>%
  summarise(best=str_c(best,collapse = ", "),.groups = "drop") %>%
  left_join(summary_table %>% 
              filter(best_worst=="Worst") %>%
              select(country_main,listed_in,worst=title,worst_rating=rating,order,) %>%
              group_by(country_main,listed_in,worst_rating,order) %>%
              summarise(worst=str_c(worst,collapse = ", "),.groups="drop"),
            by=c("country_main","listed_in","order")) %>%
  arrange(order) %>% 
  mutate(`Country-Genre`=str_c(country_main,"\n",listed_in),
         `Best Rated`=str_c(best," (",best_rating,")"),
         `Worst Rated`=str_c(worst," (",worst_rating,")"),
         .keep="none")


## Theming

font_name <- "Source Sans Pro"
sysfonts::font_add_google(font_name, font_name)
showtext::showtext_auto()

#colours
# from  https://codepen.io/claudio_bonfati/pen/mdryxPv
netflix_palette <- c("#8cabed","#ffde01","#ff00bf","#04fd8f","#0078fe","#d8f806","#ffae01","#01ffff")
palette1 <- netflix_palette[1:nrow(categories_to_higlight)]

bg_colour <- "black"
text_0_colour <- "azure4"
text_1_colour <- "white"
netflix_red <- "#E50914"


#basic theme
nf_theme      <-   custom_plot_theme_md(background_colour = bg_colour,
                                        plot_margin = c(0,0,0,0),
                                        title_colour = text_1_colour, google_font=font_name,
                                        general_colour = text_0_colour) +
                   theme(panel.grid.major =element_blank(),
                         panel.grid.minor=element_blank(),
                         panel.border=element_blank(),
                         legend.text = element_text(colour=text_1_colour, family = font_name),
                         legend.title = element_text(colour=text_1_colour, family = font_name)
                        ) 


#arrow shapes

my_arrow <- arrow(angle = 30, length = unit(0.09, "inches"),
                  ends = "first", type = "open")

#big plot
titles_filter <- 15

p1.0 <- ggplot() + 
  geom_point(data=(country_stats %>% filter(n>titles_filter & !(group_id %in% categories_to_higlight$group_id))),
             aes(x=median,y=range,size=n,text=label_inter),
             alpha=0.6,colour=netflix_red) +
  geom_point(data=(country_stats %>% filter(group_id %in% categories_to_higlight$group_id)),
             aes(x=median,y=range,size=n,colour=label,text=label_inter),
             alpha=1) +
  scale_colour_manual(values=palette1)+
  scale_x_continuous(name="median rating",breaks = seq(min(country_stats$median),max(country_stats$median),0.5))+
  scale_size_binned(name="Number of titles",breaks=c(0,20,30,50,100,200)) +
  scale_y_continuous(name="rating range",breaks = seq(min(country_stats$range),max(country_stats$range),0.5)) +
  guides(col=FALSE,
         size=guide_legend(override.aes=list(colour=netflix_red)),
                           title.theme = element_text(colour = text_1_colour,size=6)) +
  nf_theme

set.seed(123)
p1.1 <- p1.0 + geom_text_repel(data=(country_stats %>% filter(group_id %in% categories_to_higlight$group_id)),
                               aes(x=median,y=range,label=label),
                               colour=text_1_colour,
                               size=3.5,
                               force=100,
                               direction ="both",
                               nudge_x=-0.1,
                               point.padding = 0.1,
                               box.padding = 0.5,
                               arrow=my_arrow,
                               segment.curvature = -0.2,
                               segment.ncp = 1,
                               segment.angle = 60) 



# ggplotly(p1)


# category plot


nf_theme2 <- nf_theme + theme(legend.position="none",
                              axis.text.y = element_text(colour=text_1_colour,size=12)) 

p2_subtitle <- "  Ratings across selected categories"
p2 <- netflix_catalogue %>%
  left_join(country_stats,by=c("country_main","listed_in")) %>%
  inner_join(categories_to_higlight, by="group_id")  %>%
  mutate(label_inter=str_c("<b>",title,"</b>\n <b>Rating:</b> ",rating)) %>%
  select(label_inter,order,title,rating,label) %>%
  ggplot(aes(x=fct_reorder(label,desc(order)),y=rating,colour=label,text=label_inter)) +
  geom_beeswarm(alpha=0.5) +
  scale_colour_manual(values=palette1)+
  coord_flip() +
  labs(y="Rating",x="",
       subtitle=p2_subtitle) +
  nf_theme2
  



table <- summary_table %>%
         flextable() %>%
         border_remove() %>%
         theme_vader() %>%
         bg(bg=bg_colour,part="all") %>%
         font(fontname = font_name,part="all") %>%
         color(color=text_0_colour,part="header") %>%
         color(color=text_1_colour,part="body") %>%
         fontsize(size=28,part="all") %>%
         height(height=20, part = "body") %>%
         width(j=1,width=12) %>%
         width(j=2:3,width=30) %>%
         as_raster(zoom=3,webshot = "webshot")

Sys.sleep(2)

p3 <- ggplot() + 
     annotation_raster(table,0,1,0,1, interpolate = TRUE) +
     labs(subtitle=" ")

Sys.sleep(2)
#putting everything together     


plot_title <- str_c("What to watch on <span style='color:",netflix_red,"'> NETFLIX</span>?")
plot_subtitle <- str_c("*Comparison of rating statistics (median and range) for content in 2019's catalogue, grouped by Country and Genre*")
plot_caption <-str_c(str_c('**Sources:** Shivam Bansal via Kaggle and tidytuesday, OMDb API'),
                     '<br><br>',
                     add_social_ref("@carlosyanez"))

plot_caption_inter <-'<b>Sources:<b> <a href="https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-20/readme.md">Shivam Bansal via Kaggle and tidytuesday</a>, <a href="https://www.omdbapi.com/">OMDb API</a>'


p <- p1.1 / (p2 + p3) + plot_annotation(title=plot_title,subtitle = plot_subtitle,caption = plot_caption) & 
        theme(plot.title.position = "plot",
        plot.title = element_markdown(size = 18,family=font_name,face="bold",
                                      hjust=0,colour=text_1_colour),
        plot.subtitle = element_markdown(size = 12,family=font_name,hjust=0,
                                         colour=darken(text_1_colour,0.08)),
        plot.caption = element_markdown(size = 10,hjust=0,vjust=0,family=font_name,
                                        colour=darken(text_1_colour,0.08)))


p[[1]] <- p[[1]] + nf_theme
p[[2]] <- p[[2]] + nf_theme2
p[[3]] <- p[[3]] + custom_map_theme_md(background_colour = bg_colour) 

p <- p & theme(plot.background = element_rect(fill=bg_colour,colour=bg_colour))


##save on file
wv <- 12
save_image(p,here(tt_date,"netflix_categories.png"),width=wv,height=wv*0.85) 



#interactive
p1_inter <- ggplotly(p1.0, tooltip = c("text")) 
p2_inter <- ggplotly(p2, tooltip = c("text")) %>% layout(paper_bgcolor=bg_colour,
                                                         plot_bgcolor=bg_colour,
                                                         title=list(text=str_c("<sup>",
                                                                               p2_subtitle,
                                                                               "</sup>"),
                                                                    font=list(color=text_1_colour),
                                                                    x=0
                                                         )
)

p3_inter <- ggplotly(p3)
  


part_2 <- subplot(p2_inter,p3_inter)

p_inter <- subplot(p1_inter,part_2,nrows = 2) %>% 
           layout(paper_bgcolor=bg_colour,
                  plot_bgcolor=bg_colour,
                  title=list(text=str_c(plot_title,"\n",
                                        "<sup>",
                                        str_remove_all(plot_subtitle,"\\*"),"\n",
                                        plot_caption_inter,
                                        "</sup>"),
                              font=list(color=text_1_colour),
                              x=0
                              ),
                  margin=list(t = 75)) %>%
          config(displayModeBar = FALSE)





#pointing to local folder of my github.io repo - modify accordingly
io_repo <- str_c(here() %>% str_remove("Snippets/tidytuesday"),"carlosyanez.github.io/")

saveWidget(as_widget(p_inter), str_c(io_repo,"netflix_categories.html"),
           selfcontained = FALSE,
           libdir = "lib",
           background = bg_colour,
           title ="What to watch on Netflix?")

